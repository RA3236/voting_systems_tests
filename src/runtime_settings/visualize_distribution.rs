use std::{
    sync::{Arc, atomic::AtomicUsize},
    usize,
};

use charming::{
    Chart, ImageRenderer,
    component::{Axis, Title, VisualMap, VisualMapChannel},
    df,
    element::{AxisType, ItemStyle, Orient},
    series::{Heatmap, Scatter},
};
use internment::ArcIntern;
use kiddo::{KdTree, SquaredEuclidean};
use methods::{Method, MethodFn, VectorMethod};
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use rustc_hash::FxHashMap;

use crate::{CONTEXT, Message, MessageSender, runtime_settings::common::StandardData};

use super::{DistributionManager, ScoreHandle};

#[derive(Clone, Debug, PartialEq)]
pub struct VisualizeDistribution {
    pub(crate) num_voters: usize,
    pub(crate) num_candidates: usize,
    pub(crate) num_iterations: usize,
}

impl Default for VisualizeDistribution {
    fn default() -> Self {
        Self {
            num_voters: 5000,
            num_candidates: 6,
            num_iterations: 10000,
        }
    }
}

impl VisualizeDistribution {
    pub fn show(&mut self, name: &'static str, ui: &mut egui::Ui) {
        ui.heading(name);

        ui.label("Simulate and visualize the distribution of winners");
        egui::Grid::new(format!("{name} grid")).show(ui, |ui| {
            for (n, v) in [
                ("Number of voters", &mut self.num_voters),
                ("Number of candidates", &mut self.num_candidates),
                ("Number of iterations", &mut self.num_iterations),
            ] {
                ui.label(n);
                ui.add(egui::DragValue::new(v).range(1..=usize::MAX));
                ui.end_row();
            }
        });
    }

    pub fn simulate(
        &mut self,
        sender: &MessageSender,
        methods: &FxHashMap<Method, Vec<ScoreHandle>>,
        distr: &DistributionManager,
        progress: Arc<AtomicUsize>,
        total: Arc<AtomicUsize>,
    ) {
        // Clone stuff for rayon sendoff
        let sender = sender.clone();
        let settings = self.clone();
        let distr = distr.clone();
        let methods = methods.clone();
        total.store(
            settings.num_iterations * methods.len(),
            std::sync::atomic::Ordering::SeqCst,
        );
        rayon::spawn(move || {
            // Setup voters and candidates
            let num_iters = settings.num_iterations;
            let main = StandardData::new(
                distr,
                settings.num_voters,
                settings.num_candidates,
                settings.num_iterations,
            );

            // Get method names and functions
            let methods = methods
                .into_iter()
                .flat_map(|(method, score_values)| {
                    score_values.into_iter().map(move |v| {
                        (
                            match method.is_score() {
                                true => format!("{} ({v})", method.name()),
                                false => method.name().to_string(),
                            },
                            match v {
                                ScoreHandle::Full => method.func() as MethodFn<[f64; 2]>,
                                ScoreHandle::Custom(v, _) => method.score_fn(v).unwrap(),
                            },
                        )
                    })
                })
                .collect::<Vec<_>>();

            // Add the voters and candidates
            let ((vx, vy), (cx, cy), _, _, _) = main.into_data();

            // Perform simulations
            let method_data = methods
                .into_par_iter()
                .map(|(name, func)| {
                    let results = (0..num_iters)
                        .into_par_iter()
                        .map(|i| {
                            let populace = main.create_populace(i);
                            progress.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                            func(&populace).unwrap()
                        })
                        .collect::<Vec<_>>();
                    (name, results)
                })
                .chain(
                    [
                        ("Voters".to_string(), vx.into_iter().zip(vy)),
                        ("Candidates".to_string(), cx.into_iter().zip(cy)),
                    ]
                    .into_par_iter()
                    .map(|(name, iter)| {
                        let results = iter.map(|(&x, &y)| [x, y]).collect::<Vec<_>>();
                        (name, results)
                    }),
                )
                .collect::<Vec<_>>();

            // Plot the distributions
            for (name, results) in method_data {
                let (save_file, uri) =
                    super::get_file_and_uri(super::SAVE_VISUALIZE, &format!("{name}.svg"));
                super::common::plot_density::<SquaredEuclidean, _>(
                    results,
                    &sender,
                    &name,
                    &save_file,
                    &uri,
                    "Visualizations",
                );
                CONTEXT.get().inspect(|c| c.request_repaint());
            }
            sender.send(Message::SimulationFinished).unwrap();
        });
    }
}
