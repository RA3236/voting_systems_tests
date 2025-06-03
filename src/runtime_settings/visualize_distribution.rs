use std::usize;

use internment::ArcIntern;
use kiddo::SquaredEuclidean;
use methods::{Method, VectorMethod};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use rustc_hash::FxHashSet;

use crate::{CONTEXT, Message, MessageSender, runtime_settings::common::StandardData, util::input};

use super::DistributionManager;

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
            // for (n, v) in [
            //     ("Number of voters", &mut self.num_voters),
            //     ("Number of candidates", &mut self.num_candidates),
            //     ("Number of iterations", &mut self.num_iterations),
            // ] {
            //     ui.label(n);
            //     ui.add(egui::DragValue::new(v).range(1..=usize::MAX));
            //     ui.end_row();
            // }
            input!(
                ui,
                "Number of voters",
                self.num_voters,
                usize,
                "Number of candidates",
                self.num_candidates,
                usize,
                "Number of iterations",
                self.num_iterations,
                usize
            );
        });
    }

    pub fn simulate(
        &mut self,
        sender: &MessageSender,
        methods: &FxHashSet<Method>,
        distr: &DistributionManager,
    ) {
        // Clone stuff for rayon sendoff
        let sender = sender.clone();
        let settings = self.clone();
        let distr = distr.clone();
        let methods = methods.clone();
        let name_arc: ArcIntern<str> = ArcIntern::from("Visualize Distribution");
        sender
            .send(Message::TotalProgress(
                name_arc.clone(),
                settings.num_iterations * methods.len(),
            ))
            .unwrap();
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
                .map(|method| (method.name(), method.func()))
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
                            sender
                                .send(Message::IncrementProgress(name_arc.clone()))
                                .unwrap();
                            func(&populace).unwrap()
                        })
                        .collect::<Vec<_>>();
                    (name, results)
                })
                .chain(
                    [
                        ("Voters", vx.into_iter().zip(vy)),
                        ("Candidates", cx.into_iter().zip(cy)),
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
