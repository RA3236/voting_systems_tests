use std::usize;

use charming::{
    Chart, ImageRenderer,
    component::{Axis, Title, VisualMap, VisualMapChannel},
    df,
    element::{AxisType, Orient},
    series::Heatmap,
};
use kiddo::{KdTree, SquaredEuclidean};
use methods::{Method, MethodFn, VectorMethod};
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use rustc_hash::FxHashMap;

use crate::{CONTEXT, Message, Sender, runtime_settings::common::StandardData};

use super::{Distribution, ScoreHandle};

#[derive(Clone, Debug, PartialEq)]
pub struct VisualizeDistribution {
    pub(crate) num_voters: usize,
    pub(crate) num_candidates: usize,
    pub(crate) num_iterations: usize,
    pub(crate) num_voters_string: String,
    pub(crate) num_candidates_string: String,
    pub(crate) num_iterations_string: String,
}

impl Default for VisualizeDistribution {
    fn default() -> Self {
        Self {
            num_voters: 5000,
            num_candidates: 6,
            num_iterations: 10000,
            num_voters_string: 5000usize.to_string(),
            num_candidates_string: 6usize.to_string(),
            num_iterations_string: 10000usize.to_string(),
        }
    }
}

impl VisualizeDistribution {
    pub fn show(&mut self, name: &'static str, ui: &mut egui::Ui) {
        ui.heading(name);

        ui.label("Simulate and visualize the distribution of winners");
        egui::Grid::new(format!("{name} grid")).show(ui, |ui| {
            for (n, v, vs) in [
                (
                    "Number of voters",
                    &mut self.num_voters,
                    &mut self.num_voters_string,
                ),
                (
                    "Number of candidates",
                    &mut self.num_candidates,
                    &mut self.num_candidates_string,
                ),
                (
                    "Number of iterations",
                    &mut self.num_iterations,
                    &mut self.num_iterations_string,
                ),
            ] {
                ui.label(n);
                crate::util::typed_textbox(v, vs, ui, 1, usize::MAX);
                ui.end_row();
            }
        });
    }

    pub fn simulate(
        &mut self,
        sender: &Sender,
        methods: &FxHashMap<Method, Vec<ScoreHandle>>,
        distr: &Distribution,
    ) {
        // Clone stuff for rayon sendoff
        let sender = sender.clone();
        let settings = self.clone();
        let distr = distr.clone();
        let methods = methods.clone();
        rayon::spawn(move || {
            // Setup voters and candidates
            let num_iters = settings.num_iterations;
            let main = StandardData::new(
                distr,
                settings.num_voters,
                settings.num_candidates,
                settings.num_iterations,
            );

            // Label ticks for chart
            let x = (0..=100)
                .into_iter()
                .map(|i| i as f64 / 100.0)
                .collect::<Vec<_>>();
            let y = (0..=100)
                .into_iter()
                .map(|i| i as f64 / 100.0)
                .collect::<Vec<_>>();

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
                .map(|(name, func)| -> (_, i64, String) {
                    let results = (0..num_iters)
                        .into_par_iter()
                        .map(|i| {
                            let populace = main.create_populace(i);
                            func(&populace).unwrap()
                        })
                        .collect::<Vec<_>>();

                    // Convert into KDTree
                    let tree = KdTree::from(&results);

                    // Convert into chart data format
                    let mut max = 0;
                    let z = x
                        .iter()
                        .flat_map(|x| {
                            y.iter()
                                .copied()
                                .map(|y| {
                                    // As an ad-hoc density function, we count the number of points within a squared distance
                                    let count = tree
                                        .within_unsorted_iter::<SquaredEuclidean>(&[*x, y], 0.03)
                                        .count()
                                        as i64;
                                    max = max.max(count);
                                    df![x.to_string(), y.to_string(), count]
                                })
                                .collect::<Vec<_>>()
                        })
                        .collect::<Vec<_>>();
                    (z, max, name)
                })
                .chain(
                    [
                        ("Voters".to_string(), vx.into_par_iter().zip(vy)),
                        ("Candidates".to_string(), cx.into_par_iter().zip(cy)),
                    ]
                    .into_par_iter()
                    .map(|(name, iter)| {
                        let z = iter.map(|(&x, &y)| [x, y]).collect::<Vec<_>>();
                        let tree = KdTree::from(&z);

                        let mut max = 0;
                        let z = x
                            .iter()
                            .flat_map(|x| {
                                y.iter()
                                    .copied()
                                    .map(|y| {
                                        let count = tree
                                            .within_unsorted_iter::<SquaredEuclidean>(
                                                &[*x, y],
                                                0.01,
                                            )
                                            .count()
                                            as i64;
                                        max = max.max(count);
                                        df![x.to_string(), y.to_string(), count]
                                    })
                                    .collect::<Vec<_>>()
                            })
                            .collect::<Vec<_>>();
                        // let name = name.to_string();
                        (z, max, name)
                    }),
                )
                .collect::<Vec<_>>();

            // Plot the distributions
            for (z, max, name) in method_data {
                let chart = Chart::new()
                    .series(Heatmap::new().data(z).name(&name))
                    .visual_map(
                        VisualMap::new()
                            .calculable(true)
                            .left("center")
                            .orient(Orient::Horizontal)
                            .min(0)
                            .max(max as f64)
                            .in_range(VisualMapChannel::new().color(vec![
                                "#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#ffffbf",
                                "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026",
                            ])),
                    )
                    .x_axis(
                        Axis::new()
                            .data(x.iter().map(f64::to_string).collect())
                            .type_(AxisType::Category),
                    )
                    .y_axis(
                        Axis::new()
                            .data(x.iter().map(f64::to_string).collect())
                            .type_(AxisType::Category),
                    )
                    .title(
                        Title::new()
                            .left("center")
                            .text(format!("{name} (Distribution)")),
                    )
                    .background_color("#FFFFFF");

                let mut renderer = ImageRenderer::new(400, 400);
                
                let (save_path, uri) = super::get_file_and_uri(super::SAVE_VISUALIZE, "distribution.svg");
                renderer.save(&chart, save_path).unwrap();

                sender
                    .send(Message::ImageFilePath(uri))
                    .unwrap();
                CONTEXT.get().inspect(|c| c.request_repaint());
            }
            sender.send(Message::Unlock).unwrap();
            println!("finished");
        });
    }
}
