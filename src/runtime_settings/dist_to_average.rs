use std::simd::{Simd, num::SimdFloat};

use charming::{
    Chart, ImageRenderer,
    component::{Axis, Grid, Legend, Title},
    element::AxisType,
    series::Bar,
};
use internment::ArcIntern;
use methods::Method;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use rustc_hash::FxHashSet;

use crate::{Message, MessageSender, runtime_settings::common::StandardData};

use super::{DistributionManager, visualize_distribution::VisualizeDistribution};

#[derive(Clone, Debug, PartialEq)]
pub struct DistanceSettings {
    inner: VisualizeDistribution,
}

impl Default for DistanceSettings {
    fn default() -> Self {
        Self {
            inner: VisualizeDistribution::default(),
        }
    }
}

impl DistanceSettings {
    pub fn show(&mut self, ui: &mut egui::Ui) {
        self.inner.show("Distance Settings", ui);
    }

    pub fn simulate(
        &mut self,
        sender: &MessageSender,
        methods: &FxHashSet<Method>,
        distr: &DistributionManager,
    ) {
        let sender = sender.clone();
        let settings = self.inner.clone();
        let distr = distr.clone();
        let methods = methods.clone();
        let name: ArcIntern<str> = ArcIntern::from("Distance to Average");
        sender
            .send(Message::TotalProgress(
                name.clone(),
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
            let (method_names, funcs) = methods
                .into_iter()
                .map(|method| (method.name(), method.func()))
                .collect::<(Vec<_>, Vec<_>)>();

            let average = methods::average::average(&main).unwrap();

            // Perform simulations
            let method_data = funcs
                .into_par_iter()
                .enumerate()
                .map(|(i, func)| {
                    let results = (0..num_iters)
                        .into_par_iter()
                        .map(|i| {
                            let populace = main.create_populace(i);
                            let this_winner = func(&populace).unwrap();
                            let average_winner = average[i];
                            let condorcet_winner =
                                methods::condorcet::condorcet(&populace).unwrap();

                            let this_winner = Simd::from_array(this_winner);
                            let average_winner = Simd::from_array(average_winner);
                            let condorcet_winner = condorcet_winner.map(Simd::from_array);

                            sender
                                .send(Message::IncrementProgress(name.clone()))
                                .unwrap();
                            (
                                ((this_winner - average_winner) * (this_winner - average_winner))
                                    .reduce_sum()
                                    .sqrt(),
                                condorcet_winner.map(|condorcet_winner| {
                                    ((this_winner - condorcet_winner)
                                        * (this_winner - condorcet_winner))
                                        .reduce_sum()
                                        .sqrt()
                                }),
                            )
                        })
                        .collect::<Vec<_>>();
                    (i, results)
                })
                .collect::<Vec<_>>();

            // Reformat into the correct type
            let mut data = vec![vec![0.0f64; method_names.len()]; 2];
            let mut counts = vec![vec![0usize; method_names.len()]; 2];
            method_data.into_iter().for_each(|(i, method_data)| {
                method_data.into_iter().for_each(|(av, cond)| {
                    data[0][i] += av;
                    counts[0][i] += 1;
                    cond.inspect(|&v| {
                        data[1][i] += v;
                        counts[1][i] += 1;
                    });
                })
            });
            for x in 0..2 {
                for y in 0..method_names.len() {
                    data[x][y] /= counts[x][y] as f64;
                }
            }

            let mut chart = Chart::new()
                .legend(Legend::new().top("bottom").left("center").show(true))
                .x_axis(Axis::new().type_(AxisType::Value))
                .y_axis(Axis::new().type_(AxisType::Category).data(method_names))
                .grid(Grid::new().left("center").top("middle"))
                .title(
                    Title::new()
                        .text("Average Distances")
                        .left("center")
                        .subtext("to certain points"),
                )
                .background_color("#ffffff");
            for (data, name) in data.into_iter().zip(["Average voter", "Condorcet winner"]) {
                chart = chart.series(Bar::new().name(name).data(data))
            }

            let mut renderer = ImageRenderer::new(800, 500);
            let (save_file, uri) = super::get_file_and_uri(super::SAVE_DISTANCES, "distances.svg");

            renderer.save(&chart, save_file).unwrap();
            sender
                .send(Message::ImageStandard(
                    ArcIntern::from("Distances"),
                    ArcIntern::from("Average Distances"),
                    uri.into(),
                ))
                .unwrap();
            sender.send(Message::SimulationFinished).unwrap();
        });
    }
}
