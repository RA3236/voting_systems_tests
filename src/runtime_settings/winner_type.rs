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

use crate::{
    Message, MessageSender,
    runtime_settings::common::{StandardData, WinnerType},
};

use super::{DistributionManager, visualize_distribution::VisualizeDistribution};

#[derive(Clone, Debug, PartialEq)]
pub struct WinnerTypeSettings {
    inner: VisualizeDistribution,
}

impl Default for WinnerTypeSettings {
    fn default() -> Self {
        Self {
            inner: VisualizeDistribution::default(),
        }
    }
}

impl WinnerTypeSettings {
    pub fn show(&mut self, ui: &mut egui::Ui) {
        self.inner.show("Winner Type Settings", ui);
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

        let name_arc: ArcIntern<str> = ArcIntern::from("Winner Types");
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

                            sender
                                .send(Message::IncrementProgress(name_arc.clone()))
                                .unwrap();
                            if let Some(condorcet_winner) = condorcet_winner {
                                if this_winner == condorcet_winner {
                                    if this_winner == average_winner {
                                        WinnerType::CondorcetAndAverage
                                    } else {
                                        WinnerType::CondorcetButNotAverage
                                    }
                                } else if this_winner == average_winner {
                                    WinnerType::NotCondorcetButAverage
                                } else {
                                    WinnerType::NotCondorcetAndNotAverage
                                }
                            } else if this_winner == average_winner {
                                WinnerType::NoCondorcetButAverage
                            } else {
                                WinnerType::NoCondorcetAndNotAverage
                            }
                        })
                        .collect::<Vec<_>>();
                    (i, results)
                })
                .collect::<Vec<_>>();

            // Reformat into correct order
            let data = WinnerType::iter()
                .map(|ty| {
                    let mut counts = vec![0i64; method_names.len()];

                    method_data.iter().for_each(|(i, tys)| {
                        tys.iter().copied().for_each(|ty2| {
                            if ty == ty2 {
                                counts[*i] += 1;
                            }
                        });
                    });
                    (ty.to_str(), counts)
                })
                .collect::<Vec<_>>();

            // Create the chart
            let mut chart = Chart::new()
                .legend(Legend::new().top("bottom").left("center").show(true))
                .x_axis(Axis::new().type_(AxisType::Value))
                .y_axis(Axis::new().type_(AxisType::Category).data(method_names))
                .grid(Grid::new().left("center").top("middle"))
                .title(Title::new().text("Winner Types").left("center"))
                .background_color("#ffffff");
            for (name, data) in data {
                chart = chart.series(
                    Bar::new()
                        .name(name)
                        .stack("total")
                        .bar_width("80%")
                        .data(data),
                )
            }

            let mut renderer = ImageRenderer::new(800, 500);

            let (save_path, uri) =
                super::get_file_and_uri(super::SAVE_WINNER_TYPE, "winner types.svg");

            renderer.save(&chart, save_path).unwrap();
            sender
                .send(Message::ImageStandard(
                    ArcIntern::from("Winner Types"),
                    ArcIntern::from("Winner Types"),
                    uri.into(),
                ))
                .unwrap();
            sender.send(Message::SimulationFinished).unwrap();
        });
    }
}
