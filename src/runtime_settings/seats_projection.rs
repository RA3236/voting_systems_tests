use std::time::{Duration, Instant};

use charming::{
    Chart, ImageRenderer,
    component::{Axis, Grid, Legend, Title},
    datatype::{DataPoint, DataPointItem},
    element::{
        AxisType, Color, ItemStyle, Label, LineStyle, MarkLine, MarkLineData, MarkLineVariant,
        Symbol,
    },
    series::{Bar, Scatter},
};
use egui::{Align, Color32, FontSelection, Layout, RichText, UiBuilder, text::LayoutJob};
use internment::ArcIntern;
use kiddo::SquaredEuclidean;
use methods::{Method, PopulaceMethod};
use rand::{Rng, SeedableRng, rngs::SmallRng};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use rustc_hash::FxHashSet;

use crate::{Message, util::input};

#[derive(Clone, Debug, PartialEq)]
pub struct SeatsProjectionSettings {
    num_voters: usize,
    num_parties: usize,
    num_seats: usize,
    num_iterations: usize,

    watch_method: Method,

    max_offset: f64,

    save_winner_types_to_file: bool,
}

impl Default for SeatsProjectionSettings {
    fn default() -> Self {
        Self {
            num_voters: 5000,
            num_parties: 6,
            num_seats: 151,
            num_iterations: 1000,
            watch_method: Method::default(),
            max_offset: 0.25,
            save_winner_types_to_file: true,
        }
    }
}

impl SeatsProjectionSettings {
    pub fn show(&mut self, available_methods: &FxHashSet<Method>, ui: &mut egui::Ui) {
        ui.heading("Seat Projection Settings");

        ui.label("Simulate elections and collect seat data");
        ui.label(
            RichText::new(
                "Keep iteration + seat count where they are unless you want to wait for hours!",
            )
            .color(Color32::RED),
        );

        egui::Grid::new("Seat Projection grid").show(ui, |ui| {
            input!(ui,
                "Number of voters per seat", self.num_voters, usize,
                "Number of parties", self.num_parties, usize,
                "Number of seats", self.num_seats, usize,
                "NUmber of iterations", self.num_iterations, usize
            );
            // input!("Number of voters per seat", self.num_voters, ui, 1, usize::MAX);
            // input!("Number of parties", self.num_parties, ui, 1, usize::MAX);
            // input!("Number of seats", self.num_seats, ui, 1, usize::MAX);
            // input!("Number of iterations", self.num_iterations, ui, 1, usize::MAX);

            ui.vertical(|ui| {
                ui.label("Method to watch");
                ui.small("When simulating, if this method results in a change in potential governing coalitions compared to proportional voting, then those differences will be shown");
            });
            ui.add_enabled_ui(!available_methods.is_empty(), |ui| {
                if !available_methods.contains(&self.watch_method) {
                    if available_methods.len() > 0 {
                        self.watch_method = available_methods.iter().copied().next().unwrap();
                    }
                }
                egui::ComboBox::new("watch method", "")
                    .selected_text(self.watch_method.name())
                    .show_ui(ui, |ui| {
                        for method in available_methods.iter().copied() {
                            let name = method.name();
                            ui.selectable_value(&mut self.watch_method, method, name);
                        }
                    });
                });
            ui.end_row();

            ui.vertical(|ui| {
                ui.label("Max voter caucus offset");
                ui.small("The voter caucus will be generated per seat based on dividing this value and moving towards either end of the spectrum");
            });
            //crate::util::typed_textbox(&mut self.max_offset, &mut self.max_offset_string, ui, 0.0, 0.75);
            ui.add(
                egui::DragValue::new(&mut self.max_offset)
                    .range(0.0..=0.75)
            );
            ui.end_row();

            ui.label("Save winner type charts to directory");
            ui.checkbox(&mut self.save_winner_types_to_file, "");
        });

        let style = ui.style();
        let mut job = LayoutJob::default();
        RichText::new("Will save all charts to ").append_to(
            &mut job,
            &style,
            FontSelection::default(),
            Align::Min,
        );
        RichText::new("output/seat_projections").code().append_to(
            &mut job,
            style,
            FontSelection::default(),
            Align::Min,
        );

        ui.scope_builder(
            UiBuilder::new().layout(Layout::top_down(Align::Min).with_cross_justify(false)),
            |ui| {
                ui.label(job);
            },
        );
    }

    pub fn simulate(
        &self,
        sender: &std::sync::mpsc::Sender<crate::Message>,
        methods: &FxHashSet<Method>,
        distrs: &super::DistributionManager,
    ) {
        let sender = sender.clone();
        let methods = methods.clone();
        let distr = distrs.clone();

        let num_voters = self.num_voters;
        let num_parties = self.num_parties;
        let num_seats = self.num_seats;
        let num_iterations = self.num_iterations;

        let watch_method = self.watch_method;

        let max_offset = self.max_offset;
        let _save_to_file = self.save_winner_types_to_file;

        let name: ArcIntern<str> = ArcIntern::from("Seat Projections");
        sender
            .send(Message::TotalProgress(name.clone(), num_iterations))
            .unwrap();
        rayon::spawn(move || {
            // Get method names and functions
            let (mut method_names, methods) = methods
                .into_iter()
                .map(|method| (method.name(), method.func()))
                .collect::<(Vec<_>, Vec<_>)>();
            method_names.push("Proportional");

            // Sender/reciever for first iteration
            let (results_sender, results_receiver) =
                std::sync::mpsc::channel::<(usize, Vec<usize>, Vec<Vec<i64>>, Vec<Vec<f64>>)>();

            // Determine method index of watched method
            let mut watch_method_index = 0;
            for i in 0..method_names.len() {
                if method_names[i] == watch_method.name() {
                    watch_method_index = i;
                    break;
                }
            }

            let methods_len = method_names.len();
            let len_wo_prop = methods_len - 1;
            let main_data = SeatsProjectionMainData::new(
                distr,
                max_offset,
                num_voters,
                num_parties,
                num_seats,
                num_iterations,
            );
            // Perform all iterations
            let iteration_results = (0..num_iterations)
                .into_par_iter()
                .map_with(
                    (
                        // Main Results
                        vec![Vec::new(); num_seats],
                        // Proportional votes
                        vec![0; num_parties],
                        // Main results data
                        vec![vec![0i64; methods_len]; num_parties],
                        // Sainte-Lague Allocated seats
                        vec![0i64; num_parties],
                    ),
                    |(main_results, proportional_votes, main_results_data, allocated_seats),
                     iter| {
                        // We need to calculate multiple things here:
                        // 1. The average voter for each round
                        // 2. The Condorcet winner for each round
                        // 3. The seat winner for each method
                        // Note that unlike the usual simulations, the average voter is different every seat
                        // We store the results in three different
                        let main_data = main_data.get_iter_data(iter);

                        // Collect results
                        (0..num_seats)
                            .into_iter()
                            .map(|seat_num| {
                                // Get the populace
                                let populace = main_data.get_populace(seat_num);

                                // Calculate per-method results, compare with condorcet to get winner type as well
                                let main_results = methods
                                    .iter()
                                    .map(|func| func(&populace).unwrap())
                                    .collect::<Vec<_>>();

                                (seat_num, main_results)
                            })
                            .for_each(|(seat_num, main)| {
                                main_results[seat_num] = main;
                            });

                        // Show the seat counts for each of the methods
                        // First calculate using party lists to compare with proportional
                        proportional_votes.fill(0);
                        (0..num_seats).into_iter().for_each(|i| {
                            let populace = main_data.get_populace(i);
                            populace
                                .iter_populace(&mut |voter: &[f64]| {
                                    proportional_votes[voter
                                        .into_iter()
                                        .enumerate()
                                        .min_by(|(_, a), (_, b)| a.total_cmp(b))
                                        .map(|(i, _)| i)
                                        .unwrap()] += 1;
                                })
                                .unwrap();
                        });

                        // Now we calculate the number of seats each party got for each method
                        // Result data is currently in form [seat][method] -> party
                        // We need it in the form [party][method] -> seat_count
                        for party in 0..num_parties {
                            main_results_data[party].fill(0i64);
                        }
                        for seat in 0..num_seats {
                            for method in 0..len_wo_prop {
                                let party = main_results[seat][method];
                                main_results_data[party][method] += 1;
                            }
                        }

                        // Sainte-Lague method for PLPR
                        allocated_seats.fill(0i64);
                        for _ in 0..num_seats {
                            // Calculate quotients
                            let winner = proportional_votes
                                .iter()
                                .copied()
                                .enumerate()
                                .map(|(i, v)| (i, v as i64 / (2 * allocated_seats[i] + 1)))
                                .max_by(|(_, a), (_, b)| a.cmp(b))
                                .map(|(i, _)| i)
                                .unwrap();
                            allocated_seats[winner] += 1;
                        }
                        // Splat into method results
                        for party in 0..num_parties {
                            main_results_data[party][len_wo_prop] += allocated_seats[party];
                        }

                        // Go through every combination of party, and determine if the result for the
                        // watch method is different to proportional
                        let mut different = vec![false; len_wo_prop];
                        let mut sent = false;
                        let parties = (0..num_parties).into_iter().collect::<Vec<_>>();
                        for length in 1..num_parties {
                            let diff = parties
                                .windows(length)
                                .filter_map(|parties| {
                                    // Get proportional count
                                    let prop: i64 = parties
                                        .iter()
                                        .copied()
                                        .map(|i| main_results_data[i][len_wo_prop])
                                        .sum();
                                    // Get the counts for each method
                                    if (0..methods_len)
                                        .into_iter()
                                        .map(|method| {
                                            parties
                                                .iter()
                                                .copied()
                                                .map(|party| main_results_data[party][method])
                                                .sum::<i64>()
                                        })
                                        .enumerate()
                                        .any(|(i, method)| {
                                            let winning_count = num_seats as i64 / 2 + 1;
                                            let matched = prop >= winning_count
                                                && method < winning_count
                                                || prop < winning_count && method >= winning_count;
                                            if matched {
                                                different[i] = true;
                                                if i == watch_method_index { true } else { false }
                                            } else {
                                                false
                                            }
                                        })
                                    {
                                        Some(parties)
                                    } else {
                                        None
                                    }
                                })
                                .next();
                            if !sent {
                                if let Some(parties) = diff {
                                    let (cx, cy) = main_data.get_candidates();
                                    let party_positions = (0..num_parties)
                                        .into_iter()
                                        .map(|i| vec![cx[i], cy[i]])
                                        .collect();
                                    results_sender
                                        .send((
                                            iter,
                                            parties.to_vec(),
                                            main_results_data.clone(),
                                            party_positions,
                                        ))
                                        .unwrap();
                                    sent = true;
                                }
                            }
                        }
                        sender
                            .send(Message::IncrementProgress(name.clone()))
                            .unwrap();

                        different
                    },
                )
                .fold(
                    || vec![0i64; len_wo_prop],
                    |mut accum, diff| {
                        for i in 0..len_wo_prop {
                            accum[i] += diff[i] as i64
                        }
                        accum
                    },
                )
                .reduce(
                    || vec![0i64; len_wo_prop],
                    |mut a, b| {
                        for i in 0..len_wo_prop {
                            a[i] += b[i];
                        }
                        a
                    },
                );

            let instant = Instant::now();
            // Force wait until we are certain the results have come back
            while instant.elapsed() < Duration::from_secs(1) {
                rayon::yield_now();
            }

            let party_names = (0..num_parties)
                .into_iter()
                .map(|i| format!("Party {i}"))
                .collect::<Vec<_>>();

            let mut renderer = ImageRenderer::new(800, 500);

            // Plot number of times the result was different to proportional
            let chart = Chart::new()
                .y_axis(
                    Axis::new()
                        .type_(AxisType::Category)
                        .data(method_names[0..len_wo_prop].to_vec()),
                )
                .x_axis(Axis::new().type_(AxisType::Value))
                .grid(Grid::new().left("center").top("middle"))
                .series(Bar::new().data(iteration_results))
                .title(
                    Title::new()
                        .text("# Different Governments from Proportional")
                        .left("center"),
                )
                .background_color("#ffffff");
            let (save_path, uri) =
                super::get_file_and_uri(super::SAVE_SEATS_PROJECTION, "differences.svg");
            renderer.save(&chart, save_path).unwrap();
            sender
                .send(Message::ImageSeatProjectionSingle(
                    ArcIntern::from("Seat Projections"),
                    ArcIntern::from("Number of differences"),
                    ArcIntern::from(uri),
                ))
                .unwrap();

            // Plot both the voters and candidates, so we know where they are
            let (vx, vy) = (&main_data.vx, &main_data.vy);
            // let (cx, cy) = (&main_data.cx, &main_data.cy);
            // let (voters_save_file, voters_uri) = super::get_file_and_uri(super::SAVE_SEATS_PROJECTION, "voters.svg");
            // let (parties_save_file, parties_uri) = super::get_file_and_uri(super::SAVE_SEATS_PROJECTION, "parties.svg");
            // super::common::plot_density::<SquaredEuclidean, _>((vx.as_ref(), vy.as_ref()), &sender, "Voters", &voters_save_file, &voters_uri, "Seat Projections (Data)");
            // super::common::plot_density::<SquaredEuclidean, _>((cx.as_ref(), cy.as_ref()), &sender, "Parties", &parties_save_file, &parties_uri, "Seat Projections (Data)");

            let colors = [
                "#FF3030", "#FF9900", "#FFFF33", "#33CC33", "#00CCCC", "#3399FF", "#9933FF",
                "#FF33CC", "#00FFCC", "#3366FF",
            ]
            .into_iter()
            .map(|c| Color::from(c))
            .collect::<Vec<_>>();

            for (iter, method_party_diff, main_results_data, party_positions) in
                results_receiver.try_iter().take(5)
            {
                // Convert main_results_data into datapoint vec
                let main_results_data: Vec<Vec<DataPoint>> = main_results_data
                    .into_iter()
                    .enumerate()
                    .map(|(party, data)| {
                        let datapoint: Vec<DataPoint> = data
                            .into_iter()
                            .enumerate()
                            .map(
                                |(method, method_data)| match method_party_diff.contains(&party) {
                                    true => DataPointItem::new(method_data)
                                        .item_style(
                                            ItemStyle::new()
                                                .border_color(if method == watch_method_index {
                                                    "#FF0000"
                                                } else {
                                                    "#000000"
                                                })
                                                .border_width(1.0),
                                        )
                                        .into(),
                                    false => DataPointItem::new(method_data).into(),
                                },
                            )
                            .collect::<Vec<DataPoint>>();
                        datapoint
                    })
                    .collect();

                // Plot seat projections
                let mut chart = Chart::new()
                    .legend(Legend::new().top("bottom").left("center").show(true))
                    .color(colors.clone())
                    .x_axis(Axis::new().type_(AxisType::Value))
                    .y_axis(
                        Axis::new()
                            .type_(AxisType::Category)
                            .data(method_names.clone()),
                    )
                    .grid(Grid::new().left("center").top("middle"))
                    .title(
                        Title::new()
                            .text(format!("Seat Projections ({iter})"))
                            .left("center"),
                    )
                    .background_color("#ffffff");
                for i in 0..num_parties {
                    let name = party_names[i].clone();
                    let data = main_results_data[i].clone();
                    chart = chart.series({
                        let mut bar = Bar::new()
                            .name(name)
                            .stack("total")
                            .bar_width("80%")
                            .label(Label::new().show(true))
                            .data(data);
                        if i == 0 {
                            bar = bar.mark_line(
                                MarkLine::new()
                                    .data(vec![MarkLineVariant::Simple(
                                        MarkLineData::new().name("Majority").x_axis(76),
                                    )])
                                    .line_style(LineStyle::new().color("black"))
                                    .symbol(vec![Symbol::Circle, Symbol::Circle]),
                            );
                        }
                        bar
                    })
                }

                let (save_file, uri) = super::get_file_and_uri(
                    super::SAVE_SEATS_PROJECTION,
                    &format!("seat projections ({iter}).svg"),
                );
                renderer.save(&chart, &save_file).unwrap();
                sender
                    .send(Message::ImageSeatProjectionMultiple(
                        ArcIntern::from("Seat Projections"),
                        ArcIntern::from("Individual Results"),
                        ArcIntern::from(format!("Seat Counts ({iter})")),
                        ArcIntern::from(uri),
                    ))
                    .unwrap();

                // Visualise candidate positions
                let mut chart = super::common::create_density_chart::<SquaredEuclidean, _>(
                    (vx.as_ref(), vy.as_ref()),
                    &format!("Party Positions ({iter})"),
                    false,
                )
                .color(colors.clone())
                .legend(Legend::new().top("bottom").left("center").show(true))
                .x_axis(Axis::new().min(0.0).max(1.0))
                .y_axis(Axis::new().min(0.0).max(1.0));
                for i in 0..num_parties {
                    let name = party_names[i].clone();
                    let position = vec![party_positions[i].clone()];
                    chart = chart.series(
                        Scatter::new()
                            .name(name)
                            .data(position)
                            .x_axis_index(1)
                            .y_axis_index(1)
                            .item_style(ItemStyle::new().border_width(2.0))
                            .mark_line(
                                MarkLine::new()
                                    .data(vec![
                                        MarkLineVariant::Simple(
                                            MarkLineData::new().name("Midpoint").x_axis(0.5),
                                        ),
                                        MarkLineVariant::Simple(
                                            MarkLineData::new().name("Midpoint").y_axis(0.5),
                                        ),
                                    ])
                                    .line_style(LineStyle::new().color("black"))
                                    .symbol(vec![Symbol::Circle, Symbol::Circle]),
                            ),
                    )
                }

                std::fs::write("test.json", chart.to_string().as_bytes()).unwrap();

                let (save_file, uri) = super::get_file_and_uri(
                    super::SAVE_SEATS_PROJECTION,
                    &format!("positions ({iter}).svg"),
                );

                renderer.save(&chart, &save_file).unwrap();
                sender
                    .send(Message::ImageSeatProjectionMultiple(
                        ArcIntern::from("Seat Projections"),
                        ArcIntern::from("Individual Results"),
                        ArcIntern::from(format!("Party Positions ({iter})")),
                        ArcIntern::from(uri),
                    ))
                    .unwrap();
            }
            sender.send(Message::SimulationFinished).unwrap();
        });
    }
}

pub struct SeatsProjectionMainData {
    vx: Vec<f64>,
    vy: Vec<f64>,
    cx: Vec<f64>,
    cy: Vec<f64>,
    num_voters: usize,
    num_parties: usize,
    num_seats: usize,
}

impl SeatsProjectionMainData {
    pub fn new(
        distr: super::DistributionManager,
        max_offset: f64,
        num_voters: usize,
        num_parties: usize,
        num_seats: usize,
        num_iterations: usize,
    ) -> Self {
        let (vxd, vyd, cxd, cyd) = distr.into_default_distribution();

        let mut rng = SmallRng::from_os_rng();

        fn create_random<R>(num: usize, rng: &mut R, distr: Box<dyn Fn(&mut R) -> f64>) -> Vec<f64>
        where
            R: Rng,
        {
            (0..num).into_iter().map(|_| distr(rng)).collect()
        }

        let vx1 = create_random(num_voters, &mut rng, vxd);
        let vy = create_random(num_voters * num_seats, &mut rng, vyd);

        let mut cx = create_random(num_parties * num_iterations, &mut rng, cxd);
        let cy = create_random(num_parties * num_iterations, &mut rng, cyd);

        // Sort every subpart of cx so that we are in order
        for i in 0..num_iterations {
            let cx = &mut cx[(i * num_parties)..((i + 1) * num_parties)];
            cx.sort_by(|a, b| a.total_cmp(b));
        }

        let mut midpoints = vec![0.0; num_seats];

        let lower = (1.0 - max_offset) / 2.0;
        let step_size = max_offset / num_seats as f64;

        let mut start = lower;
        let mut i = 0;
        for _ in 0..num_seats {
            midpoints[i] = start;
            start += step_size;
            i += 1;
        }

        // Create the full VX array
        let vx = midpoints
            .into_iter()
            .flat_map(|b| {
                let (a, c) = if b <= 0.5 {
                    (0.0, 2.0 * b)
                } else {
                    (1.0 - 2.0 * (1.0 - b), 1.0)
                };
                vx1.iter().copied().map(move |x| a + ((c - a) * x))
            })
            .collect::<Vec<_>>();

        Self {
            vx,
            vy,
            cx,
            cy,
            num_voters,
            num_parties,
            num_seats,
        }
    }

    pub fn get_iter_data(&self, iter: usize) -> IterData {
        let cx = &self.cx[(iter * self.num_parties)..((iter + 1) * self.num_parties)];
        let cy = &self.cy[(iter * self.num_parties)..((iter + 1) * self.num_parties)];

        // Calculate all populace
        let populace = (0..self.num_seats)
            .into_iter()
            .map(|seat| {
                crate::util::create_populace(
                    &self.vx[(seat * self.num_voters)..((seat + 1) * self.num_voters)],
                    &self.vy,
                    cx,
                    cy,
                    self.num_parties,
                )
            })
            .collect();

        IterData {
            cx,
            cy,
            num_parties: self.num_parties,
            populace,
        }
    }
}

pub struct IterData<'a> {
    cx: &'a [f64],
    cy: &'a [f64],
    populace: Vec<Vec<Vec<f64>>>,
    num_parties: usize,
}

impl<'a> IterData<'a> {
    pub fn get_populace(&'a self, seat: usize) -> SeatsProjectionPopulace<'a> {
        SeatsProjectionPopulace {
            populace: &self.populace[seat],
            num_parties: self.num_parties,
        }
    }

    pub fn get_candidates(&'a self) -> (&'a [f64], &'a [f64]) {
        (&self.cx, &self.cy)
    }
}

pub struct SeatsProjectionPopulace<'a> {
    populace: &'a [Vec<f64>],
    num_parties: usize,
}

impl<'a> PopulaceMethod<usize> for SeatsProjectionPopulace<'a> {
    fn populace<'b>(&'b self) -> &'b [Vec<f64>] {
        &self.populace
    }

    fn candidate_at(&self, index: usize) -> methods::Result<usize> {
        Ok(index)
    }

    fn num_candidates(&self) -> usize {
        self.num_parties
    }
}
