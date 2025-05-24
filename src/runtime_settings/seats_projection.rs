use std::usize;

use charming::{
    component::{Axis, Grid, Legend, Title}, element::AxisType, series::{Bar, Scatter}, Chart, ImageRenderer
};
use egui::{text::LayoutJob, Align, FontSelection, Layout, RichText, UiBuilder};
use methods::{MethodFn, PopulaceMethod, VectorMethod};
use rand::{Rng, SeedableRng, rngs::SmallRng};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{Message, runtime_settings::common::WinnerType};

use super::ScoreHandle;

#[derive(Clone, Debug, PartialEq)]
pub struct SeatsProjectionSettings {
    num_voters: usize,
    num_voters_string: String,
    num_parties: usize,
    num_parties_string: String,
    num_seats: usize,
    num_seats_string: String,

    max_offset: f64,
    max_offset_string: String,

    save_winner_types_to_file: bool,
}

impl Default for SeatsProjectionSettings {
    fn default() -> Self {
        Self {
            num_voters: 5000,
            num_parties: 6,
            num_seats: 151,
            num_voters_string: 5000usize.to_string(),
            num_parties_string: 6usize.to_string(),
            num_seats_string: 151usize.to_string(),
            max_offset: 4.0,
            max_offset_string: 4.0f64.to_string(),
            save_winner_types_to_file: true,
        }
    }
}

impl SeatsProjectionSettings {
    pub fn show(&mut self, ui: &mut egui::Ui) {
        ui.heading("Seat Projection Settings");

        ui.label("Simulate elections and collect seat data");

        egui::Grid::new("Seat Projection grid").show(ui, |ui| {
            for (name, value, value_string) in [
                (
                    "Number of voters per seat",
                    &mut self.num_voters,
                    &mut self.num_voters_string,
                ),
                (
                    "Number of parties",
                    &mut self.num_parties,
                    &mut self.num_parties_string,
                ),
                (
                    "Number of seats",
                    &mut self.num_seats,
                    &mut self.num_seats_string,
                ),
            ] {
                ui.label(name);
                crate::util::typed_textbox(value, value_string, ui, 1, usize::MAX);
                ui.end_row();
            }

            ui.vertical(|ui| {
                ui.label("Max voter caucus offset");
                ui.small("The voter caucus will be generated per seat based on dividing this value and moving towards either end of the spectrum");
            });
            crate::util::typed_textbox(&mut self.max_offset, &mut self.max_offset_string, ui, 1.0, f64::MAX);
            ui.end_row();

            ui.label("Save winner type charts to directory");
            ui.checkbox(&mut self.save_winner_types_to_file, "");
        });

        let style = ui.style();
        let mut job = LayoutJob::default();
        RichText::new("Will save all charts to ").append_to(&mut job, &style, FontSelection::default(), Align::Min);
        RichText::new("output/seat_projections").code().append_to(&mut job, style, FontSelection::default(), Align::Min);

        ui.scope_builder(UiBuilder::new().layout(Layout::top_down(Align::Min).with_cross_justify(false)), |ui| {
            ui.label(job);
        });
    }

    pub fn simulate(
        &self,
        sender: &std::sync::mpsc::Sender<crate::Message>,
        methods: &std::collections::HashMap<
            methods::Method,
            Vec<super::ScoreHandle>,
            rustc_hash::FxBuildHasher,
        >,
        distrs: &super::Distribution,
    ) {
        let sender = sender.clone();
        let methods = methods.clone();
        let distr = distrs.clone();

        let num_voters = self.num_voters;
        let num_parties = self.num_parties;
        let num_seats = self.num_seats;

        let max_offset = self.max_offset;
        let _save_to_file = self.save_winner_types_to_file;
        rayon::spawn(move || {
            // Get method names and functions
            let (mut method_names, methods) = methods
                .into_iter()
                .flat_map(|(method, score_values)| {
                    score_values.into_iter().map(move |v| {
                        (
                            match method.is_score() {
                                true => format!("{} ({v})", method.name()),
                                false => method.name().to_string(),
                            },
                            match v {
                                ScoreHandle::Full => method.func() as MethodFn<usize>,
                                ScoreHandle::Custom(v, _) => method.score_fn(v).unwrap(),
                            },
                        )
                    })
                })
                .collect::<(Vec<_>, Vec<_>)>();
            method_names.push("Proportional".to_string());

            let (_, _, cx, cy) = distr.clone().into_default_distribution();
            fn create_random<R>(
                num: usize,
                rng: &mut R,
                distr: Box<dyn Fn(&mut R) -> f64>,
            ) -> Vec<f64>
            where
                R: Rng,
            {
                (0..num).into_iter().map(|_| distr(rng)).collect()
            }
            let mut rng = SmallRng::from_os_rng();
            let mut cx = create_random(num_parties, &mut rng, cx);
            let mut cy = create_random(num_parties, &mut rng, cy);

            let mut indices: Vec<usize> = (0..cx.len()).collect();

            indices.sort_by(|&i, &j| cx[i].total_cmp(&cx[j]));

            cx = indices.iter().map(|&i| cx[i]).collect();
            cy = indices.iter().map(|&i| cy[i]).collect();

            // We need to calculate multiple things here:
            // 1. The average voter for each round
            // 2. The Condorcet winner for each round
            // 3. The seat winner for each method
            // Note that unlike the usual simulations, the average voter is different every seat
            // We store the results in three different
            let main_data = SeatsProjectionMainData::new(
                distr,
                &cx,
                &cy,
                max_offset,
                num_voters,
                num_parties,
                num_seats,
            );

            // Collect results
            let results = (0..num_seats)
                .into_par_iter()
                .map(|seat_num| {
                    // Get the offset voter positions
                    let offset_voters = main_data.offset_into(seat_num);
                    // Calculate the average voter
                    let (ax, ay) = methods::average_voter(
                        &offset_voters.vx,
                        &offset_voters.vy,
                        offset_voters.num_voters,
                    );
                    let average_voter = [ax, ay];
                    // Calculate the closest-to-average party
                    // We can do this manually here
                    let closest_to_average_party = cx
                        .iter()
                        .copied()
                        .zip(cy.iter().copied())
                        .enumerate()
                        .map(|(i, (cx, cy))| (i, ((ax - cx).powi(2) + (ay - cy).powi(2)).sqrt()))
                        .min_by(|(_, a), (_, b)| a.total_cmp(b))
                        .map(|(i, _)| i)
                        .unwrap();

                    // Get the populace
                    let populace = offset_voters.create_populace();

                    // Get the Condorcet winner, if they exist
                    let condorcet_winner = methods::condorcet(&populace).unwrap();

                    // Calculate per-method results, compare with condorcet to get winner type as well
                    let (main_results, winner_types) = methods
                        .iter()
                        .map(|func| {
                            let result = func(&populace).unwrap();
                            let winner_type = match condorcet_winner {
                                Some(condorcet_winner) => {
                                    if result == condorcet_winner {
                                        if result == closest_to_average_party {
                                            WinnerType::CondorcetAndAverage
                                        } else {
                                            WinnerType::CondorcetButNotAverage
                                        }
                                    } else if result == closest_to_average_party {
                                        WinnerType::NotCondorcetButAverage
                                    } else {
                                        WinnerType::NotCondorcetAndNotAverage
                                    }
                                }
                                None => {
                                    if result == closest_to_average_party {
                                        WinnerType::NoCondorcetButAverage
                                    } else {
                                        WinnerType::NoCondorcetAndNotAverage
                                    }
                                }
                            };
                            (result, winner_type)
                        })
                        .collect::<(Vec<_>, Vec<_>)>();

                    (
                        seat_num,
                        main_results,
                        average_voter,
                        closest_to_average_party,
                        condorcet_winner,
                        winner_types,
                    )
                })
                .collect::<Vec<_>>();

            let mut main_results = vec![Vec::new(); num_seats];
            let mut average_voters = vec![[0.0, 0.0]; num_seats];
            let mut closest_to_average_parties = vec![0; num_seats];
            let mut condorcet_winners = vec![None; num_seats];
            let mut winner_types = vec![Vec::new(); num_seats];

            results
                .into_iter()
                .for_each(|(seat_num, main, average, closest, condorcet, ty)| {
                    main_results[seat_num] = main;
                    average_voters[seat_num] = average;
                    closest_to_average_parties[seat_num] = closest;
                    condorcet_winners[seat_num] = condorcet;
                    winner_types[seat_num] = ty;
                });

            // Show the seat counts for each of the methods
            // First calculate using party lists to compare with proportional
            let populace = main_data.create_populace();
            let mut proportional_votes = vec![0usize; num_parties];
            populace.populace
                .iter()
                .for_each(|voter| {
                    proportional_votes[voter
                        .into_iter()
                        .enumerate()
                        .min_by(|(_, a), (_, b)| a.total_cmp(b))
                        .map(|(i, _)| i)
                        .unwrap()] += 1;
                });
            
            let methods_len = method_names.len();
            let len_wo_prop = methods_len - 1;

            // Now we calculate the number of seats each party got for each method
            // Result data is currently in form [seat][method] -> party
            // We need it in the form [party][method] -> count
            let mut main_results_data = vec![vec![0i64; methods_len]; num_parties];
            for seat in 0..num_seats {
                for method in 0..len_wo_prop {
                    let party = main_results[seat][method];
                    main_results_data[party][method] += 1;
                }
            }
            
            // Sainte-Lague method for PLPR
            let mut allocated_seats = vec![0i64; num_parties];
            for _ in 0..num_seats {
                // Calculate quotients
                let mut quotients = proportional_votes
                    .iter()
                    .copied()
                    .enumerate()
                    .map(|(i, v)| (i, v as i64 / (2 * allocated_seats[i] + 1)))
                    .collect::<Vec<_>>();
                quotients.sort_by(|(_, a), (_, b)| b.cmp(a));
                let winner = quotients[0].0;
                allocated_seats[winner] += 1;
            }
            // Splat into method results
            for party in 0..num_parties {
                main_results_data[party][len_wo_prop] += allocated_seats[party];
            }

            let party_names = (0..num_parties)
                .into_iter()
                .map(|i| format!("Party {i}"))
                .collect::<Vec<_>>();
            let party_positions = (0..num_parties)
                .into_iter()
                .map(|i| {
                    vec![cx[i], cy[i]]
                })
                .collect::<Vec<_>>();

            let mut chart = Chart::new()
                .legend(Legend::new().top("bottom").left("center").show(true))
                .x_axis(Axis::new().type_(AxisType::Value))
                .y_axis(Axis::new().type_(AxisType::Category).data(method_names.clone()))
                .grid(Grid::new().left("center").top("middle"))
                .title(Title::new().text("Seat Projections").left("center"))
                .background_color("#ffffff");
            for i in 0..num_parties {
                let name = party_names[i].clone();
                let data = main_results_data[i].clone();
                chart = chart.series(
                    Bar::new()
                        .name(name)
                        .stack("total")
                        .bar_width("80%")
                        .data(data),
                )
            }

            let mut renderer = ImageRenderer::new(800, 500);
            let (save_file, uri) = super::get_file_and_uri(super::SAVE_SEATS_PROJECTION, "seat projections.svg");
            renderer.save(&chart, &save_file).unwrap();
            sender
                .send(Message::ImageFilePath(uri))
                .unwrap();

            let mut labels = party_names.clone();
            labels.push(format!("Seats"));
            // Visualise candidate positions
            let mut chart = Chart::new()
                .legend(Legend::new().top("bottom").left("center").show(true))
                .title(Title::new().text("Party/Seat Positions").left("center"))
                .x_axis(Axis::new().min(0.0).max(1.0))
                .y_axis(Axis::new().min(0.0).max(1.0))
                .background_color("#ffffff");
            for i in 0..num_parties {
                let name = party_names[i].clone();
                let position = vec![party_positions[i].clone()];
                chart = chart.series(
                    Scatter::new()
                        .name(name)
                        .data(position)
                )
            }
            let average_voters = average_voters
                .into_iter()
                .map(|[x, y]| vec![x, y])
                .collect::<Vec<_>>();
            chart = chart.series(
                Scatter::new()
                    .name("Seats")
                    .data(average_voters)
            );

            let (save_file, uri) = super::get_file_and_uri(super::SAVE_SEATS_PROJECTION, "positions.svg");

            renderer.save(&chart, &save_file).unwrap();
            sender
                .send(Message::ImageFilePath(uri))
                .unwrap();

            sender.send(Message::Unlock).unwrap();
        });
    }
}

pub struct SeatsProjectionMainData<'a> {
    vx: Vec<f64>,
    vy: Vec<f64>,
    cx: &'a [f64],
    cy: &'a [f64],
    offsets: Vec<f64>,
    num_voters: usize,
    num_parties: usize,
    num_seats: usize,
}

impl<'a> SeatsProjectionMainData<'a> {
    pub fn new(
        distr: super::Distribution,
        cx: &'a [f64],
        cy: &'a [f64],
        max_offset: f64,
        num_voters: usize,
        num_parties: usize,
        num_seats: usize,
    ) -> Self {
        let (vx, vy, _, _) = distr.into_default_distribution();

        let mut rng = SmallRng::from_os_rng();

        fn create_random<R>(num: usize, rng: &mut R, distr: Box<dyn Fn(&mut R) -> f64>) -> Vec<f64>
        where
            R: Rng,
        {
            (0..num).into_iter().map(|_| distr(rng)).collect()
        }

        let vx = create_random(num_voters, &mut rng, vx);
        let vy = create_random(num_voters, &mut rng, vy);

        let mut offsets = Vec::with_capacity(num_seats);
        let one_side = max_offset - 1.0;
        let step_size = one_side / (num_seats / 2) as f64;
        let mut start = -max_offset;
        for _ in 0..(num_seats / 2) {
            offsets.push(start);
            start += step_size;
        }
        if num_seats % 2 == 1 {
            offsets.push(1.0);
        }
        start = 1.0 + step_size;
        for _ in 0..(num_seats / 2) {
            offsets.push(start);
            start += step_size;
        }

        Self {
            vx,
            vy,
            cx,
            cy,
            offsets,
            num_voters,
            num_parties,
            num_seats: 1, // This is a single seat
        }
    }

    pub fn offset_into(&'a self, i: usize) -> Self {
        let offset = *self
            .offsets
            .get(i)
            .expect(&format!("cannot offset into this struct more than once: {i}, {}", self.offsets.len()));

        let abs_scale = offset.abs();
        let scaled_vx = self
            .vx
            .iter()
            .copied()
            .map(|x| {
                if offset > 0.0 {
                    x / abs_scale
                } else {
                    1.0 - (x / abs_scale)
                }
            })
            .collect::<Vec<_>>();

        Self {
            vx: scaled_vx,
            vy: self.vy.clone(),
            cx: self.cx,
            cy: self.cy,
            offsets: Vec::new(),
            num_voters: self.num_voters,
            num_parties: self.num_parties,
            num_seats: self.num_seats,
        }
    }

    pub fn create_populace(&self) -> SeatsProjectionPopulace {
        SeatsProjectionPopulace {
            populace: crate::util::create_populace(
                &self.vx,
                &self.vy,
                &self.cx,
                &self.cy,
                self.num_parties,
            ),
            num_parties: self.num_parties,
        }
    }
}

impl<'a> VectorMethod for SeatsProjectionMainData<'a> {
    fn into_data<'b>(
        &'b self,
    ) -> (
        (&'b [f64], &'b [f64]),
        (&'a [f64], &'a [f64]),
        usize,
        usize,
        usize,
    ) {
        (
            (&self.vx, &self.vy),
            (self.cx, self.cy),
            self.num_voters,
            self.num_parties,
            self.num_seats,
        )
    }
}

pub struct SeatsProjectionPopulace {
    populace: Vec<Vec<f64>>,
    num_parties: usize,
}

impl PopulaceMethod<usize> for SeatsProjectionPopulace {
    fn populace<'a>(&'a self) -> &'a [Vec<f64>] {
        &self.populace
    }

    fn candidate_at(&self, index: usize) -> methods::Result<usize> {
        Ok(index)
    }

    fn num_candidates(&self) -> usize {
        self.num_parties
    }
}
