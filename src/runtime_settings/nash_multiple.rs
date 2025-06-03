use charming::{
    Chart, ImageRenderer,
    component::{Axis, Grid, Legend, Title},
    datatype::{CompositeValue, DataPoint},
    element::{AxisLabel, AxisType, Symbol},
    series::{Line, Scatter},
};
use egui::{Color32, RichText};
use internment::ArcIntern;
use methods::{CachedMethod, Method, PopulaceMethod};
use rand::{rngs::SmallRng, seq::IndexedRandom, SeedableRng};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use rustc_hash::FxHashSet;

use crate::{Message, MessageSender, util::input};

pub const GRID_SIZE: usize = 101;

#[derive(Debug)]
pub struct NashMultipleSettings {
    num_voters_per_seat: usize,
    num_parties: usize,
    num_iterations: usize,
    num_seats: usize,
    max_offset: f64,
}

impl Default for NashMultipleSettings {
    fn default() -> Self {
        Self {
            num_voters_per_seat: 5000,
            num_parties: 6,
            num_iterations: 200,
            num_seats: 151,
            max_offset: 0.25,
        }
    }
}

impl NashMultipleSettings {
    pub fn show(&mut self, ui: &mut egui::Ui) {
        ui.heading("Nash Equilibrium (Multiple)");

        ui.label("Find the Nash equilibrium of a full election");

        egui::Grid::new("nash_multiple_grid").show(ui, |ui| {
            input!(ui,
                "Number of voters per seat", self.num_voters_per_seat, usize,
                "Number of parties", self.num_parties, usize,
                "Number of seats", self.num_seats, usize,
                "Number of iterations", self.num_iterations, usize,
                "Maximum voter offset", self.max_offset, f64, min 0.0, max 1.0
            );
        });
        ui.label(RichText::new("This will take a long time to compute!").color(Color32::RED));
    }

    pub fn simulate(
        &self,
        sender: &MessageSender,
        methods: &FxHashSet<Method>,
        distrs: &super::DistributionManager,
    ) {
        let sender = sender.clone();
        let methods = methods.clone();
        let num_voters = self.num_voters_per_seat;
        let num_parties = self.num_parties;
        let num_iterations = self.num_iterations;
        let num_seats = self.num_seats;
        let max_offset = self.max_offset;
        let distrs = distrs.clone();

        rayon::spawn(move || {
            // Go straight into simulating
            methods.into_par_iter().for_each(|method| {
                // Essentially identical to NashSingle
                // In this case, we can use all cores

                let (vxd, vyd, pxd, pyd) = distrs.into_default_distribution();
                let method_name = method.name();

                let method_name_arc: ArcIntern<str> =
                    ArcIntern::from(format!("Nash Multiple ({method_name})"));
                sender
                    .send(Message::TotalProgress(
                        method_name_arc.clone(),
                        num_iterations,
                    ))
                    .unwrap();

                // Determine the voters and parties
                fn create_random(
                    num: usize,
                    rng: &mut SmallRng,
                    distr: Box<dyn Fn(&mut SmallRng) -> f64>,
                ) -> Vec<f64> {
                    (0..num).into_iter().map(|_| distr(rng)).collect()
                }

                let mut rng = SmallRng::from_os_rng();

                let px = create_random(num_parties, &mut rng, pxd);
                let py = create_random(num_parties, &mut rng, pyd);

                let px = px
                    .into_iter()
                    .map(|x| (x * GRID_SIZE as f64).floor() as usize)
                    .collect::<Vec<_>>();
                let py = py
                    .into_iter()
                    .map(|y| (y * GRID_SIZE as f64).floor() as usize)
                    .collect::<Vec<_>>();
                let mut parties = Parties { px, py };

                let mut cache_fns = create_populi(
                    &mut rng,
                    vxd,
                    vyd,
                    &parties.px,
                    &parties.py,
                    max_offset,
                    num_voters,
                    num_parties,
                    num_seats,
                    method,
                );

                let mut new_parties = parties.clone();

                let mut positions = vec![Vec::with_capacity(num_iterations); num_parties];
                let mut seat_counts = vec![vec![0usize; num_parties]; num_iterations];

                let mut stopped_at = num_iterations - 1;

                let _ = (0..num_iterations)
                    .map(|iter| {
                        let party_data = (0..num_parties)
                            .into_iter()
                            .filter_map(|party| {
                                let px = parties.px[party];
                                let py = parties.py[party];
                                // Calculate actual position
                                let (minx, maxx) = if px == 0 {
                                    (0, px + 1)
                                } else if px >= GRID_SIZE {
                                    (px - 1, GRID_SIZE)
                                } else {
                                    (px - 1, px + 1)
                                };
                                let (miny, maxy) = if py == 0 {
                                    (0, py + 1)
                                } else if py >= GRID_SIZE {
                                    (py - 1, GRID_SIZE)
                                } else {
                                    (py - 1, py + 1)
                                };

                                let mut candidates = Vec::new();
                                let mut max_score = isize::MIN;

                                for x in minx..=maxx {
                                    for y in miny..=maxy {
                                        let xf = x as f64 / GRID_SIZE as f64;
                                        let yf = y as f64 / GRID_SIZE as f64;
                                        let (score, distances) = cache_fns
                                            .iter_mut()
                                            .map(|(func, vx, vy)| {
                                                let distances = vx
                                                    .iter()
                                                    .copied()
                                                    .zip(vy.iter().copied())
                                                    .map(|(vx, vy)| (vx - xf).powi(2) + (vy - yf).powi(2))
                                                    .collect::<Vec<_>>();
                                                let score = func.mock_score_candidate(party, &distances).unwrap();
                                                (score, distances)
                                            })
                                            .fold((0isize, Vec::with_capacity(num_seats)), |(s, mut d), (score, distances)| {
                                                d.push(distances);
                                                (s + score, d)
                                            });

                                        if score > max_score {
                                            candidates.clear();
                                            candidates.push((x, y, score, distances));
                                            max_score = score;
                                        } else if score == max_score {
                                            candidates.push((x, y, score, distances));
                                        }
                                    }
                                }

                                if let Some(&(x, y, _, ref distances)) = candidates.choose(&mut rng) {
                                    if !(x == px && y == py) {
                                        Some((party, x, y, distances.clone()))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<_>>();
                        if party_data.len() == 0 {
                            stopped_at = iter;
                            sender.send(Message::TotalProgress(method_name_arc.clone(), num_iterations)).unwrap();
                            return None;
                        }
                        party_data.into_iter().for_each(|(i, x, y, distances)| {
                            new_parties.px[i] = x;
                            new_parties.py[i] = y;
                            cache_fns.iter_mut().zip(distances).for_each(
                                |((func, _, _), distances)| {
                                    func.update_candidate(i, &distances).unwrap();
                                },
                            );
                        });

                        cache_fns.iter_mut().for_each(|(func, _, _)| {
                            let winner = func.get_current_result().unwrap();
                            seat_counts[iter][winner] += 1;
                        });

                        // Copy all data over
                        parties.px.copy_from_slice(&new_parties.px);
                        parties.py.copy_from_slice(&new_parties.py);
                        std::mem::swap(&mut parties, &mut new_parties);

                        sender
                            .send(Message::IncrementProgress(method_name_arc.clone()))
                            .unwrap();

                        // Add current positions and seat counts
                        for i in 0..num_parties {
                            positions[i].push(vec![
                                parties.px[i] as f64 / (GRID_SIZE - 1) as f64,
                                parties.py[i] as f64 / (GRID_SIZE - 1) as f64,
                            ]);
                        }
                        Some(())
                    })
                    .collect::<Option<()>>();
                // Visualize the seat counts
                let mut seat_counts_average =
                    vec![
                        vec![
                            DataPoint::Value(CompositeValue::OptionalNumber(None));
                            num_iterations
                        ];
                        num_parties
                    ];
                for i in 0..stopped_at {
                    let start = i.saturating_sub(5);
                    let end = (i + 6).min(stopped_at);
                    for party in 0..num_parties {
                        let mut sum = 0;
                        for i in start..end {
                            sum += seat_counts[i][party];
                        }
                        let av = sum as i64 / (end - start) as i64;
                        seat_counts_average[party][i] = DataPoint::from(vec![i as i64, av]);
                    }
                }
                let mut chart = Chart::new()
                    .title(
                        Title::new()
                            .text(format!("{method_name} (Nash Multiple, Seat Counts)"))
                            .left("center"),
                    )
                    .background_color("#FFFFFF")
                    .legend(Legend::new().top("bottom").left("center"))
                    .x_axis(
                        Axis::new()
                            .type_(AxisType::Category)
                            .data((0..stopped_at).map(|i| i.to_string()).collect()),
                    )
                    .y_axis(Axis::new().type_(AxisType::Value));
                for (i, data) in seat_counts_average.into_iter().enumerate() {
                    chart = chart.series(
                        Line::new()
                            .data(data)
                            .name(format!("Party {i}"))
                            .show_symbol(false),
                    );
                }

                let (save_file, uri) = super::get_file_and_uri(
                    super::SAVE_NASH_MULTIPLE,
                    &format!("{method_name} (Seat Counts).svg"),
                );
                let mut renderer = ImageRenderer::new(400, 400);
                renderer.save(&chart, &save_file).unwrap();
                sender
                    .send(Message::ImageStandard(
                        ArcIntern::from("Nash Equilibriums (Multiple)"),
                        ArcIntern::from(format!("{method_name} (Seat Counts)")),
                        ArcIntern::from(uri),
                    ))
                    .unwrap();

                // Visualize the candidate positions
                let mut chart = Chart::new()
                    .title(
                        Title::new()
                            .text(format!("{method_name} (Nash Multiple)"))
                            .left("center"),
                    )
                    .grid(Grid::new().left("center").top("middle"))
                    .legend(Legend::new().left("center").top("bottom"))
                    .x_axis(
                        Axis::new()
                            .min(0.0)
                            .max(1.0)
                            .axis_label(AxisLabel::new().show(false)),
                    )
                    .y_axis(
                        Axis::new()
                            .min(0.0)
                            .max(1.0)
                            .axis_label(AxisLabel::new().show(false)),
                    )
                    .background_color("#ffffff");
                {
                    let chart = &mut chart;
                    parties
                        .px
                        .into_iter()
                        .zip(parties.py.into_iter())
                        .enumerate()
                        .for_each(|(i, (px, py))| {
                            let position = vec![vec![
                                px as f64 / (GRID_SIZE - 1) as f64,
                                py as f64 / (GRID_SIZE - 1) as f64,
                            ]];
                            *chart = std::mem::replace(chart, Chart::new()).series(
                                Scatter::new()
                                    .name(format!(
                                        "Party {i} [{}, {}]",
                                        position[0][0],
                                        position[0][1]
                                    ))
                                    .data(position),
                            );
                        });
                }

                let (save_file, uri) = super::get_file_and_uri(
                    super::SAVE_NASH_MULTIPLE,
                    &format!("{method_name}.svg"),
                );
                let mut renderer = ImageRenderer::new(400, 400);
                renderer.save(&chart, &save_file).unwrap();
                sender
                    .send(Message::ImageStandard(
                        ArcIntern::from("Nash Equilibriums (Multiple)"),
                        ArcIntern::from(method_name),
                        ArcIntern::from(uri),
                    ))
                    .unwrap();

                // Visualise the paths of the candidates
                let mut chart = Chart::new()
                    .title(
                        Title::new()
                            .text(format!("{method_name} (Nash, candidate paths, multiple)"))
                            .left("center"),
                    )
                    .grid(Grid::new().left("center").top("middle"))
                    .legend(Legend::new().left("center").top("bottom"))
                    .x_axis(Axis::new().min(0.0).max(1.0).type_(AxisType::Value))
                    .y_axis(Axis::new().min(0.0).max(1.0).type_(AxisType::Value))
                    .background_color("#ffffff");
                {
                    let chart = &mut chart;
                    positions
                        .into_iter()
                        .enumerate()
                        .for_each(|(candidate, iterations)| {
                            let len = iterations.len();
                            let iterations = iterations
                                .into_iter()
                                .map(|data| vec![data[0], data[1]])
                                .collect();
                            *chart = std::mem::replace(chart, Chart::new()).series(
                                Line::new()
                                    .data(iterations)
                                    .name(format!("Party {candidate}"))
                                    .symbol(Symbol::None),
                            )
                        });
                }

                let (save_file, uri) = super::get_file_and_uri(
                    super::SAVE_NASH_MULTIPLE,
                    &format!("{method_name} (candidate paths).svg"),
                );
                renderer.save(&chart, &save_file).unwrap();
                sender
                    .send(Message::ImageStandard(
                        ArcIntern::from("Nash Equilibriums (Multiple)"),
                        ArcIntern::from(format!("{method_name} (Candidate Paths)")),
                        ArcIntern::from(uri),
                    ))
                    .unwrap();
            });

            sender.send(Message::SimulationFinished).unwrap();
        });
    }
}

pub fn create_populi(
    rng: &mut SmallRng,
    vxd: Box<dyn Fn(&mut SmallRng) -> f64>,
    vyd: Box<dyn Fn(&mut SmallRng) -> f64>,
    px: &[usize],
    py: &[usize],
    max_offset: f64,
    num_voters: usize,
    num_parties: usize,
    num_seats: usize,
    method: Method,
) -> Vec<(Box<dyn CachedMethod + Send>, Vec<f64>, Vec<f64>)> {
    fn create_random(
        num: usize,
        rng: &mut SmallRng,
        distr: Box<dyn Fn(&mut SmallRng) -> f64>,
    ) -> Vec<f64> {
        (0..num).into_iter().map(|_| distr(rng)).collect()
    }

    let vx1 = create_random(num_voters, rng, vxd);
    let vy = create_random(num_voters * num_seats, rng, vyd);

    let mut midpoints = vec![0.0; num_seats];
    let lower = (1.0 - max_offset) / 2.0;
    let step_size = max_offset / num_seats as f64;

    let mut start = lower;
    for i in 0..num_seats {
        midpoints[i] = start;
        start += step_size;
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

    (0..num_seats)
        .map(|seat_num| {
            // Get the voters for this populace
            let vx = &vx[(seat_num * num_voters)..((seat_num + 1) * num_voters)];
            let vy = &vy[(seat_num * num_voters)..((seat_num + 1) * num_voters)];

            let pxf = px
                .iter()
                .copied()
                .map(|x| (x as f64) / GRID_SIZE as f64)
                .collect::<Vec<_>>();
            let pyf = py
                .iter()
                .copied()
                .map(|x| (x as f64) / GRID_SIZE as f64)
                .collect::<Vec<_>>();

            let populace = crate::util::create_populace(vx, vy, &pxf, &pyf, pxf.len());

            (
                method.cached(populace, num_voters, num_parties).unwrap(),
                vx.to_vec(),
                vy.to_vec(),
            )
        })
        .collect()
}

#[derive(Clone, Debug)]
struct Parties {
    px: Vec<usize>,
    py: Vec<usize>,
}
