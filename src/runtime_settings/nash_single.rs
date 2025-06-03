use std::isize;

use charming::{
    Chart, HtmlRenderer, ImageRenderer,
    component::{Axis, Legend, Title},
    element::{AxisLabel, AxisType, Symbol},
    series::{Line, Scatter},
};
use internment::ArcIntern;
use methods::{Method, PopulaceMethod};
use rand::{Rng, SeedableRng, rngs::SmallRng};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use rustc_hash::FxHashSet;

use crate::{Message, MessageSender, util::input};

#[derive(Debug)]
pub struct NashSingleSettings {
    num_voters: usize,
    num_candidates: usize,
    num_iterations: usize,
    max_iters_until_elimination: usize,
    eliminate_until_min: usize,
}

impl Default for NashSingleSettings {
    fn default() -> Self {
        Self {
            num_voters: 5000,
            num_candidates: 6,
            num_iterations: 1000,
            max_iters_until_elimination: 50,
            eliminate_until_min: 2,
        }
    }
}

impl NashSingleSettings {
    pub fn show(&mut self, ui: &mut egui::Ui) {
        ui.heading("Nash Equilibrium (Single)");

        ui.label("Find the nash equilibrium of an isolated election");

        egui::Grid::new("nash_single_grid").show(ui, |ui| {
            input!(ui,
                "Number of voters", self.num_voters, usize,
                "Number of candidates", self.num_candidates, usize,
                "Number of iterations", self.num_iterations, usize,
                "Maximum iterations before dropping out", self.max_iters_until_elimination, usize,
                "Minimum candidates", self.eliminate_until_min, usize, max self.num_candidates
            );
        });
    }

    pub fn simulate(
        &self,
        sender: &MessageSender,
        methods: &FxHashSet<Method>,
        distrs: &super::DistributionManager,
    ) {
        let sender = sender.clone();
        let methods = methods.clone();
        let num_voters = self.num_voters;
        let num_candidates = self.num_candidates;
        let num_iterations = self.num_iterations;
        let max_iters = self.max_iters_until_elimination;
        let until_min = self.eliminate_until_min;
        let distrs = distrs.clone();

        rayon::spawn(move || {
            // Go straight into simulating
            methods
                .into_par_iter()
                // Unfortunately we can't use all cores for these simulations
                .for_each(|method| {
                    let (vxd, vyd, cxd, cyd) = distrs.into_default_distribution();
                    let method_name = method.name();
                    let method_func = method.score();

                    let method_name_arc: ArcIntern<str> =
                        ArcIntern::from(format!("Nash Single ({method_name})"));
                    sender
                        .send(Message::TotalProgress(
                            method_name_arc.clone(),
                            num_iterations,
                        ))
                        .unwrap();

                    // Determine the voters and candidates
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
                    let vx = create_random(num_voters, &mut rng, vxd);
                    let vy = create_random(num_voters, &mut rng, vyd);
                    let cx = create_random(num_candidates, &mut rng, cxd);
                    let cy = create_random(num_candidates, &mut rng, cyd);

                    // Convert candidates into integer coordinates
                    let cx = cx
                        .into_iter()
                        .map(|x| (x * (GRID_SIZE - 1) as f64).round() as usize)
                        .collect::<Vec<_>>();
                    let cy = cy
                        .into_iter()
                        .map(|x| (x * (GRID_SIZE - 1) as f64).round() as usize)
                        .collect::<Vec<_>>();
                    let mut candidates = Candidates {
                        cx,
                        cy,
                        excluded: vec![false; num_candidates],
                        best_scores: vec![isize::MIN; num_candidates],
                        iters_since_best_score: vec![0usize; num_candidates],
                        num_candidates,
                    };
                    let mut new_candidates = candidates.clone();

                    let mut scores = vec![0; num_candidates];

                    // Perform iterations
                    let mut positions = vec![Vec::with_capacity(num_iterations); num_candidates];
                    (0..num_iterations).for_each(|_| {
                        // Iterate over the candidates and determine their new best position
                        for i in 0..num_candidates {
                            if candidates.excluded[i] {
                                continue;
                            }

                            let cx = candidates.cx[i];
                            let cy = candidates.cy[i];

                            let (minx, maxx) = if cx == 0 {
                                (0, cx + 1)
                            } else if cx == GRID_SIZE {
                                (cx - 1, GRID_SIZE)
                            } else {
                                (cx - 1, cx + 1)
                            };

                            let (miny, maxy) = if cy == 0 {
                                (0, cy + 1)
                            } else if cy == GRID_SIZE {
                                (cy - 1, GRID_SIZE)
                            } else {
                                (cy - 1, cy + 1)
                            };

                            let (cx, cy) = (minx..=maxx)
                                .map(|x| {
                                    (miny..=maxy)
                                        .map(|y| {
                                            let populace = NashPopulace::new(
                                                &vx,
                                                &vy,
                                                &mut candidates,
                                                i,
                                                (x, y),
                                            );
                                            let i = populace.candidates_map[i].unwrap();
                                            let score = method_func(i, &populace).unwrap();
                                            (x, y, score)
                                        })
                                        .max_by_key(|(_, _, score)| *score)
                                        .unwrap()
                                })
                                .max_by_key(|(_, _, score)| *score)
                                .map(|(x, y, _)| (x, y))
                                .unwrap();
                            new_candidates.cx[i] = cx;
                            new_candidates.cy[i] = cy;
                        }

                        // Score the candidates directly with the new changes
                        let populace =
                            NashPopulace::new_without_replacement(&vx, &vy, &mut new_candidates);

                        (0..num_candidates)
                            .into_iter()
                            .filter(|i| !candidates.excluded[*i])
                            .for_each(|i| {
                                scores[i] =
                                    method_func(populace.candidates_map[i].unwrap(), &populace)
                                        .unwrap()
                            });

                        // Determine all scores that are new
                        for i in 0..num_candidates {
                            if candidates.excluded[i] {
                                continue;
                            }
                            if scores[i] >= candidates.best_scores[i] {
                                new_candidates.best_scores[i] = scores[i];
                                new_candidates.iters_since_best_score[i] = 0;
                            } else {
                                new_candidates.iters_since_best_score[i] =
                                    candidates.iters_since_best_score[i] + 1;
                                if new_candidates.iters_since_best_score[i] >= max_iters
                                    && candidates.num_candidates > until_min
                                {
                                    new_candidates.excluded[i] = true;
                                    new_candidates.num_candidates = candidates.num_candidates - 1;

                                    new_candidates.best_scores.fill(isize::MIN);
                                    new_candidates.iters_since_best_score.fill(0);
                                    break;
                                }
                            }
                        }

                        // Copy all data over
                        candidates.cx.copy_from_slice(&new_candidates.cx);
                        candidates.cy.copy_from_slice(&new_candidates.cy);
                        candidates
                            .excluded
                            .copy_from_slice(&new_candidates.excluded);
                        candidates
                            .best_scores
                            .copy_from_slice(&new_candidates.best_scores);
                        candidates
                            .iters_since_best_score
                            .copy_from_slice(&new_candidates.iters_since_best_score);
                        candidates.num_candidates = new_candidates.num_candidates;
                        std::mem::swap(&mut candidates, &mut new_candidates);

                        sender
                            .send(Message::IncrementProgress(method_name_arc.clone()))
                            .unwrap();

                        // Add current positions
                        for i in 0..num_candidates {
                            if candidates.excluded[i] {
                                continue;
                            }

                            positions[i].push(vec![
                                candidates.cx[i] as f64 / (GRID_SIZE - 1) as f64,
                                candidates.cy[i] as f64 / (GRID_SIZE - 1) as f64,
                            ]);
                        }
                    });
                    // Visualize the candidate positions
                    let mut chart = Chart::new()
                        .title(
                            Title::new()
                                .text(format!("{method_name} (Nash)"))
                                .left("center"),
                        )
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
                        candidates
                            .cx
                            .into_iter()
                            .zip(candidates.cy.into_iter())
                            .enumerate()
                            .filter(|(i, _)| !candidates.excluded[*i])
                            .for_each(|(i, (cx, cy))| {
                                let position = vec![vec![
                                    cx as f64 / (GRID_SIZE - 1) as f64,
                                    cy as f64 / (GRID_SIZE - 1) as f64,
                                ]];
                                *chart = std::mem::replace(chart, Chart::new()).series(
                                    Scatter::new()
                                        .name(format!(
                                            "{i} ({}, [{}, {}])",
                                            positions[i].len(),
                                            position[0][0],
                                            position[0][1]
                                        ))
                                        .data(position),
                                );
                            });
                    }

                    let (save_file, uri) = super::get_file_and_uri(
                        super::SAVE_NASH_SINGLE,
                        &format!("{method_name}.svg"),
                    );
                    let mut renderer = ImageRenderer::new(400, 400);
                    renderer.save(&chart, &save_file).unwrap();
                    sender
                        .send(Message::ImageStandard(
                            ArcIntern::from("Nash Equilibriums (Single)"),
                            ArcIntern::from(method_name),
                            ArcIntern::from(uri),
                        ))
                        .unwrap();

                    // Visualise the paths of the candidates
                    let mut chart = Chart::new()
                        .title(
                            Title::new()
                                .text(format!("{method_name} (Nash, candidate paths)"))
                                .left("center"),
                        )
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
                                        .name(format!("{candidate} ({})", len))
                                        .symbol(Symbol::None),
                                )
                            });
                    }

                    let (save_file, uri) = super::get_file_and_uri(
                        super::SAVE_NASH_SINGLE,
                        &format!("{method_name} (candidate paths).svg"),
                    );
                    renderer.save(&chart, &save_file).unwrap();
                    let mut html_renderer = HtmlRenderer::new("Test1", 800, 800);
                    let (save_file, _) = super::get_file_and_uri(
                        super::SAVE_NASH_SINGLE,
                        &format!("{method_name} (candidate paths).html"),
                    );
                    html_renderer.save(&chart, save_file).unwrap();
                    sender
                        .send(Message::ImageStandard(
                            ArcIntern::from("Nash Equilibriums (Single)"),
                            ArcIntern::from(format!("{method_name} (Candidate Paths)")),
                            ArcIntern::from(uri),
                        ))
                        .unwrap();
                });

            sender.send(Message::SimulationFinished).unwrap();
        });
    }
}

pub const GRID_SIZE: usize = 101;

struct NashPopulace {
    populace: Vec<Vec<f64>>,
    num_candidates: usize,
    candidates_map: Vec<Option<usize>>,
}

impl NashPopulace {
    fn new_without_replacement(vx: &[f64], vy: &[f64], candidates: &mut Candidates) -> Self {
        let cxf = candidates
            .cx
            .iter()
            .enumerate()
            .filter(|(i, _)| !candidates.excluded[*i])
            .map(|(_, v)| v)
            .copied()
            .map(|x| (x as f64) / (GRID_SIZE - 1) as f64)
            .collect::<Vec<_>>();
        let cyf = candidates
            .cy
            .iter()
            .enumerate()
            .filter(|(i, _)| !candidates.excluded[*i])
            .map(|(_, v)| v)
            .copied()
            .map(|x| (x as f64) / (GRID_SIZE - 1) as f64)
            .collect::<Vec<_>>();

        let populace = crate::util::create_populace(vx, vy, &cxf, &cyf, candidates.num_candidates);

        // Create the index map
        let mut current_index = 0;
        let mut candidates_map = vec![None; candidates.cx.len()];
        for i in 0..candidates.cx.len() {
            if !candidates.excluded[i] {
                candidates_map[i] = Some(current_index);
                current_index += 1;
            }
        }

        Self {
            populace,
            num_candidates: candidates.num_candidates,
            candidates_map,
        }
    }

    fn new(
        vx: &[f64],
        vy: &[f64],
        candidates: &mut Candidates,
        replace: usize,
        (with_x, with_y): (usize, usize),
    ) -> Self {
        let cx = &mut candidates.cx;
        let cy = &mut candidates.cy;

        let (from_x, from_y) = (cx[replace], cy[replace]);

        cx[replace] = with_x;
        cy[replace] = with_y;

        let cxf = cx
            .iter()
            .enumerate()
            .filter(|(i, _)| !candidates.excluded[*i])
            .map(|(_, v)| v)
            .copied()
            .map(|x| (x as f64) / (GRID_SIZE - 1) as f64)
            .collect::<Vec<_>>();
        let cyf = cy
            .iter()
            .enumerate()
            .filter(|(i, _)| !candidates.excluded[*i])
            .map(|(_, v)| v)
            .copied()
            .map(|x| (x as f64) / (GRID_SIZE - 1) as f64)
            .collect::<Vec<_>>();

        let populace = crate::util::create_populace(vx, vy, &cxf, &cyf, candidates.num_candidates);

        cx[replace] = from_x;
        cy[replace] = from_y;

        // Create the index map
        let mut current_index = 0;
        let mut candidates_map = vec![None; candidates.cx.len()];
        for i in 0..candidates.cx.len() {
            if !candidates.excluded[i] {
                candidates_map[i] = Some(current_index);
                current_index += 1;
            }
        }

        Self {
            populace,
            num_candidates: candidates.num_candidates,
            candidates_map,
        }
    }
}

impl PopulaceMethod<Option<usize>> for NashPopulace {
    fn populace<'a>(&'a self) -> &'a [Vec<f64>] {
        &self.populace
    }

    fn candidate_at(&self, index: usize) -> methods::Result<Option<usize>> {
        Ok(self.candidates_map[index])
    }

    fn num_candidates(&self) -> usize {
        self.num_candidates
    }
}

#[derive(Clone, Debug)]
struct Candidates {
    cx: Vec<usize>,
    cy: Vec<usize>,
    excluded: Vec<bool>,
    best_scores: Vec<isize>,
    iters_since_best_score: Vec<usize>,
    num_candidates: usize,
}
