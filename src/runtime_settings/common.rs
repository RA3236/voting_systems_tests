use charming::{
    Chart, ImageRenderer,
    component::{Axis, Title, VisualMap, VisualMapChannel},
    datatype::DataPoint,
    df,
    element::{AxisType, Orient},
    series::Heatmap,
};
use internment::ArcIntern;
use kiddo::{KdTree, traits::DistanceMetric};
use methods::{PopulaceMethod, VectorMethod};
use rand::{Rng, SeedableRng, rngs::SmallRng};

use crate::Message;

use super::DistributionManager;
use super::constants::*;

pub fn linspace(start: f64, end: f64, steps: usize, endpoint: bool) -> Vec<f64> {
    let step_size = (end - start) / (steps - endpoint as usize) as f64;
    (0..steps)
        .into_iter()
        .map(|i| start + (i as f64 * step_size))
        .collect()
}

/// Bar chart with multiple series
///
/// This assumes that all stacked bars will sum up nicely to the same value.

/// Visualize a set of points
pub fn plot_density<D: DistanceMetric<f64, 2>, V: IntoDistanceCount<D>>(
    values: V,
    sender: &std::sync::mpsc::Sender<crate::Message>,
    title: &str,
    save_file: &str,
    uri: &str,
    category: &str,
) {
    let chart = create_density_chart(values, title, true);

    let mut renderer = ImageRenderer::new(400, 400);
    renderer.save(&chart, save_file).unwrap();

    sender
        .send(Message::ImageStandard(
            ArcIntern::from(category),
            ArcIntern::from(title),
            ArcIntern::from(uri),
        ))
        .unwrap();
}

pub fn create_density_chart<D: DistanceMetric<f64, 2>, V: IntoDistanceCount<D>>(
    values: V,
    title: &str,
    visual_map: bool,
) -> Chart {
    let (max, z) = values.into_kdtree(&DENSITY_X, &DENSITY_Y);

    let mut chart = Chart::new()
        .title(Title::new().left("center").text(title))
        .background_color("#ffffff")
        .series(
            Heatmap::new()
                .data(z)
                //.name(title)
                .x_axis_index(0)
                .y_axis_index(0),
        )
        .x_axis(
            Axis::new()
                .type_(AxisType::Category)
                .data(DENSITY_X_LABELS.to_vec()),
        )
        .y_axis(
            Axis::new()
                .type_(AxisType::Category)
                .data(DENSITY_Y_LABELS.to_vec()),
        );

    if visual_map {
        chart = chart.visual_map(
            VisualMap::new()
                .min(0.0)
                .max(max as f64)
                .calculable(true)
                .orient(Orient::Horizontal)
                .left("center")
                .in_range(VisualMapChannel::new().color(vec![
                    "#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#ffffbf", "#fee090",
                    "#fdae61", "#f46d43", "#d73027", "#a50026",
                ])),
        )
    } else {
        chart = chart.visual_map(
            VisualMap::new()
                .min(0.0)
                .max(max as f64)
                .show(false)
                .series_index(0)
                .in_range(VisualMapChannel::new().color(vec![
                    "#31369544",
                    "#4575b444",
                    "#74add144",
                    "#abd9e944",
                    "#e0f3f844",
                    "#ffffbf44",
                    "#fee09044",
                    "#fdae6144",
                    "#f46d4344",
                    "#d7302744",
                    "#a5002644",
                ])),
        )
    }
    chart
}

pub trait IntoDistanceCount<D: DistanceMetric<f64, 2>> {
    fn into_kdtree(self, x: &[f64], y: &[f64]) -> (i64, Vec<Vec<DataPoint>>);

    fn into_datapoints(kdtree: KdTree<f64, 2>, x: &[f64], y: &[f64]) -> (i64, Vec<Vec<DataPoint>>) {
        let kdtree = &kdtree;

        let mut max = 0;

        let radius = D::dist(&[0.01, 0.01], &[0.0, 0.0]);

        let z = x
            .into_iter()
            .copied()
            .zip(DENSITY_X_LABELS.iter())
            .flat_map(|(x, xname)| {
                y.into_iter()
                    .copied()
                    .zip(DENSITY_Y_LABELS.iter())
                    .map(|(y, yname)| {
                        let count =
                            kdtree.within_unsorted_iter::<D>(&[x, y], radius).count() as i64;
                        max = max.max(count);
                        df![xname.clone(), yname.clone(), count]
                    })
                    .collect::<Vec<_>>()
            })
            .collect();
        (max, z)
    }
}

impl<D: DistanceMetric<f64, 2>> IntoDistanceCount<D> for Vec<[f64; 2]> {
    fn into_kdtree(self, x: &[f64], y: &[f64]) -> (i64, Vec<Vec<DataPoint>>) {
        let kdtree = KdTree::from(&self);
        <Self as IntoDistanceCount<D>>::into_datapoints(kdtree, x, y)
    }
}

impl<'a, D: DistanceMetric<f64, 2>> IntoDistanceCount<D> for (&'a [f64], &'a [f64]) {
    fn into_kdtree(self, x: &[f64], y: &[f64]) -> (i64, Vec<Vec<DataPoint>>) {
        let kdtree = self
            .0
            .into_iter()
            .copied()
            .zip(self.1.into_iter().copied())
            .map(|(x, y)| [x, y])
            .enumerate()
            .map(|(i, v)| (v, i as u64))
            .collect::<KdTree<f64, 2>>();
        <Self as IntoDistanceCount<D>>::into_datapoints(kdtree, x, y)
    }
}

macro_rules! define_winner_type {
    ($($name:ident, $str:expr),*) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub enum WinnerType {
            $($name),*
        }

        impl WinnerType {
            pub fn iter() -> impl Iterator<Item = Self> {
                [
                    $(Self::$name),*
                ]
                    .into_iter()
            }

            pub fn to_str(&self) -> &'static str {
                match self {
                    $(Self::$name => $str),*
                }
            }
        }
    };
}

define_winner_type! {
    NoCondorcetButAverage, "closest to average",
    NoCondorcetAndNotAverage, "not closest to average",
    NotCondorcetButAverage, "not Condorcet, closest to average",
    NotCondorcetAndNotAverage, "not Condorcet",
    CondorcetAndAverage, "Condorcet, closest to average",
    CondorcetButNotAverage, "Condorcet"
}

#[derive(Clone)]
pub struct StandardData {
    vx: Vec<f64>,
    vy: Vec<f64>,
    cx: Vec<f64>,
    cy: Vec<f64>,
    num_voters: usize,
    num_candidates: usize,
    num_iterations: usize,
}

impl VectorMethod for StandardData {
    fn into_data<'a>(
        &'a self,
    ) -> (
        (&'a [f64], &'a [f64]),
        (&'a [f64], &'a [f64]),
        usize,
        usize,
        usize,
    ) {
        (
            (&self.vx, &self.vy),
            (&self.cx, &self.cy),
            self.num_voters,
            self.num_candidates,
            self.num_iterations,
        )
    }
}

impl StandardData {
    pub fn new(
        distr: DistributionManager,
        num_voters: usize,
        num_candidates: usize,
        num_iterations: usize,
    ) -> Self {
        let (vx, vy, cx, cy) = distr.into_default_distribution();

        let mut rng = SmallRng::from_os_rng();

        fn create_random<R>(num: usize, rng: &mut R, distr: Box<dyn Fn(&mut R) -> f64>) -> Vec<f64>
        where
            R: Rng,
        {
            (0..num).into_iter().map(|_| distr(rng)).collect()
        }

        let vx = create_random(num_voters, &mut rng, vx);
        let vy = create_random(num_voters, &mut rng, vy);
        let cx = create_random(num_candidates * num_iterations, &mut rng, cx);
        let cy = create_random(num_candidates * num_iterations, &mut rng, cy);

        Self {
            vx,
            vy,
            cx,
            cy,
            num_voters,
            num_candidates,
            num_iterations,
        }
    }

    pub fn create_populace<'a>(&'a self, i: usize) -> StandardPopulace<'a> {
        let range = (i * self.num_candidates)..((i + 1) * self.num_candidates);
        let (cx, cy) = (&self.cx[range.clone()], &self.cy[range]);

        let populace =
            crate::util::create_populace(&self.vx, &self.vy, cx, cy, self.num_candidates);
        StandardPopulace {
            cx,
            cy,
            populace,
            num_candidates: self.num_candidates,
        }
    }
}

#[derive(Clone)]
pub struct StandardPopulace<'a> {
    cx: &'a [f64],
    cy: &'a [f64],
    populace: Vec<Vec<f64>>,
    num_candidates: usize,
}

impl<'a> PopulaceMethod<[f64; 2]> for StandardPopulace<'a> {
    fn populace<'b>(&'b self) -> &'b [Vec<f64>] {
        &self.populace
    }

    fn candidate_at(&self, index: usize) -> methods::Result<[f64; 2]> {
        Ok([self.cx[index], self.cy[index]])
    }

    fn num_candidates(&self) -> usize {
        self.num_candidates
    }
}
