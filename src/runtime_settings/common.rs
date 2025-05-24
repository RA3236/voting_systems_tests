use methods::{PopulaceMethod, VectorMethod};
use rand::{Rng, SeedableRng, rngs::SmallRng};

use super::Distribution;

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
        distr: Distribution,
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
