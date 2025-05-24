#![feature(portable_simd)]
#![feature(array_chunks)]
#![deny(missing_docs)]

//! A collection of voting methods.

/// The result type for this crate.
pub type Result<T> = std::result::Result<T, MethodsError>;

mod anti_plurality;
mod average;
mod condorcet;
mod fptp;
mod irv;
mod ranked_pairs;
mod score;

use anti_plurality::*;
pub use average::*;
pub use condorcet::*;
use fptp::*;
use irv::*;
use ranked_pairs::*;
use score::*;

macro_rules! handle_function {
    (box ($min:literal, $max:literal, $func:expr)) => {
        Box::new(score::<T>(
            $min,
            $max,
            std::f64::consts::FRAC_1_SQRT_2,
            $func,
        ))
    };
    (box $func:expr) => {
        Box::new($func)
    };
    (score_fn $max_dist:ident, ($min:literal, $max:literal, $func:expr)) => {
        Some(Box::new(score::<T>($min, $max, $max_dist, $func)))
    };
    (score_fn $max_dist:ident, $func:expr) => {
        None
    };
    (is_score ($min:literal, $max:literal, $func:expr)) => {
        true
    };
    (is_score $func:expr) => {
        false
    };
}

macro_rules! define_method {
    ($($name:ident, $str:literal, $desc:literal, $func:tt);*) => {
        /// The methods this crate defines.
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum Method {
            $(#[doc(hidden)] $name),*
        }

        impl Method {
            /// An iterator over all methods supported by this crate.
            pub fn iter() -> impl Iterator<Item = Self> {
                [
                    $(Self::$name),*
                ]
                    .into_iter()
            }

            /// The method name as a string.
            pub fn name(&self) -> &'static str {
                match self {
                    $(Self::$name => $str),*
                }
            }

            /// The method description.
            pub fn desc(&self) -> &'static str {
                match self {
                    $(Self::$name => $desc),*
                }
            }

            /// Whether this method is a score function.
            pub fn is_score(&self) -> bool {
                match self {
                    $(Self::$name => handle_function!(is_score $func)),*
                }
            }


            /// For non-score methods, their method function.
            ///
            /// Each function takes in a `&`[`PopulaceMethod`]`<T>` and outputs a `Result<T>`.
            pub fn func<T: 'static>(&self) -> MethodFn<T> {
                match self {
                    $(Self::$name => handle_function!(box $func)),*
                }
            }

            /// For score methods, constructs the method function given a maximum distance.
            pub fn score_fn<T: 'static>(&self, max_dist: f64) -> Option<MethodFn<T>> {
                match self {
                    $(Self::$name => handle_function!(score_fn max_dist, $func)),*
                }
            }
        }
    };
}

/// The function type for each method.
pub type MethodFn<T> = Box<dyn Fn(&dyn PopulaceMethod<T>) -> Result<T> + Send + Sync>;

define_method! {
    FirstPastThePost, "First Past the Post", "Vote once for closest; candidate with the most votes wins", fptp;
    InstantRunoff, "Instant Runoff", "Rank candidates; eliminate least-votes then distribute preferences until one remains", irv;
    RankedPairs, "Ranked Pairs", "Rank candidates; sort preference margins, eliminate cycles, find Condorcet winner", ranked_pairs;
    AntiPlurality, "Anti-Plurality", "Vote once for furthest; candidate with the least votes wins", anti_plurality
    // Approval, "Approval", "Vote for closest few; candidate with the most votes wins", (0, 1, single_round);
    // Score, "Score", "Score candidates; candidate with the highest score wins", (0, 10, single_round);
    // Star, "STAR", "Score candidates; find highest two scores, then FPTP runoff between those two candidates", (0, 5, two_round);
    // StarD, "STAR doubled", "STAR with double maximum score (same as Score, but with runoff)", (0, 10, two_round)
}

impl Default for Method {
    fn default() -> Self {
        Self::FirstPastThePost
    }
}

/// An Error that can be returned from a method.
#[derive(Debug, thiserror::Error)]
pub enum MethodsError {
    /// Array sizes do not match
    #[error("size mismatch: expected {}, found {}", .0, .1)]
    SizeMismatch(usize, usize),
    /// Some error occurred
    #[error("error occurred: {}", .0)]
    Generic(String),
}

/// A type that can be passed to methods that return vectors of candidates.
///
/// Validation is performed inside the methods, so implementors can ignore validation.
pub trait VectorMethod {
    /// Deconstruct this data. The returned data is a tuple, with each element consisting of:
    ///
    /// 1. Voters, with X and Y coordinates in separate slices
    /// 2. Candidates, in the same form
    /// 3. Number of voters
    /// 4. Number of candidates
    /// 5. Number of iterations
    ///
    /// The candidates should be stored as `num_iters` blocks of `num_candidates` candidates.
    fn into_data<'a>(
        &'a self,
    ) -> (
        (&'a [f64], &'a [f64]),
        (&'a [f64], &'a [f64]),
        usize,
        usize,
        usize,
    );
}

/// A type that can be passed to methods expecting a populace slice.
///
/// A populace slice is a slice of length `num_voters` which contains
/// equally-sized slices of length `num_candidates`, which contains the
/// distance from the voter to the candidate.
pub trait PopulaceMethod<T> {
    /// Returns the populace slice.
    ///
    /// Note that this returns a reference, which may be held in self.
    fn populace<'a>(&'a self) -> &'a [Vec<f64>];

    /// Returns the candidate corresponding to this index, if it exists.
    fn candidate_at(&self, index: usize) -> Result<T>;

    /// Returns the number of candidates in this populace slice.
    fn num_candidates(&self) -> usize;

    /// Iterates over the populace slice, performing some action.
    ///
    /// You should never implement this directly.
    fn iter_populace(&self, func: &mut dyn FnMut(&[f64])) -> Result<()> {
        let num_candidates = self.num_candidates();
        if num_candidates == 0 {
            return Err(MethodsError::Generic(format!(
                "number of candidates must not be zero"
            )));
        }
        self.populace()
            .into_iter()
            .map(|voter| {
                if voter.len() != num_candidates {
                    return Err(MethodsError::SizeMismatch(num_candidates, voter.len()));
                }

                func(voter);
                Ok(())
            })
            .collect::<Result<()>>()
    }
}
