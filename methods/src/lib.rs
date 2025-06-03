#![feature(portable_simd)]
#![feature(array_chunks)]
#![feature(array_windows)]

//! A collection of voting methods.

/// The result type for this crate.
pub type Result<T> = std::result::Result<T, MethodsError>;

pub mod average;
pub mod condorcet;
pub mod fptp;
pub mod irv;
pub mod ranked_pairs;

macro_rules! define_method {
    ($($name:ident, $str:literal, $desc:literal, $func:ident, $cached:ident);*) => {
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

            /// For non-score methods, their method function.
            ///
            /// Each function takes in a `&`[`PopulaceMethod`]`<T>` and outputs a `Result<T>`.
            pub fn func<T: 'static>(&self) -> MethodFn<T> {
                match self {
                    $(Self::$name => Box::new($func::$func)),*
                }
            }

            pub fn cached(&self, populace: Vec<Vec<f64>>, num_voters: usize, num_candidates: usize) -> Result<Box<dyn CachedMethod + Send>> {
                match self {
                    $(Self::$name => $func::$cached::new(populace, num_voters, num_candidates)),*
                }
            }
        }
    };
}

/// The function type for each method.
pub type MethodFn<T> = Box<dyn Fn(&dyn PopulaceMethod<T>) -> Result<T> + Send + Sync>;

define_method! {
    FirstPastThePost, "First Past the Post", "Vote once for closest; candidate with the most votes wins", fptp, FptpCached;
    InstantRunoff, "Instant Runoff", "Rank candidates; eliminate least-votes then distribute preferences until one remains", irv, IrvCached;
    RankedPairs, "Ranked Pairs", "Rank candidates; sort preference margins, eliminate cycles, find Condorcet winner", ranked_pairs, RankedPairsCached
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

pub trait CachedMethod {
    fn new(
        initial_populace: Vec<Vec<f64>>,
        num_voters: usize,
        num_candidates: usize,
    ) -> Result<Box<dyn CachedMethod + Send>>
    where
        Self: Sized;

    fn get_current_result(&mut self) -> Result<usize>;

    fn mock_score_candidate(&mut self, candidate: usize, distances: &[f64]) -> Result<isize>;

    fn update_candidate(&mut self, candidate: usize, new_distances: &[f64]) -> Result<()>;
}
