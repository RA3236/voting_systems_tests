use crate::{CachedMethod, MethodsError, PopulaceMethod};

/// First Past the Post
pub fn fptp<T>(populace: &dyn PopulaceMethod<T>) -> crate::Result<T> {
    let num_candidates = populace.num_candidates();

    let mut counts = vec![0usize; num_candidates];
    populace.iter_populace(&mut |voter| {
        voter
            .into_iter()
            .enumerate()
            .min_by(|(_, a), (_, b)| a.total_cmp(b))
            .inspect(|&(i, _)| counts[i] += 1)
            .unwrap();
    })?;

    counts
        .into_iter()
        .enumerate()
        .max_by(|(_, a), (_, b)| a.cmp(b))
        .map(|(i, _)| populace.candidate_at(i))
        .ok_or_else(|| MethodsError::Generic(format!("malformed array")))?
}

pub struct FptpCached {
    populace: Vec<Vec<f64>>,
    current_voter_preferred: Vec<usize>,
    current_counts: Vec<usize>,
    current_winner: usize,
    cached_counts: Vec<usize>,
    num_voters: usize,
    updated: bool,
}

impl CachedMethod for FptpCached {
    fn new(
        initial_populace: Vec<Vec<f64>>,
        num_voters: usize,
        num_candidates: usize,
    ) -> crate::Result<Box<dyn CachedMethod + Send>>
    where
        Self: Sized,
    {
        // We need to simulate an election now in order to calculate the current maximum distances
        let mut current_counts = vec![0usize; num_candidates];
        let current_voter_preferred = initial_populace
            .iter()
            .map(|preferences| {
                let max = preferences
                    .into_iter()
                    .copied()
                    .enumerate()
                    .max_by(|(_, a), (_, b)| a.total_cmp(b))
                    .map(|(i, _)| i)
                    .ok_or_else(|| {
                        crate::MethodsError::Generic(format!("Malformed populace array"))
                    })?;
                current_counts[max] += 1;
                Ok(max)
            })
            .collect::<crate::Result<_>>()?;
        let current_winner = current_counts
            .iter()
            .copied()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.cmp(b))
            .map(|(i, _)| i)
            .ok_or_else(|| crate::MethodsError::Generic(format!("Malformed populace array")))?;

        Ok(Box::new(Self {
            populace: initial_populace,
            num_voters,
            cached_counts: current_counts.clone(),
            current_counts,
            current_voter_preferred,
            current_winner,
            updated: false,
        }))
    }

    fn get_current_result(&mut self) -> crate::Result<usize> {
        if self.updated {
            self.updated = false;
            self.current_counts.fill(0);
            for voter in 0..self.num_voters {
                let distances = &self.populace[voter];
                let mut min = f64::MAX;
                let mut min_index = 0;
                for candidate in 0..distances.len() {
                    let dist = distances[candidate];
                    if min > dist {
                        min = dist;
                        min_index = candidate;
                    }
                }
                self.current_counts[min_index] += 1;
            }
            self.current_winner = self
                .current_counts
                .iter()
                .copied()
                .enumerate()
                .max_by(|(_, a), (_, b)| a.cmp(b))
                .map(|(i, _)| i)
                .ok_or_else(|| crate::MethodsError::Generic(format!("Malformed populace array")))?;
        }

        Ok(self.current_winner)
    }

    fn mock_score_candidate(
        &mut self,
        candidate: usize,
        new_distances: &[f64],
    ) -> crate::Result<isize> {
        self.cached_counts.copy_from_slice(&self.current_counts);
        for voter in 0..self.num_voters {
            let current_preferred = self.current_voter_preferred[voter];

            let distances = &self.populace[voter];
            if distances[current_preferred] > new_distances[candidate] {
                self.cached_counts[current_preferred] -= 1;
                self.cached_counts[candidate] += 1;
            }
        }

        let winner = self
            .cached_counts
            .iter()
            .copied()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.cmp(b))
            .map(|(i, _)| i)
            .ok_or_else(|| crate::MethodsError::Generic(format!("Malformed populace array")))?;

        Ok(match candidate == winner {
            true => self.cached_counts[candidate] as isize,
            false => self.cached_counts[candidate] as isize - self.num_voters as isize,
        })
    }

    fn update_candidate(&mut self, candidate: usize, new_distances: &[f64]) -> crate::Result<()> {
        for voter in 0..self.num_voters {
            self.populace[voter][candidate] = new_distances[voter];
        }
        self.updated = true;

        Ok(())
    }
}
