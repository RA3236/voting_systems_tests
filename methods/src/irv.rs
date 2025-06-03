use std::usize;

use crate::{CachedMethod, MethodsError, PopulaceMethod};

/// Instant Runoff
pub fn irv<T>(populace: &dyn PopulaceMethod<T>) -> crate::Result<T> {
    let num_candidates = populace.num_candidates();

    let mut counts = vec![0usize; num_candidates];
    let mut still_running = vec![true; num_candidates];
    for _ in 0..num_candidates - 1 {
        counts.fill(0);

        populace.iter_populace(&mut |voter| {
            voter
                .into_iter()
                .enumerate()
                .filter(|&(i, _)| still_running[i])
                .min_by(|(_, a), (_, b)| a.total_cmp(b))
                .inspect(|&(i, _)| counts[i] += 1)
                .unwrap();
        })?;

        // Determine who is to be excluded
        let least = counts
            .iter()
            .enumerate()
            .filter(|&(i, _)| still_running[i])
            .min_by(|(_, a), (_, b)| a.cmp(b))
            .map(|(i, _)| i)
            .ok_or_else(|| MethodsError::Generic(format!("malformed populace array")))?;
        still_running[least] = false;
    }

    // Winner is whoever isn't excluded
    still_running
        .into_iter()
        .enumerate()
        .find(|(_, v)| *v)
        .map(|(i, _)| populace.candidate_at(i))
        .ok_or_else(|| MethodsError::Generic(format!("malformed populace array")))?
}

pub struct IrvCached {
    populace: Vec<Vec<f64>>,

    // Cached data
    // This data is modified in a manner that helps prevent a full election change
    // There are multiple caches:
    // Stores the populace, copied from populace to prevent mutation in mock rounds
    mock_populace: Vec<Vec<f64>>,
    // Stores the eliminated candidates each round
    is_still_running: Vec<Vec<bool>>,
    // Stores the eliminated candidates each round, copied from is_still_running to prevent
    // mutation in mock rounds
    mock_is_still_running: Vec<Vec<bool>>,
    // Stores the eliminated indices
    eliminated: Vec<usize>,
    // Stores the eliminated indices, copied from eliminated to prevent mutation
    mock_eliminated: Vec<usize>,
    // Stores the counts per round
    counts_per_round: Vec<Vec<usize>>,
    // Stores the counts per round, copied from counts_per_round to prevent mutation
    mock_counts_per_round: Vec<Vec<usize>>,
    // Stores the voter preference order (most preferred is first)
    // This is also relatively performant (more first-preference votes = less likely to be eliminated early)
    populace_ordered: Vec<Vec<usize>>,
    // Stores the voter preference order, copied from populace_ordered to prevent mutation
    mock_populace_ordered: Vec<Vec<usize>>,

    current_winner: usize,
    num_voters: usize,
    num_candidates: usize,

    // Whether or not we need to recalculate the results because of a change
    // This is only set if we detect that the winner may be unable to win with
    // the change
    recalculate: bool,

    // A working array
    working: Vec<usize>
}

impl CachedMethod for IrvCached {
    // Create the initial state.
    fn new(
        initial_populace: Vec<Vec<f64>>,
        num_voters: usize,
        num_candidates: usize,
    ) -> crate::Result<Box<dyn CachedMethod + Send>>
    where
        Self: Sized,
    {
        let populace_ordered: Vec<Vec<usize>> = (0..num_voters)
            .map(|voter| {
                // We sort here so that we aren't doing it later. We can sort later if there is a change.
                let distances = &initial_populace[voter];
                let mut indices = (0..num_candidates).collect::<Vec<_>>();
                indices.sort_unstable_by(|&a, &b| distances[a].total_cmp(&distances[b]));
                indices
            })
            .collect();
        let counts_per_round: Vec<Vec<usize>> = vec![vec![0usize; num_candidates]; num_candidates];
        let is_still_running: Vec<Vec<bool>> = vec![vec![true; num_candidates]; num_candidates];
        Ok(Box::new(Self {
            populace: initial_populace.clone(),
            mock_populace: initial_populace,
            mock_is_still_running: is_still_running.clone(),
            mock_counts_per_round: counts_per_round.clone(),
            mock_populace_ordered: populace_ordered.clone(),
            eliminated: vec![0; num_candidates],
            mock_eliminated: vec![0; num_candidates],
            is_still_running,
            counts_per_round,
            populace_ordered,
            current_winner: 0,
            num_voters,
            num_candidates,
            recalculate: true,
            working: vec![0; num_candidates]
        }))
    }

    // Get the current result, calculating it if necessary
    fn get_current_result(&mut self) -> crate::Result<usize> {
        if self.recalculate {
            self.recalculate = false;
            // Reset all states
            self.counts_per_round
                .iter_mut()
                .for_each(|round| round.fill(0));
            self.is_still_running
                .iter_mut()
                .for_each(|round| round.fill(true));

            // Compute the winner
            for round in 0..self.num_candidates {
                // Calculate current first-preference votes
                for voter in 0..self.num_voters {
                    for position in 0..self.num_candidates {
                        let candidate = self.populace_ordered[voter][position];
                        if self.is_still_running[round][candidate] {
                            self.counts_per_round[round][candidate] += 1;
                            break;
                        }
                    }
                }

                // Determine the candidate with the current lowest vote share
                let mut lowest_index = usize::MAX;
                let mut lowest_count = usize::MAX;
                for candidate in 0..self.num_candidates {
                    let count = self.counts_per_round[round][candidate];
                    if count < lowest_count {
                        lowest_count = count;
                        lowest_index = candidate;
                    }
                }
                if lowest_index >= self.num_candidates {
                    return Err(crate::MethodsError::Generic("unreachable: no lowest index found".to_string()));
                }

                // Eliminate/finish
                if round == self.num_candidates - 1 {
                    self.current_winner = lowest_index;
                } else {
                    // We have to eliminate across all rounds
                    self.eliminated[round] = lowest_index;
                    for i in round + 1 .. self.num_candidates {
                        self.is_still_running[i][lowest_index] = false;
                    }
                }
            }

            // Copy all of the data to the mock data, so that it can be used by [`Self::mock_score_candidate`]
            self.mock_populace
                .iter_mut()
                .enumerate()
                .for_each(|(i, data)| data.copy_from_slice(&self.populace[i]));
            self.mock_counts_per_round
                .iter_mut()
                .enumerate()
                .for_each(|(i, data)| data.copy_from_slice(&self.counts_per_round[i]));
            self.mock_is_still_running
                .iter_mut()
                .enumerate()
                .for_each(|(i, data)| data.copy_from_slice(&self.is_still_running[i]));
            self.mock_populace_ordered
                .iter_mut()
                .enumerate()
                .for_each(|(i, data)| data.copy_from_slice(&self.populace_ordered[i]));
            self.mock_eliminated.copy_from_slice(&self.eliminated);
        }

        Ok(self.current_winner)
    }

    // Calculate the result as if we replaced the distances from each voter to the candidate with the
    // input array. Does not change the current winner.
    fn mock_score_candidate(
        &mut self,
        candidate: usize,
        new_distances: &[f64],
    ) -> crate::Result<isize> {
        let mut any_change = false;
        let mut recalculate = false;
        for voter in 0..self.num_voters {
            let populace = &mut self.mock_populace[voter];
            let new_ordering = &mut self.mock_populace_ordered[voter];

            // Update the distance
            populace[candidate] = new_distances[voter];

            // No need to check if we already have a major change
            if !recalculate {
                // Check that we are indeed still in the correct order
                // Custom check to stop when we encounter the candidate
                let is_sorted = new_ordering.is_sorted_by(|&a, &b| populace[a] <= populace[b]);

                if !is_sorted {
                    any_change = true;
                    // Create a copy of the array so we have the old ordering
                    let old_ordering = &mut self.working;
                    old_ordering.copy_from_slice(&new_ordering);

                    // Sort the current ordering
                    // Note: this may result in equal-distance candidates getting reordered. We
                    // don't particularly care because those are ties.
                    new_ordering.sort_unstable_by(|&a, &b| populace[a].total_cmp(&populace[b]));

                    // The current index of the comparison (i.e. taking into account exclusions) 
                    let mut current_index = 0;
                    // Whether we have started changes
                    let mut change_occurred = false;
                    let mut start_round = 0;
                    'round:
                    for round in 0..self.num_candidates {
                        loop {
                            if new_ordering[current_index] == old_ordering[current_index] {
                                // If we are eliminated here, increment the index
                                if !self.mock_is_still_running[round][new_ordering[current_index]] {
                                    current_index += 1;
                                }
                                // Otherwise just continue the main loop (no changes)
                                else {
                                    continue 'round;
                                }
                            } else {
                                // If both sides are eliminated, then we ignore this difference
                                // since it will be caught later
                                let a = new_ordering[current_index];
                                let b = old_ordering[current_index];
                                if !self.mock_is_still_running[round][a] && !self.mock_is_still_running[round][b] {
                                    current_index += 1;
                                }
                                // Otherwise we have hit a change, and should break now
                                else {
                                    change_occurred = true;
                                    start_round = round;
                                    break 'round;
                                }
                            }
                        }
                    }

                    if change_occurred {
                        // We now modify all rounds for both orderings
                        let mut old_index = current_index;
                        let mut new_index = current_index;
                        for round in start_round..self.num_candidates {
                            // Increment indices if eliminated
                            while !self.mock_is_still_running[round][old_ordering[old_index]] {
                                old_index += 1;
                            }
                            while !self.mock_is_still_running[round][new_ordering[new_index]] {
                                new_index += 1;
                            }

                            // Modify the counts
                            self.mock_counts_per_round[round][old_ordering[old_index]] -= 1;
                            self.mock_counts_per_round[round][new_ordering[new_index]] += 1;

                            // Get the index of the lowest count in this round
                            let lowest_index = self.mock_counts_per_round[round]
                                .iter()
                                .copied()
                                .enumerate()
                                .filter(|&(i, _)| self.mock_is_still_running[round][i])
                                .min_by(|(_, a), (_, b)| a.cmp(b))
                                .map(|(i, _)| i)
                                .ok_or_else(|| crate::MethodsError::Generic("unreachable: no lowest index found in change detection (mock)".to_string()))?;
                            // If the index is different, we need to recalculate
                            if lowest_index != self.mock_eliminated[round] {
                                recalculate = true;
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Recalculate data as needed
        if recalculate {
            // Reset all states
            self.mock_counts_per_round
                .iter_mut()
                .for_each(|round| round.fill(0));
            self.mock_is_still_running
                .iter_mut()
                .for_each(|round| round.fill(true));

            // Compute the winner
            for round in 0..self.num_candidates {
                // Calculate current first-preference votes
                for voter in 0..self.num_voters {
                    for position in 0..self.num_candidates {
                        let candidate = self.mock_populace_ordered[voter][position];
                        if self.mock_is_still_running[round][candidate] {
                            self.mock_counts_per_round[round][candidate] += 1;
                            break;
                        }
                    }
                }

                // Determine the candidate with the current lowest vote share
                let mut lowest_index = usize::MAX;
                let mut lowest_count = usize::MAX;
                for candidate in 0..self.num_candidates {
                    let count = self.mock_counts_per_round[round][candidate];
                    if count < lowest_count {
                        lowest_count = count;
                        lowest_index = candidate;
                    }
                }
                if lowest_index >= self.num_candidates {
                    return Err(crate::MethodsError::Generic("unreachable: unable to find lowest index (mock)".to_string()));
                }

                // Eliminate/finish
                if round == self.num_candidates - 1 {
                    // nothing
                } else {
                    // We have to eliminate across all rounds
                    self.mock_eliminated[round] = lowest_index;
                    for i in round + 1 .. self.num_candidates {
                        self.mock_is_still_running[i][lowest_index] = false;
                    }
                }
            }
        }

        // Calculate the score: round_count + (10000 * round_num)
        let score = self.mock_eliminated
            .iter()
            .copied()
            .enumerate()
            .find_map(|(round, c)| {
                if c == candidate {
                    Some(self.mock_counts_per_round[round][c] as isize + (round * 10000) as isize)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| self.mock_counts_per_round[self.num_candidates - 1][candidate] as isize + ((self.num_candidates - 1) * 10000) as isize);

        // Copy all data back over
        if any_change || recalculate {
            self.mock_populace
                .iter_mut()
                .enumerate()
                .for_each(|(i, data)| data.copy_from_slice(&self.populace[i]));
            self.mock_counts_per_round
                .iter_mut()
                .enumerate()
                .for_each(|(i, data)| data.copy_from_slice(&self.counts_per_round[i]));
            self.mock_is_still_running
                .iter_mut()
                .enumerate()
                .for_each(|(i, data)| data.copy_from_slice(&self.is_still_running[i]));
            self.mock_populace_ordered
                .iter_mut()
                .enumerate()
                .for_each(|(i, data)| data.copy_from_slice(&self.populace_ordered[i]));
            self.mock_eliminated.copy_from_slice(&self.eliminated);
        }

        Ok(score)
    }

    // Updates the candidate with the new distance data.
    fn update_candidate(&mut self, candidate: usize, new_distances: &[f64]) -> crate::Result<()> {
        'main:
        for voter in 0..self.num_voters {
            let populace = &mut self.populace[voter];
            let new_ordering = &mut self.populace_ordered[voter];

            // Update the distance
            populace[candidate] = new_distances[voter];

            // No need to check if we already have a major change
            if !self.recalculate {
                // Check that we are indeed still in the correct order
                // Custom check to stop when we encounter the candidate
                let is_sorted = new_ordering.is_sorted_by(|&a, &b| populace[a] <= populace[b]);

                if !is_sorted {
                    // Create a copy of the array so we have the old ordering
                    let old_ordering = &mut self.working;
                    old_ordering.copy_from_slice(&new_ordering);

                    // Sort the current ordering
                    // Note: this may result in equal-distance candidates getting reordered.
                    new_ordering.sort_unstable_by(|&a, &b| populace[a].total_cmp(&populace[b]));

                    // The current index of the comparison (i.e. taking into account exclusions) 
                    let mut current_index = 0;
                    // Whether we have started changes
                    let mut change_occurred = false;
                    let mut start_round = 0;
                    'round:
                    for round in 0..self.num_candidates {
                        loop {
                            if new_ordering[current_index] == old_ordering[current_index] {
                                // If we are eliminated here, increment the index
                                if !self.is_still_running[round][new_ordering[current_index]] {
                                    current_index += 1;
                                }
                                // Otherwise just continue the main loop (no changes)
                                else {
                                    continue 'round;
                                }
                            } else {
                                // If both sides are eliminated, then we ignore this difference
                                // since it will be caught later
                                let a = new_ordering[current_index];
                                let b = old_ordering[current_index];
                                if !self.is_still_running[round][a] && !self.is_still_running[round][b] {
                                    current_index += 1;
                                }
                                // Otherwise we have hit a change, and should break now
                                else {
                                    change_occurred = true;
                                    start_round = round;
                                    break 'round;
                                }
                            }
                        }
                    }

                    if change_occurred {
                        // We now modify all rounds for both orderings
                        let mut old_index = current_index;
                        let mut new_index = current_index;
                        for round in start_round..self.num_candidates {
                            // Increment indices if eliminated
                            while !self.is_still_running[round][old_ordering[old_index]] {
                                old_index += 1;
                            }
                            while !self.is_still_running[round][new_ordering[new_index]] {
                                new_index += 1;
                            }

                            // Modify the counts
                            self.counts_per_round[round][old_ordering[old_index]] -= 1;
                            self.counts_per_round[round][new_ordering[new_index]] += 1;

                            // Get the index of the lowest count in this round
                            let lowest_index = self.counts_per_round[round]
                                .iter()
                                .copied()
                                .enumerate()
                                .filter(|&(i, _)| self.is_still_running[round][i])
                                .min_by(|(_, a), (_, b)| a.cmp(b))
                                .map(|(i, _)| i)
                                .ok_or_else(|| crate::MethodsError::Generic("unreachable: no lowest index found in change detection".to_string()))?;
                            // If the index is different, we need to recalculate
                            if lowest_index != self.eliminated[round] {
                                self.recalculate = true;
                                continue 'main;
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }
}
