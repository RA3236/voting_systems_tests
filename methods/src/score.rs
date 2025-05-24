use std::cmp::Ordering;

use crate::{MethodsError, PopulaceMethod};

#[inline(always)]
fn linear_score(max_score: usize, bin_size: f64, dist: f64) -> usize {
    max_score - (dist / bin_size) as usize
}

/// Score voting.
///
/// This is a higher-order function, which means you need to compose it
/// via both a score function (which assigns scores to distances), and a function to pick
/// the winner from the number of candidates and list of counts for each candidate.
pub fn score<T>(
    min_score: usize,
    max_score: usize,
    max_dist: f64,
    pick_winner: fn(usize, f64, &dyn PopulaceMethod<T>, &[usize]) -> crate::Result<T>,
) -> impl Fn(&dyn PopulaceMethod<T>) -> crate::Result<T> {
    let bin_size = max_dist / (max_score - min_score) as f64;
    move |populace| {
        let num_candidates = populace.num_candidates();
        if num_candidates == 1 {
            return populace.candidate_at(0);
        }
        let mut counts = vec![0usize; num_candidates];
        populace.iter_populace(&mut |voter| {
            voter
                .into_iter()
                .copied()
                .enumerate()
                .for_each(|(i, dist)| counts[i] += max_score - (dist / bin_size) as usize);
        })?;

        pick_winner(max_score, bin_size, populace, &counts)
    }
}

/// Determines the winner based off of a single-round system.
pub fn single_round<T>(
    _: usize,
    _: f64,
    populace: &dyn PopulaceMethod<T>,
    counts: &[usize],
) -> crate::Result<T> {
    // Simply whoever has the highest score. If there are multiple, we choose the first that pops up.
    counts
        .into_iter()
        .enumerate()
        .max_by(|(_, a), (_, b)| a.cmp(b))
        .map(|(i, _)| populace.candidate_at(i))
        .ok_or_else(|| MethodsError::Generic(format!("unreachable")))?
}

/// Determines the winner based off of a two-round system.
pub fn two_round<T>(
    max_score: usize,
    bin_size: f64,
    populace: &dyn PopulaceMethod<T>,
    counts: &[usize],
) -> crate::Result<T> {
    // Determine who are the two highest winners
    let mut first_score = 0usize;
    let mut first_indices = Vec::with_capacity(1); // over 1 is rare, so don't care
    let mut second_score = 0usize;
    let mut second_indices = Vec::with_capacity(1);

    // Start counting
    counts
        .into_iter()
        .copied()
        .enumerate()
        .for_each(|(i, count)| {
            match count.cmp(&first_score) {
                Ordering::Greater => {
                    // Replace second score
                    second_score = first_score;
                    second_indices.clear();
                    second_indices.extend_from_slice(&first_indices);

                    first_score = count;
                    first_indices.clear();
                    first_indices.push(i);
                }
                Ordering::Equal => {
                    // Uh oh, tie?
                    first_indices.push(i);
                }
                Ordering::Less => match count.cmp(&second_score) {
                    Ordering::Greater => {
                        second_score = count;
                        second_indices.clear();
                        second_indices.push(i);
                    }
                    Ordering::Equal => {
                        // Uh oh
                        second_indices.push(i);
                    }
                    _ => {}
                },
            }
        });

    // Check if we have more than one first winner
    // If we do, then we go straight to the second round
    if first_indices.len() > 1 {
        // If there are exactly 2 winners, then go straight to the second round
        if first_indices.len() == 2 {
            two_round_second_round(populace, first_indices[0], first_indices[1])
        } else {
            // Problem alert!
            // This is a bit complicated. We have to determine who is preferred more by most voters.
            // The problem is preference cycles. If a cycle occurs, we pick the first two candidates
            // according to Rust's sorting algorithm (in case it feels like swapping stuff).
            // We could be fancy and use ranked pairs in such a case, but why would you not use
            // ranked pairs in the first round?
            // If a tie otherwise occurs we pick whichever one Rust thinks is first.
            first_indices.sort_unstable_by(|&a, &b| {
                let mut a_count = 0usize;
                let mut b_count = 0usize;
                populace
                    .iter_populace(
                        &mut |voter| match linear_score(max_score, bin_size, voter[a])
                            .cmp(&linear_score(max_score, bin_size, voter[b]))
                        {
                            Ordering::Greater => a_count += 1,
                            Ordering::Less => b_count += 1,
                            Ordering::Equal => {}
                        },
                    )
                    .unwrap(); // Unavoidable unwrap
                b_count.cmp(&a_count) // We want max first
            });
            two_round_second_round(populace, first_indices[0], first_indices[1])
        }
    } else {
        let first = first_indices[0];

        // Same deal with second place. If there are two "second place"ers, we sort.
        if second_indices.len() > 1 {
            second_indices.sort_unstable_by(|&a, &b| {
                let mut a_count = 0usize;
                let mut b_count = 0usize;
                populace
                    .iter_populace(
                        &mut |voter| match linear_score(max_score, bin_size, voter[a])
                            .cmp(&linear_score(max_score, bin_size, voter[b]))
                        {
                            Ordering::Greater => a_count += 1,
                            Ordering::Less => b_count += 1,
                            Ordering::Equal => {}
                        },
                    )
                    .unwrap();
                b_count.cmp(&a_count) // We want max first
            });
        }
        let second = second_indices[0];
        two_round_second_round(populace, first, second)
    }
}

fn two_round_second_round<T>(
    populace: &dyn PopulaceMethod<T>,
    first: usize,
    second: usize,
) -> crate::Result<T> {
    // Essentially FPTP. We don't pass directly to FPTP since that will involve recreating the populace slice.
    // Start counting
    let mut first_count = 0usize;
    let mut second_count = 0usize;
    populace.iter_populace(&mut |voter| {
        match voter[first].total_cmp(&voter[second]) {
            Ordering::Greater => first_count += 1,
            Ordering::Less => second_count += 1,
            Ordering::Equal => {} // no
        }
    })?;

    populace.candidate_at(match first_count.cmp(&second_count) {
        Ordering::Greater => first,
        Ordering::Less => second,
        Ordering::Equal => first, // Tiebreaker - first guy will do
    })
}
