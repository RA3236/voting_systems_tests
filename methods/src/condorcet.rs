use std::cmp::Ordering;

use crate::PopulaceMethod;

/// Calculates the Condorcet winner for this populace, if it exists.
///
/// Returns the position of the winner, if it exists.
pub fn condorcet<T>(populace: &dyn PopulaceMethod<T>) -> crate::Result<Option<T>> {
    let num_candidates = populace.num_candidates();

    let mut counts = vec![vec![0usize; num_candidates]; num_candidates];
    populace.iter_populace(&mut |voter| {
        for x in 0..num_candidates {
            for y in (x + 1)..num_candidates {
                match voter[x].total_cmp(&voter[y]) {
                    Ordering::Less => counts[x][y] += 1,
                    Ordering::Greater => counts[y][x] += 1,
                    Ordering::Equal => {} // Not possible
                }
            }
        }
    })?;

    // Convert into adjacency matrix
    let mut matrix = vec![vec![false; num_candidates]; num_candidates];
    for x in 0..num_candidates {
        for y in (x + 1)..num_candidates {
            match counts[x][y].cmp(&counts[y][x]) {
                Ordering::Less => matrix[y][x] = true,
                Ordering::Greater => matrix[x][y] = true,
                Ordering::Equal => {}
            }
        }
    }

    // Winner has no incoming edges
    (0..num_candidates)
        .into_iter()
        .find(|&x| (0..num_candidates).into_iter().all(|y| !matrix[y][x]))
        .map(|i| populace.candidate_at(i))
        .transpose()
}
