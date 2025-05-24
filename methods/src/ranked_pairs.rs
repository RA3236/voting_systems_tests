use std::cmp::Ordering;

use crate::{MethodsError, PopulaceMethod};

/// Ranked Pairs
pub fn ranked_pairs<T>(populace: &dyn PopulaceMethod<T>) -> crate::Result<T> {
    let num_candidates = populace.num_candidates();

    let mut counts = vec![vec![0usize; num_candidates]; num_candidates];
    populace.iter_populace(&mut |voter| {
        for x in 0..num_candidates {
            for y in (x + 1)..num_candidates {
                match voter[x].total_cmp(&voter[y]) {
                    Ordering::Less => counts[x][y] += 1,
                    Ordering::Greater => counts[y][x] += 1,
                    Ordering::Equal => counts[x][y] += 1, // Choose whatever appears first on ballot
                }
            }
        }
    })?;

    let mut ranked_pairs = Vec::with_capacity(num_candidates * num_candidates);
    for x in 0..num_candidates {
        for y in (x + 1)..num_candidates {
            match counts[x][y].cmp(&counts[y][x]) {
                Ordering::Greater => ranked_pairs.push((x, y, counts[x][y] - counts[y][x])),
                Ordering::Less => ranked_pairs.push((y, x, counts[y][x] - counts[x][y])),
                Ordering::Equal => {}
            }
        }
    }

    ranked_pairs.sort_by(|(_, _, a), (_, _, b)| b.cmp(a));

    let mut graph = vec![vec![false; num_candidates]; num_candidates];
    let mut prev = vec![];
    fn has_cycle(
        graph: &Vec<Vec<bool>>,
        prev: &mut Vec<usize>,
        x: usize,
        num_candidates: usize,
    ) -> bool {
        for y in 0..num_candidates {
            if graph[x][y] {
                if prev.contains(&y) {
                    return true;
                }
                prev.push(y);
                if has_cycle(graph, prev, y, num_candidates) {
                    return true;
                }
                prev.pop();
            }
        }

        false
    }

    for (x, y, _) in ranked_pairs {
        graph[x][y] = true;
        prev.push(x);
        if has_cycle(&mut graph, &mut prev, x, num_candidates) {
            graph[x][y] = false;
        }
        prev.clear();
    }

    (0..num_candidates)
        .into_iter()
        .find(|&x| (0..num_candidates).into_iter().all(|y| !graph[y][x]))
        .map(|i| populace.candidate_at(i))
        .ok_or_else(|| MethodsError::Generic(format!("unreachable")))?
}
