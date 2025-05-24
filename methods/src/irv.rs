use crate::{MethodsError, PopulaceMethod};

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
