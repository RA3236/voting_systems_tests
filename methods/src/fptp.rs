use crate::{MethodsError, PopulaceMethod};

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
