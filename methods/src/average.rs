use std::f64;
use std::simd::{LaneCount, Simd, SupportedLaneCount, num::SimdFloat};

use crate::{MethodsError, VectorMethod};

/// Calculates the candidate that is the closest to the average voter for each iteration.
///
/// Returns a vector of the positions of the candidates.
pub fn average(data: &dyn VectorMethod) -> crate::Result<Vec<[f64; 2]>> {
    let ((vx, vy), (cx, cy), num_voters, num_candidates, num_iters) = data.into_data();

    // Check sizes
    if vx.len() != num_voters {
        return Err(MethodsError::SizeMismatch(num_voters, vx.len()));
    }
    if vy.len() != num_voters {
        return Err(MethodsError::SizeMismatch(num_voters, vy.len()));
    }
    if cx.len() != num_candidates * num_iters {
        return Err(MethodsError::SizeMismatch(num_candidates, cx.len()));
    }
    if cy.len() != num_candidates * num_iters {
        return Err(MethodsError::SizeMismatch(num_candidates, cy.len()));
    }

    let (vxa, vya) = average_voter(vx, vy, num_voters);

    (0..num_iters)
        .into_iter()
        .map(|i| {
            let (cx, cy) = (
                &cx[(i * num_candidates)..((i + 1) * num_candidates)],
                &cy[(i * num_candidates)..((i + 1) * num_candidates)],
            );

            let winner = cx
                .into_iter()
                .copied()
                .zip(cy.into_iter().copied())
                .map(|(x, y)| ((vxa - x).powi(2) + (vya - y).powi(2)).sqrt())
                .enumerate()
                .min_by(|(_, a), (_, b)| a.partial_cmp(b).expect("distance is NaN"))
                .map(|(i, _)| i)
                .ok_or_else(|| MethodsError::Generic(format!("malformed candidates array")))?;

            Ok([cx[winner], cy[winner]])
        })
        .collect()
}

/// Calculates the average voter.
pub fn average_voter(vx: &[f64], vy: &[f64], len: usize) -> (f64, f64) {
    fn inner<const N: usize>(vx: &[f64], vy: &[f64], len: usize) -> (f64, f64)
    where
        LaneCount<N>: SupportedLaneCount,
    {
        let mut xs: Simd<f64, N> = Simd::splat(0.0);
        let mut ys: Simd<f64, N> = Simd::splat(0.0);
        for i in (0..len).step_by(N) {
            let x = Simd::from_slice(&vx[i..]);
            let y = Simd::from_slice(&vy[i..]);
            xs += x;
            ys += y;
        }

        (xs.reduce_sum() / len as f64, ys.reduce_sum() / len as f64)
    }

    macro_rules! check {
        ($n:literal, $feature:expr) => {
            #[cfg(target_feature = $feature)]
            if len % $n == 0 {
                return inner::<$n>(vx, vy, len);
            }
        };
        () => {
            return inner::<1>(vx, vy, len);
        };
    }

    check!(8, "avx2");
    check!(4, "avx");
    check!(2, "sse2");
    check!();
}
