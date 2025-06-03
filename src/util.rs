use std::simd::{LaneCount, Simd, StdFloat, SupportedLaneCount};

macro_rules! min_ty {
    (usize) => {
        1
    };
    (f64) => {
        0.0
    };
}

macro_rules! max_ty {
    ($ty:ident) => {
        $ty::MAX
    };
}
pub(crate) use max_ty;
pub(crate) use min_ty;

macro_rules! drag_value {
    ($v:expr, $_:ident, min $min:expr, max $max:expr) => {
        egui::DragValue::new(&mut $v).range($min..=$max)
    };
    ($v:expr, $ty:ident, min $min:expr) => {
        egui::DragValue::new(&mut $v).range($min..=$crate::util::max_ty!($ty))
    };
    ($v:expr, $ty:ident, max $max:expr) => {
        egui::DragValue::new(&mut $v).range($crate::util::min_ty!($ty)..=$max)
    };
    ($v:expr, $ty:ident) => {
        egui::DragValue::new(&mut $v).range($crate::util::min_ty!($ty)..=$crate::util::max_ty!($ty))
    };
}
pub(crate) use drag_value;

macro_rules! input {
    ($ui:expr, $($n:literal, $v:expr, $ty:ident $(, min $min:expr)? $(, max $max:expr)?),*) => {
        $(
            $ui.label($n);
            $ui.add($crate::util::drag_value!($v, $ty $(, min $min)? $(, max $max)?));
            $ui.end_row();
        )*
    };
}
pub(crate) use input;

pub fn create_populace(
    vx: &[f64],
    vy: &[f64],
    cx: &[f64],
    cy: &[f64],
    num_candidates: usize,
) -> Vec<Vec<f64>> {
    fn inner<const N: usize>(
        vx: &[f64],
        vy: &[f64],
        cx: &[f64],
        cy: &[f64],
        num_candidates: usize,
    ) -> Vec<Vec<f64>>
    where
        LaneCount<N>: SupportedLaneCount,
    {
        vx.into_iter()
            .copied()
            .zip(vy.into_iter().copied())
            .map(|(vx, vy)| {
                let (vx, vy) = (Simd::splat(vx), Simd::splat(vy));

                let mut array = vec![0.0; num_candidates];
                cx.array_chunks::<N>()
                    .copied()
                    .zip(cy.array_chunks().copied())
                    .enumerate()
                    .for_each(|(i, (cx, cy))| {
                        let (cx, cy) = (Simd::from_array(cx), Simd::from_array(cy));

                        ((vx - cx) * (vx - cx) + (vy - cy) * (vy - cy))
                            .copy_to_slice(&mut array[i * N..])
                    });
                array
            })
            .collect()
    }

    macro_rules! check {
        ($n:literal, $f:expr) => {
            #[cfg(target_feature = $f)]
            if vx.len() % $n == 0 && num_candidates % $n == 0 {
                return inner::<$n>(vx, vy, cx, cy, num_candidates);
            }
        };
        () => {
            return inner::<1>(vx, vy, cx, cy, num_candidates);
        };
    }

    check!(8, "avx2");
    check!(4, "avx");
    check!(2, "sse2");
    check!();
}

pub fn create_populace_mut(
    vx: &[f64],
    vy: &[f64],
    cx: &[f64],
    cy: &[f64],
    num_candidates: usize,
    populace: &mut Vec<Vec<f64>>,
) {
    fn inner<const N: usize>(
        vx: &[f64],
        vy: &[f64],
        cx: &[f64],
        cy: &[f64],
        populace: &mut Vec<Vec<f64>>,
    ) where
        LaneCount<N>: SupportedLaneCount,
    {
        vx.into_iter()
            .copied()
            .zip(vy.into_iter().copied())
            .zip(populace.iter_mut())
            .for_each(|((vx, vy), array)| {
                let (vx, vy) = (Simd::splat(vx), Simd::splat(vy));

                cx.array_chunks::<N>()
                    .copied()
                    .zip(cy.array_chunks().copied())
                    .enumerate()
                    .for_each(|(i, (cx, cy))| {
                        let (cx, cy) = (Simd::from_array(cx), Simd::from_array(cy));

                        ((vx - cx) * (vx - cx) + (vy - cy) * (vy - cy))
                            .sqrt()
                            .copy_to_slice(&mut array[i * N..])
                    });
            });
    }

    populace.iter_mut().for_each(|voter| voter.fill(0.0));

    macro_rules! check {
        ($n:literal, $f:expr) => {
            #[cfg(target_feature = $f)]
            if vx.len() % $n == 0 && num_candidates % $n == 0 {
                inner::<$n>(vx, vy, cx, cy, populace);
            }
        };
        () => {
            inner::<1>(vx, vy, cx, cy, populace);
        };
    }

    check!(8, "avx2");
    check!(4, "avx");
    check!(2, "sse2");
    check!();
}
