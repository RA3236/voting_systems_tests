use std::{
    fmt::Display,
    iter::FusedIterator,
    simd::{LaneCount, Simd, StdFloat, SupportedLaneCount},
    str::FromStr,
};

pub fn typed_textbox<T: FromStr + Display + PartialOrd + Copy>(
    value: &mut T,
    value_string: &mut String,
    ui: &mut egui::Ui,
    min: T,
    max: T,
) {
    let response = ui.add(egui::TextEdit::singleline(value_string));
    if response.lost_focus() {
        if let Ok(new) = value_string.parse() {
            *value = new;
        }
        *value = if *value < min {
            min
        } else if *value > max {
            max
        } else {
            *value
        };
        *value_string = format!("{value}");
    }
}

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
                            .sqrt()
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

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct RangeFloatIterator {
    start: f64,
    end: f64,
    step_by: f64,
    is_exclusive: bool,
    current: f64,
    finished: bool,
}

impl Iterator for RangeFloatIterator {
    type Item = f64;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let tmp = self.current;
        if tmp >= self.end {
            self.finished = true;
            if self.is_exclusive {
                return None;
            } else {
                return Some(self.end);
            }
        }
        self.current = tmp + self.step_by;
        Some(tmp)
    }
}

impl FusedIterator for RangeFloatIterator {}

pub trait RangeFloatIter {
    fn into_iter_step(self, step: f64) -> RangeFloatIterator;
}

use std::ops::*;

impl RangeFloatIter for Range<f64> {
    fn into_iter_step(self, step_by: f64) -> RangeFloatIterator {
        RangeFloatIterator {
            start: self.start,
            end: self.end,
            step_by,
            is_exclusive: true,
            current: self.start,
            finished: false,
        }
    }
}

impl RangeFloatIter for RangeInclusive<f64> {
    fn into_iter_step(self, step_by: f64) -> RangeFloatIterator {
        RangeFloatIterator {
            start: *self.start(),
            end: *self.end(),
            step_by,
            is_exclusive: true,
            current: *self.start(),
            finished: false,
        }
    }
}
