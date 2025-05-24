use std::sync::LazyLock;

pub const POPULACE_BOUNDS: [f64; 2] = [0.0, 1.0];

/// The integer range used for discretising the [`POPULACE_BOUNDS`],
/// for use in density visualizations.
pub const DENSITY_INT_RANGE: [usize; 2] = [0, 100];
/// The number of density binds.
pub const DENSITY_NUM_BINS: usize = const { DENSITY_INT_RANGE[1] - DENSITY_INT_RANGE[0] + 1 };
/// The size of each density bin in one axis.
pub const DENSITY_STEP: f64 =
    const { (POPULACE_BOUNDS[1] - POPULACE_BOUNDS[0]) / ((DENSITY_NUM_BINS - 1) as f64) };
/// The X values for the density visualizations.
pub const DENSITY_X: [f64; DENSITY_NUM_BINS] = const {
    let mut data = [0.0; DENSITY_NUM_BINS];
    let mut i = 0;
    while i < DENSITY_NUM_BINS {
        data[i] = POPULACE_BOUNDS[0] + i as f64 * DENSITY_STEP;
        i += 1;
    }
    data
};
/// The Y values for the density visualizations.
pub const DENSITY_Y: [f64; DENSITY_NUM_BINS] = const {
    let mut data = [0.0; DENSITY_NUM_BINS];
    let mut i = 0;
    while i < DENSITY_NUM_BINS {
        data[i] = POPULACE_BOUNDS[0] + i as f64 * DENSITY_STEP;
        i += 1;
    }
    data
};

/// The X labels for the density visualizations.
pub static DENSITY_X_LABELS: LazyLock<[String; DENSITY_NUM_BINS]> =
    LazyLock::new(|| DENSITY_X.map(|x| format!("{x:.2}")));
/// The Y labels for the density visualizations.
pub static DENSITY_Y_LABELS: LazyLock<[String; DENSITY_NUM_BINS]> =
    LazyLock::new(|| DENSITY_Y.map(|y| format!("{y:.2}")));
