use std::{f64, fmt::Display, usize};

use dist_to_average::DistanceSettings;
use egui::{Color32, PopupCloseBehavior, RichText};
use methods::Method;
use nash_multiple::NashMultipleSettings;
// use nash_single::NashSingleSettings;
use rand::Rng;
use rand_distr::{Beta, Distribution as RandDistr, Open01};
use rustc_hash::FxHashSet;
use seats_projection::SeatsProjectionSettings;
use visualize_distribution::VisualizeDistribution;
use winner_type::WinnerTypeSettings;

use crate::{Message, MessageSender, util::input};

pub mod common;
pub mod constants;
pub mod dist_to_average;
pub mod nash_multiple;
// pub mod nash_single;
pub mod seats_projection;
pub mod visualize_distribution;
pub mod winner_type;

pub const SAVE_LOCATION: &str = "output";
pub const SAVE_VISUALIZE: &str = "distribution";
pub const SAVE_WINNER_TYPE: &str = "winner type";
pub const SAVE_DISTANCES: &str = "distances";
pub const SAVE_SEATS_PROJECTION: &str = "seat projections";
pub const SAVE_NASH_SINGLE: &str = "nash single";
pub const SAVE_NASH_SINGLE_INDIVIDUAL: &str = "nash single/individual";
pub const SAVE_NASH_MULTIPLE: &str = "nash multiple";
pub const SAVE_NASH_MULTIPLE_INDIVIDIAL: &str = "nash multiple/individual";

pub fn get_file_and_uri(loc: &str, filename: &str) -> (String, String) {
    let save_path = format!("{SAVE_LOCATION}/{loc}/{filename}");
    let uri = format!("file://{save_path}");
    (save_path, uri)
}

#[derive(Debug)]
pub struct AppRuntimeSettings {
    locked: bool,
    num_locks: usize,
    current: usize,
    actions: FxHashSet<Action>,
    methods: FxHashSet<Method>,
    distrs: DistributionManager,

    visualize: VisualizeDistribution,
    winner_types: WinnerTypeSettings,
    distances: DistanceSettings,
    seat_projections: SeatsProjectionSettings,
    // nash_single: NashSingleSettings,
    nash_multiple: NashMultipleSettings,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ScoreHandle {
    Full,
    Custom(f64, String),
}

impl Display for ScoreHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Full => write!(f, "Full"),
            Self::Custom(_, s) => write!(f, "{s}"),
        }
    }
}

impl Default for AppRuntimeSettings {
    fn default() -> Self {
        for location in [
            SAVE_VISUALIZE,
            SAVE_WINNER_TYPE,
            SAVE_DISTANCES,
            SAVE_SEATS_PROJECTION,
            SAVE_NASH_SINGLE,
            SAVE_NASH_SINGLE_INDIVIDUAL,
            SAVE_NASH_MULTIPLE,
            SAVE_NASH_MULTIPLE_INDIVIDIAL,
        ] {
            let dir_name = format!("{SAVE_LOCATION}/{location}");
            std::fs::create_dir_all(dir_name).unwrap();
        }

        Self {
            locked: false,
            current: 0,
            num_locks: 0,
            actions: FxHashSet::default(),
            methods: FxHashSet::default(),
            distrs: DistributionManager::default(),
            visualize: VisualizeDistribution::default(),
            winner_types: WinnerTypeSettings::default(),
            distances: DistanceSettings::default(),
            seat_projections: SeatsProjectionSettings::default(),
            // nash_single: NashSingleSettings::default(),
            nash_multiple: NashMultipleSettings::default(),
        }
    }
}

impl AppRuntimeSettings {
    pub fn unlock(&mut self) {
        self.current += 1;
        if self.num_locks == self.current {
            self.locked = false;
        }
    }

    pub fn locked(&self) -> bool {
        self.locked
    }

    fn total_selected_methods(&self) -> usize {
        self.methods.len()
    }

    pub fn show(&mut self, sender: &MessageSender, ui: &mut egui::Ui) {
        egui::ScrollArea::vertical().show(ui, |ui| {
            ui.vertical_centered_justified(|ui| {
                ui.heading("Voting Simulations");

                if ui.button("Start Simulation").clicked() {
                    self.locked = true;
                    self.num_locks = self.actions.len();
                    self.current = 0;
                    sender.send(Message::DropImages).unwrap();

                    std::fs::remove_dir_all("output").unwrap();
                    for location in [
                        SAVE_VISUALIZE,
                        SAVE_WINNER_TYPE,
                        SAVE_DISTANCES,
                        SAVE_SEATS_PROJECTION,
                        SAVE_NASH_SINGLE,
                        SAVE_NASH_SINGLE_INDIVIDUAL,
                        SAVE_NASH_MULTIPLE,
                        SAVE_NASH_MULTIPLE_INDIVIDIAL,
                    ] {
                        let dir_name = format!("{SAVE_LOCATION}/{location}");
                        std::fs::create_dir_all(dir_name).unwrap();
                    }

                    for action in &self.actions {
                        match action {
                            Action::VisualizeDistribution => {
                                self.visualize.simulate(sender, &self.methods, &self.distrs)
                            }
                            Action::WinnerType => {
                                self.winner_types
                                    .simulate(sender, &self.methods, &self.distrs)
                            }
                            Action::Distance => {
                                self.distances.simulate(sender, &self.methods, &self.distrs)
                            }
                            Action::SimulateElection => {
                                self.seat_projections
                                    .simulate(sender, &self.methods, &self.distrs)
                            }
                            // Action::NashSingle => {
                            //     self.nash_single
                            //         .simulate(sender, &self.methods, &self.distrs)
                            // }
                            Action::NashMultiple => {
                                self.nash_multiple
                                    .simulate(sender, &self.methods, &self.distrs)
                            }
                        }
                    }
                }

                let count = |name, count| match count {
                    0 => format!("Select some {name}s"),
                    1 => format!("1 {name} selected"),
                    _ => format!("{count} {name}s selected"),
                };

                egui::Grid::new("Main grid").show(ui, |ui| {
                    ui.label("Actions to perform");
                    egui::ComboBox::new("Actions combo", "")
                        .selected_text(count("action", self.actions.len()))
                        .close_behavior(PopupCloseBehavior::CloseOnClickOutside)
                        .show_ui(ui, |ui| {
                            if ui.button("Select All").clicked() {
                                for action in Action::iter() {
                                    self.actions.insert(action);
                                }
                            }
                            for action in Action::iter() {
                                let mut value = self.actions.contains(&action);
                                ui.checkbox(&mut value, action.to_str());
                                match value {
                                    true => self.actions.insert(action),
                                    false => self.actions.remove(&action),
                                };
                            }
                        });
                    ui.end_row();

                    ui.label("Methods to simulate");
                    egui::ComboBox::new("Methods combo", "")
                        .selected_text(count("method", self.methods.len()))
                        .close_behavior(PopupCloseBehavior::CloseOnClickOutside)
                        .show_ui(ui, |ui| {
                            egui::Grid::new("Methods grid")
                                .num_columns(2)
                                .show(ui, |ui| {
                                    if ui.button("Select All").clicked() {
                                        for method in Method::iter() {
                                            self.methods.insert(method);
                                        }
                                    }
                                    ui.end_row();
                                    for method in Method::iter() {
                                        let mut value = self.methods.contains(&method);
                                        ui.checkbox(&mut value, method.name());
                                        ui.label(method.desc());
                                        ui.end_row();

                                        match value {
                                            true => self.methods.insert(method),
                                            false => self.methods.remove(&method),
                                        };
                                    }
                                });
                        });
                    ui.end_row();
                });

                if self.total_selected_methods() > 4 {
                    ui.label(
                        RichText::new(
                            "Picking too many methods might make it difficult to read some graphs!",
                        )
                        .color(Color32::RED)
                        .strong(),
                    );
                }

                ui.group(|ui| {
                    self.distrs.show(ui);
                });

                for action in &self.actions {
                    ui.group(|ui| match action {
                        Action::VisualizeDistribution => {
                            self.visualize.show("Visualization Settings", ui)
                        }
                        Action::WinnerType => self.winner_types.show(ui),
                        Action::Distance => self.distances.show(ui),
                        Action::SimulateElection => self.seat_projections.show(&self.methods, ui),
                        // Action::NashSingle => self.nash_single.show(ui),
                        Action::NashMultiple => self.nash_multiple.show(ui),
                    });
                }
            });
        });
    }
}

macro_rules! define_distribution {
    (
        $default_name:ident, $default_str:literal, ($($default_ty:ty),*), ($($default_voters:literal),*), ($($default_candidates:literal),*),
        $($name:ident, $str:literal, ($($ty:ty),*), ($($voters:literal),*), ($($candidates:literal),*)),*
    ) => {
        #[derive(Clone, Debug, PartialEq)]
        pub enum Distribution {
            $default_name($($default_ty),*),
            $($name($($ty),*)),*
        }

        impl Distribution {
            fn iter_voters() -> impl Iterator<Item = Self> {
                [
                    Self::$default_name($($default_voters),*),
                    $(Self::$name($($voters),*)),*
                ]
                    .into_iter()
            }

            fn iter_candidates() -> impl Iterator<Item = Self> {
                [
                    Self::$default_name($($default_candidates),*),
                    $(Self::$name($($candidates),*)),*
                ]
                    .into_iter()
            }

            fn default_voters() -> Self {
                Self::$default_name($($default_voters),*)
            }

            fn default_candidates() -> Self {
                Self::$default_name($($default_candidates),*)
            }

            fn to_str(&self) -> &'static str {
                match self {
                    Self::$default_name(..) => $default_str,
                    $(Self::$name(..) => $str),*
                }
            }
        }
    };
}

define_distribution! {
    Beta, "Beta", (f64, f64, f64, f64, bool), (2.0, 2.0, 32.0, 32.0, false), (2.0, 2.0, 32.0, 32.0, false),
    Uniform, "Uniform", (f64, f64), (1.0, 4.0), (1.0, 4.0)
}

#[derive(Clone, Debug, PartialEq)]
pub struct DistributionManager {
    voters: Distribution,
    candidates: Distribution,
}

impl Default for DistributionManager {
    fn default() -> Self {
        Self {
            voters: Distribution::default_voters(),
            candidates: Distribution::default_candidates(),
        }
    }
}

impl DistributionManager {
    pub fn show(&mut self, ui: &mut egui::Ui) {
        ui.heading("Distribution Settings");

        egui::Grid::new("Distribution Settings grid").show(ui, |ui| {
            ui.label("Voters distribution");
            egui::ComboBox::new("voters distribution settings", "")
                .selected_text(self.voters.to_str())
                .show_ui(ui, |ui| {
                    for distr in Distribution::iter_voters() {
                        let name = distr.to_str();
                        ui.selectable_value(&mut self.voters, distr, name);
                    }
                });

            ui.end_row();

            match &mut self.voters {
                Distribution::Beta(xa, xb, ya, yb, complex) => {
                    ui.label("Advanced Settings");
                    ui.checkbox(complex, "");

                    ui.end_row();

                    if *complex {
                        input!(ui,
                            "X alpha", *xa, f64, min 1.0,
                            "X beta", *xb, f64, min 1.0,
                            "Y alpha", *ya, f64, min 1.0,
                            "Y beta", *yb, f64, min 1.0
                        );
                    } else {
                        input!(ui,
                            "X params", *xa, f64, min 1.0,
                            "Y params", *ya, f64, min 1.0
                        );
                        *xb = *xa;
                        *yb = *ya;
                    }
                }
                Distribution::Uniform(x, y) => {
                    // for (name, value) in [("X scale", x), ("Y scale", y)] {
                    //     ui.label(name);
                    //     ui.add(egui::DragValue::new(value).range(1.0..=f64::MAX));
                    //     ui.end_row();
                    // }
                    input!(ui,
                        "X scale", *x, f64, min 1.0,
                        "Y scale", *y, f64, min 1.0
                    );
                }
            }

            ui.label("Candidates distribution");
            egui::ComboBox::new("candidates distribution settings", "")
                .selected_text(self.candidates.to_str())
                .show_ui(ui, |ui| {
                    for distr in Distribution::iter_candidates() {
                        let name = distr.to_str();
                        ui.selectable_value(&mut self.candidates, distr, name);
                    }
                });

            ui.end_row();

            match &mut self.candidates {
                Distribution::Beta(xa, xb, ya, yb, complex) => {
                    ui.label("Advanced Settings");
                    ui.checkbox(complex, "");

                    ui.end_row();

                    if *complex {
                        for (name, value) in [
                            ("X alpha", xa),
                            ("X beta", xb),
                            ("Y alpha", ya),
                            ("Y beta", yb),
                        ] {
                            ui.label(name);
                            ui.add(egui::DragValue::new(value).range(1.0..=f64::MAX));
                            ui.end_row();
                        }
                    } else {
                        for (name, a, b) in [("X params", xa, xb), ("Y params", ya, yb)] {
                            ui.label(name);
                            ui.add(egui::DragValue::new(a).range(1.0..=f64::MAX));
                            ui.end_row();

                            *b = *a;
                        }
                    }
                }
                Distribution::Uniform(x, y) => {
                    for (name, value) in [("X scale", x), ("Y scale", y)] {
                        ui.label(name);
                        ui.add(egui::DragValue::new(value).range(1.0..=f64::MAX));
                        ui.end_row();
                    }
                }
            }
        });
    }

    pub fn into_default_distribution<R>(
        &self,
    ) -> (
        Box<dyn Fn(&mut R) -> f64>,
        Box<dyn Fn(&mut R) -> f64>,
        Box<dyn Fn(&mut R) -> f64>,
        Box<dyn Fn(&mut R) -> f64>,
    )
    where
        R: Rng,
    {
        fn into_functions<R: Rng>(
            distr: &Distribution,
        ) -> (Box<dyn Fn(&mut R) -> f64>, Box<dyn Fn(&mut R) -> f64>) {
            match distr {
                Distribution::Beta(xa, xb, ya, yb, _) => {
                    let (x, y) = (Beta::new(*xa, *xb).unwrap(), Beta::new(*ya, *yb).unwrap());
                    (
                        Box::new(move |rng| x.sample(rng)),
                        Box::new(move |rng| y.sample(rng)),
                    )
                }
                Distribution::Uniform(x, y) => {
                    let (x, y) = (*x, *y);
                    (
                        Box::new(move |rng| {
                            (RandDistr::<f64>::sample(&Open01, rng) - 0.5) / x + 0.5
                        }),
                        Box::new(move |rng| {
                            (RandDistr::<f64>::sample(&Open01, rng) - 0.5) / y + 0.5
                        }),
                    )
                }
            }
        }

        let (vx, vy) = into_functions(&self.voters);
        let (cx, cy) = into_functions(&self.candidates);
        (vx, vy, cx, cy)
    }
}

macro_rules! define_action {
    (
        $($name:ident, $str:expr),*
    ) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub enum Action {
            $($name),*
        }

        impl Action {
            pub fn iter() -> impl Iterator<Item = Self> {
                [
                    $(Self::$name),*
                ]
                    .into_iter()
            }

            pub fn to_str(&self) -> &'static str {
                match self {
                    $(Self::$name => $str),*
                }
            }
        }
    };
}

define_action! {
    VisualizeDistribution, "Visualize distribution",
    WinnerType, "Types of winners",
    Distance, "Distances to Condorcet/average voter",
    SimulateElection, "Simulate a country-wide election",
    // NashSingle, "Simulate isolated elections to find Nash equilibrium",
    NashMultiple, "Simulate country-wide elections to find Nash equilibrium"
}
