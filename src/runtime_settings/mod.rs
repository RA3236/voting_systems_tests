use std::{f64, fmt::Display, usize};

use dist_to_average::DistanceSettings;
use egui::{Color32, PopupCloseBehavior, RichText};
use methods::Method;
use rand::Rng;
use rand_distr::{Beta, Distribution as RandDistr, Open01};
use rustc_hash::{FxHashMap, FxHashSet};
use seats_projection::SeatsProjectionSettings;
use visualize_distribution::VisualizeDistribution;
use winner_type::WinnerTypeSettings;

use crate::{Message, Sender};

pub mod common;
pub mod dist_to_average;
pub mod seats_projection;
pub mod visualize_distribution;
pub mod winner_type;

pub const SAVE_LOCATION: &str = "output";
pub const SAVE_VISUALIZE: &str = "distribution";
pub const SAVE_WINNER_TYPE: &str = "winner type";
pub const SAVE_DISTANCES: &str = "distances";
pub const SAVE_SEATS_PROJECTION: &str = "seat projections";

pub fn get_file_and_uri(loc: &str, filename: &str) -> (String, String) {
    let save_path = format!("{SAVE_LOCATION}/{loc}/{filename}");
    let uri = format!("file://{save_path}");
    (save_path, uri)
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppRuntimeSettings {
    locked: bool,
    num_locks: usize,
    current: usize,
    actions: FxHashSet<Action>,
    methods: FxHashMap<Method, Vec<ScoreHandle>>,
    distrs: Distribution,

    visualize: VisualizeDistribution,
    winner_types: WinnerTypeSettings,
    distances: DistanceSettings,
    seat_projections: SeatsProjectionSettings,
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
        for location in [SAVE_VISUALIZE, SAVE_WINNER_TYPE, SAVE_DISTANCES, SAVE_SEATS_PROJECTION] {
            let dir_name = format!("{SAVE_LOCATION}/{location}");
            std::fs::create_dir_all(dir_name).unwrap();
        }

        Self {
            locked: false,
            current: 0,
            num_locks: 0,
            actions: FxHashSet::default(),
            methods: FxHashMap::default(),
            distrs: Distribution::default(),
            visualize: VisualizeDistribution::default(),
            winner_types: WinnerTypeSettings::default(),
            distances: DistanceSettings::default(),
            seat_projections: SeatsProjectionSettings::default(),
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
        self.methods.iter().map(|(_, v)| v.len()).sum()
    }

    pub fn show(&mut self, sender: &Sender, ui: &mut egui::Ui) {
        egui::ScrollArea::vertical().show(ui, |ui| {
            ui.vertical_centered_justified(|ui| {
                ui.heading("Voting Simulations");

                if ui.button("Start Simulation").clicked() {
                    self.locked = true;
                    self.num_locks = self.actions.len();
                    self.current = 0;
                    sender.send(Message::DropImages).unwrap();

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
                                            self.methods.insert(method, vec![ScoreHandle::Full]);
                                        }
                                    }
                                    ui.end_row();
                                    for method in Method::iter() {
                                        let mut value = self.methods.contains_key(&method);
                                        ui.checkbox(&mut value, method.name());
                                        ui.label(method.desc());
                                        ui.end_row();

                                        match value {
                                            true => {
                                                self.methods.insert(method, vec![ScoreHandle::Full])
                                            }
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
                        Action::SimulateElection => self.seat_projections.show(ui),
                    });
                }

                // Allow the user to modify each score method
                for (method, score_handles) in self.methods.iter_mut() {
                    if !method.is_score() {
                        continue;
                    }

                    ui.group(|ui| {
                        ui.heading(format!("{} Settings", method.name()));
                        ui.label("Configure the distances for score voting");
                        ui.label("Each row represents a separate simulation");
                        let mut change = false;
                        let mut remove_index = 0;

                        let has_full = match &score_handles[0] {
                            ScoreHandle::Full => true,
                            ScoreHandle::Custom(..) => false,
                        };

                        egui::Grid::new(format!("{} score grid", method.name()))
                            .num_columns(3)
                            .show(ui, |ui| {
                                let len = score_handles.len();
                                for (i, score_handle) in score_handles.iter_mut().enumerate() {
                                    match score_handle {
                                        ScoreHandle::Full => {
                                            ui.label("Full");
                                            if ui.button("Switch to custom").clicked() {
                                                *score_handle =
                                                    ScoreHandle::Custom(1.0, 1.0f64.to_string());
                                            }
                                        }
                                        ScoreHandle::Custom(value, value_string) => {
                                            crate::util::typed_textbox(
                                                value,
                                                value_string,
                                                ui,
                                                0.0,
                                                2.0,
                                            );
                                            ui.add_enabled_ui(!has_full, |ui| {
                                                if ui.button("Switch to full").clicked() {
                                                    *score_handle = ScoreHandle::Full;
                                                }
                                            });
                                        }
                                    }
                                    ui.add_enabled_ui(len > 1, |ui| {
                                        if ui.button("Remove").clicked() {
                                            change = true;
                                            remove_index = i;
                                        }
                                    });
                                    ui.end_row();
                                }
                            });

                        if change {
                            score_handles.remove(remove_index);
                        }

                        ui.vertical_centered_justified(|ui| {
                            if !has_full && ui.button("Add Full").clicked() {
                                score_handles.insert(0, ScoreHandle::Full);
                            }

                            if ui.button("Add Custom").clicked() {
                                score_handles.push(ScoreHandle::Custom(1.0f64, 1.0f64.to_string()));
                            }
                        });
                    });
                }
            });
        });
    }
}

macro_rules! define_distribution {
    (
        $dname:ident, $dstr:expr, ($dtyf:ident $(, $dty:ident)* $(,)?), ($dvf:expr $(, $dv:expr)* $(,)?),
        $($name:ident, $str:expr, ($tyf:ident $(, $ty:ident)* $(,)?), ($vf:expr $(, $v:expr)* $(,)?)),*
    ) => {
        #[derive(Clone, Debug, PartialEq)]
        pub enum Distribution {
            $dname($dtyf, String, $($dty, String),*),
            $($name($tyf, String, $($ty, String),*)),*
        }

        impl Default for Distribution {
            fn default() -> Self {
                Self::$dname($dvf, $dtyf::to_string(&$dvf), $($dv, $dty::to_string(&$dv)),*)
            }
        }

        impl Distribution {
            fn iter() -> impl Iterator<Item = (&'static str, Self)> {
                [
                    ($dstr, Self::$dname($dvf, $dtyf::to_string(&$dvf), $($dv, $dty::to_string(&$dv)),*)),
                    $(($str, Self::$name($vf, $tyf::to_string(&$vf), $($v, $ty::to_string(&$v)),*))),*
                ]
                    .into_iter()
            }

            fn to_str(&self) -> &'static str {
                match self {
                    Self::$dname(..) => $dstr,
                    $(Self::$name(..) => $str),*
                }
            }
        }
    };
}

define_distribution! {
    Beta, "Beta", (f64, f64, f64, f64), (2.0, 32.0, 8.0, 128.0),
    Uniform, "Uniform", (f64, f64, f64, f64), (1.0, 0.25, 0.75, 0.125)
}

impl Distribution {
    pub fn show(&mut self, ui: &mut egui::Ui) {
        ui.heading("Distribution Settings");

        egui::Grid::new("Distribution Settings grid").show(ui, |ui| {
            // Choose distribution type
            ui.label("Distribution");

            egui::ComboBox::new("distribution_settings", "")
                .selected_text(self.to_str())
                .show_ui(ui, |ui| {
                    for (name, distr) in Self::iter() {
                        ui.selectable_value(self, distr, name);
                    }
                });
            ui.end_row();
            match self {
                Self::Beta(vx, vxs, vy, vys, cx, cxs, cy, cys) => {
                    for (name, var, var_string) in [
                        ("Voters (X)", vx, vxs),
                        ("Voters (Y)", vy, vys),
                        ("Candidates (X)", cx, cxs),
                        ("Candidates (Y)", cy, cys),
                    ] {
                        ui.label(name);
                        crate::util::typed_textbox(var, var_string, ui, 1.0, f64::MAX);
                        ui.end_row();
                    }
                }
                Self::Uniform(vx, vxs, vy, vys, cx, cxs, cy, cys) => {
                    for (name, var, var_string) in [
                        ("Voters max distance (X)", vx, vxs),
                        ("Voters max distance (Y)", vy, vys),
                        ("Candidates max distance (X)", cx, cxs),
                        ("Candidates max distance (Y)", cy, cys),
                    ] {
                        ui.label(name);
                        crate::util::typed_textbox(var, var_string, ui, 0.0, 1.0);
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
        match self {
            Self::Beta(vx, _, vy, _, cx, _, cy, _) => {
                let (vx, vy, cx, cy) = (
                    Beta::new(*vx, *vx).unwrap(),
                    Beta::new(*vy, *vy).unwrap(),
                    Beta::new(*cx, *cx).unwrap(),
                    Beta::new(*cy, *cy).unwrap(),
                );
                (
                    Box::new(move |rng| vx.sample(rng)),
                    Box::new(move |rng| vy.sample(rng)),
                    Box::new(move |rng| cx.sample(rng)),
                    Box::new(move |rng| cy.sample(rng)),
                )
            }
            Self::Uniform(vx, _, vy, _, cx, _, cy, _) => {
                let (vx, vy, cx, cy) = (*vx, *vy, *cx, *cy);
                (
                    Box::new(move |rng| (RandDistr::<f64>::sample(&Open01, rng) - 0.5) * vx + 0.5),
                    Box::new(move |rng| (RandDistr::<f64>::sample(&Open01, rng) - 0.5) * vy + 0.5),
                    Box::new(move |rng| (RandDistr::<f64>::sample(&Open01, rng) - 0.5) * cx + 0.5),
                    Box::new(move |rng| (RandDistr::<f64>::sample(&Open01, rng) - 0.5) * cy + 0.5),
                )
            }
        }
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
    SimulateElection, "Simulate a country-wide election"
}
