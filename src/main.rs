#![feature(portable_simd)]
#![feature(slice_as_array)]
#![feature(array_chunks)]

use std::{
    ops::Deref,
    sync::OnceLock,
};

use chrono::{prelude::*, TimeDelta};

use eframe::NativeOptions;
use egui::{Context, Rect};
use internment::ArcIntern;
use runtime_settings::AppRuntimeSettings;
use rustc_hash::FxHashMap;

pub mod runtime_settings;
pub mod util;

pub fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    rayon::ThreadPoolBuilder::new()
        .use_current_thread()
        .build_global()?;

    let mut options = NativeOptions::default();
    options.window_builder = Some(Box::new(|builder| builder.with_maximized(true)));

    eframe::run_native(
        "Voting Methods",
        Default::default(),
        Box::new(|_cc| Ok(Box::new(EguiApp::new()))),
    )
    .unwrap();

    Ok(())
}

pub type MessageSender = std::sync::mpsc::Sender<Message>;
pub type MessageReceiver = std::sync::mpsc::Receiver<Message>;

pub static CONTEXT: OnceLock<Context> = OnceLock::new();

pub struct Progress {
    current: usize,
    total: usize,
    start: DateTime<Utc>,
}

struct EguiApp {
    runtime_settings: AppRuntimeSettings,

    images: ImageStore,

    sender: MessageSender,
    receiver: MessageReceiver,

    progress: FxHashMap<ArcIntern<str>, Progress>,

    prev_rect: Rect,
    dur: Option<DateTime<Utc>>,
}

impl EguiApp {
    fn new() -> Self {
        let (egui_sender, egui_receiver) = std::sync::mpsc::channel();
        Self {
            runtime_settings: AppRuntimeSettings::default(),

            images: ImageStore::default(),

            sender: egui_sender,
            receiver: egui_receiver,

            progress: FxHashMap::default(),

            prev_rect: Rect::EVERYTHING,
            dur: None,
        }
    }
}

impl eframe::App for EguiApp {
    fn on_exit(&mut self, _gl: Option<&eframe::glow::Context>) {
        self.sender.send(Message::AppShutdown).unwrap();
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if CONTEXT.get().is_none() {
            CONTEXT.set(ctx.clone()).unwrap();
        }

        egui_extras::install_image_loaders(ctx);

        for msg in self.receiver.try_iter() {
            match msg {
                msg if self.images.process_msg(&msg) => {}
                Message::DropImages => {
                    self.images = ImageStore::default();
                    ctx.forget_all_images();
                    self.progress.clear();
                }
                Message::SimulationFinished => {
                    self.runtime_settings.unlock();
                }
                Message::IncrementProgress(key) => {
                    self.progress
                        .entry(key)
                        .and_modify(|v| v.current += 1)
                        .or_insert_with(|| Progress {
                            current: 1,
                            total: 0,
                            start: Utc::now(),
                        });
                }
                Message::TotalProgress(key, total) => {
                    self.progress
                        .entry(key)
                        .and_modify(|v| v.total = total)
                        .or_insert_with(|| Progress {
                            current: 0,
                            total,
                            start: Utc::now(),
                        });
                }
                _ => {} // We don't care about anything else
            }
        }

        ctx.input(|i| {
            if i.screen_rect() != self.prev_rect {
                self.dur = Some(Utc::now());
                self.prev_rect = i.screen_rect;
            }
        });

        if self.dur.is_some() {
            ctx.request_repaint();
        }

        if self
            .dur
            .as_ref()
            .map(|i| Utc::now() - *i > TimeDelta::seconds(1))
            .unwrap_or(false)
        {
            self.dur.take();
            ctx.forget_all_images();
        }

        egui::SidePanel::left("main_left")
            .resizable(false)
            .show(ctx, |ui| {
                ui.visuals_mut().striped = true;
                ui.add_enabled_ui(!self.runtime_settings.locked(), |ui| {
                    self.runtime_settings.show(&self.sender, ui);
                });
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add_enabled_ui(!self.runtime_settings.locked(), |ui| {
                if self.runtime_settings.locked() {
                    ui.vertical(|ui| {
                        for (name, progress) in &self.progress {
                            let start = progress.start;
                            let progress = progress.current as f64 / progress.total as f64;
                            let elapsed = Utc::now() - start;
                            let res = elapsed * ((1.0 - progress) / progress * 100000.0) as i32 / 100000;
                            let formatted = if res.num_minutes() > 0 {
                                format!("{}m {}s", res.num_minutes(), res.num_seconds() % 60)
                            } else {
                                format!("{}s", res.num_seconds())
                            };
                            if progress < 1.0 {
                                ui.heading(format!("{name} ({formatted})"));
                            } else {
                                ui.heading(name.deref());
                            }
                            ui.add(
                                egui::ProgressBar::new(progress as f32)
                                    .show_percentage()
                                    .animate(true),
                            );
                        }

                        ui.centered_and_justified(|ui| {
                            ui.label("Images may take a second to load...");
                        });
                    });
                } else if self.images.categories.is_empty() {
                    ui.centered_and_justified(|ui| {
                        ui.label("Waiting for images...");
                    });
                } else {
                    // Process category
                    let available_space = ui.available_width();
                    let spacing = ui.style().spacing.item_spacing.x;
                    let widget_width = (available_space - spacing) / 2.0;
                    let mut uri = ArcIntern::from("");
                    egui::Grid::new("select_image_grid")
                        .min_col_width(widget_width)
                        .max_col_width(widget_width)
                        .show(ui, |ui| {
                            ui.label("Select category");
                            egui::ComboBox::new("category_box", "")
                                .selected_text(self.images.current_category.deref())
                                .width(widget_width)
                                .show_ui(ui, |ui| {
                                    for category in self.images.categories.keys() {
                                        ui.selectable_value(
                                            &mut self.images.current_category,
                                            category.clone(),
                                            category.deref(),
                                        );
                                    }
                                });
                            ui.end_row();

                            match &self.images.categories[&self.images.current_category] {
                                Category::Standard { first_key, data } => {
                                    if !data.contains_key(&self.images.current_name) {
                                        self.images.current_name = first_key.clone();
                                    }

                                    ui.label("Select image");
                                    egui::ComboBox::new("image_box", "")
                                        .selected_text(self.images.current_name.deref())
                                        .width(widget_width)
                                        .show_ui(ui, |ui| {
                                            for name in data.keys() {
                                                ui.selectable_value(
                                                    &mut self.images.current_name,
                                                    name.clone(),
                                                    name.deref(),
                                                );
                                            }
                                        });
                                    uri = data[&self.images.current_name].clone();
                                }
                                Category::SeatProjection { first_key, data } => {
                                    if !data.contains_key(&self.images.current_ty) {
                                        self.images.current_ty = first_key.clone();
                                    }

                                    ui.label("Select data");
                                    egui::ComboBox::new("data_box", "")
                                        .selected_text(self.images.current_ty.deref())
                                        .width(widget_width)
                                        .show_ui(ui, |ui| {
                                            for ty in data.keys() {
                                                ui.selectable_value(
                                                    &mut self.images.current_ty,
                                                    ty.clone(),
                                                    ty.deref(),
                                                );
                                            }
                                        });
                                    ui.end_row();

                                    match &data[&self.images.current_ty] {
                                        SeatProjectionImage::Single(new_uri) => {
                                            uri = new_uri.clone()
                                        }
                                        SeatProjectionImage::Multiple { first_key, data } => {
                                            if !data.contains_key(&self.images.current_name) {
                                                self.images.current_name = first_key.clone();
                                            }

                                            ui.label("Select image");
                                            egui::ComboBox::new("image_box", "")
                                                .selected_text(self.images.current_name.deref())
                                                .width(widget_width)
                                                .show_ui(ui, |ui| {
                                                    let mut keys = data.keys().collect::<Vec<_>>();
                                                    keys.sort();
                                                    for name in keys {
                                                        ui.selectable_value(
                                                            &mut self.images.current_name,
                                                            name.clone(),
                                                            name.deref(),
                                                        );
                                                    }
                                                });
                                            uri = data[&self.images.current_name].clone();
                                        }
                                    }
                                }
                            }
                        });

                    if !uri.is_empty() {
                        ui.add(egui::Image::new(uri.deref()).shrink_to_fit());
                    }
                }
            });
        });
    }
}

pub enum Message {
    // If there are multiple images then we cycle through them
    ImageStandard(ArcIntern<str>, ArcIntern<str>, ArcIntern<str>),
    ImageSeatProjectionSingle(ArcIntern<str>, ArcIntern<str>, ArcIntern<str>),
    ImageSeatProjectionMultiple(
        ArcIntern<str>,
        ArcIntern<str>,
        ArcIntern<str>,
        ArcIntern<str>,
    ),
    DropImages,
    SimulationFinished,
    TotalProgress(ArcIntern<str>, usize),
    IncrementProgress(ArcIntern<str>),
    AppShutdown,
}

#[derive(Clone, Debug, PartialEq)]
enum Category {
    Standard {
        first_key: ArcIntern<str>,
        data: FxHashMap<ArcIntern<str>, ArcIntern<str>>,
    },
    SeatProjection {
        first_key: ArcIntern<str>,
        data: FxHashMap<ArcIntern<str>, SeatProjectionImage>,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum SeatProjectionImage {
    Single(ArcIntern<str>),
    Multiple {
        first_key: ArcIntern<str>,
        data: FxHashMap<ArcIntern<str>, ArcIntern<str>>,
    },
}

#[derive(Default)]
struct ImageStore {
    categories: FxHashMap<ArcIntern<str>, Category>,

    current_category: ArcIntern<str>,
    current_ty: ArcIntern<str>,
    current_name: ArcIntern<str>,
}

impl ImageStore {
    fn process_msg(&mut self, msg: &Message) -> bool {
        match msg {
            Message::ImageStandard(category, name, uri) => {
                self.categories
                    .entry(category.clone())
                    .and_modify(|category| match category {
                        Category::Standard { data, .. } => {
                            data.insert(name.clone(), uri.clone());
                        }
                        Category::SeatProjection { .. } => panic!("incorrect category"),
                    })
                    .or_insert_with(|| Category::Standard {
                        data: {
                            let mut map = FxHashMap::default();
                            map.insert(name.clone(), uri.clone());
                            map
                        },
                        first_key: name.clone(),
                    });

                if self.current_category.is_empty() {
                    self.current_category = category.clone();
                }
                if self.current_name.is_empty() {
                    self.current_name = name.clone();
                }

                true
            }
            Message::ImageSeatProjectionSingle(category, name, uri) => {
                self.categories
                    .entry(category.clone())
                    .and_modify(|category| match category {
                        Category::Standard { .. } => panic!("incorrect category"),
                        Category::SeatProjection { data, .. } => {
                            data.entry(name.clone())
                                .and_modify(|_| panic!("entry already exists!"))
                                .or_insert_with(|| SeatProjectionImage::Single(uri.clone()));
                        }
                    })
                    .or_insert_with(|| {
                        self.current_category = category.clone();
                        Category::SeatProjection {
                            data: {
                                let mut map = FxHashMap::default();
                                map.insert(name.clone(), SeatProjectionImage::Single(uri.clone()));
                                map
                            },
                            first_key: name.clone(),
                        }
                    });

                if self.current_category.is_empty() {
                    self.current_category = category.clone();
                }
                if self.current_name.is_empty() {
                    self.current_name = name.clone();
                }

                true
            }
            Message::ImageSeatProjectionMultiple(category, ty, name, uri) => {
                let create = || SeatProjectionImage::Multiple {
                    data: {
                        let mut map = FxHashMap::default();
                        map.insert(name.clone(), uri.clone());
                        map
                    },
                    first_key: name.clone(),
                };
                self.categories
                    .entry(category.clone())
                    .and_modify(|category| match category {
                        Category::Standard { .. } => panic!("incorrect category"),
                        Category::SeatProjection { data, .. } => {
                            data.entry(ty.clone())
                                .and_modify(|v| match v {
                                    SeatProjectionImage::Single(..) => {
                                        panic!("incorrect category!")
                                    }
                                    SeatProjectionImage::Multiple { data, .. } => {
                                        data.entry(name.clone())
                                            .and_modify(|_| panic!("entry already exists!"))
                                            .or_insert_with(|| uri.clone());
                                    }
                                })
                                .or_insert_with(create);
                        }
                    })
                    .or_insert_with(|| Category::SeatProjection {
                        data: {
                            let mut map = FxHashMap::default();
                            map.insert(name.clone(), SeatProjectionImage::Single(uri.clone()));
                            map
                        },
                        first_key: name.clone(),
                    });

                if self.current_category.is_empty() {
                    self.current_category = category.clone();
                }
                if self.current_ty.is_empty() {
                    self.current_ty = ty.clone();
                }
                if self.current_name.is_empty() {
                    self.current_name = name.clone();
                }

                true
            }
            _ => false,
        }
    }
}
