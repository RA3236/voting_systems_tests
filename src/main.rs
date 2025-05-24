#![feature(portable_simd)]
#![feature(slice_as_array)]
#![feature(array_chunks)]

use std::{
    sync::OnceLock,
    time::{Duration, Instant},
};

use color_eyre::eyre::eyre;
use eframe::NativeOptions;
use egui::{Context, Rect};
use runtime_settings::AppRuntimeSettings;

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
        Box::new(|_cc| Ok(Box::new(EguiApp::default()))),
    )
    .map_err(|err| eyre!("{}", err))?;

    Ok(())
}

pub type Sender = std::sync::mpsc::Sender<Message>;
pub type Receiver = std::sync::mpsc::Receiver<Message>;

pub static CONTEXT: OnceLock<Context> = OnceLock::new();

struct EguiApp {
    runtime_settings: AppRuntimeSettings,

    images: Vec<String>,
    current_image: usize,

    sender: Sender,
    receiver: Receiver,

    prev_rect: Rect,
    dur: Option<Instant>,
}

impl Default for EguiApp {
    fn default() -> Self {
        let (sender, receiver) = std::sync::mpsc::channel();

        Self {
            runtime_settings: AppRuntimeSettings::default(),

            images: vec![],
            current_image: 0,

            sender,
            receiver,

            prev_rect: Rect::EVERYTHING,
            dur: None,
        }
    }
}

impl eframe::App for EguiApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if CONTEXT.get().is_none() {
            CONTEXT.set(ctx.clone()).unwrap();
        }

        egui_extras::install_image_loaders(ctx);

        for msg in self.receiver.try_iter() {
            match msg {
                Message::ImageFilePath(data) => {
                    self.images.push(data);
                }
                Message::DropImages => {
                    self.images.clear();
                    ctx.forget_all_images();
                }
                Message::Unlock => {
                    self.runtime_settings.unlock();
                }
            }
        }

        ctx.input(|i| {
            if i.screen_rect() != self.prev_rect {
                self.dur = Some(Instant::now());
                self.prev_rect = i.screen_rect;
            }
        });

        if self.dur.is_some() {
            ctx.request_repaint();
        }

        if self
            .dur
            .as_ref()
            .map(|i| i.elapsed() > Duration::from_secs(1))
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
            if self.runtime_settings.locked() {
                ui.centered_and_justified(|ui| {
                    ui.spinner();
                });
            } else if self.images.is_empty() {
                ui.centered_and_justified(|ui| {
                    ui.label("Start a simulation to begin.");
                });
            } else {
                if self.images.len() == 1 {
                    self.current_image = 0;
                }
                ui.add_enabled_ui(self.images.len() > 1, |ui| {
                    ui.vertical_centered_justified(|ui| {
                        let available_space = ui.available_width();
                        let spacing = ui.style().spacing.item_spacing.x;
                        let button_width = (available_space - spacing) / 2.0;

                        egui::Grid::new("image_buttons_grid")
                            .min_col_width(button_width)
                            .max_col_width(button_width)
                            .show(ui, |ui| {
                                ui.vertical_centered_justified(|ui| {
                                    if ui.button("Previous Image").clicked() {
                                        if self.current_image == 0 {
                                            self.current_image = self.images.len() - 1;
                                        } else {
                                            self.current_image -= 1;
                                        }
                                    }
                                });
                                ui.vertical_centered_justified(|ui| {
                                    if ui.button("Next Image").clicked() {
                                        self.current_image =
                                            (self.current_image + 1) % self.images.len();
                                    }
                                });
                                ui.end_row();
                            });
                    });
                });

                let filepath = &self.images[self.current_image];
                ui.add(egui::Image::new(filepath).shrink_to_fit());
            }
        });

        let _ = rayon::yield_now();
    }
}

pub enum Message {
    ImageFilePath(String),
    DropImages,
    Unlock,
}
