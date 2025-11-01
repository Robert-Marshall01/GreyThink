use eframe::egui;
use eframe::egui::{CentralPanel, Context, ScrollArea};

use password_checker::{classify_score, mask_password, score_password};

#[derive(Default)]
struct App {
    password: String,
    last_masked: String,
    last_score: Option<i32>,
    last_class: String,
    last_suggestions: Vec<String>,
}

impl eframe::App for App {
    fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame) {
        CentralPanel::default().show(ctx, |ui| {
            ui.heading("Password Strength Checker");
            ui.label("Enter a password and press Evaluate:");

            ui.horizontal(|ui| {
                let pw = ui.text_edit_singleline(&mut self.password);
                if ui.button("Evaluate").clicked() {
                    let pw = self.password.trim().to_string();
                    if pw.is_empty() {
                        self.last_masked = "".into();
                        self.last_score = None;
                        self.last_class = "".into();
                        self.last_suggestions.clear();
                    } else {
                        let (score, suggestions) = score_password(&pw);
                        self.last_score = Some(score);
                        self.last_class = classify_score(score).to_string();
                        self.last_masked = mask_password(&pw);
                        self.last_suggestions = suggestions;
                    }
                }
                if ui.button("Clear").clicked() {
                    self.password.clear();
                }
                if pw.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)) {
                    // Evaluate on Enter
                    let pw = self.password.trim().to_string();
                    if !pw.is_empty() {
                        let (score, suggestions) = score_password(&pw);
                        self.last_score = Some(score);
                        self.last_class = classify_score(score).to_string();
                        self.last_masked = mask_password(&pw);
                        self.last_suggestions = suggestions;
                    }
                }
            });

            ui.separator();

            if let Some(score) = self.last_score {
                ui.label(format!("Masked: {}", self.last_masked));
                ui.label(format!("Score: {}", score));
                ui.label(format!("Classification: {}", self.last_class));

                if !self.last_suggestions.is_empty() {
                    ui.label("Suggestions:");
                    ScrollArea::vertical().max_height(150.0).show(ui, |ui| {
                        for s in &self.last_suggestions {
                            ui.label(format!("- {}", s));
                        }
                    });
                }
            } else {
                ui.label("No evaluation yet.");
            }
        });
    }
}

fn main() {
    let options = eframe::NativeOptions::default();
    let _ = eframe::run_native(
        "Password Strength Checker",
        options,
        Box::new(|_cc| Box::new(App::default())),
    );
}
