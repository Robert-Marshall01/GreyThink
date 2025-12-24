use gtk4::prelude::*;
use gtk4::{
    Application, ApplicationWindow, Box, Button, Label, Orientation, Scale, Switch,
};
use std::sync::{Arc, Mutex};

use crate::equalizer::EqualizerState;
use crate::config;

pub fn build_window(app: &Application, eq_state: Arc<Mutex<EqualizerState>>) -> ApplicationWindow {
    let window = ApplicationWindow::builder()
        .application(app)
        .title("Linux Equalizer")
        .default_width(900)
        .default_height(400)
        .build();

    let main_box = Box::new(Orientation::Vertical, 10);
    main_box.set_margin_top(10);
    main_box.set_margin_bottom(10);
    main_box.set_margin_start(10);
    main_box.set_margin_end(10);

    // Header with enable switch
    let header_box = Box::new(Orientation::Horizontal, 10);
    let title_label = Label::new(Some("10-Band Equalizer"));
    title_label.set_markup("<big><b>10-Band Equalizer</b></big>");
    header_box.append(&title_label);

    let enable_switch = Switch::new();
    enable_switch.set_active(true);
    enable_switch.set_halign(gtk4::Align::End);
    enable_switch.set_hexpand(true);
    
    let eq_state_clone = eq_state.clone();
    enable_switch.connect_active_notify(move |switch| {
        if let Ok(mut state) = eq_state_clone.lock() {
            state.enabled = switch.is_active();
        }
    });
    
    header_box.append(&enable_switch);
    main_box.append(&header_box);

    // Equalizer bands
    let bands_box = Box::new(Orientation::Horizontal, 5);
    bands_box.set_homogeneous(true);
    bands_box.set_hexpand(true);
    bands_box.set_vexpand(true);

    let mut sliders = Vec::new();
    let mut gain_labels = Vec::new();
    
    let state = eq_state.lock().unwrap();
    for (i, band) in state.bands.iter().enumerate() {
        let (band_box, scale, gain_label) = create_band_slider(i, band.frequency, band.gain, eq_state.clone());
        sliders.push(scale);
        gain_labels.push(gain_label);
        bands_box.append(&band_box);
    }
    drop(state);

    main_box.append(&bands_box);

    // Control buttons
    let button_box = Box::new(Orientation::Horizontal, 10);
    button_box.set_halign(gtk4::Align::Center);

    let reset_button = Button::with_label("Reset");
    let eq_state_clone = eq_state.clone();
    let sliders_clone = sliders.clone();
    let labels_clone = gain_labels.clone();
    reset_button.connect_clicked(move |_| {
        if let Ok(mut state) = eq_state_clone.lock() {
            state.reset();
        }
        // Update all sliders to 0
        for slider in &sliders_clone {
            slider.set_value(0.0);
        }
        for label in &labels_clone {
            label.set_markup("<small>0.0 dB</small>");
        }
    });
    button_box.append(&reset_button);

    let save_button = Button::with_label("Save Preset");
    let eq_state_clone = eq_state.clone();
    save_button.connect_clicked(move |_| {
        if let Ok(state) = eq_state_clone.lock() {
            if let Err(e) = config::save_preset(&state, "default") {
                eprintln!("Failed to save preset: {}", e);
            }
        }
    });
    button_box.append(&save_button);

    let load_button = Button::with_label("Load Preset");
    let eq_state_clone = eq_state.clone();
    let sliders_clone2 = sliders.clone();
    let labels_clone2 = gain_labels.clone();
    load_button.connect_clicked(move |_| {
        if let Ok(loaded_state) = config::load_preset("default") {
            // Update sliders to match loaded state
            for (i, slider) in sliders_clone2.iter().enumerate() {
                if i < loaded_state.bands.len() {
                    slider.set_value(loaded_state.bands[i].gain as f64);
                }
            }
            for (i, label) in labels_clone2.iter().enumerate() {
                if i < loaded_state.bands.len() {
                    label.set_markup(&format!("<small>{:.1} dB</small>", loaded_state.bands[i].gain));
                }
            }
            if let Ok(mut state) = eq_state_clone.lock() {
                *state = loaded_state;
            }
        }
    });
    button_box.append(&load_button);

    main_box.append(&button_box);
    window.set_child(Some(&main_box));

    window
}

fn create_band_slider(
    index: usize,
    frequency: f32,
    initial_gain: f32,
    eq_state: Arc<Mutex<EqualizerState>>,
) -> (Box, Scale, Label) {
    let band_box = Box::new(Orientation::Vertical, 5);
    band_box.set_hexpand(true);

    // Frequency label
    let freq_label = Label::new(Some(&format_frequency(frequency)));
    freq_label.set_markup(&format!("<small>{}</small>", format_frequency(frequency)));
    band_box.append(&freq_label);

    // Gain slider (vertical)
    let scale = Scale::with_range(Orientation::Vertical, -12.0, 12.0, 0.5);
    scale.set_inverted(true); // Higher values at top
    scale.set_value(initial_gain as f64);
    scale.set_draw_value(true);
    scale.set_value_pos(gtk4::PositionType::Bottom);
    scale.set_vexpand(true);
    scale.set_hexpand(true);

    // Gain value label
    let gain_label = Label::new(Some(&format!("{:.1} dB", initial_gain)));
    gain_label.set_markup(&format!("<small>{:.1} dB</small>", initial_gain));

    let gain_label_clone = gain_label.clone();
    scale.connect_value_changed(move |scale| {
        let value = scale.value();
        if let Ok(mut state) = eq_state.lock() {
            state.set_band_gain(index, value as f32);
        }
        gain_label_clone.set_markup(&format!("<small>{:.1} dB</small>", value));
    });

    band_box.append(&scale);
    band_box.append(&gain_label.clone());

    (band_box, scale, gain_label)
}

fn format_frequency(freq: f32) -> String {
    if freq >= 1000.0 {
        format!("{:.1}k", freq / 1000.0)
    } else {
        format!("{:.0}", freq)
    }
}
