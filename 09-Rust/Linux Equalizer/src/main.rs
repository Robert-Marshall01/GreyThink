mod equalizer;
mod gui;
mod audio;
mod config;
mod easyeffects;

use gtk4::prelude::*;
use gtk4::Application;
use std::sync::{Arc, Mutex};
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let headless = args.contains(&"--headless".to_string()) || args.contains(&"--daemon".to_string());
    
    // CRITICAL: Cleanup any orphaned virtual devices from previous crashes FIRST
    println!("Checking for orphaned audio devices...");
    audio::cleanup_orphaned_devices();
    
    // Register cleanup handler for Ctrl+C
    ctrlc::set_handler(move || {
        println!("\n\nShutting down...");
        audio::cleanup_virtual_device();
        std::process::exit(0);
    }).expect("Error setting Ctrl-C handler");

    if headless {
        // Run without GUI
        run_headless();
    } else {
        // Run with GUI
        let app = Application::builder()
            .application_id("com.example.linux-equalizer")
            .build();

        app.connect_activate(build_ui);
        
        // CRITICAL: Cleanup when app shuts down (window closed, etc.)
        app.connect_shutdown(|_| {
            println!("\n\nGUI closing, cleaning up audio...");
            audio::cleanup_virtual_device();
        });
        
        app.run();
        
        // Also cleanup after run() returns (belt and suspenders)
        audio::cleanup_virtual_device();
    }
}

fn run_headless() {
    println!("Starting Linux Equalizer in headless mode...");
    
    // Load saved settings
    let loaded_state = config::load_preset("current")
        .unwrap_or_else(|_| equalizer::EqualizerState::new());
    let eq_state = Arc::new(Mutex::new(loaded_state));
    
    println!("\n=== Linux Equalizer Ready (Headless Mode) ===");
    println!("Virtual audio device created!");
    println!("All applications now output through the equalizer.");
    println!("Run without --headless to open GUI for adjustments.");
    
    // Start audio processing (blocks until error or interrupt)
    if let Err(e) = audio::start_audio_processing(eq_state) {
        eprintln!("Audio processing error: {}", e);
        audio::cleanup_virtual_device();
        std::process::exit(1);
    }
}

fn is_service_running() -> bool {
    // Check if the systemd service is running
    std::process::Command::new("systemctl")
        .args(&["--user", "is-active", "--quiet", "linux-equalizer.service"])
        .status()
        .map(|status| status.success())
        .unwrap_or(false)
}

fn build_ui(app: &Application) {
    // Load saved settings or create new state
    let loaded_state = config::load_preset("current")
        .unwrap_or_else(|_| equalizer::EqualizerState::new());
    let eq_state = Arc::new(Mutex::new(loaded_state));
    
    let window = gui::build_window(app, eq_state.clone());
    window.present();
    
    // Check if background service is already running
    if is_service_running() {
        println!("\n=== Linux Equalizer GUI ===");
        println!("✓ Background service is running");
        println!("✓ Use the sliders to adjust EQ settings");
        println!("✓ Changes are applied in real-time");
        println!("\nNote: Close this window to keep the service running in background");
    } else {
        println!("\n=== Linux Equalizer GUI (Standalone Mode) ===");
        println!("Note: Background service not detected, starting audio processing...");
        
        // Start audio processing with virtual device (only if service not running)
        let eq_state_audio = eq_state.clone();
        std::thread::spawn(move || {
            if let Err(e) = audio::start_audio_processing(eq_state_audio) {
                eprintln!("Audio processing error: {}", e);
                audio::cleanup_virtual_device();
            }
        });
        
        println!("✓ Virtual audio device created!");
        println!("✓ All applications now output through the equalizer.");
    }
    
    // Save state periodically (always needed to update the service)
    let eq_state_clone = eq_state.clone();
    gtk4::glib::timeout_add_seconds_local(2, move || {
        if let Ok(state) = eq_state_clone.lock() {
            let _ = config::save_preset(&state, "current");
        }
        gtk4::glib::ControlFlow::Continue
    });
}
