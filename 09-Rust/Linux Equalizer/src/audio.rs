use crate::equalizer::EqualizerState;
use std::process::Command;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::{Arc, Mutex, RwLock};
use std::thread;
use std::time::Duration;

// Global flag to signal shutdown
static SHUTDOWN_REQUESTED: AtomicBool = AtomicBool::new(false);
// Track our LADSPA module ID
static LADSPA_MODULE_ID: AtomicU32 = AtomicU32::new(0);
// Store original default sink (using RwLock for safe access)
static ORIGINAL_SINK: RwLock<Option<String>> = RwLock::new(None);

/// Get the current default sink (excluding our virtual device)
fn get_hardware_default_sink() -> Result<String, String> {
    let output = Command::new("pactl")
        .args(["list", "short", "sinks"])
        .output()
        .map_err(|e| format!("Failed to list sinks: {}", e))?;
    
    let sinks = String::from_utf8_lossy(&output.stdout);
    
    for line in sinks.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 2 {
            let sink_name = parts[1];
            // Skip virtual/equalizer sinks
            if !sink_name.contains("equalizer") 
               && !sink_name.contains("easyeffects")
               && !sink_name.contains("null") {
                return Ok(sink_name.to_string());
            }
        }
    }
    
    Err("No hardware sink found".to_string())
}

/// Check if our equalizer sink exists
fn equalizer_exists() -> bool {
    if let Ok(output) = Command::new("pactl")
        .args(["list", "short", "sinks"])
        .output()
    {
        let sinks = String::from_utf8_lossy(&output.stdout);
        return sinks.contains("linux_equalizer");
    }
    false
}

/// Get all modules with "linux_equalizer" in their config
fn find_ladspa_modules() -> Vec<String> {
    let mut modules = Vec::new();
    if let Ok(output) = Command::new("pactl")
        .args(["list", "short", "modules"])
        .output()
    {
        let mods = String::from_utf8_lossy(&output.stdout);
        for line in mods.lines() {
            if line.contains("linux_equalizer") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if let Some(id) = parts.first() {
                    modules.push(id.to_string());
                }
            }
        }
    }
    modules
}

/// Build the control string from EQ state (15 values in dB)
fn build_control_string(state: &EqualizerState) -> String {
    // Map 10-band state to 15-band mbeq plugin
    // Our bands: 32, 64, 125, 250, 500, 1k, 2k, 4k, 8k, 16k
    // mbeq bands: 50, 100, 156, 220, 311, 440, 622, 880, 1250, 1750, 2500, 3500, 5000, 10000, 20000
    
    let gains = [
        state.band_32hz(),   // 50Hz - closest to 32Hz
        state.band_64hz(),   // 100Hz - closest to 64Hz
        (state.band_64hz() + state.band_125hz()) / 2.0,  // 156Hz
        state.band_125hz(),  // 220Hz - closest to 125Hz
        state.band_250hz(),  // 311Hz - closest to 250Hz
        state.band_500hz(),  // 440Hz - closest to 500Hz
        state.band_500hz(),  // 622Hz
        (state.band_500hz() + state.band_1khz()) / 2.0,  // 880Hz
        state.band_1khz(),   // 1250Hz - closest to 1kHz
        (state.band_1khz() + state.band_2khz()) / 2.0,   // 1750Hz
        state.band_2khz(),   // 2500Hz - closest to 2kHz
        (state.band_2khz() + state.band_4khz()) / 2.0,   // 3500Hz
        state.band_4khz(),   // 5000Hz - closest to 4kHz
        state.band_8khz(),   // 10000Hz - closest to 8kHz
        state.band_16khz(),  // 20000Hz - closest to 16kHz
    ];
    
    gains.iter()
        .map(|g| format!("{:.1}", g))
        .collect::<Vec<_>>()
        .join(",")
}

/// Create the LADSPA EQ sink
fn create_ladspa_sink(master_sink: &str, control_string: &str) -> Result<u32, String> {
    // First remove any existing
    remove_ladspa_sink();
    thread::sleep(Duration::from_millis(200));
    
    let output = Command::new("pactl")
        .args([
            "load-module",
            "module-ladspa-sink",
            "sink_name=linux_equalizer",
            "sink_properties=device.description='Linux Equalizer'",
            &format!("master={}", master_sink),
            "plugin=mbeq_1197",
            "label=mbeq",
            &format!("control={}", control_string),
        ])
        .output()
        .map_err(|e| format!("Failed to create LADSPA sink: {}", e))?;
    
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("LADSPA sink creation failed: {}", stderr));
    }
    
    let module_id_str = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let module_id = module_id_str.parse::<u32>()
        .map_err(|_| format!("Invalid module ID: {}", module_id_str))?;
    
    LADSPA_MODULE_ID.store(module_id, Ordering::Relaxed);
    Ok(module_id)
}

/// Remove the LADSPA EQ sink
fn remove_ladspa_sink() {
    // Unload our tracked module
    let our_id = LADSPA_MODULE_ID.load(Ordering::Relaxed);
    if our_id > 0 {
        let _ = Command::new("pactl")
            .args(["unload-module", &our_id.to_string()])
            .output();
        LADSPA_MODULE_ID.store(0, Ordering::Relaxed);
    }
    
    // Also find and unload any other linux_equalizer modules
    for module_id in find_ladspa_modules() {
        let _ = Command::new("pactl")
            .args(["unload-module", &module_id])
            .output();
    }
}

/// Move all sink-inputs to a target sink
fn move_all_sink_inputs_to(target_sink: &str) {
    if let Ok(output) = Command::new("pactl")
        .args(["list", "short", "sink-inputs"])
        .output()
    {
        let inputs = String::from_utf8_lossy(&output.stdout);
        for line in inputs.lines() {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if let Some(input_id) = parts.first() {
                let _ = Command::new("pactl")
                    .args(["move-sink-input", input_id, target_sink])
                    .output();
            }
        }
    }
}

/// Cleanup any orphaned virtual devices from previous crashes
pub fn cleanup_orphaned_devices() {
    if equalizer_exists() {
        println!("⚠️  Found orphaned 'Linux Equalizer' from previous session");
        
        // Get a hardware sink first
        if let Ok(hw_sink) = get_hardware_default_sink() {
            // Set hardware as default before removing virtual
            let _ = Command::new("pactl")
                .args(["set-default-sink", &hw_sink])
                .output();
            
            // Move all streams to hardware
            move_all_sink_inputs_to(&hw_sink);
        }
        
        // Remove the orphaned LADSPA sink
        remove_ladspa_sink();
        
        thread::sleep(Duration::from_millis(300));
        println!("✓ Orphaned devices cleaned up");
    }
}

/// Start the equalizer (creates LADSPA sink and manages it)
pub fn start_audio_processing(
    eq_state: Arc<Mutex<EqualizerState>>,
) -> Result<(), Box<dyn std::error::Error>> {
    SHUTDOWN_REQUESTED.store(false, Ordering::Relaxed);
    
    println!("\n=== Starting Linux Equalizer ===");
    
    // Find the hardware sink to use as master
    let master_sink = get_hardware_default_sink()?;
    println!("✓ Hardware output: {}", master_sink);
    
    // Store original sink for cleanup
    if let Ok(mut original) = ORIGINAL_SINK.write() {
        *original = Some(master_sink.clone());
    }
    
    // Get initial EQ settings
    let initial_control = {
        let state = eq_state.lock().unwrap();
        build_control_string(&state)
    };
    
    // Create the LADSPA EQ sink
    let module_id = create_ladspa_sink(&master_sink, &initial_control)?;
    println!("✓ LADSPA EQ sink created (module {})", module_id);
    
    // Wait for sink to be ready
    thread::sleep(Duration::from_millis(300));
    
    // Set the EQ sink as default
    let _ = Command::new("pactl")
        .args(["set-default-sink", "linux_equalizer"])
        .output();
    println!("✓ 'Linux Equalizer' set as default output");
    
    // Move existing streams to the EQ sink
    move_all_sink_inputs_to("linux_equalizer");
    println!("✓ Existing audio streams moved to equalizer");
    
    println!("\n✅ SYSTEM-WIDE EQUALIZER IS NOW ACTIVE ✅");
    println!("✅ All audio → Linux Equalizer → {} → Speakers", master_sink);
    println!("\nTo stop: Close the GUI or press Ctrl+C\n");
    
    // Main loop: monitor for EQ changes and new sink-inputs
    let mut last_control = initial_control;
    
    loop {
        if SHUTDOWN_REQUESTED.load(Ordering::Relaxed) {
            break;
        }
        
        // Check for EQ state changes
        if let Ok(state) = eq_state.try_lock() {
            let new_control = build_control_string(&state);
            
            if new_control != last_control {
                println!("📊 EQ settings changed, updating...");
                
                // Need to reload the module with new control values
                // First move streams back to hardware temporarily
                move_all_sink_inputs_to(&master_sink);
                
                // Reload the LADSPA module with new settings
                match create_ladspa_sink(&master_sink, &new_control) {
                    Ok(_) => {
                        thread::sleep(Duration::from_millis(100));
                        // Set as default again
                        let _ = Command::new("pactl")
                            .args(["set-default-sink", "linux_equalizer"])
                            .output();
                        // Move streams back
                        move_all_sink_inputs_to("linux_equalizer");
                        println!("✓ EQ updated");
                        last_control = new_control;
                    }
                    Err(e) => {
                        eprintln!("❌ Failed to update EQ: {}", e);
                    }
                }
            }
        }
        
        // Periodically move new sink-inputs to EQ
        move_new_sink_inputs();
        
        // Sleep to avoid busy-waiting
        thread::sleep(Duration::from_millis(500));
    }
    
    Ok(())
}

/// Move any sink-inputs not on our EQ to our EQ
fn move_new_sink_inputs() {
    // Get EQ sink ID
    let eq_sink_id: Option<String> = Command::new("pactl")
        .args(["list", "short", "sinks"])
        .output()
        .ok()
        .and_then(|output| {
            let sinks = String::from_utf8_lossy(&output.stdout);
            sinks.lines()
                .find(|line| line.contains("linux_equalizer"))
                .and_then(|line| line.split_whitespace().next())
                .map(|s| s.to_string())
        });
    
    let eq_sink_id = match eq_sink_id {
        Some(id) => id,
        None => return, // EQ sink doesn't exist
    };
    
    // Move any inputs not on EQ
    if let Ok(output) = Command::new("pactl")
        .args(["list", "short", "sink-inputs"])
        .output()
    {
        let inputs = String::from_utf8_lossy(&output.stdout);
        for line in inputs.lines() {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 2 {
                let input_id = parts[0];
                let current_sink = parts[1];
                
                if current_sink != eq_sink_id {
                    let _ = Command::new("pactl")
                        .args(["move-sink-input", input_id, "linux_equalizer"])
                        .output();
                }
            }
        }
    }
}

/// Cleanup and restore normal audio
pub fn cleanup_virtual_device() {
    println!("\n=== Restoring Normal Audio ===");
    
    SHUTDOWN_REQUESTED.store(true, Ordering::Relaxed);
    
    // Get original sink
    let restore_sink = ORIGINAL_SINK.read()
        .ok()
        .and_then(|r| r.clone())
        .or_else(|| get_hardware_default_sink().ok());
    
    if let Some(ref sink) = restore_sink {
        // Move all streams back to hardware
        move_all_sink_inputs_to(sink);
        
        // Set hardware as default
        let _ = Command::new("pactl")
            .args(["set-default-sink", sink])
            .output();
        
        println!("✓ Default output restored to: {}", sink);
    }
    
    // Remove the LADSPA sink
    remove_ladspa_sink();
    
    println!("✓ Equalizer disabled");
}
