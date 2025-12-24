use crate::equalizer::EqualizerState;
use serde_json::{json, Value};
use std::fs;
use std::path::PathBuf;
use std::process::Command;

/// Get EasyEffects preset directory
fn get_preset_dir() -> PathBuf {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/home".to_string());
    PathBuf::from(format!("{}/.config/easyeffects/output", home))
}

/// Convert our EQ state to EasyEffects preset format
fn create_easyeffects_preset(state: &EqualizerState) -> Value {
    // EasyEffects uses a specific JSON format for presets
    json!({
        "output": {
            "equalizer": {
                "bypass": !state.enabled,
                "input-gain": 0.0,
                "output-gain": 0.0,
                "mode": "IIR",
                "num-bands": 10,
                "split-channels": false,
                "left": {
                    "band0": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[0].gain,
                        "frequency": state.bands[0].frequency,
                        "q": state.bands[0].q
                    },
                    "band1": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[1].gain,
                        "frequency": state.bands[1].frequency,
                        "q": state.bands[1].q
                    },
                    "band2": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[2].gain,
                        "frequency": state.bands[2].frequency,
                        "q": state.bands[2].q
                    },
                    "band3": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[3].gain,
                        "frequency": state.bands[3].frequency,
                        "q": state.bands[3].q
                    },
                    "band4": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[4].gain,
                        "frequency": state.bands[4].frequency,
                        "q": state.bands[4].q
                    },
                    "band5": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[5].gain,
                        "frequency": state.bands[5].frequency,
                        "q": state.bands[5].q
                    },
                    "band6": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[6].gain,
                        "frequency": state.bands[6].frequency,
                        "q": state.bands[6].q
                    },
                    "band7": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[7].gain,
                        "frequency": state.bands[7].frequency,
                        "q": state.bands[7].q
                    },
                    "band8": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[8].gain,
                        "frequency": state.bands[8].frequency,
                        "q": state.bands[8].q
                    },
                    "band9": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[9].gain,
                        "frequency": state.bands[9].frequency,
                        "q": state.bands[9].q
                    }
                },
                "right": {
                    "band0": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[0].gain,
                        "frequency": state.bands[0].frequency,
                        "q": state.bands[0].q
                    },
                    "band1": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[1].gain,
                        "frequency": state.bands[1].frequency,
                        "q": state.bands[1].q
                    },
                    "band2": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[2].gain,
                        "frequency": state.bands[2].frequency,
                        "q": state.bands[2].q
                    },
                    "band3": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[3].gain,
                        "frequency": state.bands[3].frequency,
                        "q": state.bands[3].q
                    },
                    "band4": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[4].gain,
                        "frequency": state.bands[4].frequency,
                        "q": state.bands[4].q
                    },
                    "band5": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[5].gain,
                        "frequency": state.bands[5].frequency,
                        "q": state.bands[5].q
                    },
                    "band6": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[6].gain,
                        "frequency": state.bands[6].frequency,
                        "q": state.bands[6].q
                    },
                    "band7": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[7].gain,
                        "frequency": state.bands[7].frequency,
                        "q": state.bands[7].q
                    },
                    "band8": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[8].gain,
                        "frequency": state.bands[8].frequency,
                        "q": state.bands[8].q
                    },
                    "band9": {
                        "type": "Bell",
                        "mode": "RLC (BT)",
                        "slope": "x1",
                        "solo": false,
                        "mute": false,
                        "gain": state.bands[9].gain,
                        "frequency": state.bands[9].frequency,
                        "q": state.bands[9].q
                    }
                }
            }
        }
    })
}

/// Apply EQ settings to EasyEffects
pub fn apply_to_easyeffects(state: &EqualizerState) -> Result<(), Box<dyn std::error::Error>> {
    // Check if EasyEffects is running
    let check = Command::new("pgrep")
        .arg("easyeffects")
        .output()?;
    
    let is_running = check.status.success();
    
    if !is_running {
        // Start EasyEffects in the background
        println!("Starting EasyEffects...");
        Command::new("easyeffects")
            .arg("--gapplication-service")
            .spawn()?;
        std::thread::sleep(std::time::Duration::from_secs(3));
    }
    
    // Create preset directory if it doesn't exist
    let preset_dir = get_preset_dir();
    fs::create_dir_all(&preset_dir)?;
    
    // Write preset file
    let preset_path = preset_dir.join("LinuxEqualizer.json");
    let preset_json = create_easyeffects_preset(state);
    let json_str = serde_json::to_string_pretty(&preset_json)?;
    fs::write(&preset_path, &json_str)?;
    
    println!("✓ Preset written with gains: {:?}", 
             state.bands.iter().map(|b| b.gain).collect::<Vec<_>>());
    
    // Force reload and activate the preset using gsettings
    let _ = Command::new("gsettings")
        .args(&[
            "set",
            "com.github.wwmm.easyeffects",
            "last-loaded-output-preset",
            "LinuxEqualizer"
        ])
        .output();
    
    // Send reset signal to force EasyEffects to reload
    let _ = Command::new("easyeffects")
        .arg("-l")
        .arg("LinuxEqualizer")
        .output();
    
    // Small delay to let it load
    std::thread::sleep(std::time::Duration::from_millis(100));
    
    // Verify it loaded
    let verify = Command::new("gsettings")
        .args(&[
            "get",
            "com.github.wwmm.easyeffects",
            "last-loaded-output-preset"
        ])
        .output()?;
    
    let active_preset = String::from_utf8_lossy(&verify.stdout);
    if active_preset.contains("LinuxEqualizer") {
        println!("✓ EasyEffects preset ACTIVE - system audio is being processed!");
    } else {
        println!("⚠ Preset saved but may need manual activation in EasyEffects");
    }
    
    Ok(())
}
