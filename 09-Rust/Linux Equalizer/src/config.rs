use crate::equalizer::EqualizerState;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;

#[derive(Debug, Serialize, Deserialize)]
pub struct Preset {
    pub name: String,
    pub state: EqualizerState,
}

fn get_config_dir() -> Result<PathBuf, Box<dyn std::error::Error>> {
    let config_dir = dirs::config_dir()
        .ok_or("Could not determine config directory")?
        .join("linux-equalizer");
    
    fs::create_dir_all(&config_dir)?;
    Ok(config_dir)
}

pub fn save_preset(state: &EqualizerState, name: &str) -> Result<(), Box<dyn std::error::Error>> {
    let config_dir = get_config_dir()?;
    let preset_file = config_dir.join(format!("{}.json", name));
    
    let preset = Preset {
        name: name.to_string(),
        state: state.clone(),
    };
    
    let json = serde_json::to_string_pretty(&preset)?;
    fs::write(preset_file, json)?;
    
    println!("Preset '{}' saved successfully", name);
    Ok(())
}

pub fn load_preset(name: &str) -> Result<EqualizerState, Box<dyn std::error::Error>> {
    let config_dir = get_config_dir()?;
    let preset_file = config_dir.join(format!("{}.json", name));
    
    let json = fs::read_to_string(preset_file)?;
    let preset: Preset = serde_json::from_str(&json)?;
    
    println!("Preset '{}' loaded successfully", name);
    Ok(preset.state)
}

pub fn list_presets() -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let config_dir = get_config_dir()?;
    let mut presets = Vec::new();
    
    for entry in fs::read_dir(config_dir)? {
        let entry = entry?;
        let path = entry.path();
        
        if path.extension().and_then(|s| s.to_str()) == Some("json") {
            if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                presets.push(name.to_string());
            }
        }
    }
    
    Ok(presets)
}
