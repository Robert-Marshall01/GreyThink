use eframe::egui;
use sysinfo::{System, SystemExt, ComponentExt, DiskExt, NetworkExt, CpuExt};
use num_cpus;
use std::time::{Duration, Instant};
use std::collections::HashMap;
use std::process::Command;

#[derive(PartialEq, Eq, Copy, Clone)]
enum Tab {
    Overview,
    OperatingSystem,
    CPU,
    Memory,
    Disks,
    Fans,
    Motherboard,
    Graphics,
    Display,
    Audio,
    Peripherals,
    Power,
    Network,
}

struct SpectasticApp {
    sys: System,
    last_refresh: Instant,
    refresh_interval: Duration,
    selected: Tab,
    prev_selected: Tab,
    prev_network: HashMap<String, (u64, u64)>,
    // Caches and background fetch receivers for heavy (blocking) queries
    motherboard_cache: Option<Vec<(String, String)>>,
    motherboard_rx: Option<std::sync::mpsc::Receiver<Vec<(String, String)>>>,

    graphics_cache: Option<Vec<Vec<(String, String)>>>,
    graphics_rx: Option<std::sync::mpsc::Receiver<Vec<Vec<(String, String)>>>>,

    audio_cache: Option<Vec<Vec<(String, String)>>>,
    audio_rx: Option<std::sync::mpsc::Receiver<Vec<Vec<(String, String)>>>>,

    display_cache: Option<Vec<Vec<(String, String)>>>,
    display_rx: Option<std::sync::mpsc::Receiver<Vec<Vec<(String, String)>>>>,

    controller_cache: Option<Vec<Vec<(String, String)>>>,
    controller_rx: Option<std::sync::mpsc::Receiver<Vec<Vec<(String, String)>>>>,
    pci_cache: Option<Vec<Vec<(String, String)>>>,
    pci_rx: Option<std::sync::mpsc::Receiver<Vec<Vec<(String, String)>>>>,
    peripherals_cache: Option<Vec<Vec<(String, String)>>>,
    peripherals_rx: Option<std::sync::mpsc::Receiver<Vec<Vec<(String, String)>>>>,
    optical_cache: Option<Vec<Vec<(String, String)>>>,
    optical_rx: Option<std::sync::mpsc::Receiver<Vec<Vec<(String, String)>>>>,
    os_cache: Option<Vec<(String, String)>>,
    os_rx: Option<std::sync::mpsc::Receiver<Vec<(String, String)>>>,
    power_cache: Option<Vec<Vec<(String, String)>>>,
    power_rx: Option<std::sync::mpsc::Receiver<Vec<Vec<(String, String)>>>>,
}

impl Default for SpectasticApp {
    fn default() -> Self {
        let mut sys = System::new_all();
        sys.refresh_all();
        Self {
            sys,
            last_refresh: Instant::now(),
            refresh_interval: Duration::from_secs(2),
            selected: Tab::Overview,
            prev_selected: Tab::Overview,
            prev_network: HashMap::new(),
            motherboard_cache: None,
            motherboard_rx: None,
            graphics_cache: None,
            graphics_rx: None,
            audio_cache: None,
            audio_rx: None,
            display_cache: None,
            display_rx: None,
            controller_cache: None,
            controller_rx: None,
            pci_cache: None,
            pci_rx: None,
            peripherals_cache: None,
            peripherals_rx: None,
            optical_cache: None,
            optical_rx: None,
            os_cache: None,
            os_rx: None,
            power_cache: None,
            power_rx: None,
        }
    }
}

fn human_bytes(mut bytes: f64) -> String {
    const UNITS: [&str; 5] = ["B", "KB", "MB", "GB", "TB"];
    let mut i = 0usize;
    while bytes >= 1024.0 && i < UNITS.len() - 1 {
        bytes /= 1024.0;
        i += 1;
    }
    format!("{:.2} {}", bytes, UNITS[i])
}

fn gb_tenth_from_sysvalue(value: u64) -> String {
    // sysinfo historically returned KB, but some versions/platforms return bytes.
    // Heuristic: if the value looks like bytes (> 1e9), treat as bytes; otherwise treat as KB.
    let gb = if value > 1_000_000_000u64 {
        // value is bytes
        value as f64 / 1024.0 / 1024.0 / 1024.0
    } else {
        // value is kilobytes
        value as f64 / 1024.0 / 1024.0
    };
    format!("{:.1} GB", gb)
}

fn get_motherboard_info() -> Vec<(String, String)> {
    // Try platform-specific approaches. On Windows, use `wmic baseboard`.
    if cfg!(target_os = "windows") {
        let out = Command::new("wmic")
            .args(&["baseboard", "get", "Manufacturer,Product,SerialNumber,Version", "/format:list"])
            .output();

        if let Ok(o) = out {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut res = Vec::new();
                for line in s.lines() {
                    if let Some(idx) = line.find('=') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx + 1..].trim().to_string();
                        if !k.is_empty() {
                            res.push((k, v));
                        }
                    }
                }
                if !res.is_empty() {
                    return res;
                }
            }
        }
        // WMIC not available or returned nothing — fallback to PowerShell CIM query
        let ps = Command::new("powershell")
            .args(&["-NoProfile", "-Command", "Get-CimInstance -ClassName Win32_BaseBoard | Format-List Manufacturer,Product,SerialNumber,Version"]) 
            .output();

        // We'll collect results from multiple sources into a single vector so UI can present them.
        let mut res = Vec::new();

        if let Ok(o) = ps {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                for line in s.lines() {
                    if let Some(idx) = line.find(':') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx + 1..].trim().to_string();
                        if !k.is_empty() {
                            res.push((k, v));
                        }
                    }
                }
            }
        }

        // Add BIOS/firmware details from Win32_BIOS
        let bios_ps = Command::new("powershell")
            .args(&["-NoProfile", "-Command", "Get-CimInstance -ClassName Win32_BIOS | Format-List Manufacturer,SMBIOSBIOSVersion,Version,ReleaseDate | Out-String"]) 
            .output();

        if let Ok(o) = bios_ps {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() { continue; }
                    if let Some(idx) = line.find(':') {
                        let k = format!("BIOS {}", line[..idx].trim());
                        let v = line[idx + 1..].trim().to_string();
                        if !k.is_empty() {
                            res.push((k, v));
                        }
                    }
                }
            }
        }

        // Detect firmware type (UEFI vs BIOS) via registry value if available
        let fw_ps = Command::new("powershell")
            .args(&["-NoProfile", "-Command", "try { $v = (Get-ItemProperty -Path 'HKLM:\\\\SYSTEM\\\\CurrentControlSet\\\\Control' -ErrorAction SilentlyContinue).PEFirmwareType; if ($null -ne $v) { if ($v -eq 2) { Write-Output 'FirmwareType: UEFI' } elseif ($v -eq 1) { Write-Output 'FirmwareType: BIOS' } else { Write-Output \"FirmwareType: Unknown ($v)\" } } else { Write-Output 'FirmwareType: Unknown' } } catch { Write-Output 'FirmwareType: Unknown' }"])
            .output();

        if let Ok(o) = fw_ps {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                for line in s.lines() {
                    if let Some(idx) = line.find(':') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx + 1..].trim().to_string();
                        if !k.is_empty() {
                            res.push((k, v));
                        }
                    }
                }
            }
        }

        // (PCI device enumeration moved to get_pci_devices to be shown as separate cards)

        // If we collected anything, return it (even if partial)
        if !res.is_empty() {
            return res;
        }

        return Vec::new();
    }

    // Non-Windows: we don't have a portable implementation here. Return empty so UI can show a message.
    Vec::new()
}

fn get_os_info() -> Vec<(String, String)> {
    // Return expanded operating system information
    if cfg!(target_os = "windows") {
        let ps_cmd = "Get-CimInstance -ClassName Win32_OperatingSystem | Format-List Caption,Version,BuildNumber,OSArchitecture,InstallDate,SerialNumber | Out-String";
        let ps = Command::new("powershell")
            .args(&["-NoProfile", "-Command", ps_cmd])
            .output();

        if let Ok(o) = ps {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut res: Vec<(String, String)> = Vec::new();
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() { continue; }
                    if let Some(idx) = line.find(':') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx+1..].trim().to_string();
                        if !k.is_empty() {
                            res.push((k, v));
                        }
                    }
                }
                if !res.is_empty() {
                    return res;
                }
            }
        }
    }
    Vec::new()
}

fn get_power_info() -> Vec<Vec<(String, String)>> {
    // Gather grouped battery / power related information on Windows using structured JSON from PowerShell
    let mut devices: Vec<Vec<(String, String)>> = Vec::new();
    if cfg!(target_os = "windows") {
        // Batteries
        let ps_batt_cmd = "Get-CimInstance -ClassName Win32_Battery | Select-Object * | ConvertTo-Json -Depth 4";
        if let Ok(out) = Command::new("powershell").args(&["-NoProfile", "-Command", ps_batt_cmd]).output() {
            if out.status.success() {
                let s = String::from_utf8_lossy(&out.stdout).to_string();
                if !s.trim().is_empty() {
                    if let Ok(v) = serde_json::from_str::<serde_json::Value>(&s) {
                        let items: Vec<serde_json::Value> = if v.is_array() { v.as_array().cloned().unwrap_or_default() } else { vec![v] };
                        for (i, item) in items.into_iter().enumerate() {
                            let mut dev: Vec<(String, String)> = Vec::new();
                            let title = format!("Battery #{}", i + 1);
                            dev.push(("Device".to_string(), title.clone()));
                            if let Some(map) = item.as_object() {
                                for (k, val) in map {
                                    let vs = if val.is_string() { val.as_str().unwrap_or("").to_string() } else { val.to_string() };
                                    dev.push((k.clone(), vs));
                                }

                                // Compute approximate watts if we have FullChargeCapacity (mWh) and EstimatedRunTime (minutes)
                                let full_mwh = map.get("FullChargeCapacity").and_then(|v| v.as_f64().or_else(|| v.as_i64().map(|x| x as f64)));
                                let est_min = map.get("EstimatedRunTime").and_then(|v| v.as_f64().or_else(|| v.as_i64().map(|x| x as f64)));
                                if let (Some(full), Some(est)) = (full_mwh, est_min) {
                                    if est > 0.0 {
                                        let hours = est / 60.0;
                                        if hours > 0.0 {
                                            // full is in mWh (milliwatt-hours), so power in mW = mWh / h
                                            let power_mw = full / hours;
                                            let power_w = power_mw / 1000.0;
                                            dev.push(("ApproxPowerW".to_string(), format!("{:.2}", power_w)));
                                        }
                                    }
                                }
                            }
                            devices.push(dev);
                        }
                    }
                }
            }
        }

        // PSUs
        let ps_psu_cmd = "Get-CimInstance -ClassName Win32_PowerSupply | Select-Object * | ConvertTo-Json -Depth 4";
        if let Ok(o2) = Command::new("powershell").args(&["-NoProfile", "-Command", ps_psu_cmd]).output() {
            if o2.status.success() {
                let s2 = String::from_utf8_lossy(&o2.stdout).to_string();
                if !s2.trim().is_empty() {
                    if let Ok(v2) = serde_json::from_str::<serde_json::Value>(&s2) {
                        let items: Vec<serde_json::Value> = if v2.is_array() { v2.as_array().cloned().unwrap_or_default() } else { vec![v2] };
                        for (i, item) in items.into_iter().enumerate() {
                            let mut dev: Vec<(String, String)> = Vec::new();
                            let title = format!("PSU #{}", i + 1);
                            dev.push(("Device".to_string(), title.clone()));
                            if let Some(map) = item.as_object() {
                                for (k, val) in map {
                                    let vs = if val.is_string() { val.as_str().unwrap_or("").to_string() } else { val.to_string() };
                                    dev.push((k.clone(), vs));
                                }
                            }
                            devices.push(dev);
                        }
                    }
                }
            }
        }

        // Active power scheme via powercfg
        if let Ok(o) = Command::new("powercfg").args(&["/getactivescheme"]).output() {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut dev: Vec<(String, String)> = Vec::new();
                dev.push(("Device".to_string(), "PowerScheme".to_string()));
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() { continue; }
                    dev.push(("ActivePowerScheme".to_string(), line.to_string()));
                }
                devices.push(dev);
            }
        }
    }

    devices
}

    fn get_pci_devices() -> Vec<Vec<(String, String)>> {
        // Return a vector of PCI devices; each device is Vec<(key,value)>
        if cfg!(target_os = "windows") {
            let pci_ps = Command::new("powershell")
                .args(&["-NoProfile", "-Command", "Get-CimInstance Win32_PnPEntity | Where-Object { $_.PNPDeviceID -like 'PCI*' } | Select-Object Name,PNPDeviceID,Manufacturer | Format-List | Out-String"]) 
                .output();

            if let Ok(o) = pci_ps {
                if o.status.success() {
                    let s = String::from_utf8_lossy(&o.stdout);
                    let mut devices: Vec<Vec<(String, String)>> = Vec::new();
                    let mut current: Vec<(String, String)> = Vec::new();
                    for line in s.lines() {
                        let line = line.trim();
                        if line.is_empty() {
                            if !current.is_empty() {
                                devices.push(current);
                                current = Vec::new();
                            }
                            continue;
                        }
                        if let Some(col) = line.find(':') {
                            let k = line[..col].trim().to_string();
                            let v = line[col+1..].trim().to_string();
                            if !k.is_empty() {
                                current.push((k, v));
                            }
                        }
                    }
                    if !current.is_empty() {
                        devices.push(current);
                    }
                    if !devices.is_empty() {
                        return devices;
                    }
                }
            }
        }
        Vec::new()
    }

fn get_graphics_info() -> Vec<Vec<(String, String)>> {
    // Return a vector of controllers, each controller is Vec<(key,value)>
    if cfg!(target_os = "windows") {
        // Try WMIC first
        let out = Command::new("wmic")
            .args(&["path", "win32_VideoController", "get", "Name,AdapterRAM,DriverVersion,VideoProcessor,PNPDeviceID,Description", "/format:list"])
            .output();

        if let Ok(o) = out {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut controllers = Vec::new();
                let mut current = Vec::new();
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() {
                        if !current.is_empty() {
                            controllers.push(current);
                            current = Vec::new();
                        }
                        continue;
                    }
                    if let Some(idx) = line.find('=') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx + 1..].trim().to_string();
                        if !k.is_empty() {
                            current.push((k, v));
                        }
                    }
                }
                if !current.is_empty() {
                    controllers.push(current);
                }
                if !controllers.is_empty() {
                    return controllers;
                }
            }
        }

        // WMIC failed or returned nothing; fallback to PowerShell CIM
        let ps = Command::new("powershell")
            .args(&["-NoProfile", "-Command", "Get-CimInstance -ClassName Win32_VideoController | Format-List Name,AdapterRAM,DriverVersion,VideoProcessor,PNPDeviceID,Description"]) 
            .output();

        if let Ok(o) = ps {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut controllers = Vec::new();
                let mut current = Vec::new();
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() {
                        if !current.is_empty() {
                            controllers.push(current);
                            current = Vec::new();
                        }
                        continue;
                    }
                    if let Some(idx) = line.find(':') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx + 1..].trim().to_string();
                        if !k.is_empty() {
                            current.push((k, v));
                        }
                    }
                }
                if !current.is_empty() {
                    controllers.push(current);
                }
                if !controllers.is_empty() {
                    return controllers;
                }
            }
        }
    }

    // Non-windows: no portable implementation here
    Vec::new()
}

fn get_peripherals_info() -> Vec<Vec<(String, String)>> {
    // Return a vector of peripheral devices (USB/HID/Keyboard/Mouse)
    if cfg!(target_os = "windows") {
        let ps_cmd = "Get-CimInstance Win32_PnPEntity | Where-Object { ($_.PNPDeviceID -like 'USB*') -or ($_.PNPClass -eq 'HIDClass') -or ($_.PNPClass -eq 'Mouse') -or ($_.PNPClass -eq 'Keyboard') } | Select-Object Name,PNPDeviceID,Manufacturer,PNPClass | Format-List | Out-String";
        let pci_ps = Command::new("powershell")
            .args(&["-NoProfile", "-Command", ps_cmd])
            .output();

        if let Ok(o) = pci_ps {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut devices: Vec<Vec<(String, String)>> = Vec::new();
                let mut current: Vec<(String, String)> = Vec::new();
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() {
                        if !current.is_empty() {
                            devices.push(current);
                            current = Vec::new();
                        }
                        continue;
                    }
                    if let Some(col) = line.find(':') {
                        let k = line[..col].trim().to_string();
                        let v = line[col+1..].trim().to_string();
                        if !k.is_empty() {
                            current.push((k, v));
                        }
                    }
                }
                if !current.is_empty() {
                    devices.push(current);
                }
                if !devices.is_empty() {
                    return devices;
                }
            }
        }
    }
    Vec::new()
}

fn get_optical_drives_info() -> Vec<Vec<(String, String)>> {
    // Return optical/CD-ROM drive information (Win32_CDROMDrive)
    if cfg!(target_os = "windows") {
        // Try WMIC first
        let out = Command::new("wmic")
            .args(&["cdrom", "get", "Name,Drive,MediaType,Status,Manufacturer,Caption", "/format:list"])
            .output();

        if let Ok(o) = out {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut devices: Vec<Vec<(String, String)>> = Vec::new();
                let mut current: Vec<(String, String)> = Vec::new();
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() {
                        if !current.is_empty() {
                            devices.push(current);
                            current = Vec::new();
                        }
                        continue;
                    }
                    if let Some(idx) = line.find('=') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx+1..].trim().to_string();
                        if !k.is_empty() {
                            current.push((k, v));
                        }
                    }
                }
                if !current.is_empty() {
                    devices.push(current);
                }
                if !devices.is_empty() {
                    return devices;
                }
            }
        }

        // Fallback to PowerShell CIM query
        let ps_cmd = "Get-CimInstance -ClassName Win32_CDROMDrive | Format-List Name,Drive,MediaType,Status,Manufacturer,Caption | Out-String";
        let ps = Command::new("powershell")
            .args(&["-NoProfile", "-Command", ps_cmd])
            .output();

        if let Ok(o) = ps {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut devices: Vec<Vec<(String, String)>> = Vec::new();
                let mut current: Vec<(String, String)> = Vec::new();
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() {
                        if !current.is_empty() {
                            devices.push(current);
                            current = Vec::new();
                        }
                        continue;
                    }
                    if let Some(idx) = line.find(':') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx+1..].trim().to_string();
                        if !k.is_empty() {
                            current.push((k, v));
                        }
                    }
                }
                if !current.is_empty() {
                    devices.push(current);
                }
                if !devices.is_empty() {
                    return devices;
                }
            }
        }
    }

    Vec::new()
}

fn get_audio_info() -> Vec<Vec<(String, String)>> {
    // Return a vector of audio devices; each device is Vec<(key,value)>
    if cfg!(target_os = "windows") {
        // Try WMIC first
        let out = Command::new("wmic")
            .args(&["path", "win32_SoundDevice", "get", "Name,Manufacturer,Status,PNPDeviceID,ProductName", "/format:list"])
            .output();

        if let Ok(o) = out {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut devices = Vec::new();
                let mut current = Vec::new();
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() {
                        if !current.is_empty() {
                            devices.push(current);
                            current = Vec::new();
                        }
                        continue;
                    }
                    if let Some(idx) = line.find('=') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx + 1..].trim().to_string();
                        if !k.is_empty() {
                            current.push((k, v));
                        }
                    }
                }
                if !current.is_empty() {
                    devices.push(current);
                }
                if !devices.is_empty() {
                    return devices;
                }
            }
        }

        // WMIC failed or returned nothing; fallback to PowerShell CIM
        let ps = Command::new("powershell")
            .args(&["-NoProfile", "-Command", "Get-CimInstance -ClassName Win32_SoundDevice | Format-List Name,Manufacturer,Status,PNPDeviceID,ProductName"]) 
            .output();

        if let Ok(o) = ps {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut devices = Vec::new();
                let mut current = Vec::new();
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() {
                        if !current.is_empty() {
                            devices.push(current);
                            current = Vec::new();
                        }
                        continue;
                    }
                    if let Some(idx) = line.find(':') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx + 1..].trim().to_string();
                        if !k.is_empty() {
                            current.push((k, v));
                        }
                    }
                }
                if !current.is_empty() {
                    devices.push(current);
                }
                if !devices.is_empty() {
                    return devices;
                }
            }
        }
    }

    // Non-windows: no portable implementation here
    Vec::new()
}

fn get_display_info() -> Vec<Vec<(String, String)>> {
    // Per-monitor info. Use PowerShell's System.Windows.Forms.Screen on Windows.
    if cfg!(target_os = "windows") {
        let ps_cmd = r#"Add-Type -AssemblyName System.Windows.Forms; [System.Windows.Forms.Screen]::AllScreens | ForEach-Object { Write-Output \"DeviceName: $($_.DeviceName)\"; Write-Output \"Bounds: $($_.Bounds.Width)x$($_.Bounds.Height)\"; Write-Output \"Primary: $($_.Primary)\"; Write-Output \"BitsPerPixel: $($_.BitsPerPixel)\"; Write-Output \"WorkingArea: $($_.WorkingArea.Width)x$($_.WorkingArea.Height)\"; Write-Output \"\" }"#;

        let ps = Command::new("powershell")
            .args(&["-NoProfile", "-Command", ps_cmd])
            .output();

        if let Ok(o) = ps {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut monitors = Vec::new();
                let mut current = Vec::new();
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() {
                        if !current.is_empty() {
                            monitors.push(current);
                            current = Vec::new();
                        }
                        continue;
                    }
                    if let Some(idx) = line.find(':') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx + 1..].trim().to_string();
                        if !k.is_empty() {
                            current.push((k, v));
                        }
                    }
                }
                if !current.is_empty() {
                    monitors.push(current);
                }
                // Try to augment monitors with controller-reported refresh rates/resolutions
                // Query Win32_VideoController for current resolution/refresh
                let vc = Command::new("powershell")
                    .args(&["-NoProfile", "-Command", "Get-CimInstance -ClassName Win32_VideoController | Format-List CurrentHorizontalResolution,CurrentVerticalResolution,CurrentRefreshRate,Name"]) 
                    .output();

                if let Ok(o2) = vc {
                    if o2.status.success() {
                        let s2 = String::from_utf8_lossy(&o2.stdout);
                        // parse controllers into tuples (h,v,refresh,name)
                        let mut controllers: Vec<(u32,u32,u32,String)> = Vec::new();
                        let mut ch: u32 = 0;
                        let mut cv: u32 = 0;
                        let mut cr: u32 = 0;
                        let mut cname = String::new();
                        for line in s2.lines() {
                            let line = line.trim();
                            if line.is_empty() {
                                if ch != 0 || cv != 0 || cr != 0 || !cname.is_empty() {
                                    controllers.push((ch, cv, cr, cname.clone()));
                                }
                                ch = 0; cv = 0; cr = 0; cname.clear();
                                continue;
                            }
                            if let Some(idx) = line.find(':') {
                                let k = line[..idx].trim();
                                let v = line[idx+1..].trim();
                                match k {
                                    "CurrentHorizontalResolution" => { ch = v.parse().unwrap_or(0); }
                                    "CurrentVerticalResolution" => { cv = v.parse().unwrap_or(0); }
                                    "CurrentRefreshRate" => { cr = v.parse().unwrap_or(0); }
                                    "Name" => { cname = v.to_string(); }
                                    _ => {}
                                }
                            }
                        }
                        if ch != 0 || cv != 0 || cr != 0 || !cname.is_empty() {
                            controllers.push((ch, cv, cr, cname.clone()));
                        }

                        // For each monitor, parse its Bounds WxH and try to match a controller
                        for mon in monitors.iter_mut() {
                            let mut w: u32 = 0;
                            let mut h: u32 = 0;
                            for (k, v) in mon.iter() {
                                if k == "Bounds" {
                                    if let Some(xidx) = v.find('x') {
                                        let ws = v[..xidx].trim();
                                        let hs = v[xidx+1..].trim();
                                        w = ws.parse().unwrap_or(0);
                                        h = hs.parse().unwrap_or(0);
                                    }
                                }
                            }
                            if w != 0 && h != 0 {
                                // find controller with matching res
                                if let Some((_,_,refresh,name)) = controllers.iter().find(|(ch,cv,_,_)| *ch == w && *cv == h) {
                                    if *refresh != 0 {
                                        mon.push(("RefreshRate".to_string(), format!("{} Hz", refresh)));
                                    }
                                    if !name.is_empty() {
                                        mon.push(("Controller".to_string(), name.clone()));
                                    }
                                }
                            }
                        }
                    }
                }

                if !monitors.is_empty() {
                    return monitors;
                }
            }
        }
    }

    Vec::new()
}

fn get_controller_current_info() -> Vec<Vec<(String, String)>> {
    // Return per-controller current resolution/refresh info
    if cfg!(target_os = "windows") {
        let ps = Command::new("powershell")
            .args(&["-NoProfile", "-Command", "Get-CimInstance -ClassName Win32_VideoController | Format-List CurrentHorizontalResolution,CurrentVerticalResolution,CurrentRefreshRate,Name"]) 
            .output();

        if let Ok(o) = ps {
            if o.status.success() {
                let s = String::from_utf8_lossy(&o.stdout);
                let mut controllers = Vec::new();
                let mut current = Vec::new();
                for line in s.lines() {
                    let line = line.trim();
                    if line.is_empty() {
                        if !current.is_empty() {
                            controllers.push(current);
                            current = Vec::new();
                        }
                        continue;
                    }
                    if let Some(idx) = line.find(':') {
                        let k = line[..idx].trim().to_string();
                        let v = line[idx + 1..].trim().to_string();
                        if !k.is_empty() {
                            current.push((k, v));
                        }
                    }
                }
                if !current.is_empty() {
                    controllers.push(current);
                }
                if !controllers.is_empty() {
                    return controllers;
                }
            }
        }
    }
    Vec::new()
}

impl eframe::App for SpectasticApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // periodic refresh
        if self.last_refresh.elapsed() >= self.refresh_interval {
            // capture previous network totals so we can compute throughput
            self.prev_network.clear();
            for (name, data) in self.sys.networks() {
                self.prev_network.insert(name.clone(), (data.total_received(), data.total_transmitted()));
            }

            self.sys.refresh_all();
            self.last_refresh = Instant::now();
        }

        // Poll background fetch receivers and install caches when ready
        if let Some(rx) = &self.motherboard_rx {
            match rx.try_recv() {
                Ok(data) => {
                    self.motherboard_cache = Some(data);
                    self.motherboard_rx = None;
                    ctx.request_repaint();
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(_) => { self.motherboard_rx = None; }
            }
        }

        if let Some(rx) = &self.graphics_rx {
            match rx.try_recv() {
                Ok(data) => {
                    self.graphics_cache = Some(data);
                    self.graphics_rx = None;
                    ctx.request_repaint();
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(_) => { self.graphics_rx = None; }
            }
        }

        if let Some(rx) = &self.audio_rx {
            match rx.try_recv() {
                Ok(data) => {
                    self.audio_cache = Some(data);
                    self.audio_rx = None;
                    ctx.request_repaint();
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(_) => { self.audio_rx = None; }
            }
        }

        if let Some(rx) = &self.display_rx {
            match rx.try_recv() {
                Ok(data) => {
                    self.display_cache = Some(data);
                    self.display_rx = None;
                    ctx.request_repaint();
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(_) => { self.display_rx = None; }
            }
        }

        if let Some(rx) = &self.controller_rx {
            match rx.try_recv() {
                Ok(data) => {
                    self.controller_cache = Some(data);
                    self.controller_rx = None;
                    ctx.request_repaint();
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(_) => { self.controller_rx = None; }
            }
        }

        if let Some(rx) = &self.peripherals_rx {
            match rx.try_recv() {
                Ok(data) => {
                    self.peripherals_cache = Some(data);
                    self.peripherals_rx = None;
                    ctx.request_repaint();
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(_) => { self.peripherals_rx = None; }
            }
        }

        if let Some(rx) = &self.optical_rx {
            match rx.try_recv() {
                Ok(data) => {
                    self.optical_cache = Some(data);
                    self.optical_rx = None;
                    ctx.request_repaint();
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(_) => { self.optical_rx = None; }
            }
        }

        if let Some(rx) = &self.os_rx {
            match rx.try_recv() {
                Ok(data) => {
                    self.os_cache = Some(data);
                    self.os_rx = None;
                    ctx.request_repaint();
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(_) => { self.os_rx = None; }
            }
        }

        if let Some(rx) = &self.power_rx {
            match rx.try_recv() {
                Ok(data) => {
                    self.power_cache = Some(data);
                    self.power_rx = None;
                    ctx.request_repaint();
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(_) => { self.power_rx = None; }
            }
        }

        if let Some(rx) = &self.pci_rx {
            match rx.try_recv() {
                Ok(data) => {
                    self.pci_cache = Some(data);
                    self.pci_rx = None;
                    ctx.request_repaint();
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(_) => { self.pci_rx = None; }
            }
        }

        // If selection changed, kick off a background fetch (if needed)
        if self.selected != self.prev_selected {
            self.prev_selected = self.selected;
            match self.selected {
                Tab::Motherboard => {
                    if self.motherboard_cache.is_none() && self.motherboard_rx.is_none() {
                        let (tx, rx) = std::sync::mpsc::channel();
                        self.motherboard_rx = Some(rx);
                        std::thread::spawn(move || {
                            let data = get_motherboard_info();
                            let _ = tx.send(data);
                        });
                    }
                    // Also start PCI device fetch in parallel (structured per-device data)
                    if self.pci_cache.is_none() && self.pci_rx.is_none() {
                        let (txp, rxp) = std::sync::mpsc::channel();
                        self.pci_rx = Some(rxp);
                        std::thread::spawn(move || {
                            let data = get_pci_devices();
                            let _ = txp.send(data);
                        });
                    }
                }
                Tab::Graphics => {
                    if self.graphics_cache.is_none() && self.graphics_rx.is_none() {
                        let (tx, rx) = std::sync::mpsc::channel();
                        self.graphics_rx = Some(rx);
                        std::thread::spawn(move || {
                            let data = get_graphics_info();
                            let _ = tx.send(data);
                        });
                    }
                }
                Tab::Audio => {
                    if self.audio_cache.is_none() && self.audio_rx.is_none() {
                        let (tx, rx) = std::sync::mpsc::channel();
                        self.audio_rx = Some(rx);
                        std::thread::spawn(move || {
                            let data = get_audio_info();
                            let _ = tx.send(data);
                        });
                    }
                }
                Tab::Display => {
                    if self.display_cache.is_none() && self.display_rx.is_none() {
                        let (tx, rx) = std::sync::mpsc::channel();
                        self.display_rx = Some(rx);
                        std::thread::spawn(move || {
                            let data = get_display_info();
                            let _ = tx.send(data);
                        });
                    }
                    
                    if self.controller_cache.is_none() && self.controller_rx.is_none() {
                        let (tx2, rx2) = std::sync::mpsc::channel();
                        self.controller_rx = Some(rx2);
                        std::thread::spawn(move || {
                            let data = get_controller_current_info();
                            let _ = tx2.send(data);
                        });
                    }
                }
                Tab::OperatingSystem => {
                    if self.os_cache.is_none() && self.os_rx.is_none() {
                        let (txo, rxo) = std::sync::mpsc::channel();
                        self.os_rx = Some(rxo);
                        std::thread::spawn(move || {
                            let data = get_os_info();
                            let _ = txo.send(data);
                        });
                    }
                }
                
                Tab::Peripherals => {
                    if self.peripherals_cache.is_none() && self.peripherals_rx.is_none() {
                        let (txp, rxp) = std::sync::mpsc::channel();
                        self.peripherals_rx = Some(rxp);
                        std::thread::spawn(move || {
                            let data = get_peripherals_info();
                            let _ = txp.send(data);
                        });
                    }
                }
                Tab::Power => {
                    if self.power_cache.is_none() && self.power_rx.is_none() {
                        let (txp, rxp) = std::sync::mpsc::channel();
                        self.power_rx = Some(rxp);
                        std::thread::spawn(move || {
                            let data = get_power_info();
                            let _ = txp.send(data);
                        });
                    }
                }
                _ => {}
            }
        }

        egui::CentralPanel::default().show(ctx, |ui| {
            // Use a single vertical layout inside the central panel to avoid deep nested closures
            egui::ScrollArea::vertical().show(ui, |ui| {
                ui.vertical(|ui| {
                ui.heading("Spectastic — Basic System Inspector");
                ui.separator();

                ui.horizontal(|ui| {
                    if ui.selectable_label(self.selected == Tab::Overview, "Overview").clicked() {
                        self.selected = Tab::Overview;
                    }
                    if ui.selectable_label(self.selected == Tab::OperatingSystem, "Operating System").clicked() {
                        self.selected = Tab::OperatingSystem;
                    }
                    if ui.selectable_label(self.selected == Tab::CPU, "CPU").clicked() {
                        self.selected = Tab::CPU;
                    }
                    if ui.selectable_label(self.selected == Tab::Memory, "Memory").clicked() {
                        self.selected = Tab::Memory;
                    }
                    if ui.selectable_label(self.selected == Tab::Disks, "Disks").clicked() {
                        self.selected = Tab::Disks;
                    }
                    if ui.selectable_label(self.selected == Tab::Fans, "Fans").clicked() {
                        self.selected = Tab::Fans;
                    }
                    if ui.selectable_label(self.selected == Tab::Motherboard, "Motherboard").clicked() {
                        self.selected = Tab::Motherboard;
                    }
                    if ui.selectable_label(self.selected == Tab::Graphics, "Graphics").clicked() {
                        self.selected = Tab::Graphics;
                    }
                    if ui.selectable_label(self.selected == Tab::Audio, "Audio").clicked() {
                        self.selected = Tab::Audio;
                    }
                    if ui.selectable_label(self.selected == Tab::Display, "Display").clicked() {
                        self.selected = Tab::Display;
                    }
                    
                    if ui.selectable_label(self.selected == Tab::Peripherals, "Peripherals").clicked() {
                        self.selected = Tab::Peripherals;
                    }
                    if ui.selectable_label(self.selected == Tab::Power, "Power").clicked() {
                        self.selected = Tab::Power;
                    }
                    if ui.selectable_label(self.selected == Tab::Network, "Network").clicked() {
                        self.selected = Tab::Network;
                    }
                });

                ui.separator();

                // Render the selected tab
                match self.selected {
                    Tab::Overview => {
                        ui.label(format!("OS: {}", self.sys.name().unwrap_or_default()));
                        ui.label(format!("OS Version: {}", self.sys.long_os_version().unwrap_or_default()));
                        ui.label(format!("Hostname: {}", self.sys.host_name().unwrap_or_default()));
                        // show cached detailed OS info if available
                        if let Some(os) = &self.os_cache {
                            ui.separator();
                            ui.label("Detailed OS info:");
                            for (k, v) in os {
                                ui.horizontal(|ui| {
                                    ui.label(format!("{}:", k));
                                    ui.label(v);
                                });
                            }
                        } else if self.os_rx.is_some() {
                            ui.separator();
                            ui.label("(OS details loading in background)");
                        }
                        ui.label(format!("Uptime: {} s", self.sys.uptime()));
                        ui.label(format!("Total CPUs: {}", num_cpus::get()));

                        if !self.sys.components().is_empty() {
                            ui.separator();
                            ui.label("Temperatures:");
                            for c in self.sys.components() {
                                ui.label(format!("{}: {:.1} °C", c.label(), c.temperature()));
                            }
                        }
                    }

                    Tab::OperatingSystem => {
                        ui.heading("Operating System");
                        ui.separator();
                        if let Some(os) = &self.os_cache {
                            for (k, v) in os {
                                ui.horizontal(|ui| {
                                    ui.label(format!("{}:", k));
                                    ui.label(v);
                                });
                            }
                        } else if self.os_rx.is_some() {
                            ui.label("Loading operating system info...");
                        } else {
                            ui.label("No OS info cached. Select tab to load.");
                        }
                    }

                    Tab::CPU => {
                        ui.label(format!("Global CPU usage: {:.1}%", self.sys.global_cpu_info().cpu_usage()));
                        ui.separator();
                        ui.label("Per-core details not available in this build; showing global CPU usage.");

                        let cpu_sensors: Vec<_> = self
                            .sys
                            .components()
                            .iter()
                            .filter(|c| {
                                let l = c.label().to_lowercase();
                                l.contains("cpu") || l.contains("package") || l.contains("core")
                            })
                            .collect();

                        if !cpu_sensors.is_empty() {
                            ui.separator();
                            ui.label("CPU Temperatures:");
                            for c in cpu_sensors {
                                ui.horizontal(|ui| {
                                    ui.label(format!("{}:", c.label()));
                                    ui.label(format!("{:.1} °C", c.temperature()));
                                });
                            }
                        } else {
                            ui.separator();
                            ui.label("No CPU temperature sensors available.");
                        }
                    }

                    Tab::Memory => {
                        let total = self.sys.total_memory();
                        let used = self.sys.used_memory();
                        ui.label(format!("Memory: {} total", gb_tenth_from_sysvalue(total)));
                        ui.label(format!("Memory: {} used", gb_tenth_from_sysvalue(used)));
                        ui.separator();
                        let swap_total = self.sys.total_swap();
                        let swap_used = self.sys.used_swap();
                        ui.label(format!("Swap: {} total", gb_tenth_from_sysvalue(swap_total)));
                        ui.label(format!("Swap: {} used", gb_tenth_from_sysvalue(swap_used)));
                    }

                    Tab::Disks => {
                        for d in self.sys.disks() {
                            ui.group(|ui| {
                                ui.label(format!("Name: {:?}", d.name()));
                                ui.label(format!("Type: {:?}", d.type_()));
                                ui.label(format!("File system: {:?}", d.file_system()));
                                ui.label(format!("Total space: {} bytes", d.total_space()));
                                ui.label(format!("Available: {} bytes", d.available_space()));
                            });
                            ui.separator();
                        }
                        // Kick off optical drive fetch if selecting Disks and not already fetched
                        if self.optical_cache.is_none() && self.optical_rx.is_none() {
                            let (txo, rxo) = std::sync::mpsc::channel();
                            self.optical_rx = Some(rxo);
                            std::thread::spawn(move || {
                                let data = get_optical_drives_info();
                                let _ = txo.send(data);
                            });
                        }

                        ui.separator();
                        ui.label("Optical Drives:");
                        ui.separator();
                        if let Some(optical) = &self.optical_cache {
                            if optical.is_empty() {
                                ui.label("No optical drives detected.");
                            } else {
                                for (i, dev) in optical.iter().enumerate() {
                                    ui.group(|ui| {
                                        ui.label(format!("Optical Drive #{}", i + 1));
                                        ui.separator();
                                        for (k, v) in dev {
                                            ui.horizontal(|ui| {
                                                ui.label(format!("{}:", k));
                                                ui.label(v);
                                            });
                                        }
                                    });
                                    ui.separator();
                                }
                            }
                        } else if self.optical_rx.is_some() {
                            ui.label("Loading optical drive info...");
                        } else {
                            ui.label("No optical drive info cached. Select tab to load.");
                        }
                    }

                    

                    Tab::Fans => {
                        let fan_sensors: Vec<_> = self
                            .sys
                            .components()
                            .iter()
                            .filter(|c| c.label().to_lowercase().contains("fan"))
                            .collect();

                        if fan_sensors.is_empty() {
                            ui.label("No fan sensors available on this system.");
                        } else {
                            ui.label("Fan sensors:");
                            for f in fan_sensors {
                                ui.horizontal(|ui| {
                                    ui.label(format!("{}:", f.label()));
                                    ui.label(format!("{:.1} °C", f.temperature()));
                                });
                            }
                        }
                    }

                    Tab::Motherboard => {
                        ui.heading("Motherboard");
                        ui.separator();
                        if let Some(info) = &self.motherboard_cache {
                            for (k, v) in info {
                                ui.horizontal(|ui| {
                                    ui.label(format!("{}:", k));
                                    ui.label(v);
                                });
                            }
                        } else if self.motherboard_rx.is_some() {
                            ui.label("Loading motherboard info...");
                        } else {
                            // Not fetched yet (or nothing available)
                            if cfg!(target_os = "windows") {
                                ui.label("No motherboard info cached. Select tab to load.");
                            } else {
                                ui.label("Motherboard info not available on this platform.");
                            }
                        }

                        ui.separator();
                        ui.label("PCIe devices:");
                        ui.separator();
                        if let Some(pci_devices) = &self.pci_cache {
                            if pci_devices.is_empty() {
                                ui.label("No PCIe devices found.");
                            } else {
                                for (i, dev) in pci_devices.iter().enumerate() {
                                    ui.group(|ui| {
                                        ui.label(format!("PCI Device #{}", i + 1));
                                        ui.separator();
                                        for (k, v) in dev {
                                            ui.horizontal(|ui| {
                                                ui.label(format!("{}:", k));
                                                ui.label(v);
                                            });
                                        }
                                    });
                                    ui.separator();
                                }
                            }
                        } else if self.pci_rx.is_some() {
                            ui.label("Loading PCIe devices...");
                        } else {
                            ui.label("No PCIe device info cached. Select tab to load.");
                        }

                        ui.separator();
                        ui.label("Peripherals:");
                        ui.separator();
                        if let Some(peripherals) = &self.peripherals_cache {
                            if peripherals.is_empty() {
                                ui.label("No peripherals found.");
                            } else {
                                for (i, dev) in peripherals.iter().enumerate() {
                                    ui.group(|ui| {
                                        ui.label(format!("Peripheral #{}", i + 1));
                                        ui.separator();
                                        for (k, v) in dev {
                                            ui.horizontal(|ui| {
                                                ui.label(format!("{}:", k));
                                                ui.label(v);
                                            });
                                        }
                                    });
                                    ui.separator();
                                }
                            }
                        } else if self.peripherals_rx.is_some() {
                            ui.label("Loading peripherals...");
                        } else {
                            ui.label("No peripherals cached. Select tab to load.");
                        }
                    }

                    Tab::Peripherals => {
                        ui.heading("Peripherals");
                        ui.separator();
                        if let Some(peripherals) = &self.peripherals_cache {
                            if peripherals.is_empty() {
                                ui.label("No peripherals found.");
                            } else {
                                for (i, dev) in peripherals.iter().enumerate() {
                                    ui.group(|ui| {
                                        ui.label(format!("Peripheral #{}", i + 1));
                                        ui.separator();
                                        for (k, v) in dev {
                                            ui.horizontal(|ui| {
                                                ui.label(format!("{}:", k));
                                                ui.label(v);
                                            });
                                        }
                                    });
                                    ui.separator();
                                }
                            }
                        } else if self.peripherals_rx.is_some() {
                            ui.label("Loading peripherals...");
                        } else {
                            ui.label("No peripherals cached. Select tab to load.");
                        }
                    }

                    Tab::Graphics => {
                        ui.heading("Graphics / GPU");
                        ui.separator();
                        if let Some(info) = &self.graphics_cache {
                            if info.is_empty() {
                                ui.label("Graphics info not available on this platform or no data exposed.");
                            } else {
                                for (i, controller) in info.iter().enumerate() {
                                    ui.group(|ui| {
                                        ui.label(format!("Controller #{}", i + 1));
                                        ui.separator();
                                        for (k, v) in controller {
                                            ui.horizontal(|ui| {
                                                ui.label(format!("{}:", k));
                                                ui.label(v);
                                            });
                                        }
                                    });
                                    ui.separator();
                                }
                            }
                        } else if self.graphics_rx.is_some() {
                            ui.label("Loading graphics info...");
                        } else {
                            ui.label("No graphics info cached. Select tab to load.");
                        }
                    }

                    Tab::Audio => {
                        ui.heading("Audio Devices");
                        ui.separator();
                        if let Some(devices) = &self.audio_cache {
                            if devices.is_empty() {
                                ui.label("Audio device info not available on this platform or no data exposed.");
                            } else {
                                for (i, dev) in devices.iter().enumerate() {
                                    ui.group(|ui| {
                                        ui.label(format!("Device #{}", i + 1));
                                        ui.separator();
                                        for (k, v) in dev {
                                            ui.horizontal(|ui| {
                                                ui.label(format!("{}:", k));
                                                ui.label(v);
                                            });
                                        }
                                    });
                                    ui.separator();
                                }
                            }
                        } else if self.audio_rx.is_some() {
                            ui.label("Loading audio device info...");
                        } else {
                            ui.label("No audio info cached. Select tab to load.");
                        }
                    }

                    Tab::Display => {
                        ui.heading("Displays");
                        ui.separator();
                        if let Some(mons) = &self.display_cache {
                            if mons.is_empty() {
                                ui.label("Display info not available on this platform.");
                            } else {
                                for (i, m) in mons.iter().enumerate() {
                                    ui.group(|ui| {
                                        ui.label(format!("Monitor #{}", i + 1));
                                        ui.separator();
                                        for (k, v) in m {
                                            ui.horizontal(|ui| {
                                                ui.label(format!("{}:", k));
                                                ui.label(v);
                                            });
                                        }
                                    });
                                    ui.separator();
                                }
                            }
                        } else if self.display_rx.is_some() {
                            ui.label("Loading display info...");
                        } else {
                            ui.label("No display info cached. Select tab to load.");
                        }

                        ui.separator();
                        ui.label("Controller reported current modes:");
                        ui.separator();
                        if let Some(ctrl) = &self.controller_cache {
                            if ctrl.is_empty() {
                                ui.label("No controller current-mode info available.");
                            } else {
                                for (i, c) in ctrl.iter().enumerate() {
                                    ui.group(|ui| {
                                        ui.label(format!("Controller #{}", i + 1));
                                        ui.separator();
                                        for (k, v) in c {
                                            ui.horizontal(|ui| {
                                                ui.label(format!("{}:", k));
                                                ui.label(v);
                                            });
                                        }
                                    });
                                    ui.separator();
                                }
                            }
                        } else if self.controller_rx.is_some() {
                            ui.label("Loading controller info...");
                        } else {
                            ui.label("No controller info cached. Select tab to load.");
                        }
                    }

                    Tab::Power => {
                        ui.heading("Power");
                        ui.separator();
                        if let Some(power) = &self.power_cache {
                            if power.is_empty() {
                                ui.label("No power/battery information available on this system.");
                            } else {
                                for (i, dev) in power.iter().enumerate() {
                                    ui.group(|ui| {
                                        // header: find Device or default
                                        let header = dev.iter().find(|(k,_)| k == "Device").map(|(_,v)| v.clone()).unwrap_or(format!("Power Device #{}", i+1));
                                        ui.label(header);
                                        ui.separator();
                                        for (k, v) in dev {
                                            if k == "Device" { continue; }
                                            ui.horizontal(|ui| {
                                                ui.label(format!("{}:", k));
                                                ui.label(v);
                                            });
                                        }
                                    });
                                    ui.separator();
                                }
                            }
                        } else if self.power_rx.is_some() {
                            ui.label("Loading power info...");
                        } else {
                            ui.label("No power info cached. Select tab to load.");
                        }
                    }

                    Tab::Network => {
                        let mut iface_count = 0usize;
                        for _ in self.sys.networks() {
                            iface_count += 1;
                        }
                        ui.label(format!("Interfaces: {}", iface_count));
                        ui.separator();
                        for (name, data) in self.sys.networks() {
                            let cur_rx = data.total_received();
                            let cur_tx = data.total_transmitted();

                            let (prev_rx, prev_tx) = self.prev_network.get(name)
                                .copied()
                                .unwrap_or((cur_rx, cur_tx));

                            let interval_s = self.refresh_interval.as_secs_f64();
                            let rx_per_s = if cur_rx >= prev_rx {
                                (cur_rx - prev_rx) as f64 / interval_s
                            } else {
                                0.0
                            };
                            let tx_per_s = if cur_tx >= prev_tx {
                                (cur_tx - prev_tx) as f64 / interval_s
                            } else {
                                0.0
                            };

                            ui.group(|ui| {
                                ui.label(format!("Interface: {}", name));
                                ui.label(format!("Received: {} ({}/s)", human_bytes(cur_rx as f64), human_bytes(rx_per_s)));
                                ui.label(format!("Transmitted: {} ({}/s)", human_bytes(cur_tx as f64), human_bytes(tx_per_s)));
                            });
                            ui.separator();
                        }
                    }
                }
                }); // end ui.vertical
            }); // end ScrollArea
        }); // end CentralPanel

        // keep updating UI regularly
        ctx.request_repaint_after(Duration::from_millis(200));
    }
}

fn main() {
    let options = eframe::NativeOptions::default();
    if let Err(err) = eframe::run_native(
        "Spectastic",
        options,
        Box::new(|_cc| Box::new(SpectasticApp::default())),
    ) {
        eprintln!("eframe::run_native error: {:?}", err);
    }
}
