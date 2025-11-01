# Spectastic — Basic System Inspector

This is a minimal Rust GUI app that displays basic system statistics (overview, CPU, memory, disks, network) using:

- eframe (egui) for the UI
- sysinfo for system information

Run on Windows (PowerShell):

```powershell
cargo build --release
.
# or run directly
cargo run --release
```

The app refreshes system stats every ~2 seconds.
