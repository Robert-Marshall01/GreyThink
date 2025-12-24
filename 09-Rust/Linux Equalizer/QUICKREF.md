# Quick Reference

## ✅ Fixed Issues

1. **No more robotic/echoy sound on other devices** - The equalizer only affects the "Linux Equalizer" device you select
2. **No more automatic device switching** - YOU choose when to use it
3. **No more feedback loops** - Proper hardware sink detection
4. **No more log spam** - Settings reload every 5 seconds instead of twice per second

## 🎵 Usage

### First Time Setup
```bash
./first-time-setup.sh
```

### Manually Select Device
**System Sound Settings → Output → "Linux Equalizer"**

### Adjust EQ (GUI)
```bash
./target/release/linux-equalizer
```

### Service Control
```bash
# Check status
systemctl --user status linux-equalizer

# Stop service
systemctl --user stop linux-equalizer

# Start service
systemctl --user start linux-equalizer

# Disable autostart
systemctl --user disable linux-equalizer
```

### Switch Back to Normal Audio
**System Sound Settings → Output → Your Hardware Device**

Or run:
```bash
./restore-audio.sh
```

## 🔍 How It Works Now

1. Service creates virtual device "Linux Equalizer" (doesn't auto-switch)
2. YOU manually select it in sound settings when you want EQ
3. Only audio going to that device gets equalized
4. Your other devices (headphones, speakers) work normally
5. GUI connects to running service to change settings

## ⚠️ Important

- The service must be running for the EQ to work
- Selecting "Linux Equalizer" when service is stopped = no audio
- If you hear issues, just switch to your hardware device in sound settings
