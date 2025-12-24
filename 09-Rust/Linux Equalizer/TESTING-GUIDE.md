# Testing Guide for Refactored Linux Equalizer

## ✅ Current Status

The equalizer has been completely refactored with a new architecture. You should now test:

---

## 🧪 Test 1: Basic Audio Playback

**Goal**: Verify audio plays through the equalizer without issues

1. **Check the equalizer is running:**
```bash
ps aux | grep linux-equalizer
```
You should see the main process, parec, and pacat running.

2. **Play audio from ANY source:**
   - Open Spotify and play a song
   - Open YouTube in a browser
   - Use `speaker-test -t sine -f 440 -c 2 -l 1`

3. **Expected Result:**
   - ✅ Audio plays clearly through your speakers/headphones
   - ✅ NO screeching or feedback
   - ✅ NO robotic/echoey sound
   - ✅ Low latency (< 50ms perceptible delay)

---

## 🧪 Test 2: Device Switching

**Goal**: Verify behavior when switching between audio devices

1. **Start playing audio** (Spotify, YouTube, etc.)

2. **Open system sound settings and switch to a different device**
   - Example: Switch from "Analog Stereo" to "Headphones"

3. **Check the terminal output:**
   - You should see: `⚠️ Device change detected: [device name]`
   - Message: `(Manual restart required to use new device)`

4. **Expected Result:**
   - ✅ Warning appears in terminal
   - ⚠️ Audio may stop playing (known limitation)
   - ✅ No crashes or errors

5. **To use the new device:**
```bash
killall linux-equalizer
./target/release/linux-equalizer --headless
```

---

## 🧪 Test 3: GUI Control (Real-time EQ)

**Goal**: Verify GUI controls affect audio in real-time

1. **Start the GUI:**
```bash
./target/release/linux-equalizer
```

2. **Play audio** (music with good bass and treble)

3. **Adjust EQ sliders:**
   - Move "32 Hz" slider up → More bass
   - Move "16k Hz" slider up → More treble
   - Move "1k Hz" slider down → Reduce midrange

4. **Expected Result:**
   - ✅ Changes are immediate (no restart needed)
   - ✅ Audio quality changes match slider adjustments
   - ✅ No crackling or distortion (unless gain is excessive)

---

## 🧪 Test 4: Preset Save/Load

**Goal**: Verify presets persist across restarts

1. **In the GUI, adjust EQ to your preference**

2. **Type a preset name** (e.g., "Bass Boost")

3. **Click "Save Preset"**

4. **Stop the equalizer:**
```bash
killall linux-equalizer
```

5. **Restart the GUI:**
```bash
./target/release/linux-equalizer
```

6. **Select your preset from the dropdown**

7. **Click "Load"**

8. **Expected Result:**
   - ✅ All slider positions match what you saved
   - ✅ Audio sounds the same as before restart

---

## 🧪 Test 5: Multi-Application Testing

**Goal**: Verify ALL applications route through equalizer

1. **Start the equalizer in headless mode**

2. **Play audio from multiple sources simultaneously:**
   - Spotify
   - YouTube in browser
   - System notification sounds
   - `speaker-test`

3. **Expected Result:**
   - ✅ ALL audio sources play through equalizer
   - ✅ Audio from different apps mixes correctly
   - ✅ No audio bypasses the EQ

---

## 🧪 Test 6: Cleanup and Restore

**Goal**: Verify clean shutdown and audio restoration

1. **While audio is playing, stop the equalizer:**
```bash
killall linux-equalizer
```

2. **Check audio devices:**
```bash
pactl list short sinks
pactl info | grep "Default Sink"
```

3. **Expected Result:**
   - ✅ `linux_eq_output` is removed
   - ✅ Default sink is restored to hardware device
   - ✅ Audio continues playing from hardware device
   - ✅ No lingering parec/pacat processes

4. **Verify no processes remain:**
```bash
ps aux | grep -E "parec|pacat|linux-equalizer"
```
Should return nothing (or just the grep command itself).

---

## 🐛 Known Issues to Test

### Issue 1: Device Switching Requires Restart
- **Test**: Switch audio device while equalizer is running
- **Current Behavior**: Audio stops, needs manual restart
- **Future**: Will auto-adapt to new device

### Issue 2: Single Output Device
- **Test**: Try using multiple output devices simultaneously
- **Current Behavior**: Only outputs to one device
- **Future**: May support multiple simultaneous outputs

---

## 📊 Performance Testing

### CPU Usage
```bash
top -p $(pgrep linux-equalizer)
```
**Expected**: < 2% CPU on modern processors

### Memory Usage
```bash
ps aux | grep linux-equalizer | awk '{print $6 " KB"}'
```
**Expected**: < 30 MB

### Latency Check
Play audio and clap near microphone while recording through EQ:
```bash
# Record with EQ active
parecord --device=linux_eq_output.monitor test-latency.wav
# Stop after 5 seconds

# Check waveform for delay between clap and recorded sound
```
**Expected**: < 30ms total latency

---

## 🔍 Debugging Failed Tests

### No Audio Plays
```bash
# Check if virtual device exists
pactl list short sinks | grep linux_eq

# Check if processes are running
ps aux | grep -E "parec|pacat"

# Check for errors
journalctl --user -xeu linux-equalizer
```

### Screeching/Feedback
```bash
# Check what pacat is outputting to
ps aux | grep pacat

# Should NOT be linux_eq_output (that would cause feedback)
# Should be a hardware device like alsa_output...
```

### Robotic/Echoey Sound
```bash
# Check buffer settings
ps aux | grep -E "parec|pacat"

# Should see --latency-msec=20 --process-time-msec=10
# If different, audio.rs may need adjustment
```

### Device Not Switching
```bash
# Check current default
pactl info | grep "Default Sink"

# If not linux_eq_output when equalizer is running, something's wrong
# Restart equalizer and check again
```

---

## ✅ Final Validation Checklist

After all tests, verify:

- [ ] Audio plays correctly through the equalizer
- [ ] All applications route through the EQ automatically
- [ ] No feedback, screeching, or distortion
- [ ] GUI controls work in real-time
- [ ] Presets save and load correctly
- [ ] Device switching logs warnings (manual restart needed)
- [ ] Clean shutdown restores normal audio
- [ ] No lingering processes after shutdown
- [ ] CPU usage < 2%
- [ ] Memory usage < 30 MB
- [ ] Latency < 30ms

---

## 📝 Report Template

If you encounter issues, please provide:

1. **What you tested**: [Test name from above]
2. **What happened**: [Actual behavior]
3. **What you expected**: [Expected behavior]
4. **System info**:
```bash
# Run these commands and include output:
pactl list short sinks
pactl info | grep "Default Sink"
ps aux | grep -E "linux-equalizer|parec|pacat"
```
5. **Error messages**: [Any terminal errors]

---

**Ready to test!** Start with Test 1 (Basic Audio Playback) and work through the list.
