# GreyAV: Extreme Brute Force Resilience

## Test 2
Defender (1 CPU, 2 GB RAM)
<img width="1279" height="1448" alt="Screenshot from 2026-01-01 23-55-39" src="https://github.com/user-attachments/assets/1138839a-fc6e-4482-a8a1-27dae4c5441c" />

Attacker (16 CPU, 16 GB RAM)
<img width="1097" height="1276" alt="image" src="https://github.com/user-attachments/assets/3411a781-cb9a-442b-a65b-9f78b458d507" />

## Test 1
Defender (2 CPU, 4 GB RAM)
<img width="1279" height="1448" alt="Screenshot from 2026-01-01 23-14-53" src="https://github.com/user-attachments/assets/cad8d01d-6b07-495e-88c9-43a0ad2479d0" />

Attacker (16 CPU, 8 GB RAM)
<img width="1097" height="1276" alt="image" src="https://github.com/user-attachments/assets/a9d14909-236c-41f8-ac17-09dbfafe7b62" />

GreyAV isn't just another antivirus. It's a breakthrough in **asymmetric defense** — a system that flips the economics of brute force attacks and forces attackers into exponential overcommitment.

## 🚀 Key Breakthroughs
- **30:1 Efficiency Ratio**  
  In real stress tests, a 0.6‑CPU defender calmly tanked a 16‑CPU attacker. That's ~30× more compute required by the attacker just to keep pace.
- **1000+ CPU Attack Resistance**  
  A 32‑CPU defender can theoretically withstand floods from over **1000 attacker CPUs**, sustaining resilience without fail‑open or crash.
- **Efficiency Under Constraint**  
  Unlike traditional AVs that degrade under load, GreyAV actually gets **more efficient when constrained** — pushing ratios up to **100:1**.
- **Fail‑Safe Blocking**  
  No crash, no fail‑open. Attackers burn resources, defenders stay lean.

## 🔍 How GreyAV Compares
- **GreyAV**: Asymmetric resilience, exponential efficiency, extreme brute force resistance.  
- **Major Vendors (Norton, McAfee)**: Broad detection features, but struggle to maintain even 1:1 parity under brute force floods.  

GreyAV proves that lean architecture can outperform billion‑dollar AV suites in the one metric that matters most under attack saturation: **resilience**.

## 📈 Why It Matters
- **For engineers**: A new model of defense that scales in the defender's favor.  
- **For enterprises**: Reduced compute burn, improved uptime, cheaper economics under attack.  
- **For the industry**: A paradigm shift in how brute force floods are resisted.

## ⚡ Call to Action
GreyAV is more than a project — it's a **new paradigm in cybersecurity resilience**. Explore, test, and contribute to a system that forces attackers to spend **100× more compute than defenders**.

---

## 🛠️ Advanced Antivirus CLI

A comprehensive antivirus command-line tool written in Python with enterprise-grade features.

## To Start
```bash
python3 greyav.py | less
```

## Features

### Core Scanning
- **File Scanning**: Scan individual files or entire directories
- **Multi-Algorithm Signature Detection**: Uses SHA256, SHA512, MD5, SHA1, and fuzzy hashing against known malware signatures
- **Fuzzy Hash Matching**: Detect malware variants using ssdeep-style similarity matching
- **Advanced Heuristic Analysis**: Multi-layer behavior-based detection including:
  - Pattern matching with MITRE ATT&CK mapping
  - Entropy analysis with section distribution checking
  - PE/ELF header analysis for executables
  - Packed/compressed executable detection
  - Extension mismatch detection
  - Suspicious string extraction
- **Parallel Scanning**: Multi-threaded scanning for faster performance
- **Archive Scanning**: Scan inside ZIP, TAR, and GZIP archives

### Protection
- **Real-Time Monitoring**: Advanced file system monitoring with:
  - Worker thread pool for parallel processing
  - Event deduplication and debouncing
  - Auto-quarantine on threat detection
  - Session statistics tracking
  - Rate-limited logging and secure event handling
- **Encrypted Quarantine System**: Isolate suspicious files with AES-256 encryption, secure key management, and metadata preservation
- **Clean/Repair**: Attempt to neutralize malicious patterns in files
- **Whitelist**: Mark known-safe files to skip scanning
- **Advanced Auto Threat Response System**: Intelligent automated threat handling with:
  - Configurable response levels (passive, cautious, moderate, aggressive, maximum)
  - Automatic process termination for malware processes
  - Network connection blocking for C2 communications
  - Persistence mechanism disabling (cron, systemd, init scripts)
  - File quarantine with encrypted backup and rollback support
  - Threat context gathering (related processes, network connections, sibling files)
  - ML-based threat scoring and confidence assessment
  - Dry-run mode for testing without taking actions
  - Response history tracking and auditing
  - Self-healing and system recovery capabilities

### Advanced Detection
- **Memory Scanning**: Scan process memory for suspicious patterns
- **Behavioral Analysis**: Monitor executable behavior using syscall tracing
- **Rootkit Detection**: Detect hidden processes, files, and kernel modules
- **Boot Sector Scanning**: Check boot sectors for rootkit indicators
- **MITRE ATT&CK Integration**: Map detections to MITRE framework techniques

### Monitoring
- **File Integrity Monitoring**: Create baselines and detect unauthorized file changes
- **Process Scanning**: Scan running processes for threats
- **Network Scanning**: Detect suspicious network connections
- **USB Monitoring**: Watch for USB device insertions and auto-run threats

### Email & Communication
- **Email Scanning**: Scan .eml and .msg files for malicious attachments and links
- **Attachment Analysis**: Extract and scan email attachments

### Security Analysis
- **Startup Scanning**: Scan cron jobs, systemd services, init scripts, autostart files
- **Browser Security**: Scan browser extensions, downloads, and history for threats
- **Document Analysis**: Analyze Office documents for malicious macros
- **PDF Analysis**: Detect JavaScript, embedded files, and exploits in PDFs
- **Cryptominer Detection**: Detect cryptocurrency mining malware
- **System Hardening**: Security configuration assessment and recommendations
- **Forensic Timeline**: Generate file activity timeline for incident response
- **Log Analysis**: Analyze system logs for suspicious activities

### Linux Security
- **Ransomware Protection**: Monitor directories for ransomware-like activity
- **SUID/SGID Scanner**: Detect suspicious setuid/setgid binaries
- **World-Writable Files**: Find insecure file permissions
- **WebShell Detection**: Detect web shells in web directories
- **Hidden File Scanner**: Find suspicious hidden files in unusual locations
- **Orphan Process Detection**: Detect suspicious orphaned processes
- **Keylogger Detection**: Detect keylogger activity and tools
- **Clipboard Monitor**: Check for clipboard hijacking
- **Environment Variable Check**: Detect LD_PRELOAD injection and suspicious env vars
- **DNS Security Check**: Detect DNS hijacking and configuration issues
- **Container Security**: Scan Docker containers for security issues

### Security Auditing
- **SSH Key Audit**: Audit SSH keys for weak algorithms, insecure permissions, and suspicious configurations
- **Kernel Module Scanner**: Detect suspicious or hidden kernel modules and rootkits
- **Certificate Scanner**: Find expired, weak, or self-signed certificates
- **Port Scanner**: Detect suspicious open ports and services
- **User Account Auditor**: Find UID 0 accounts, weak passwords, and suspicious users
- **Service Auditor**: Audit systemd services for suspicious patterns and backdoors
- **Sudo/PAM Checker**: Check sudoers and PAM configuration for security issues
- **IOC Scanner**: Scan for Indicators of Compromise (IPs, domains, file paths, processes)
- **Comprehensive Firewall Auditor**: Enterprise-grade firewall security assessment with:
  - Multi-firewall support (iptables, nftables, UFW, firewalld)
  - Deep rule analysis and security scoring
  - Port exposure assessment with dangerous port detection
  - Stateful inspection verification
  - Rule optimization suggestions
  - Default policy analysis
  - Kernel network security parameter auditing
  - Network zone analysis (for firewalld)
  - Firewall logging configuration audit
  - Prioritized security recommendations
  - JSON export for detailed reports
- **File Type Validator**: Detect executables disguised as other file types

### Proactive Threat Detection

- **Proactive Threat Hunting**: Multi-phase threat hunting with:
  - Behavioral anomaly detection (process masquerading, suspicious cmdline)
  - Process chain analysis (parent-child relationship threats)
  - Recent file activity analysis (suspicious file creation patterns)
  - Network threat detection (C2 ports, promiscuous mode, raw sockets)
  - In-memory threat scanning (suspicious patterns in process environment)
  - Persistence mechanism detection (cron, systemd, init scripts)
  - Privilege escalation detection (suspicious SUID, capabilities, sudoers)
  - Lateral movement detection (SSH brute force, exploitation tools)
  - Deep entropy analysis (encrypted/packed files)
  - Signature-less detection (behavior-based threat identification)
  - MITRE ATT&CK technique mapping

- **Honeypot/Canary Files**: Ransomware detection with:
  - Automatic creation of decoy files in key directories
  - Integrity monitoring with hash verification
  - Encryption detection via entropy analysis
  - Instant ransomware activity alerts
  - Honeypot registry management

- **Predictive Threat Analysis**: Behavioral monitoring with:
  - CPU spike pattern detection (crypto miner indicators)
  - Memory consumption analysis
  - Process spawn rate monitoring (fork bomb detection)
  - Network connection fluctuation analysis (C2 beaconing)
  - Confidence-scored threat predictions

### Rules & Configuration
- **Custom Detection Rules**: Create string, regex, or hex-based rules with YARA-like syntax
- **Exclusion Rules**: Skip files by path, extension, or pattern
- **Configuration Management**: Customize scanning behavior with JSON config
- **Scheduled Scans**: Automate scans with daily or interval scheduling

### Reporting & Intelligence
- **Report Generation**: Generate reports in TXT, JSON, or HTML format with threat details
- **Scan History**: Track scanning activity over time
- **Statistics**: View overall scanning statistics and performance metrics
- **Hash Reputation Lookup**: Check file reputation against local databases
- **Update Checker**: Check for GreyAV updates

### Signature Management
- **Signature Management**: Import, export, and update malware signatures
- **Custom Signatures**: Add your own malware signatures with multiple hash types
- **Scan Logging**: All actions are logged for review and forensic analysis

### Security Hardening

- **Secure Cryptography**: AES-256 encryption with PBKDF2 key derivation for quarantine
- **Input Validation**: Comprehensive path and input sanitization to prevent injection attacks
- **Atomic File Operations**: Safe file writes with atomic rename operations
- **Rate Limiting**: Built-in rate limiting to prevent abuse and log flooding
- **Secure Logging**: Automatic redaction of sensitive data (passwords, tokens, keys)
- **Integrity Verification**: Configuration and file integrity checking with constant-time comparison
- **Secure Subprocess Execution**: Safe command execution with output sanitization
- **Permission Checks**: Proper file and directory permission enforcement

### Safeguards & Health Monitoring

- **System Health Check**: Comprehensive health check including memory, disk, circuit breakers, and resources
- **Safeguard Verification**: Built-in tests for rate limiting, memory checks, circuit breakers, and file handling
- **System Readiness Validation**: Pre-operation validation for scan, quarantine, and restore operations
- **Circuit Breakers**: Automatic failure detection with operation isolation to prevent cascade failures
- **Resource Management**: Memory limits, file handle tracking, and thread pool management
- **Retry Logic**: Automatic retry with exponential backoff for transient failures
- **Pre-operation Checks**: Validate system state before critical operations
- **Graceful Degradation**: Fallback mechanisms when resources are constrained
- **Auto-fix Mode**: Automatic correction of common issues (permissions, circuit breakers)

## Installation

No external dependencies required for basic functionality! Just Python 3.6+.

```bash
# Make executable (Linux/Mac)
chmod +x greyav.py

# Optional: Install watchdog for better real-time monitoring
pip install watchdog
```

## Usage

### Scan Files or Directories

```bash
# Scan a single file
python3 greyav.py scan /path/to/file

# Scan a directory recursively
python3 greyav.py scan /path/to/directory

# Scan without heuristic analysis (faster)
python3 greyav.py scan /path/to/directory --no-heuristics

# Scan with custom heuristic threshold (default: 50)
python3 greyav.py scan /path/to/directory --threshold 70

# Scan and auto-quarantine threats
python3 greyav.py scan /path/to/directory --quarantine

# Scan and generate report
python3 greyav.py scan /path/to/directory --report html
```

### Heuristic Analysis

```bash
# Run detailed heuristic analysis on a file
python3 greyav.py heuristic /path/to/file

# Verbose output (show all indicators)
python3 greyav.py heuristic /path/to/file -v
```

### Real-Time Monitoring

```bash
# Monitor a directory for changes
python3 greyav.py monitor /path/to/watch

# Monitor multiple directories
python3 greyav.py monitor /home/user/Downloads /tmp

# Non-recursive monitoring
python3 greyav.py monitor /path/to/watch --no-recursive

# Enable auto-response (automatically respond to detected threats)
python3 greyav.py monitor /path/to/watch --auto-response

# Set response level (passive, cautious, moderate, aggressive, maximum)
python3 greyav.py monitor /path/to/watch --auto-response --response-level aggressive

# Test auto-response without taking real actions
python3 greyav.py monitor /path/to/watch --auto-response --dry-run

# Stop monitoring with Ctrl+C
```

### Auto Threat Response

```bash
# View current auto-response status
python3 greyav.py auto-response status

# View auto-response configuration and policies
python3 greyav.py auto-response config

# Test auto-response system (dry run by default)
python3 greyav.py auto-response test

# Test with specific response level
python3 greyav.py auto-response test --level aggressive

# View response history
python3 greyav.py auto-response history
```

**Response Levels:**
- `passive`: Only alert, no automatic actions
- `cautious`: Quarantine only
- `moderate`: Quarantine and terminate processes (default)
- `aggressive`: Delete threats, block network, disable persistence
- `maximum`: Full isolation and all available countermeasures

### Enhanced Automatic Threat Management

The new `AutoThreatManager` provides fully automatic threat handling:

```bash
# Enable enhanced auto-response during real-time monitoring
python3 greyav.py monitor /path/to/watch --auto-response --response-level aggressive

# The AutoThreatManager automatically:
# - Classifies threats (ransomware, backdoor, cryptominer, etc.)
# - Prioritizes responses (immediate/high/medium/low)
# - Executes appropriate actions based on threat category
# - Tracks effectiveness and learns from responses
# - Logs all actions for audit and review
```

**Automatic Response Actions by Threat Category:**

| Threat Type | Automatic Actions |
|-------------|-------------------|
| Ransomware | Kill process, terminate children, quarantine, block network, disable persistence |
| Rootkit | System isolation, alert user (requires careful handling) |
| Backdoor | Kill process, block network, quarantine, disable persistence |
| C2 Communication | Block IP, block domain, kill process |
| Cryptominer | Kill process, quarantine, block network |
| Data Exfiltration | Block network, kill process, alert user |
| Worm | Kill process, quarantine, block network, memory scan |
| General Malware | Quarantine, kill process, disable persistence |
| Suspicious File | Quarantine, alert user |
| PUP (Potentially Unwanted) | Quarantine, alert user |

**Priority System:**
- `IMMEDIATE`: Critical threats handled in milliseconds (ransomware, rootkits, C2)
- `HIGH`: Urgent threats handled in seconds (malware, cryptominers)
- `MEDIUM`: Standard threats handled in minutes (suspicious files)
- `LOW`: Minor threats queued for convenient handling

### Memory & Behavioral Analysis

```bash
# Scan all process memory for suspicious patterns
python3 greyav.py memory

# Scan specific process memory
python3 greyav.py memory --pid 1234

# Run behavioral analysis on an executable
python3 greyav.py behavior /path/to/executable

# With custom timeout
python3 greyav.py behavior /path/to/executable --timeout 10
```

### Rootkit Detection

```bash
# Scan for rootkit indicators
python3 greyav.py rootkit

# Scan boot sector (requires root)
sudo python3 greyav.py bootscan

# Scan specific device
sudo python3 greyav.py bootscan --device /dev/sdb
```

### Email Scanning

```bash
# Scan an email file for threats
python3 greyav.py email /path/to/message.eml

# Scans for:
# - Malicious attachments
# - Dangerous file extensions
# - Double extensions (e.g., document.pdf.exe)
# - Suspicious URLs
# - Header spoofing indicators
```

### USB Monitoring

```bash
# Monitor for USB device insertions
python3 greyav.py usb

# Features:
# - Detects new USB devices
# - Scans for autorun.inf files
# - Quick-scans mounted volumes
# - Alerts on suspicious files
```

### File Integrity Monitoring

```bash
# Create an integrity baseline for a directory
python3 greyav.py integrity create /path/to/important/files

# Verify files against the baseline
python3 greyav.py integrity verify
```

### Process Scanning

```bash
# Scan all running processes for threats
python3 greyav.py processes
```

### Quarantine Management

```bash
# Manually quarantine a file
python3 greyav.py quarantine /path/to/suspicious/file

# List quarantined files
python3 greyav.py list-quarantine

# Restore a file from quarantine
python3 greyav.py restore <quarantine_filename> /path/to/restore
```

### Exclusion Rules

```bash
# Exclude a path from scanning
python3 greyav.py exclude add path /path/to/exclude

# Exclude an extension
python3 greyav.py exclude add ext .log

# Exclude by glob pattern
python3 greyav.py exclude add pattern "*.tmp"

# List all exclusions
python3 greyav.py exclude list

# Remove an exclusion
python3 greyav.py exclude remove ext .log
```

### Report Generation

```bash
# Generate a text report (after scanning)
python3 greyav.py report

# Generate HTML report
python3 greyav.py report --format html

# Generate JSON report
python3 greyav.py report --format json
```

### Signature Management

```bash
# Calculate a file's hash
python3 greyav.py hash /path/to/file

# Add a new malware signature
python3 greyav.py add-signature "Malware Name" <sha256_hash> --severity high

# List all signatures
python3 greyav.py signatures list

# Export signatures to a file
python3 greyav.py signatures export my_signatures.json

# Import signatures from a file
python3 greyav.py signatures import other_signatures.json

# Update signatures from URL
python3 greyav.py signatures update --url https://example.com/sigs.json
```

### Hash Reputation Lookup

```bash
# Lookup reputation of a hash
python3 greyav.py reputation <sha256_hash>

# Lookup reputation of a file
python3 greyav.py reputation --file /path/to/file
```

### History and Statistics

```bash
# View scan history
python3 greyav.py history

# View overall statistics
python3 greyav.py stats
```

### Parallel & Archive Scanning

```bash
# Parallel multi-threaded scan (faster for large directories)
python3 greyav.py scan /path/to/directory --parallel

# Specify number of worker threads
python3 greyav.py scan /path/to/directory --parallel --workers 8

# Scan inside archive files (ZIP, TAR, GZIP)
python3 greyav.py scan /path/to/directory --archives
```

### Network Connection Scanning

```bash
# Scan active network connections for suspicious ports
python3 greyav.py network
```

### Custom Detection Rules

```bash
# Add a string-based rule
python3 greyav.py rules add "Suspicious String" string "malicious_pattern" --severity high

# Add a regex-based rule
python3 greyav.py rules add "Email Harvester" regex "[a-z]+@[a-z]+\\.com" --severity medium

# Add a hex-based rule
python3 greyav.py rules add "Shellcode Pattern" hex "90909090" --severity critical

# List all rules
python3 greyav.py rules list

# Toggle a rule on/off
python3 greyav.py rules toggle suspicious_string

# Delete a rule
python3 greyav.py rules delete suspicious_string
```

### Configuration Management

```bash
# Show current configuration
python3 greyav.py config show

# Change a setting
python3 greyav.py config set heuristic_threshold 60
python3 greyav.py config set parallel_scans 8
python3 greyav.py config set auto_quarantine true

# Reset to defaults
python3 greyav.py config reset
```

### Scheduled Scans

```bash
# Add a daily scan at 2:00 AM
python3 greyav.py schedule add "Nightly Scan" /home/user daily 02:00

# Add an interval scan every 60 minutes
python3 greyav.py schedule add "Hourly Check" /tmp interval 60

# List scheduled scans
python3 greyav.py schedule list

# Run the scheduler daemon
python3 greyav.py schedule daemon
```

### Clean/Repair & Whitelist

```bash
# Attempt to clean a malicious file
python3 greyav.py clean /path/to/infected/file

# Add a file to the whitelist (skip in future scans)
python3 greyav.py whitelist /path/to/safe/file

# Verify GreyAV's own integrity
python3 greyav.py selfcheck
```

### Updates & Recovery

```bash
# Check for GreyAV updates
python3 greyav.py update

# Attempt to recover deleted files
python3 greyav.py recover /path/to/directory

# With file pattern
python3 greyav.py recover /path/to/directory --pattern "*.doc"
```

### Security Analysis

```bash
# Scan startup items (cron, systemd, init scripts)
python3 greyav.py startup

# Scan browser security (extensions, downloads, history)
python3 greyav.py browser

# Analyze Office documents for malicious macros
python3 greyav.py document /path/to/document.docx

# Analyze PDF files for threats
python3 greyav.py pdf /path/to/file.pdf

# Detect cryptocurrency miners
python3 greyav.py cryptominer

# Run system hardening assessment
python3 greyav.py hardening

# Generate forensic timeline
python3 greyav.py timeline /path/to/directory --days 7

# Analyze system logs for suspicious activity
python3 greyav.py logs

# Start interactive mode
python3 greyav.py interactive
```

### Linux Security Scanning

```bash
# Monitor directory for ransomware activity
python3 greyav.py ransomware /path/to/important/files --duration 60

# Scan for suspicious SUID/SGID binaries
python3 greyav.py suid

# Find world-writable files
python3 greyav.py worldwrite --directory /

# Detect web shells in web directories
python3 greyav.py webshell --webroot /var/www

# Find suspicious hidden files
python3 greyav.py hidden --directory /tmp

# Detect orphan/zombie processes
python3 greyav.py orphans

# Detect keylogger activity
python3 greyav.py keylogger

# Check clipboard access
python3 greyav.py clipboard

# Check environment variables for injection
python3 greyav.py envcheck

# DNS security check (hijacking, suspicious servers)
python3 greyav.py dns

# Scan Docker containers for security issues
python3 greyav.py containers
```

### Security Auditing

```bash
# Audit SSH keys for security issues
python3 greyav.py ssh-audit

# Scan loaded kernel modules
python3 greyav.py kernel-scan

# Scan for expired/weak certificates
python3 greyav.py cert-scan

# With specific directory
python3 greyav.py cert-scan --directory /etc/ssl

# Scan for suspicious open ports
python3 greyav.py port-scan

# Audit user accounts for security issues
python3 greyav.py user-audit

# Audit systemd services
python3 greyav.py service-audit

# Check sudo and PAM configuration
python3 greyav.py sudo-check
python3 greyav.py pam-check

# Scan for Indicators of Compromise (IOC)
python3 greyav.py ioc-scan

# With custom IOC file
python3 greyav.py ioc-scan --ioc-file iocs.json

# Comprehensive firewall security audit
python3 greyav.py firewall-audit

# Deep analysis (analyzes individual rules, policies, and configurations)
python3 greyav.py firewall-audit --deep

# Export firewall rules and findings to JSON
python3 greyav.py firewall-audit --export

# Deep scan with export
python3 greyav.py firewall-audit --deep --export

# Validate file types (detect disguised executables)
python3 greyav.py filetype-check /downloads

# Non-recursive check
python3 greyav.py filetype-check /downloads --no-recursive
```

### Health & Safeguard Commands

```bash
# Run comprehensive system health check
python3 greyav.py health

# Health check with verbose output
python3 greyav.py health --verbose

# Health check with auto-fix (corrects permissions, resets breakers)
python3 greyav.py health --auto-fix

# Verify safeguard systems
python3 greyav.py safeguards

# Run safeguard tests (rate limiting, circuit breakers, etc.)
python3 greyav.py safeguards --test

# Reset all circuit breakers
python3 greyav.py safeguards --reset

# Validate system readiness for all operations
python3 greyav.py validate

# Validate readiness for specific operation
python3 greyav.py validate --operation scan
python3 greyav.py validate --operation quarantine
python3 greyav.py validate --operation restore
```

### Proactive Threat Detection

```bash
# Run comprehensive proactive threat hunt
python3 greyav.py threat-hunt

# Deep threat hunt with entropy and signatureless detection
python3 greyav.py threat-hunt --deep

# Threat hunt on specific path with export
python3 greyav.py threat-hunt --path /home --deep --export hunt_results.json

# Set up honeypot/canary files for ransomware detection
python3 greyav.py honeypot setup

# Set up honeypots in specific directories
python3 greyav.py honeypot setup --dirs /home/user/Documents /var/www

# Check honeypot integrity (detect ransomware/tampering)
python3 greyav.py honeypot check

# View honeypot status
python3 greyav.py honeypot status

# Run predictive threat analysis (60 second default)
python3 greyav.py predict

# Predictive analysis with custom duration
python3 greyav.py predict --duration 120

# Export predictions to file
python3 greyav.py predict --duration 60 --export predictions.json
```

## Heuristic Detection

The heuristic engine analyzes files for suspicious characteristics:

| Category | Description |
|----------|-------------|
| Shell Commands | Detects shell execution patterns |
| Network Indicators | Identifies network communication code |
| Crypto Indicators | Finds encryption/ransomware patterns |
| Persistence | Detects autostart/persistence mechanisms |
| Obfuscation | Identifies encoded/obfuscated code |
| Data Exfiltration | Finds credential harvesting patterns |
| Privilege Escalation | Detects privilege elevation attempts |
| Anti-Analysis | Identifies VM/debugger detection |
| Entropy Analysis | High entropy suggests encryption/packing |
| Extension Analysis | Dangerous or hidden file extensions |

### Risk Levels

| Score | Level | Description |
|-------|-------|-------------|
| 0-19 | Clean | No suspicious indicators |
| 20-39 | Low | Minor concerns |
| 40-59 | Medium | Moderate risk |
| 60-79 | High | Significant risk |
| 80+ | Critical | Highly suspicious |

## File Structure

```
GreyAV/
├── greyav.py                    # Main AV program (CLI interface)
│
├── ─── Core Subsystems ───
├── policy_orchestrator.py       # Autonomous Policy Orchestration Engine
├── threat_knowledge_graph.py    # Adaptive Threat Knowledge Graph
├── threat_dna.py                # Adaptive Threat DNA Fingerprinting Engine
├── threat_economy.py            # Predictive Threat Economy Engine
├── threat_sandbox.py            # Adaptive Threat Simulation Sandbox
├── auto_threat_manager.py       # Automatic Threat Detection & Mitigation
├── port_manager.py              # Centralized Port Risk Management
│
├── ─── Detection & Analysis ───
├── neural_adaptation.py         # Neural Threat Adaptation Engine
├── behavioral_engine.py         # Behavioral analysis module
├── semantic_reasoner.py         # Semantic threat reasoning
├── attack_graph.py              # Attack path graph analysis
│
├── ─── Defense & Deception ───
├── deception_engine.py          # Cognitive Deception Engine
├── deception_manager.py         # Deception resource management
├── av_immune_system.py          # Bio-inspired AV Immune System
├── chaos_immunization_layer.py  # Chaos Immunization & Fault Injection
├── energy_defense.py            # Energy-aware defense mechanisms
│
├── ─── Resilience & Mesh ───
├── collective_mesh.py           # Collective Intelligence Mesh Network
├── mesh_node.py                 # Individual mesh node operations
├── resilience_manager.py        # System resilience management
├── trust_fabric.py              # Trust relationship management
│
├── ─── Utilities ───
├── process_lineage.py           # Process lineage tracking
├── replay_engine.py             # Threat replay & simulation
├── threat_simulator.py          # Threat simulation framework
│
├── ─── Configuration & Data ───
├── signatures.json              # Malware signature database
├── exclusions.json              # Scan exclusion rules
├── integrity.json               # File integrity baselines
├── scan_history.json            # Historical scan records
├── scan_log.txt                 # Action log file
├── behavioral_events.log        # Behavioral event log
│
├── ─── Deployment ───
├── install_av.sh                # Linux installation script
├── uninstall_av.sh              # Linux uninstallation script
│
├── quarantine/                  # Quarantined files directory
├── .venv/                       # Python virtual environment
├── .vscode/                     # VS Code configuration
├── .github/                     # GitHub workflows & config
└── README.md                    # This file
```

### Subsystem Architecture

| Module | Purpose | Key Classes |
|--------|---------|-------------|
| `auto_threat_manager.py` | Fully automatic threat detection & mitigation | `AutoThreatManager`, `DetectedThreat`, `MitigationExecutor`, `ResponsePolicy` |
| `port_manager.py` | Centralized port risk assessment & management | `PortManager`, `PortRisk`, `PortInfo` |
| `policy_orchestrator.py` | Autonomous policy management with conflict resolution | `PolicyOrchestrator`, `PolicyStore`, `PolicyAlert` |
| `threat_knowledge_graph.py` | Dynamic threat relationship tracking & pattern analysis | `ThreatKnowledgeGraph`, `Node`, `GraphAlert` |
| `threat_dna.py` | Behavioral fingerprinting & malware family classification | `ThreatDNAEngine`, `FingerprintMetadata`, `DNAAlert` |
| `threat_economy.py` | Attack cost/benefit modeling & ROI predictions | `ThreatEconomyEngine`, `AttackPath`, `EconomyAlert` |
| `threat_sandbox.py` | Isolated execution & behavioral simulation | `ThreatSandbox`, `ExecutionResult`, `SandboxAlert` |
| `neural_adaptation.py` | Real-time ML-based threat adaptation | `NeuralAdaptationEngine`, `LightweightClassifier`, `AdaptationAlert` |
| `deception_engine.py` | Cognitive decoy deployment & attacker manipulation | `DeceptionEngine`, `DecoyManager`, `Decoy`, `DeceptionAlert` |
| `collective_mesh.py` | Distributed threat intelligence sharing | `CollectiveMesh`, `PeerNode`, `Signal`, `MeshAlert` |
| `av_immune_system.py` | Bio-inspired innate/adaptive defense | `InnateDefense`, `AdaptiveDefense`, `ImmuneMemory`, `SelfHealing` |
| `chaos_immunization_layer.py` | Fault injection & chaos engineering for resilience | `ChaosImmunizationLayer`, `ChaosEvent` |

### Installation Scripts

| Script | Purpose |
|--------|---------|
| `install_av.sh` | Cross-platform installation with OS detection, dependency management, virtual environment setup, and service registration |
| `uninstall_av.sh` | Clean uninstallation with service removal, directory cleanup, and confirmation prompts |

#### Supported Operating Systems

| OS Family | Distributions | Package Manager | Service Manager |
|-----------|---------------|-----------------|-----------------|
| **Debian-based** | Ubuntu, Debian, Linux Mint, Pop!_OS, Zorin OS, MX Linux, Kali Linux | `apt` | systemd |
| **Fedora-based** | Fedora, CentOS, RHEL, Rocky, AlmaLinux | `dnf` / `yum` | systemd |
| **Immutable Fedora** | Bazzite, Silverblue, Kinoite | `rpm-ostree` | systemd |
| **Arch-based** | Arch Linux, Manjaro, EndeavourOS, Garuda Linux, Artix | `pacman` | systemd |
| **NixOS** | NixOS | `nix` / `nix-env` | systemd |
| **SUSE-based** | openSUSE Leap, openSUSE Tumbleweed | `zypper` | systemd |
| **macOS** | macOS 10.15+ (Catalina, Big Sur, Monterey, Ventura, Sonoma, Sequoia) | `brew` | launchd |
| **Windows** | Windows 10/11 (via WSL2 or native) | `choco` / `winget` | Windows Services |

## Testing

To test the scanner, you can use the EICAR test file:

```bash
# Create EICAR test file
echo 'X5O!P%@AP[4\PZX54(P^)7CC)7}$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*' > eicar.txt

# Scan it
python3 greyav.py scan eicar.txt
```

## Disclaimer

⚠️ **This is an educational tool** and should not be used as your primary antivirus solution. It demonstrates basic antivirus concepts but lacks the sophistication of commercial antivirus software.

## License

MIT License - Feel free to use, modify, and distribute.
