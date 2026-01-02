# Changelog

All notable changes to GreyAV will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial public release preparation

## [5.0.0] - 2026-01-02

### Added
- **Asymmetric Brute Force Resilience**: 30:1 efficiency ratio against attackers
- **Auto Threat Manager**: Fully automatic threat detection and mitigation
- **Port Manager**: Centralized port risk assessment and management
- **Policy Orchestrator**: Autonomous policy management with conflict resolution
- **Threat Knowledge Graph**: Dynamic threat relationship tracking
- **Threat DNA Engine**: Behavioral fingerprinting and malware family classification
- **Threat Economy Engine**: Attack cost/benefit modeling
- **Threat Sandbox**: Isolated execution and behavioral simulation
- **Neural Adaptation Engine**: Real-time ML-based threat adaptation
- **Deception Engine**: Cognitive decoy deployment
- **Collective Mesh**: Distributed threat intelligence sharing
- **AV Immune System**: Bio-inspired innate/adaptive defense
- **Chaos Immunization Layer**: Fault injection for resilience testing
- **Socket Intake**: Network threat probe monitoring on critical ports
- **Proactive Threat Hunting**: Multi-phase threat detection
- **Honeypot/Canary Files**: Ransomware detection system
- **Predictive Threat Analysis**: Behavioral anomaly prediction
- **Comprehensive Firewall Auditor**: Multi-firewall security assessment
- **Docker & Kubernetes Support**: Production-ready containerization
- **Multi-distro Installation**: Support for Debian, Fedora, Arch, NixOS, SUSE, macOS

### Security
- AES-256 encryption for quarantine with PBKDF2 key derivation
- Comprehensive input validation and path sanitization
- Atomic file operations with secure writes
- Rate limiting and circuit breakers
- Secure logging with automatic sensitive data redaction

## [4.0.0] - 2025-09-15

### Added
- File Integrity Monitoring with baseline creation
- Memory scanning for process inspection
- Behavioral analysis with syscall tracing
- Rootkit detection (hidden processes, files, kernel modules)
- Boot sector scanning
- Email scanning (.eml, .msg files)
- USB monitoring with auto-scan
- MITRE ATT&CK framework integration

### Changed
- Improved heuristic engine with multi-layer detection
- Enhanced parallel scanning performance

## [3.0.0] - 2025-06-01

### Added
- Real-time monitoring with watchdog integration
- Encrypted quarantine system
- Custom detection rules (string, regex, hex)
- Scheduled scanning with daemon mode
- HTML/JSON report generation
- Archive scanning (ZIP, TAR, GZIP)

### Changed
- Refactored CLI with subcommands
- Improved signature matching with fuzzy hashing

## [2.0.0] - 2025-03-01

### Added
- Heuristic analysis engine
- Process scanning
- Network connection scanning
- Configuration management
- Exclusion rules

### Changed
- Multi-algorithm signature detection (SHA256, SHA512, MD5, SHA1)

## [1.0.0] - 2025-01-15

### Added
- Initial release
- Basic file and directory scanning
- Signature-based detection
- Simple quarantine functionality
- Scan logging

[Unreleased]: https://github.com/greyav/greyav/compare/v5.0.0...HEAD
[5.0.0]: https://github.com/greyav/greyav/compare/v4.0.0...v5.0.0
[4.0.0]: https://github.com/greyav/greyav/compare/v3.0.0...v4.0.0
[3.0.0]: https://github.com/greyav/greyav/compare/v2.0.0...v3.0.0
[2.0.0]: https://github.com/greyav/greyav/compare/v1.0.0...v2.0.0
[1.0.0]: https://github.com/greyav/greyav/releases/tag/v1.0.0
