#!/usr/bin/env python3
"""
Automatic Threat Detection and Mitigation Manager for GreyAV
=============================================================

This module provides fully automatic threat detection and response without
requiring user intervention. It integrates multiple detection engines and
orchestrates responses automatically based on threat severity.

Features:
    - Continuous background threat monitoring
    - Automatic severity assessment and prioritization
    - Progressive response escalation
    - Self-learning from response effectiveness
    - Real-time network threat blocking
    - Automatic process termination
    - File quarantine automation
    - Persistence mechanism removal

Author: GreyAV Team
License: MIT
"""

import hashlib
import json
import os
import queue
import re
import shutil
import signal
import socket
import subprocess
import sys
import threading
import time
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum, auto
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Set, Tuple
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AutoThreatManager")


# =============================================================================
# Threat Classification
# =============================================================================

class ThreatCategory(Enum):
    """Categories of threats for appropriate response."""
    MALWARE = "malware"           # Known malicious files
    RANSOMWARE = "ransomware"     # Encryption-based threats
    CRYPTOMINER = "cryptominer"   # Resource hijacking
    BACKDOOR = "backdoor"         # Remote access trojans
    ROOTKIT = "rootkit"           # System-level persistence
    WORM = "worm"                 # Self-propagating
    EXPLOIT = "exploit"           # Vulnerability exploitation
    PUP = "pup"                   # Potentially unwanted program
    SUSPICIOUS = "suspicious"     # Unclassified suspicious activity
    C2_COMMUNICATION = "c2"       # Command and control traffic
    DATA_EXFIL = "exfiltration"   # Data theft attempts
    LATERAL_MOVEMENT = "lateral"  # Network spreading


class ResponsePriority(Enum):
    """Response priority levels."""
    IMMEDIATE = 0    # Respond within milliseconds
    HIGH = 1         # Respond within seconds
    MEDIUM = 2       # Respond within minutes
    LOW = 3          # Respond when convenient
    DEFERRED = 4     # Queue for batch processing


class MitigationAction(Enum):
    """Available mitigation actions."""
    KILL_PROCESS = auto()
    QUARANTINE_FILE = auto()
    DELETE_FILE = auto()
    BLOCK_NETWORK = auto()
    BLOCK_IP = auto()
    BLOCK_DOMAIN = auto()
    DISABLE_PERSISTENCE = auto()
    RESTORE_FILE = auto()
    ISOLATE_SYSTEM = auto()
    ALERT_USER = auto()
    LOG_ONLY = auto()
    MEMORY_SCAN = auto()
    YARA_SCAN = auto()
    TERMINATE_CHILDREN = auto()
    REVOKE_PERMISSIONS = auto()


@dataclass
class ThreatIndicator:
    """A single threat indicator."""
    indicator_type: str  # hash, ip, domain, filename, pattern, etc.
    value: str
    confidence: float = 0.0
    source: str = "internal"
    first_seen: datetime = field(default_factory=datetime.now)
    last_seen: datetime = field(default_factory=datetime.now)
    hit_count: int = 0
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class DetectedThreat:
    """Represents a detected threat with full context."""
    threat_id: str
    category: ThreatCategory
    severity: str  # critical, high, medium, low
    confidence: float
    priority: ResponsePriority
    
    # Detection info
    detection_time: datetime = field(default_factory=datetime.now)
    detection_source: str = "unknown"
    detection_rule: str = ""
    
    # File context
    file_path: Optional[str] = None
    file_hash: Optional[str] = None
    file_size: int = 0
    
    # Process context
    process_id: Optional[int] = None
    process_name: Optional[str] = None
    process_cmdline: Optional[str] = None
    parent_pid: Optional[int] = None
    child_pids: List[int] = field(default_factory=list)
    
    # Network context
    network_connections: List[Dict[str, Any]] = field(default_factory=list)
    remote_ips: List[str] = field(default_factory=list)
    remote_domains: List[str] = field(default_factory=list)
    
    # MITRE ATT&CK mapping
    mitre_techniques: List[str] = field(default_factory=list)
    mitre_tactics: List[str] = field(default_factory=list)
    
    # Response tracking
    actions_taken: List[str] = field(default_factory=list)
    response_success: bool = False
    response_time_ms: float = 0.0
    
    # Additional context
    indicators: List[ThreatIndicator] = field(default_factory=list)
    related_threats: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)


# =============================================================================
# Response Policy Engine
# =============================================================================

class ResponsePolicy:
    """Defines automatic response policies for different threat types."""
    
    # Default policies by category and severity
    POLICIES = {
        # Critical severity - immediate action
        ("critical", ThreatCategory.RANSOMWARE): [
            MitigationAction.KILL_PROCESS,
            MitigationAction.TERMINATE_CHILDREN,
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.BLOCK_NETWORK,
            MitigationAction.DISABLE_PERSISTENCE,
        ],
        ("critical", ThreatCategory.ROOTKIT): [
            MitigationAction.ISOLATE_SYSTEM,
            MitigationAction.ALERT_USER,
            MitigationAction.LOG_ONLY,  # Rootkits need careful handling
        ],
        ("critical", ThreatCategory.BACKDOOR): [
            MitigationAction.KILL_PROCESS,
            MitigationAction.BLOCK_NETWORK,
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.DISABLE_PERSISTENCE,
        ],
        ("critical", ThreatCategory.C2_COMMUNICATION): [
            MitigationAction.BLOCK_IP,
            MitigationAction.BLOCK_DOMAIN,
            MitigationAction.KILL_PROCESS,
        ],
        
        # High severity - quick action
        ("high", ThreatCategory.MALWARE): [
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.KILL_PROCESS,
            MitigationAction.DISABLE_PERSISTENCE,
        ],
        ("high", ThreatCategory.CRYPTOMINER): [
            MitigationAction.KILL_PROCESS,
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.BLOCK_NETWORK,
        ],
        ("high", ThreatCategory.WORM): [
            MitigationAction.KILL_PROCESS,
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.BLOCK_NETWORK,
            MitigationAction.MEMORY_SCAN,
        ],
        ("high", ThreatCategory.DATA_EXFIL): [
            MitigationAction.BLOCK_NETWORK,
            MitigationAction.KILL_PROCESS,
            MitigationAction.ALERT_USER,
        ],
        
        # Medium severity - measured action
        ("medium", ThreatCategory.SUSPICIOUS): [
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.ALERT_USER,
        ],
        ("medium", ThreatCategory.PUP): [
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.ALERT_USER,
        ],
        ("medium", ThreatCategory.EXPLOIT): [
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.YARA_SCAN,
        ],
        
        # Low severity - monitor and log
        ("low", ThreatCategory.SUSPICIOUS): [
            MitigationAction.LOG_ONLY,
            MitigationAction.ALERT_USER,
        ],
    }
    
    # Default actions for unspecified combinations
    DEFAULT_ACTIONS = {
        "critical": [
            MitigationAction.KILL_PROCESS,
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.BLOCK_NETWORK,
        ],
        "high": [
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.KILL_PROCESS,
        ],
        "medium": [
            MitigationAction.QUARANTINE_FILE,
            MitigationAction.ALERT_USER,
        ],
        "low": [
            MitigationAction.LOG_ONLY,
        ],
    }
    
    @classmethod
    def get_actions(cls, threat: DetectedThreat) -> List[MitigationAction]:
        """Get the appropriate mitigation actions for a threat."""
        key = (threat.severity.lower(), threat.category)
        
        # Look up specific policy
        if key in cls.POLICIES:
            return cls.POLICIES[key].copy()
        
        # Fall back to default by severity
        return cls.DEFAULT_ACTIONS.get(
            threat.severity.lower(),
            [MitigationAction.LOG_ONLY]
        ).copy()
    
    @classmethod
    def get_priority(cls, threat: DetectedThreat) -> ResponsePriority:
        """Determine response priority based on threat characteristics."""
        severity = threat.severity.lower()
        
        # Ransomware and rootkits are always immediate
        if threat.category in [ThreatCategory.RANSOMWARE, ThreatCategory.ROOTKIT]:
            return ResponsePriority.IMMEDIATE
        
        # Active C2 communication is immediate
        if threat.category == ThreatCategory.C2_COMMUNICATION:
            return ResponsePriority.IMMEDIATE
        
        # Map severity to priority
        severity_priority = {
            "critical": ResponsePriority.IMMEDIATE,
            "high": ResponsePriority.HIGH,
            "medium": ResponsePriority.MEDIUM,
            "low": ResponsePriority.LOW,
        }
        
        return severity_priority.get(severity, ResponsePriority.MEDIUM)


# =============================================================================
# Automatic Mitigation Executor
# =============================================================================

class MitigationExecutor:
    """Executes mitigation actions automatically."""
    
    def __init__(self, quarantine_dir: Path = None, dry_run: bool = False):
        self.quarantine_dir = quarantine_dir or Path("/var/lib/myav/quarantine")
        self.dry_run = dry_run
        self.blocked_ips: Set[str] = set()
        self.blocked_domains: Set[str] = set()
        self.killed_processes: Set[int] = set()
        self.quarantined_files: Set[str] = set()
        self._lock = threading.Lock()
        
        # Ensure quarantine directory exists
        self.quarantine_dir.mkdir(parents=True, exist_ok=True)
    
    def execute(self, action: MitigationAction, 
                threat: DetectedThreat) -> Dict[str, Any]:
        """Execute a single mitigation action."""
        result = {
            "action": action.name,
            "success": False,
            "timestamp": datetime.now().isoformat(),
            "details": {}
        }
        
        if self.dry_run:
            result["success"] = True
            result["details"]["dry_run"] = True
            logger.info(f"[DRY RUN] Would execute: {action.name}")
            return result
        
        try:
            # Dispatch to appropriate handler
            handlers = {
                MitigationAction.KILL_PROCESS: self._kill_process,
                MitigationAction.TERMINATE_CHILDREN: self._terminate_children,
                MitigationAction.QUARANTINE_FILE: self._quarantine_file,
                MitigationAction.DELETE_FILE: self._delete_file,
                MitigationAction.BLOCK_NETWORK: self._block_network,
                MitigationAction.BLOCK_IP: self._block_ip,
                MitigationAction.BLOCK_DOMAIN: self._block_domain,
                MitigationAction.DISABLE_PERSISTENCE: self._disable_persistence,
                MitigationAction.RESTORE_FILE: self._restore_file,
                MitigationAction.ISOLATE_SYSTEM: self._isolate_system,
                MitigationAction.ALERT_USER: self._alert_user,
                MitigationAction.LOG_ONLY: self._log_only,
                MitigationAction.REVOKE_PERMISSIONS: self._revoke_permissions,
            }
            
            handler = handlers.get(action, self._log_only)
            result.update(handler(threat))
            
        except Exception as e:
            result["error"] = str(e)
            logger.error(f"Mitigation failed for {action.name}: {e}")
        
        return result
    
    def _kill_process(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Kill the malicious process."""
        result = {"killed_pids": [], "failed_pids": []}
        
        if not threat.process_id:
            # Try to find process by file
            if threat.file_path:
                pids = self._find_processes_using_file(threat.file_path)
                for pid in pids:
                    if self._safe_kill(pid):
                        result["killed_pids"].append(pid)
                    else:
                        result["failed_pids"].append(pid)
        else:
            if self._safe_kill(threat.process_id):
                result["killed_pids"].append(threat.process_id)
            else:
                result["failed_pids"].append(threat.process_id)
        
        result["success"] = len(result["killed_pids"]) > 0 or not threat.process_id
        return result
    
    def _terminate_children(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Terminate all child processes of the threat."""
        result = {"terminated": [], "failed": []}
        
        pids_to_check = [threat.process_id] if threat.process_id else []
        pids_to_check.extend(threat.child_pids)
        
        for pid in pids_to_check:
            if pid:
                children = self._get_child_processes(pid)
                for child_pid in children:
                    if self._safe_kill(child_pid):
                        result["terminated"].append(child_pid)
                    else:
                        result["failed"].append(child_pid)
        
        result["success"] = True
        return result
    
    def _quarantine_file(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Move file to quarantine."""
        result = {"success": False, "quarantine_path": None}
        
        if not threat.file_path:
            result["error"] = "No file path specified"
            return result
        
        src = Path(threat.file_path)
        if not src.exists():
            result["error"] = "File not found"
            result["success"] = True  # Already gone
            return result
        
        try:
            # Create quarantine filename with timestamp
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            safe_name = re.sub(r'[^\w\-.]', '_', src.name)
            quar_name = f"{timestamp}_{safe_name}.quarantine"
            quar_path = self.quarantine_dir / quar_name
            
            # Create metadata
            meta = {
                "original_path": str(src),
                "quarantine_time": datetime.now().isoformat(),
                "threat_id": threat.threat_id,
                "category": threat.category.value,
                "severity": threat.severity,
                "file_hash": threat.file_hash,
            }
            
            # Move file to quarantine
            shutil.move(str(src), str(quar_path))
            
            # Save metadata
            meta_path = quar_path.with_suffix(quar_path.suffix + ".meta")
            with open(meta_path, 'w') as f:
                json.dump(meta, f, indent=2)
            
            # Track quarantined file
            with self._lock:
                self.quarantined_files.add(str(src))
            
            result["success"] = True
            result["quarantine_path"] = str(quar_path)
            logger.info(f"Quarantined: {src} -> {quar_path}")
            
        except Exception as e:
            result["error"] = str(e)
        
        return result
    
    def _delete_file(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Permanently delete a malicious file."""
        result = {"success": False}
        
        if not threat.file_path:
            result["error"] = "No file path specified"
            return result
        
        try:
            path = Path(threat.file_path)
            if path.exists():
                # Secure delete - overwrite before removing
                if path.is_file():
                    with open(path, 'wb') as f:
                        f.write(os.urandom(path.stat().st_size))
                path.unlink()
                result["success"] = True
                logger.info(f"Deleted malicious file: {path}")
            else:
                result["success"] = True  # Already gone
        except Exception as e:
            result["error"] = str(e)
        
        return result
    
    def _block_network(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Block network connections for the threat."""
        result = {"blocked_ips": [], "blocked_ports": []}
        
        # Block all remote IPs
        for ip in threat.remote_ips:
            if self._add_iptables_rule(ip):
                result["blocked_ips"].append(ip)
                with self._lock:
                    self.blocked_ips.add(ip)
        
        # Block connections from network_connections
        for conn in threat.network_connections:
            remote_ip = conn.get("remote_ip")
            if remote_ip and remote_ip not in self.blocked_ips:
                if self._add_iptables_rule(remote_ip):
                    result["blocked_ips"].append(remote_ip)
                    with self._lock:
                        self.blocked_ips.add(remote_ip)
        
        result["success"] = True
        return result
    
    def _block_ip(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Block a specific IP address."""
        result = {"blocked": []}
        
        for ip in threat.remote_ips:
            if self._add_iptables_rule(ip):
                result["blocked"].append(ip)
        
        result["success"] = True
        return result
    
    def _block_domain(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Block domains via hosts file."""
        result = {"blocked": []}
        
        hosts_additions = []
        for domain in threat.remote_domains:
            # Resolve domain to IP for iptables blocking
            try:
                ips = socket.getaddrinfo(domain, None)
                for ip_info in ips:
                    ip = ip_info[4][0]
                    if self._add_iptables_rule(ip):
                        result["blocked"].append(f"{domain} ({ip})")
            except socket.gaierror:
                pass
            
            # Also add to hosts file redirect
            hosts_additions.append(f"127.0.0.1 {domain}")
            hosts_additions.append(f"127.0.0.1 www.{domain}")
        
        # Append to hosts file (requires root)
        if hosts_additions:
            try:
                with open("/etc/hosts", "a") as f:
                    f.write("\n# Blocked by GreyAV\n")
                    f.write("\n".join(hosts_additions) + "\n")
            except PermissionError:
                result["warning"] = "Could not modify /etc/hosts (need root)"
        
        result["success"] = True
        return result
    
    def _disable_persistence(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Disable persistence mechanisms."""
        result = {"disabled": [], "failed": []}
        
        # Common persistence locations to check
        persistence_locations = [
            # Cron jobs
            Path("/etc/cron.d"),
            Path("/var/spool/cron"),
            Path.home() / ".config/autostart",
            # Systemd
            Path("/etc/systemd/system"),
            Path.home() / ".config/systemd/user",
            # Profile scripts
            Path("/etc/profile.d"),
            # RC scripts
            Path("/etc/rc.local"),
        ]
        
        threat_name = threat.threat_id.lower()
        file_name = Path(threat.file_path).name if threat.file_path else ""
        
        for loc in persistence_locations:
            if not loc.exists():
                continue
            
            try:
                if loc.is_dir():
                    for item in loc.iterdir():
                        # Check if file references the threat
                        if self._file_references_threat(item, threat):
                            self._safe_remove(item)
                            result["disabled"].append(str(item))
                elif loc.is_file():
                    if self._file_references_threat(loc, threat):
                        self._safe_remove(loc)
                        result["disabled"].append(str(loc))
            except PermissionError:
                result["failed"].append(str(loc))
        
        result["success"] = True
        return result
    
    def _isolate_system(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Emergency system isolation."""
        result = {"isolated": False}
        
        logger.critical(f"SYSTEM ISOLATION TRIGGERED: {threat.threat_id}")
        
        # Block all outbound except localhost and essential
        try:
            # Add isolation rules
            subprocess.run([
                "iptables", "-I", "OUTPUT", "-d", "127.0.0.0/8", "-j", "ACCEPT"
            ], check=False, capture_output=True)
            subprocess.run([
                "iptables", "-I", "OUTPUT", "-p", "udp", "--dport", "53",
                "-j", "ACCEPT"  # Allow DNS
            ], check=False, capture_output=True)
            subprocess.run([
                "iptables", "-A", "OUTPUT", "-j", "DROP"
            ], check=False, capture_output=True)
            
            result["isolated"] = True
            result["warning"] = "System is in isolation mode - outbound blocked"
        except Exception as e:
            result["error"] = str(e)
        
        result["success"] = True
        return result
    
    def _alert_user(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Send alert to user."""
        result = {"alerted": False}
        
        # Print prominent alert
        alert_msg = f"""
╔══════════════════════════════════════════════════════════════╗
║                    ⚠️  THREAT DETECTED ⚠️                      ║
╠══════════════════════════════════════════════════════════════╣
║ Threat: {threat.threat_id[:50]:<50} ║
║ Category: {threat.category.value:<48} ║
║ Severity: {threat.severity.upper():<48} ║
║ File: {(threat.file_path or 'N/A')[-48:]:<48} ║
╚══════════════════════════════════════════════════════════════╝
"""
        print(alert_msg)
        
        # Try desktop notification
        try:
            subprocess.run([
                "notify-send", "-u", "critical",
                "GreyAV Threat Detected",
                f"{threat.threat_id} ({threat.severity})"
            ], check=False, capture_output=True, timeout=5)
            result["alerted"] = True
        except Exception:
            pass
        
        result["success"] = True
        return result
    
    def _log_only(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Log the threat without taking action."""
        logger.info(f"Threat logged: {threat.threat_id} - {threat.category.value}")
        return {"success": True, "logged": True}
    
    def _revoke_permissions(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Revoke execute permissions from file."""
        result = {"success": False}
        
        if threat.file_path:
            try:
                path = Path(threat.file_path)
                if path.exists():
                    # Remove execute permission
                    current_mode = path.stat().st_mode
                    new_mode = current_mode & ~0o111
                    path.chmod(new_mode)
                    result["success"] = True
                    result["new_mode"] = oct(new_mode)
            except Exception as e:
                result["error"] = str(e)
        
        return result
    
    def _restore_file(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Restore a file from quarantine."""
        # This would be called for false positives
        result = {"success": False}
        # Implementation would search quarantine and restore
        return result
    
    # Helper methods
    
    def _safe_kill(self, pid: int, signal_type: int = signal.SIGKILL) -> bool:
        """Safely kill a process."""
        if pid <= 1:  # Never kill init or kernel
            return False
        
        try:
            os.kill(pid, signal_type)
            with self._lock:
                self.killed_processes.add(pid)
            return True
        except ProcessLookupError:
            return True  # Already dead
        except PermissionError:
            # Try with sudo
            try:
                subprocess.run(
                    ["sudo", "kill", f"-{signal_type}", str(pid)],
                    check=True, capture_output=True, timeout=5
                )
                return True
            except Exception:
                return False
        except Exception:
            return False
    
    def _get_child_processes(self, pid: int) -> List[int]:
        """Get all child processes of a PID."""
        children = []
        try:
            result = subprocess.run(
                ["pgrep", "-P", str(pid)],
                capture_output=True, text=True, timeout=5
            )
            for line in result.stdout.strip().split('\n'):
                if line.isdigit():
                    children.append(int(line))
        except Exception:
            pass
        return children
    
    def _find_processes_using_file(self, filepath: str) -> List[int]:
        """Find processes that have a file open."""
        pids = []
        try:
            result = subprocess.run(
                ["fuser", filepath],
                capture_output=True, text=True, timeout=5
            )
            for part in result.stdout.split():
                pid_str = part.rstrip('cerm')
                if pid_str.isdigit():
                    pids.append(int(pid_str))
        except Exception:
            pass
        return pids
    
    def _add_iptables_rule(self, ip: str) -> bool:
        """Add iptables rule to block an IP."""
        try:
            # Validate IP format
            socket.inet_aton(ip)
            
            # Check if rule already exists
            check = subprocess.run(
                ["iptables", "-C", "OUTPUT", "-d", ip, "-j", "DROP"],
                capture_output=True
            )
            if check.returncode == 0:
                return True  # Already blocked
            
            # Add rule
            subprocess.run(
                ["iptables", "-A", "OUTPUT", "-d", ip, "-j", "DROP"],
                check=True, capture_output=True, timeout=10
            )
            logger.info(f"Blocked IP: {ip}")
            return True
        except Exception as e:
            logger.warning(f"Failed to block IP {ip}: {e}")
            return False
    
    def _file_references_threat(self, filepath: Path, threat: DetectedThreat) -> bool:
        """Check if a file contains references to the threat."""
        if not filepath.is_file():
            return False
        
        try:
            content = filepath.read_text(errors='ignore').lower()
            
            # Check for threat file path
            if threat.file_path and threat.file_path.lower() in content:
                return True
            
            # Check for threat process name
            if threat.process_name and threat.process_name.lower() in content:
                return True
            
            return False
        except Exception:
            return False
    
    def _safe_remove(self, path: Path):
        """Safely remove a file after backing it up."""
        try:
            backup_dir = self.quarantine_dir / "persistence_backups"
            backup_dir.mkdir(exist_ok=True)
            
            backup_path = backup_dir / f"{datetime.now().strftime('%Y%m%d_%H%M%S')}_{path.name}"
            shutil.copy2(path, backup_path)
            path.unlink()
        except Exception:
            pass


# =============================================================================
# Automatic Threat Manager
# =============================================================================

class AutoThreatManager:
    """
    Main automatic threat detection and mitigation manager.
    
    This class orchestrates all automatic threat handling:
    - Receives threats from detection engines
    - Prioritizes and queues for response
    - Executes mitigation actions
    - Tracks effectiveness and learns
    """
    
    def __init__(self, 
                 quarantine_dir: Path = None,
                 dry_run: bool = False,
                 auto_enabled: bool = True):
        self.auto_enabled = auto_enabled
        self.dry_run = dry_run
        
        # Initialize components
        self.executor = MitigationExecutor(quarantine_dir, dry_run)
        
        # Threat queues by priority
        self._threat_queues = {
            priority: queue.PriorityQueue()
            for priority in ResponsePriority
        }
        
        # Response tracking
        self.response_history: List[Dict[str, Any]] = []
        self.effectiveness_stats = defaultdict(lambda: {"success": 0, "fail": 0})
        
        # Background worker
        self._running = False
        self._workers: List[threading.Thread] = []
        self._lock = threading.Lock()
        
        # Callbacks
        self._on_threat_detected: List[Callable] = []
        self._on_threat_mitigated: List[Callable] = []
        
        # Statistics
        self.stats = {
            "threats_detected": 0,
            "threats_mitigated": 0,
            "actions_executed": 0,
            "processes_killed": 0,
            "files_quarantined": 0,
            "ips_blocked": 0,
        }
    
    def start(self):
        """Start automatic threat management."""
        if self._running:
            return
        
        self._running = True
        
        # Start priority workers
        for priority in [ResponsePriority.IMMEDIATE, ResponsePriority.HIGH, 
                         ResponsePriority.MEDIUM]:
            worker = threading.Thread(
                target=self._worker_loop,
                args=(priority,),
                daemon=True
            )
            worker.start()
            self._workers.append(worker)
        
        logger.info("AutoThreatManager started")
    
    def stop(self):
        """Stop automatic threat management."""
        self._running = False
        for worker in self._workers:
            worker.join(timeout=5)
        self._workers.clear()
        logger.info("AutoThreatManager stopped")
    
    def handle_threat(self, threat: DetectedThreat) -> Dict[str, Any]:
        """
        Handle a detected threat automatically.
        
        This is the main entry point for threat handling.
        """
        with self._lock:
            self.stats["threats_detected"] += 1
        
        # Determine priority
        priority = ResponsePolicy.get_priority(threat)
        threat.priority = priority
        
        # Trigger callbacks
        for callback in self._on_threat_detected:
            try:
                callback(threat)
            except Exception:
                pass
        
        # For immediate priority, handle synchronously
        if priority == ResponsePriority.IMMEDIATE:
            return self._handle_immediately(threat)
        
        # Queue for background processing
        self._threat_queues[priority].put((time.time(), threat))
        
        return {
            "threat_id": threat.threat_id,
            "queued": True,
            "priority": priority.name,
        }
    
    def _handle_immediately(self, threat: DetectedThreat) -> Dict[str, Any]:
        """Handle a threat immediately (for critical threats)."""
        start_time = time.time()
        
        result = {
            "threat_id": threat.threat_id,
            "category": threat.category.value,
            "severity": threat.severity,
            "actions": [],
            "success": True,
            "errors": [],
        }
        
        # Get actions for this threat
        actions = ResponsePolicy.get_actions(threat)
        
        logger.info(f"[IMMEDIATE] Handling threat: {threat.threat_id}")
        logger.info(f"  Category: {threat.category.value}")
        logger.info(f"  Severity: {threat.severity}")
        logger.info(f"  Actions: {[a.name for a in actions]}")
        
        # Execute each action
        for action in actions:
            if not self.auto_enabled and action != MitigationAction.ALERT_USER:
                continue
            
            action_result = self.executor.execute(action, threat)
            result["actions"].append(action_result)
            
            # Track stats
            with self._lock:
                self.stats["actions_executed"] += 1
                if action == MitigationAction.KILL_PROCESS:
                    self.stats["processes_killed"] += len(
                        action_result.get("killed_pids", [])
                    )
                elif action == MitigationAction.QUARANTINE_FILE:
                    if action_result.get("success"):
                        self.stats["files_quarantined"] += 1
                elif action in [MitigationAction.BLOCK_IP, MitigationAction.BLOCK_NETWORK]:
                    self.stats["ips_blocked"] += len(
                        action_result.get("blocked_ips", []) +
                        action_result.get("blocked", [])
                    )
            
            if not action_result.get("success"):
                result["errors"].append(action_result.get("error", "Unknown error"))
        
        # Track effectiveness
        effectiveness_key = (threat.category.value, threat.severity)
        if all(a.get("success", False) for a in result["actions"]):
            self.effectiveness_stats[effectiveness_key]["success"] += 1
            with self._lock:
                self.stats["threats_mitigated"] += 1
        else:
            self.effectiveness_stats[effectiveness_key]["fail"] += 1
            result["success"] = False
        
        # Calculate response time
        threat.response_time_ms = (time.time() - start_time) * 1000
        threat.actions_taken = [a["action"] for a in result["actions"]]
        threat.response_success = result["success"]
        
        # Log response
        self._log_response(threat, result)
        
        # Trigger callbacks
        for callback in self._on_threat_mitigated:
            try:
                callback(threat, result)
            except Exception:
                pass
        
        return result
    
    def _worker_loop(self, priority: ResponsePriority):
        """Background worker for processing threat queue."""
        q = self._threat_queues[priority]
        
        # Different sleep times based on priority
        sleep_times = {
            ResponsePriority.IMMEDIATE: 0.01,
            ResponsePriority.HIGH: 0.1,
            ResponsePriority.MEDIUM: 1.0,
            ResponsePriority.LOW: 5.0,
        }
        sleep_time = sleep_times.get(priority, 1.0)
        
        while self._running:
            try:
                # Non-blocking get
                _, threat = q.get(timeout=sleep_time)
                self._handle_immediately(threat)
            except queue.Empty:
                continue
            except Exception as e:
                logger.error(f"Worker error: {e}")
    
    def _log_response(self, threat: DetectedThreat, result: Dict[str, Any]):
        """Log threat response."""
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "threat_id": threat.threat_id,
            "category": threat.category.value,
            "severity": threat.severity,
            "file": threat.file_path,
            "process": threat.process_name,
            "actions": result.get("actions", []),
            "success": result.get("success", False),
            "response_time_ms": threat.response_time_ms,
        }
        
        with self._lock:
            self.response_history.append(log_entry)
            # Keep only last 1000 entries
            if len(self.response_history) > 1000:
                self.response_history = self.response_history[-1000:]
        
        logger.info(f"Response logged: {threat.threat_id} - "
                    f"{'SUCCESS' if result['success'] else 'FAILED'} "
                    f"in {threat.response_time_ms:.1f}ms")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get current statistics."""
        with self._lock:
            stats = self.stats.copy()
        
        stats["effectiveness"] = dict(self.effectiveness_stats)
        stats["blocked_ips"] = len(self.executor.blocked_ips)
        stats["quarantined_files"] = len(self.executor.quarantined_files)
        
        return stats
    
    def on_threat_detected(self, callback: Callable):
        """Register callback for threat detection."""
        self._on_threat_detected.append(callback)
    
    def on_threat_mitigated(self, callback: Callable):
        """Register callback for threat mitigation."""
        self._on_threat_mitigated.append(callback)


# =============================================================================
# Integration Helper Functions
# =============================================================================

def create_threat_from_scan(scan_result: Dict[str, Any], 
                            file_path: str = None) -> DetectedThreat:
    """Create a DetectedThreat from a scan result."""
    severity = scan_result.get("severity", "medium")
    threat_name = scan_result.get("name", "Unknown")
    
    # Determine category
    category = ThreatCategory.MALWARE
    name_lower = threat_name.lower()
    
    if "ransom" in name_lower or "encrypt" in name_lower:
        category = ThreatCategory.RANSOMWARE
    elif "miner" in name_lower or "crypto" in name_lower or "xmrig" in name_lower:
        category = ThreatCategory.CRYPTOMINER
    elif "backdoor" in name_lower or "rat" in name_lower:
        category = ThreatCategory.BACKDOOR
    elif "rootkit" in name_lower:
        category = ThreatCategory.ROOTKIT
    elif "worm" in name_lower:
        category = ThreatCategory.WORM
    elif "pup" in name_lower or "adware" in name_lower:
        category = ThreatCategory.PUP
    elif "trojan" in name_lower:
        category = ThreatCategory.MALWARE
    
    return DetectedThreat(
        threat_id=threat_name,
        category=category,
        severity=severity,
        confidence=scan_result.get("confidence", 100.0),
        priority=ResponsePriority.MEDIUM,
        detection_source="scan",
        detection_rule=scan_result.get("rule", ""),
        file_path=file_path,
        file_hash=scan_result.get("hash"),
        mitre_techniques=[scan_result.get("mitre", "")],
    )


def create_threat_from_behavior(behavior_alert: Dict[str, Any]) -> DetectedThreat:
    """Create a DetectedThreat from behavioral detection."""
    alert_type = behavior_alert.get("type", "unknown").lower()
    
    # Map behavior types to categories
    category_map = {
        "ransomware": ThreatCategory.RANSOMWARE,
        "cryptominer": ThreatCategory.CRYPTOMINER,
        "c2_beacon": ThreatCategory.C2_COMMUNICATION,
        "data_exfil": ThreatCategory.DATA_EXFIL,
        "lateral_movement": ThreatCategory.LATERAL_MOVEMENT,
        "process_injection": ThreatCategory.MALWARE,
        "persistence": ThreatCategory.BACKDOOR,
    }
    
    category = category_map.get(alert_type, ThreatCategory.SUSPICIOUS)
    
    # Map severity
    severity = behavior_alert.get("severity", "medium")
    if behavior_alert.get("confidence", 0) >= 90:
        severity = "high" if severity == "medium" else severity
    
    return DetectedThreat(
        threat_id=f"Behavior_{alert_type}_{datetime.now().strftime('%H%M%S')}",
        category=category,
        severity=severity,
        confidence=behavior_alert.get("confidence", 75.0),
        priority=ResponsePriority.HIGH if severity in ["critical", "high"] else ResponsePriority.MEDIUM,
        detection_source="behavioral",
        process_id=behavior_alert.get("pid"),
        process_name=behavior_alert.get("process"),
        network_connections=behavior_alert.get("connections", []),
        remote_ips=behavior_alert.get("remote_ips", []),
        mitre_techniques=behavior_alert.get("mitre", []),
    )


# =============================================================================
# Singleton Instance
# =============================================================================

_manager_instance: Optional[AutoThreatManager] = None


def get_threat_manager(quarantine_dir: Path = None,
                       dry_run: bool = False) -> AutoThreatManager:
    """Get or create the global AutoThreatManager instance."""
    global _manager_instance
    
    if _manager_instance is None:
        _manager_instance = AutoThreatManager(
            quarantine_dir=quarantine_dir,
            dry_run=dry_run
        )
    
    return _manager_instance


# =============================================================================
# Main - Test/Demo
# =============================================================================

if __name__ == "__main__":
    print("=" * 60)
    print("AutoThreatManager - Automatic Threat Detection & Mitigation")
    print("=" * 60)

    # Use local quarantine directory for testing
    test_quarantine = Path(__file__).parent / "quarantine"
    test_quarantine.mkdir(parents=True, exist_ok=True)

    # Create manager in dry-run mode for testing
    manager = AutoThreatManager(quarantine_dir=test_quarantine, dry_run=True)
    manager.start()
    
    # Simulate various threats
    test_threats = [
        DetectedThreat(
            threat_id="Ransomware.WannaCry",
            category=ThreatCategory.RANSOMWARE,
            severity="critical",
            confidence=95.0,
            priority=ResponsePriority.IMMEDIATE,
            file_path="/tmp/malware.exe",
            process_id=12345,
        ),
        DetectedThreat(
            threat_id="Miner.XMRig",
            category=ThreatCategory.CRYPTOMINER,
            severity="high",
            confidence=85.0,
            priority=ResponsePriority.HIGH,
            file_path="/tmp/miner",
            remote_ips=["192.168.1.100"],
        ),
        DetectedThreat(
            threat_id="Suspicious.Script",
            category=ThreatCategory.SUSPICIOUS,
            severity="medium",
            confidence=60.0,
            priority=ResponsePriority.MEDIUM,
            file_path="/tmp/script.sh",
        ),
    ]
    
    print("\n[*] Testing threat handling...\n")
    
    for threat in test_threats:
        print(f"\n--- Handling: {threat.threat_id} ---")
        result = manager.handle_threat(threat)
        print(f"Result: {json.dumps(result, indent=2, default=str)}")
    
    # Wait for background processing
    time.sleep(2)
    
    # Print stats
    print("\n" + "=" * 60)
    print("Statistics:")
    print("=" * 60)
    stats = manager.get_stats()
    for key, value in stats.items():
        if key != "effectiveness":
            print(f"  {key}: {value}")
    
    manager.stop()
    print("\n[✓] Test complete")
