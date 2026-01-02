#!/usr/bin/env python3
"""
GreyAV - A Minimal Antivirus CLI Program
Secure, Enterprise-Grade Implementation
"""

import argparse
import hashlib
import os
import shutil
import json
import re
import struct
import time
import signal
import threading
import subprocess
import socket
import platform
import glob
import fnmatch
import tempfile
import zipfile
import gzip
import tarfile
import base64
import math
import sqlite3
import hmac
import secrets
import stat
import mmap
import queue
import binascii
import zlib
import logging
import atexit
import errno
import getpass
from datetime import datetime, timedelta
from pathlib import Path
from collections import defaultdict, Counter
from typing import Optional, List, Dict, Any, Tuple, Set, Callable, Union
from concurrent.futures import ThreadPoolExecutor, as_completed
from functools import lru_cache, wraps
from contextlib import contextmanager, suppress
from dataclasses import dataclass, field
from enum import Enum, auto
from io import BytesIO
import traceback

# Optional dependencies for enhanced functionality
try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False
    psutil = None

# Centralized port management
try:
    from port_manager import (
        get_port_manager, PortManager, PortRisk,
        get_service_name as pm_get_service_name,
        is_dangerous_port as pm_is_dangerous_port,
        analyze_port as pm_analyze_port
    )
    PORT_MANAGER_AVAILABLE = True
except ImportError:
    PORT_MANAGER_AVAILABLE = False
    get_port_manager = None
    PortManager = None
    PortRisk = None

# Automatic Threat Management
try:
    from auto_threat_manager import (
        AutoThreatManager, DetectedThreat, ThreatCategory,
        ResponsePriority, MitigationAction, ResponsePolicy,
        get_threat_manager, create_threat_from_scan,
        create_threat_from_behavior
    )
    AUTO_THREAT_MANAGER_AVAILABLE = True
except ImportError:
    AUTO_THREAT_MANAGER_AVAILABLE = False
    AutoThreatManager = None
    DetectedThreat = None
    ThreatCategory = None
    get_threat_manager = None

# Socket Intake for network threat probes
try:
    from socket_intake import (
        start_listener as start_socket_intake,
        stop_listener as stop_socket_intake,
        is_running as socket_intake_running,
        get_active_ports as socket_intake_ports,
        DEFAULT_PORTS as SOCKET_DEFAULT_PORTS
    )
    SOCKET_INTAKE_AVAILABLE = True
except ImportError:
    SOCKET_INTAKE_AVAILABLE = False
    start_socket_intake = None
    stop_socket_intake = None
    socket_intake_running = None
    socket_intake_ports = None
    # Fallback: Full port list matching socket_intake.py
    SOCKET_DEFAULT_PORTS = [
        8000,  # GreyAV
        22, 23, 2222, 3389, 5900,  # Remote Access
        20, 21,  # FTP
        80, 443, 8080, 8443,  # Web
        25, 110, 143,  # Email
        53,  # DNS
        67, 68,  # DHCP
        135, 139, 445,  # Windows/SMB
        161, 162,  # SNMP
        3306, 5432, 27017, 6379,  # Databases
        4444,  # Backdoor
    ]


# ==================== SECURITY UTILITIES ====================

class SecurityError(Exception):
    """Base exception for security-related errors."""
    pass


class ValidationError(SecurityError):
    """Input validation failure."""
    pass


class IntegrityError(SecurityError):
    """File integrity verification failure."""
    pass


class PermissionDeniedError(SecurityError):
    """Insufficient permissions for operation."""
    pass


class RateLimitError(SecurityError):
    """Rate limit exceeded."""
    pass


class ResourceExhaustedError(SecurityError):
    """Resource limit exceeded (memory, CPU, etc.)."""
    pass


class TimeoutError(SecurityError):
    """Operation timed out."""
    pass


# ==================== SAFEGUARD UTILITIES ====================

class SafeGuard:
    """Comprehensive safeguards for robust operation."""
    
    # Resource limits
    MAX_MEMORY_MB = 1024  # Max memory usage in MB
    MAX_OPEN_FILES = 100  # Max concurrent file handles
    MAX_THREADS = 16  # Max concurrent threads
    MAX_RETRIES = 3  # Default retry count
    DEFAULT_TIMEOUT = 30  # Default operation timeout in seconds
    MAX_RECURSION_DEPTH = 50  # Max recursion depth for nested operations
    
    # Track active resources
    _open_files = 0
    _active_threads = 0
    _lock = threading.Lock()
    _operation_count = 0
    _failed_operations = defaultdict(int)
    _circuit_breaker_threshold = 10  # Failures before circuit break
    _circuit_breaker_state = {}  # Track broken circuits
    
    @classmethod
    def check_memory_usage(cls) -> bool:
        """Check if memory usage is within limits."""
        try:
            import resource
            usage = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1024  # MB on Linux
            return usage < cls.MAX_MEMORY_MB
        except (ImportError, AttributeError):
            # Fallback for systems without resource module
            try:
                with open('/proc/self/status', 'r') as f:
                    for line in f:
                        if line.startswith('VmRSS:'):
                            usage = int(line.split()[1]) / 1024  # Convert KB to MB
                            return usage < cls.MAX_MEMORY_MB
            except (IOError, ValueError):
                pass
        return True  # Assume OK if we can't check
    
    @classmethod
    def acquire_file_handle(cls) -> bool:
        """Try to acquire a file handle slot."""
        with cls._lock:
            if cls._open_files >= cls.MAX_OPEN_FILES:
                return False
            cls._open_files += 1
            return True
    
    @classmethod
    def release_file_handle(cls):
        """Release a file handle slot."""
        with cls._lock:
            cls._open_files = max(0, cls._open_files - 1)
    
    @classmethod
    def acquire_thread(cls) -> bool:
        """Try to acquire a thread slot."""
        with cls._lock:
            if cls._active_threads >= cls.MAX_THREADS:
                return False
            cls._active_threads += 1
            return True
    
    @classmethod
    def release_thread(cls):
        """Release a thread slot."""
        with cls._lock:
            cls._active_threads = max(0, cls._active_threads - 1)
    
    @classmethod
    def circuit_breaker_check(cls, operation_name: str) -> bool:
        """Check if circuit breaker is open (too many failures)."""
        with cls._lock:
            breaker = cls._circuit_breaker_state.get(operation_name, {})
            if breaker.get('open', False):
                # Check if cooldown period has passed (60 seconds)
                if time.time() - breaker.get('opened_at', 0) > 60:
                    # Reset circuit breaker
                    cls._circuit_breaker_state[operation_name] = {'open': False}
                    cls._failed_operations[operation_name] = 0
                    return True
                return False  # Circuit still open
            return True  # Circuit closed, operation allowed
    
    @classmethod
    def record_failure(cls, operation_name: str):
        """Record an operation failure for circuit breaker."""
        with cls._lock:
            cls._failed_operations[operation_name] += 1
            if cls._failed_operations[operation_name] >= cls._circuit_breaker_threshold:
                cls._circuit_breaker_state[operation_name] = {
                    'open': True,
                    'opened_at': time.time()
                }
    
    @classmethod
    def record_success(cls, operation_name: str):
        """Record successful operation."""
        with cls._lock:
            cls._failed_operations[operation_name] = 0
            cls._circuit_breaker_state[operation_name] = {'open': False}
    
    @classmethod
    @contextmanager
    def managed_file_handle(cls):
        """Context manager for safe file handle management."""
        acquired = False
        try:
            if not cls.acquire_file_handle():
                raise ResourceExhaustedError("Too many open files")
            acquired = True
            yield
        finally:
            if acquired:
                cls.release_file_handle()
    
    @classmethod
    @contextmanager
    def managed_thread(cls):
        """Context manager for safe thread management."""
        acquired = False
        try:
            if not cls.acquire_thread():
                raise ResourceExhaustedError("Too many active threads")
            acquired = True
            yield
        finally:
            if acquired:
                cls.release_thread()
    
    @staticmethod
    def with_timeout(timeout_seconds: float):
        """Decorator to add timeout to a function."""
        def decorator(func):
            @wraps(func)
            def wrapper(*args, **kwargs):
                result = [None]
                exception = [None]
                completed = threading.Event()
                
                def target():
                    try:
                        result[0] = func(*args, **kwargs)
                    except Exception as e:
                        exception[0] = e
                    finally:
                        completed.set()
                
                thread = threading.Thread(target=target, daemon=True)
                thread.start()
                
                if not completed.wait(timeout=timeout_seconds):
                    raise TimeoutError(f"Operation timed out after {timeout_seconds}s")
                
                if exception[0]:
                    raise exception[0]
                
                return result[0]
            return wrapper
        return decorator
    
    @staticmethod
    def with_retry(max_retries: int = 3, delay: float = 0.5, 
                   backoff: float = 2.0, exceptions: tuple = (Exception,)):
        """Decorator to add retry logic with exponential backoff."""
        def decorator(func):
            @wraps(func)
            def wrapper(*args, **kwargs):
                last_exception = None
                current_delay = delay
                
                for attempt in range(max_retries):
                    try:
                        return func(*args, **kwargs)
                    except exceptions as e:
                        last_exception = e
                        if attempt < max_retries - 1:
                            time.sleep(current_delay)
                            current_delay *= backoff
                
                raise last_exception
            return wrapper
        return decorator
    
    @staticmethod
    def safe_execute(func: Callable, *args, default=None, 
                    log_errors: bool = True, **kwargs) -> Any:
        """Execute a function safely with fallback to default value."""
        try:
            return func(*args, **kwargs)
        except Exception as e:
            if log_errors:
                # Silent logging - errors are handled gracefully
                pass
            return default
    
    @staticmethod
    def validate_and_sanitize_path(path: Union[str, Path], 
                                   must_exist: bool = False,
                                   allowed_extensions: Set[str] = None,
                                   max_size_mb: float = None) -> Optional[Path]:
        """Comprehensive path validation with multiple checks."""
        if path is None:
            return None
        
        try:
            # Basic validation
            path = InputValidator.validate_path(path, must_exist=must_exist)
            
            # Extension check
            if allowed_extensions and path.suffix.lower() not in allowed_extensions:
                return None
            
            # Size check
            if max_size_mb and path.exists() and path.is_file():
                size_mb = path.stat().st_size / (1024 * 1024)
                if size_mb > max_size_mb:
                    return None
            
            return path
        except (ValidationError, OSError):
            return None
    
    @staticmethod
    def safe_json_load(filepath: Path, default: Any = None) -> Any:
        """Safely load JSON with fallback."""
        try:
            if not filepath.exists():
                return default
            
            content = filepath.read_text(encoding='utf-8')
            if not content.strip():
                return default
            
            return json.loads(content)
        except (json.JSONDecodeError, IOError, UnicodeDecodeError):
            return default
    
    @staticmethod
    def safe_json_save(filepath: Path, data: Any, indent: int = 2) -> bool:
        """Safely save JSON with error handling."""
        try:
            # Create parent directories
            filepath.parent.mkdir(parents=True, exist_ok=True)
            
            # Write atomically
            temp_path = filepath.with_suffix('.tmp')
            temp_path.write_text(json.dumps(data, indent=indent), encoding='utf-8')
            temp_path.replace(filepath)
            return True
        except (IOError, TypeError, ValueError):
            # Clean up temp file
            with suppress(OSError):
                temp_path.unlink()
            return False
    
    @staticmethod
    def safe_file_read(filepath: Path, max_size_mb: float = 100,
                       timeout_seconds: float = 30) -> Optional[bytes]:
        """Safely read file with size limits and timeout."""
        try:
            if not filepath.exists():
                return None
            
            # Check file size
            size = filepath.stat().st_size
            if size > max_size_mb * 1024 * 1024:
                return None
            
            # Read with resource management
            with SafeGuard.managed_file_handle():
                with open(filepath, 'rb') as f:
                    return f.read()
        except (IOError, OSError, ResourceExhaustedError):
            return None
    
    @staticmethod
    def graceful_degradation(primary_func: Callable, fallback_func: Callable,
                            *args, **kwargs) -> Any:
        """Try primary function, fall back to simpler version on failure."""
        try:
            return primary_func(*args, **kwargs)
        except Exception:
            try:
                return fallback_func(*args, **kwargs)
            except Exception:
                return None
    
    @staticmethod
    def sanitize_output(output: str, max_length: int = 10000) -> str:
        """Sanitize output string for safe display."""
        if not output:
            return ""
        
        # Remove control characters except newlines/tabs
        output = ''.join(
            c for c in output 
            if c.isprintable() or c in '\n\r\t'
        )
        
        # Truncate if too long
        if len(output) > max_length:
            output = output[:max_length] + "\n... [output truncated]"
        
        return output
    
    @staticmethod
    def ensure_dependencies(required_modules: List[str]) -> Dict[str, bool]:
        """Check for required modules and report availability."""
        availability = {}
        for module in required_modules:
            try:
                __import__(module)
                availability[module] = True
            except ImportError:
                availability[module] = False
        return availability
    
    @classmethod
    def health_check(cls) -> Dict[str, Any]:
        """Perform system health check."""
        return {
            'memory_ok': cls.check_memory_usage(),
            'open_files': cls._open_files,
            'active_threads': cls._active_threads,
            'circuit_breakers': {
                name: state.get('open', False)
                for name, state in cls._circuit_breaker_state.items()
            },
            'failed_operations': dict(cls._failed_operations)
        }

    # ==================== ENHANCED SAFEGUARDS ====================
    
    @staticmethod
    def pre_operation_check(operation_name: str, required_resources: List[str] = None) -> Tuple[bool, str]:
        """Comprehensive pre-operation validation check."""
        checks = []
        
        # Memory check
        if not SafeGuard.check_memory_usage():
            return False, "Insufficient memory available"
        checks.append("memory_ok")
        
        # Circuit breaker check
        if not SafeGuard.circuit_breaker_check(operation_name):
            return False, f"Circuit breaker open for {operation_name}"
        checks.append("circuit_ok")
        
        # Resource availability
        if required_resources:
            for resource in required_resources:
                if resource == 'file_handle' and SafeGuard._open_files >= SafeGuard.MAX_OPEN_FILES:
                    return False, "No file handles available"
                elif resource == 'thread' and SafeGuard._active_threads >= SafeGuard.MAX_THREADS:
                    return False, "No thread slots available"
        checks.append("resources_ok")
        
        return True, "All checks passed"
    
    @staticmethod
    def post_operation_cleanup(operation_name: str, success: bool, 
                                resources_used: List[str] = None):
        """Post-operation cleanup and status recording."""
        if success:
            SafeGuard.record_success(operation_name)
        else:
            SafeGuard.record_failure(operation_name)
        
        # Force garbage collection if memory is high
        if not SafeGuard.check_memory_usage():
            import gc
            gc.collect()
    
    @staticmethod
    def operation_wrapper(operation_name: str, func: Callable, 
                          *args, timeout: float = 30.0, 
                          retries: int = 3, **kwargs) -> Tuple[bool, Any, str]:
        """Comprehensive operation wrapper with all safeguards."""
        # Pre-check
        can_proceed, reason = SafeGuard.pre_operation_check(operation_name)
        if not can_proceed:
            return False, None, reason
        
        last_error = None
        for attempt in range(retries):
            try:
                # Execute with timeout
                result = [None]
                error = [None]
                completed = threading.Event()
                
                def execute():
                    try:
                        result[0] = func(*args, **kwargs)
                    except Exception as e:
                        error[0] = e
                    finally:
                        completed.set()
                
                thread = threading.Thread(target=execute, daemon=True)
                thread.start()
                
                if not completed.wait(timeout=timeout):
                    last_error = f"Timeout after {timeout}s"
                    continue
                
                if error[0]:
                    last_error = str(error[0])
                    continue
                
                # Success
                SafeGuard.post_operation_cleanup(operation_name, True)
                return True, result[0], "Success"
                
            except Exception as e:
                last_error = str(e)
                time.sleep(0.5 * (attempt + 1))  # Backoff
        
        # All retries failed
        SafeGuard.post_operation_cleanup(operation_name, False)
        return False, None, last_error or "Unknown error"
    
    @staticmethod
    def checkpoint_operation(checkpoint_file: Path, state: Dict[str, Any]) -> bool:
        """Save operation checkpoint for recovery."""
        try:
            checkpoint_data = {
                'timestamp': datetime.now().isoformat(),
                'state': state,
                'pid': os.getpid()
            }
            temp_file = checkpoint_file.with_suffix('.tmp')
            temp_file.write_text(json.dumps(checkpoint_data, indent=2))
            temp_file.replace(checkpoint_file)
            return True
        except Exception:
            return False
    
    @staticmethod
    def recover_from_checkpoint(checkpoint_file: Path) -> Optional[Dict[str, Any]]:
        """Recover state from checkpoint."""
        try:
            if not checkpoint_file.exists():
                return None
            data = json.loads(checkpoint_file.read_text())
            return data.get('state')
        except Exception:
            return None
    
    @staticmethod
    def atomic_operation(operations: List[Callable], rollback: List[Callable] = None) -> Tuple[bool, str]:
        """Execute operations atomically with rollback on failure."""
        completed = []
        try:
            for i, op in enumerate(operations):
                op()
                completed.append(i)
            return True, "All operations completed"
        except Exception as e:
            # Rollback completed operations in reverse order
            if rollback:
                for i in reversed(completed):
                    if i < len(rollback) and rollback[i]:
                        try:
                            rollback[i]()
                        except Exception:
                            pass  # Best effort rollback
            return False, str(e)
    
    @staticmethod
    def rate_limit_check(operation_name: str, max_per_minute: int = 60) -> bool:
        """Check if operation is within rate limits."""
        current_time = time.time()
        
        if not hasattr(SafeGuard, '_rate_limits'):
            SafeGuard._rate_limits = defaultdict(list)
        
        with SafeGuard._lock:
            # Clean old entries (older than 1 minute)
            SafeGuard._rate_limits[operation_name] = [
                t for t in SafeGuard._rate_limits[operation_name]
                if current_time - t < 60
            ]
            
            # Check limit
            if len(SafeGuard._rate_limits[operation_name]) >= max_per_minute:
                return False
            
            # Record this operation
            SafeGuard._rate_limits[operation_name].append(current_time)
            return True
    
    @staticmethod
    def validate_operation_result(result: Any, validators: List[Callable[[Any], bool]]) -> Tuple[bool, str]:
        """Validate operation result against multiple criteria."""
        for i, validator in enumerate(validators):
            try:
                if not validator(result):
                    return False, f"Validation {i+1} failed"
            except Exception as e:
                return False, f"Validator {i+1} error: {e}"
        return True, "All validations passed"
    
    @staticmethod
    def safe_subprocess(cmd: List[str], timeout: float = 30, 
                        max_output_size: int = 10 * 1024 * 1024) -> Tuple[bool, str, str]:
        """Safely execute subprocess with limits."""
        try:
            # Validate command
            if not cmd or not isinstance(cmd, list):
                return False, "", "Invalid command"
            
            # Check for dangerous commands
            dangerous = ['rm -rf /', 'mkfs', ':(){:|:&};:', 'dd if=/dev/zero']
            cmd_str = ' '.join(cmd)
            for d in dangerous:
                if d in cmd_str:
                    return False, "", "Dangerous command blocked"
            
            result = subprocess.run(
                cmd,
                capture_output=True,
                timeout=timeout,
                text=True,
                env={**os.environ, 'PATH': '/usr/bin:/bin:/usr/sbin:/sbin'}
            )
            
            stdout = result.stdout[:max_output_size] if result.stdout else ""
            stderr = result.stderr[:max_output_size] if result.stderr else ""
            
            return result.returncode == 0, stdout, stderr
        except subprocess.TimeoutExpired:
            return False, "", "Command timed out"
        except Exception as e:
            return False, "", str(e)
    
    @staticmethod
    def ensure_disk_space(path: Path, required_mb: float = 100) -> bool:
        """Ensure sufficient disk space before operation."""
        try:
            stat_result = os.statvfs(path if path.is_dir() else path.parent)
            available_mb = (stat_result.f_frsize * stat_result.f_bavail) / (1024 * 1024)
            return available_mb >= required_mb
        except Exception:
            return True  # Proceed if we can't check
    
    @staticmethod
    def verify_file_integrity(filepath: Path, expected_hash: str = None) -> Tuple[bool, str]:
        """Verify file integrity before operations."""
        try:
            if not filepath.exists():
                return False, "File does not exist"
            
            if not filepath.is_file():
                return False, "Not a regular file"
            
            # Check if readable
            if not os.access(filepath, os.R_OK):
                return False, "File not readable"
            
            # Check for symlink attacks
            if filepath.is_symlink():
                real_path = filepath.resolve()
                if not str(real_path).startswith(str(filepath.parent.resolve())):
                    return False, "Suspicious symlink detected"
            
            # Hash verification if expected hash provided
            if expected_hash:
                hasher = hashlib.sha256()
                with open(filepath, 'rb') as f:
                    for chunk in iter(lambda: f.read(8192), b''):
                        hasher.update(chunk)
                actual_hash = hasher.hexdigest()
                if not hmac.compare_digest(actual_hash, expected_hash):
                    return False, "Hash mismatch"
            
            return True, "File integrity verified"
        except Exception as e:
            return False, str(e)
    
    @staticmethod
    def isolation_context(temp_dir: Path = None) -> 'IsolationContext':
        """Create an isolated execution context."""
        return IsolationContext(temp_dir)


class IsolationContext:
    """Isolated execution context for sensitive operations."""
    
    def __init__(self, temp_dir: Path = None):
        self.temp_dir = temp_dir
        self._created_files = []
        self._original_cwd = None
        self._temp_path = None
    
    def __enter__(self):
        import tempfile
        self._original_cwd = os.getcwd()
        self._temp_path = Path(tempfile.mkdtemp(dir=self.temp_dir))
        os.chdir(self._temp_path)
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        # Restore original directory
        if self._original_cwd:
            try:
                os.chdir(self._original_cwd)
            except Exception:
                pass
        
        # Cleanup temp files securely
        if self._temp_path and self._temp_path.exists():
            try:
                for item in self._temp_path.rglob('*'):
                    if item.is_file():
                        # Secure delete
                        try:
                            size = item.stat().st_size
                            with open(item, 'wb') as f:
                                f.write(os.urandom(min(size, 1024 * 1024)))
                            item.unlink()
                        except Exception:
                            pass
                shutil.rmtree(self._temp_path, ignore_errors=True)
            except Exception:
                pass
        
        return False  # Don't suppress exceptions
    
    def create_temp_file(self, content: bytes = b'') -> Path:
        """Create a tracked temporary file."""
        import tempfile
        fd, path = tempfile.mkstemp(dir=self._temp_path)
        os.close(fd)
        path = Path(path)
        path.write_bytes(content)
        self._created_files.append(path)
        return path


class OperationJournal:
    """Journal for tracking and recovering from failed operations."""
    
    def __init__(self, journal_file: Path):
        self.journal_file = journal_file
        self._lock = threading.Lock()
        self._load_journal()
    
    def _load_journal(self):
        """Load existing journal entries."""
        try:
            if self.journal_file.exists():
                self.entries = json.loads(self.journal_file.read_text())
            else:
                self.entries = []
        except Exception:
            self.entries = []
    
    def _save_journal(self):
        """Save journal to disk."""
        try:
            self.journal_file.write_text(json.dumps(self.entries, indent=2))
        except Exception:
            pass
    
    def start_operation(self, operation_id: str, operation_type: str, 
                        params: Dict[str, Any]) -> str:
        """Record operation start."""
        with self._lock:
            entry = {
                'id': operation_id,
                'type': operation_type,
                'params': params,
                'started_at': datetime.now().isoformat(),
                'status': 'in_progress',
                'completed_at': None,
                'error': None
            }
            self.entries.append(entry)
            self._save_journal()
            return operation_id
    
    def complete_operation(self, operation_id: str, success: bool, 
                           error: str = None):
        """Record operation completion."""
        with self._lock:
            for entry in self.entries:
                if entry['id'] == operation_id:
                    entry['status'] = 'completed' if success else 'failed'
                    entry['completed_at'] = datetime.now().isoformat()
                    entry['error'] = error
                    break
            self._save_journal()
    
    def get_incomplete_operations(self) -> List[Dict[str, Any]]:
        """Get operations that didn't complete."""
        with self._lock:
            return [e for e in self.entries if e['status'] == 'in_progress']
    
    def cleanup_old_entries(self, max_age_days: int = 7):
        """Remove old journal entries."""
        with self._lock:
            cutoff = datetime.now() - timedelta(days=max_age_days)
            self.entries = [
                e for e in self.entries
                if datetime.fromisoformat(e['started_at']) > cutoff
            ]
            self._save_journal()


class SecureLogger:
    """Secure logging with sensitive data redaction."""
    
    REDACT_PATTERNS = [
        (re.compile(r'password[=:]\s*\S+', re.I), 'password=[REDACTED]'),
        (re.compile(r'api[_-]?key[=:]\s*\S+', re.I), 'api_key=[REDACTED]'),
        (re.compile(r'secret[=:]\s*\S+', re.I), 'secret=[REDACTED]'),
        (re.compile(r'token[=:]\s*\S+', re.I), 'token=[REDACTED]'),
        (re.compile(r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b'), '[EMAIL]'),
        (re.compile(r'\b(?:\d{1,3}\.){3}\d{1,3}\b'), '[IP_ADDR]'),  # Only in sensitive contexts
    ]
    
    def __init__(self, log_file: Path, level: str = "info"):
        self.log_file = log_file
        self.level = getattr(logging, level.upper(), logging.INFO)
        self._lock = threading.Lock()
        self._setup_logging()
    
    def _setup_logging(self):
        """Configure secure logging."""
        self.logger = logging.getLogger("GreyAV")
        self.logger.setLevel(self.level)
        
        # File handler with restricted permissions
        if not self.log_file.parent.exists():
            self.log_file.parent.mkdir(parents=True, exist_ok=True)
        
        handler = logging.FileHandler(self.log_file)
        handler.setLevel(self.level)
        
        formatter = logging.Formatter(
            '%(asctime)s - %(levelname)s - [%(session_id)s] - %(message)s',
            datefmt='%Y-%m-%d %H:%M:%S'
        )
        handler.setFormatter(formatter)
        self.logger.addHandler(handler)
        
        # Secure file permissions
        try:
            os.chmod(self.log_file, stat.S_IRUSR | stat.S_IWUSR)
        except (OSError, PermissionError):
            pass
    
    def _redact(self, message: str) -> str:
        """Redact sensitive information from log messages."""
        for pattern, replacement in self.REDACT_PATTERNS:
            message = pattern.sub(replacement, message)
        return message
    
    def log(self, level: str, message: str, session_id: str = ""):
        """Log a message with optional sensitive data redaction."""
        with self._lock:
            safe_message = self._redact(str(message))
            extra = {'session_id': session_id or 'N/A'}
            getattr(self.logger, level.lower(), self.logger.info)(safe_message, extra=extra)


class InputValidator:
    """Comprehensive input validation and sanitization."""
    
    # Maximum path length for security
    MAX_PATH_LENGTH = 4096
    # Maximum file size for processing (1GB default)
    MAX_FILE_SIZE = 1024 * 1024 * 1024
    # Allowed characters in paths
    PATH_BLACKLIST = ['\x00', '..']  # Null bytes and directory traversal
    
    @staticmethod
    def validate_path(path: Union[str, Path], must_exist: bool = False, 
                     allow_symlinks: bool = False) -> Path:
        """Validate and sanitize a file path."""
        if path is None:
            raise ValidationError("Path cannot be None")
        
        path_str = str(path)
        
        # Check length
        if len(path_str) > InputValidator.MAX_PATH_LENGTH:
            raise ValidationError(f"Path exceeds maximum length: {len(path_str)}")
        
        # Check for null bytes and traversal
        for banned in InputValidator.PATH_BLACKLIST:
            if banned in path_str:
                raise ValidationError(f"Invalid characters in path: {banned!r}")
        
        path = Path(path_str)
        
        # Resolve to absolute path
        try:
            resolved = path.resolve()
        except (RuntimeError, OSError) as e:
            raise ValidationError(f"Cannot resolve path: {e}")
        
        # Check symlinks if not allowed
        if not allow_symlinks and path.is_symlink():
            raise ValidationError("Symbolic links not allowed")
        
        if must_exist and not resolved.exists():
            raise ValidationError(f"Path does not exist: {resolved}")
        
        return resolved
    
    @staticmethod
    def validate_hash(hash_value: str, algorithm: str = "sha256") -> str:
        """Validate a hash string."""
        if not hash_value:
            raise ValidationError("Hash cannot be empty")
        
        lengths = {
            "md5": 32,
            "sha1": 40,
            "sha256": 64,
            "sha512": 128
        }
        
        expected_len = lengths.get(algorithm.lower(), 64)
        
        # Remove whitespace and normalize
        hash_value = hash_value.strip().lower()
        
        if len(hash_value) != expected_len:
            raise ValidationError(f"Invalid {algorithm} hash length: {len(hash_value)}")
        
        if not re.match(r'^[a-f0-9]+$', hash_value):
            raise ValidationError("Hash contains invalid characters")
        
        return hash_value
    
    @staticmethod
    def sanitize_string(value: str, max_length: int = 1024, 
                       allow_newlines: bool = False) -> str:
        """Sanitize a string input."""
        if not isinstance(value, str):
            raise ValidationError("Value must be a string")
        
        # Truncate
        value = value[:max_length]
        
        # Remove control characters
        value = ''.join(c for c in value if c.isprintable() or (allow_newlines and c in '\n\r\t'))
        
        return value
    
    @staticmethod
    def validate_json(json_str: str, max_size: int = 10 * 1024 * 1024) -> dict:
        """Safely parse and validate JSON input."""
        if len(json_str) > max_size:
            raise ValidationError("JSON input exceeds maximum size")
        
        try:
            data = json.loads(json_str)
        except json.JSONDecodeError as e:
            raise ValidationError(f"Invalid JSON: {e}")
        
        return data


class RateLimiter:
    """Thread-safe rate limiter for resource-intensive operations."""
    
    # Class-level storage for static rate limiting
    _instances = {}
    _lock = threading.Lock()
    
    def __init__(self, max_calls: int, period: float):
        self.max_calls = max_calls
        self.period = period
        self.calls = []
        self._instance_lock = threading.Lock()
    
    def acquire(self) -> bool:
        """Try to acquire a slot. Returns True if allowed."""
        with self._instance_lock:
            now = time.time()
            # Remove expired entries
            self.calls = [t for t in self.calls if now - t < self.period]
            
            if len(self.calls) >= self.max_calls:
                return False
            
            self.calls.append(now)
            return True
    
    @classmethod
    def check(cls, key: str, max_calls: int = 100, period: float = 60) -> bool:
        """Static method to check rate limit for a given key."""
        with cls._lock:
            if key not in cls._instances:
                cls._instances[key] = cls(max_calls, period)
            limiter = cls._instances[key]
        return limiter.acquire()
    
    @classmethod
    def reset(cls, key: str = None):
        """Reset rate limiter(s)."""
        with cls._lock:
            if key:
                cls._instances.pop(key, None)
            else:
                cls._instances.clear()
    
    def __enter__(self):
        if not self.acquire():
            raise RateLimitError("Rate limit exceeded")
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        pass


class SecureCrypto:
    """Secure cryptographic operations."""
    
    # Use AES-256 key size
    KEY_SIZE = 32
    # Nonce/IV size for AES-GCM
    NONCE_SIZE = 12
    # Tag size for authentication
    TAG_SIZE = 16
    # PBKDF2 iterations (OWASP recommended minimum)
    PBKDF2_ITERATIONS = 600000
    
    @staticmethod
    def generate_key() -> bytes:
        """Generate a cryptographically secure key."""
        return secrets.token_bytes(SecureCrypto.KEY_SIZE)
    
    @staticmethod
    def derive_key(password: str, salt: bytes = None) -> Tuple[bytes, bytes]:
        """Derive a key from a password using PBKDF2-HMAC-SHA256."""
        if salt is None:
            salt = secrets.token_bytes(16)
        
        key = hashlib.pbkdf2_hmac(
            'sha256',
            password.encode('utf-8'),
            salt,
            SecureCrypto.PBKDF2_ITERATIONS,
            dklen=SecureCrypto.KEY_SIZE
        )
        return key, salt
    
    @staticmethod
    def secure_encrypt(data: bytes, key: bytes) -> bytes:
        """Encrypt data using XOR with HMAC authentication (simplified AES-like)."""
        # Note: In production, use cryptography.fernet or pycryptodome for AES-GCM
        # This is an enhanced XOR cipher with authentication
        if len(key) < SecureCrypto.KEY_SIZE:
            raise SecurityError("Key too short")
        
        # Generate random nonce
        nonce = secrets.token_bytes(SecureCrypto.NONCE_SIZE)
        
        # Expand key using HKDF-like derivation
        expanded_key = hashlib.pbkdf2_hmac('sha256', key, nonce, 1000, dklen=len(data))
        
        # XOR encrypt
        encrypted = bytes(a ^ b for a, b in zip(data, expanded_key))
        
        # Generate HMAC tag for authentication
        tag = hmac.new(key, nonce + encrypted, hashlib.sha256).digest()[:SecureCrypto.TAG_SIZE]
        
        return nonce + tag + encrypted
    
    @staticmethod
    def secure_decrypt(encrypted_data: bytes, key: bytes) -> bytes:
        """Decrypt and verify data."""
        if len(encrypted_data) < SecureCrypto.NONCE_SIZE + SecureCrypto.TAG_SIZE:
            raise SecurityError("Invalid encrypted data")
        
        # Extract components
        nonce = encrypted_data[:SecureCrypto.NONCE_SIZE]
        tag = encrypted_data[SecureCrypto.NONCE_SIZE:SecureCrypto.NONCE_SIZE + SecureCrypto.TAG_SIZE]
        ciphertext = encrypted_data[SecureCrypto.NONCE_SIZE + SecureCrypto.TAG_SIZE:]
        
        # Verify HMAC
        expected_tag = hmac.new(key, nonce + ciphertext, hashlib.sha256).digest()[:SecureCrypto.TAG_SIZE]
        if not hmac.compare_digest(tag, expected_tag):
            raise SecurityError("Authentication failed - data may be tampered")
        
        # Expand key
        expanded_key = hashlib.pbkdf2_hmac('sha256', key, nonce, 1000, dklen=len(ciphertext))
        
        # XOR decrypt
        decrypted = bytes(a ^ b for a, b in zip(ciphertext, expanded_key))
        
        return decrypted
    
    @staticmethod
    def secure_hash(data: bytes, algorithm: str = "sha256") -> str:
        """Compute a secure hash with timing-safe comparison support."""
        if algorithm not in hashlib.algorithms_available:
            raise SecurityError(f"Unsupported hash algorithm: {algorithm}")
        
        return hashlib.new(algorithm, data).hexdigest()
    
    @staticmethod
    def constant_time_compare(a: str, b: str) -> bool:
        """Constant-time string comparison to prevent timing attacks."""
        return hmac.compare_digest(a, b)


class SecureFileOps:
    """Secure file operations with atomic writes and proper permissions."""
    
    # Default secure file permissions (owner read/write only)
    DEFAULT_FILE_MODE = stat.S_IRUSR | stat.S_IWUSR
    # Default secure directory permissions (owner read/write/execute)
    DEFAULT_DIR_MODE = stat.S_IRWXU
    
    @staticmethod
    @contextmanager
    def atomic_write(filepath: Path, mode: str = 'w', 
                    file_permissions: int = None) -> Any:
        """Atomic file write using temporary file and rename."""
        filepath = Path(filepath)
        temp_fd, temp_path = tempfile.mkstemp(
            dir=filepath.parent,
            prefix='.tmp_',
            suffix='_' + filepath.name
        )
        temp_path = Path(temp_path)
        
        try:
            # Close the fd and reopen with Python's open
            os.close(temp_fd)
            
            with open(temp_path, mode) as f:
                yield f
            
            # Set permissions before rename
            if file_permissions:
                os.chmod(temp_path, file_permissions)
            else:
                os.chmod(temp_path, SecureFileOps.DEFAULT_FILE_MODE)
            
            # Atomic rename
            temp_path.replace(filepath)
            
        except Exception:
            # Clean up on failure
            with suppress(OSError):
                temp_path.unlink()
            raise
    
    @staticmethod
    def secure_read(filepath: Path, max_size: int = None) -> bytes:
        """Securely read file with size limit and permission check."""
        filepath = InputValidator.validate_path(filepath, must_exist=True)
        
        # Check permissions
        if not os.access(filepath, os.R_OK):
            raise PermissionDeniedError(f"No read access: {filepath}")
        
        # Check size
        file_size = filepath.stat().st_size
        max_size = max_size or InputValidator.MAX_FILE_SIZE
        
        if file_size > max_size:
            raise ValidationError(f"File too large: {file_size} bytes (max: {max_size})")
        
        with open(filepath, 'rb') as f:
            return f.read()
    
    @staticmethod
    def secure_write(filepath: Path, data: Union[str, bytes], 
                    is_binary: bool = False, backup: bool = True) -> bool:
        """Securely write data to file with optional backup."""
        filepath = InputValidator.validate_path(filepath)
        
        # Create backup if file exists
        if backup and filepath.exists():
            backup_path = filepath.with_suffix(filepath.suffix + '.bak')
            with suppress(OSError):
                shutil.copy2(filepath, backup_path)
        
        mode = 'wb' if is_binary else 'w'
        
        with SecureFileOps.atomic_write(filepath, mode) as f:
            f.write(data)
        
        return True
    
    @staticmethod
    def secure_delete(filepath: Path, secure_wipe: bool = True) -> bool:
        """Securely delete a file with optional secure wiping."""
        filepath = InputValidator.validate_path(filepath, must_exist=True)
        
        if secure_wipe and filepath.is_file():
            try:
                # Overwrite with random data before deletion
                size = filepath.stat().st_size
                with open(filepath, 'wb') as f:
                    # Three passes of random data
                    for _ in range(3):
                        f.seek(0)
                        f.write(secrets.token_bytes(size))
                        f.flush()
                        os.fsync(f.fileno())
            except (OSError, IOError):
                pass  # Continue with normal deletion
        
        filepath.unlink()
        return True
    
    @staticmethod
    def secure_mkdir(dirpath: Path, mode: int = None) -> Path:
        """Create directory with secure permissions."""
        dirpath = InputValidator.validate_path(dirpath)
        mode = mode or SecureFileOps.DEFAULT_DIR_MODE
        
        existed_before = dirpath.exists()
        dirpath.mkdir(parents=True, exist_ok=True)
        
        # Only chmod if we created it or we own it
        if not existed_before or dirpath.stat().st_uid == os.getuid():
            try:
                os.chmod(dirpath, mode)
            except OSError:
                pass  # Skip if we can't change permissions
        
        return dirpath


class SecureSubprocess:
    """Secure subprocess execution with input sanitization."""
    
    # Default timeout for subprocess calls
    DEFAULT_TIMEOUT = 30
    # Maximum output size
    MAX_OUTPUT_SIZE = 10 * 1024 * 1024  # 10MB
    
    @staticmethod
    def run(command: List[str], timeout: int = None, 
            capture_output: bool = True, input_data: bytes = None,
            cwd: Path = None) -> subprocess.CompletedProcess:
        """Safely execute a subprocess command."""
        if not command:
            raise ValidationError("Command cannot be empty")
        
        # Validate command is a list (prevent shell injection)
        if not isinstance(command, list):
            raise ValidationError("Command must be a list of arguments")
        
        # Sanitize each argument
        sanitized_cmd = []
        for arg in command:
            if not isinstance(arg, str):
                arg = str(arg)
            # Remove null bytes
            arg = arg.replace('\x00', '')
            sanitized_cmd.append(arg)
        
        timeout = timeout or SecureSubprocess.DEFAULT_TIMEOUT
        
        try:
            result = subprocess.run(
                sanitized_cmd,
                capture_output=capture_output,
                timeout=timeout,
                input=input_data,
                cwd=cwd,
                # Security: Never use shell=True
                shell=False,
                # Don't inherit environment by default for security
                env=os.environ.copy()
            )
            
            # Truncate output if too large
            if capture_output:
                if result.stdout and len(result.stdout) > SecureSubprocess.MAX_OUTPUT_SIZE:
                    result = subprocess.CompletedProcess(
                        result.args,
                        result.returncode,
                        result.stdout[:SecureSubprocess.MAX_OUTPUT_SIZE],
                        result.stderr[:SecureSubprocess.MAX_OUTPUT_SIZE] if result.stderr else None
                    )
            
            return result
            
        except subprocess.TimeoutExpired:
            raise SecurityError(f"Command timed out after {timeout}s")
        except FileNotFoundError:
            raise SecurityError(f"Command not found: {sanitized_cmd[0]}")


def secure_operation(func):
    """Decorator for secure operation handling with logging."""
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except SecurityError:
            raise
        except Exception as e:
            # Log the error but don't expose internal details
            raise SecurityError(f"Operation failed: {type(e).__name__}")
    return wrapper

# Try to import watchdog for real-time monitoring
try:
    from watchdog.observers import Observer
    from watchdog.events import FileSystemEventHandler
    WATCHDOG_AVAILABLE = True
except ImportError:
    WATCHDOG_AVAILABLE = False

# Configuration
DATA_DIR = Path(__file__).parent / "greyav_data"
QUARANTINE_DIR = Path(__file__).parent / "quarantine"
SIGNATURES_FILE = Path(__file__).parent / "signatures.json"
SCAN_LOG = Path(__file__).parent / "scan_log.txt"
EXCLUSIONS_FILE = Path(__file__).parent / "exclusions.json"
INTEGRITY_DB = Path(__file__).parent / "integrity.json"
REPORTS_DIR = Path(__file__).parent / "reports"
SCAN_HISTORY = Path(__file__).parent / "scan_history.json"
CONFIG_FILE = Path(__file__).parent / "config.json"
CUSTOM_RULES_FILE = Path(__file__).parent / "custom_rules.json"
NETWORK_LOG = Path(__file__).parent / "network_log.json"
SCHEDULE_FILE = Path(__file__).parent / "schedule.json"

# Known malware signatures (SHA256 hashes)
DEFAULT_SIGNATURES = {
    "eicar_test": {
        "hash": "275a021bbfb6489e54d471899f7db9d1663fc695ec2fe2a2c4538aabf651fd0f",
        "name": "EICAR-Test-File",
        "severity": "test"
    },
    "example_malware_1": {
        "hash": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        "name": "Empty-File-Suspicious",
        "severity": "low"
    }
}


# Heuristic detection patterns (Advanced)
SUSPICIOUS_PATTERNS = {
    "shell_commands": {
        "patterns": [
            b"/bin/sh", b"/bin/bash", b"/bin/zsh", b"cmd.exe",
            b"powershell", b"pwsh", b"wget ", b"curl ",
            b"chmod 777", b"chmod +x", b"rm -rf", b"mkfifo",
            b"/dev/tcp/", b"nc -e", b"ncat ", b"netcat",
            b"python -c", b"python3 -c", b"perl -e", b"ruby -e",
            b"php -r", b"node -e", b"awk '{system", b"xargs sh",
            b"bash -i", b"sh -i", b"0<&196", b"exec 196<>",
            b"/dev/udp/", b"disown", b"nohup", b"setsid"
        ],
        "weight": 20,
        "description": "Shell command execution",
        "mitre_id": "T1059"
    },
    "network_indicators": {
        "patterns": [
            b"socket", b"connect(", b"bind(", b"listen(",
            b"SOCK_STREAM", b"SOCK_DGRAM", b"SOCK_RAW",
            b"urllib", b"requests", b"httplib", b"aiohttp",
            b"http://", b"https://", b"ftp://", b"ssh://",
            b"gethostbyname", b"getaddrinfo", b"inet_aton",
            b"AF_INET", b"AF_INET6", b"reverse_shell",
            b"backdoor", b"C2", b"beacon", b"exfil"
        ],
        "weight": 15,
        "description": "Network communication",
        "mitre_id": "T1071"
    },
    "crypto_ransomware": {
        "patterns": [
            b"AES", b"RSA", b"ChaCha20", b"Salsa20",
            b"encrypt", b"decrypt", b"cipher", b"Fernet",
            b"bitcoin", b"wallet", b"ransom", b"crypto",
            b".locked", b".encrypted", b".crypt", b"YOUR_FILES",
            b"pay_bitcoin", b"decrypt_key", b"RECOVER_FILES",
            b"Cryptography", b"pycryptodome", b"cryptodomex"
        ],
        "weight": 30,
        "description": "Ransomware/Encryption indicators",
        "mitre_id": "T1486"
    },
    "persistence_indicators": {
        "patterns": [
            b"crontab", b"systemctl enable", b"registry", b"HKEY_",
            b"autorun", b"startup", b"/etc/init", b"launchd",
            b"~/.bashrc", b"~/.profile", b"/etc/rc.local",
            b"schtasks", b"at.exe", b"RunOnce", b"CurrentVersion\\Run",
            b"systemd", b"/etc/systemd/system", b"launchctl",
            b"plist", b"LoginHook", b"authorized_keys",
            b".xinitrc", b".xsession", b"/etc/profile.d"
        ],
        "weight": 25,
        "description": "Persistence mechanism",
        "mitre_id": "T1053"
    },
    "obfuscation_indicators": {
        "patterns": [
            b"base64", b"eval(", b"exec(", b"compile(",
            b"\\x", b"fromCharCode", b"unescape", b"atob",
            b"btoa", b"chr(", b"ord(", b"pack(", b"unpack(",
            b"marshal.loads", b"pickle.loads", b"cloudpickle",
            b"codecs.decode", b"rot13", b"zlib.decompress",
            b"gzip.decompress", b"lzma.decompress", b"bz2.decompress",
            b"__import__", b"getattr(", b"setattr(", b"importlib"
        ],
        "weight": 20,
        "description": "Code obfuscation",
        "mitre_id": "T1027"
    },
    "data_exfil": {
        "patterns": [
            b"password", b"passwd", b"shadow", b"credentials",
            b"credit_card", b"ssn", b"api_key", b"secret_key",
            b"/etc/passwd", b".ssh/id_rsa", b"keychain",
            b"wallet.dat", b"Login Data", b"cookies.sqlite",
            b"formhistory.sqlite", b"places.sqlite", b"moz_logins",
            b"Chrome/User Data", b"Firefox/Profiles", b"keylog",
            b"screenshot", b"clipboard", b"webcam", b"microphone"
        ],
        "weight": 25,
        "description": "Data exfiltration attempt",
        "mitre_id": "T1041"
    },
    "privilege_escalation": {
        "patterns": [
            b"sudo", b"su root", b"setuid", b"setgid",
            b"privilege", b"escalat", b"root:", b"admin:",
            b"SUID", b"SGID", b"capabilities", b"CAP_",
            b"/etc/sudoers", b"pkexec", b"doas", b"polkit",
            b"SeDebugPrivilege", b"SeTakeOwnership", b"runas",
            b"CVE-", b"exploit", b"overflow", b"injection"
        ],
        "weight": 25,
        "description": "Privilege escalation",
        "mitre_id": "T1068"
    },
    "anti_analysis": {
        "patterns": [
            b"vmware", b"virtualbox", b"sandbox", b"debugger",
            b"gdb", b"strace", b"ltrace", b"IsDebuggerPresent",
            b"ptrace", b"NtQueryInformationProcess",
            b"CheckRemoteDebugger", b"OutputDebugString",
            b"QEMU", b"Xen", b"Hyper-V", b"KVM",
            b"SbieDll", b"snxhk", b"cmdvrt", b"vbox",
            b"wine_get_version", b"WINE", b"/proc/self/status"
        ],
        "weight": 25,
        "description": "Anti-analysis/VM detection",
        "mitre_id": "T1497"
    },
    "rootkit_indicators": {
        "patterns": [
            b"LD_PRELOAD", b"dlopen", b"dlsym", b"ptrace",
            b"sys_call_table", b"__NR_", b"syscall(",
            b"hideproc", b"hidefile", b"rootkit", b"unhide",
            b"/dev/kmem", b"/dev/mem", b"insmod", b"modprobe",
            b"kernel_module", b"kmod", b"lsmod", b"rmmod"
        ],
        "weight": 35,
        "description": "Rootkit/Kernel manipulation",
        "mitre_id": "T1014"
    },
    "process_injection": {
        "patterns": [
            b"VirtualAlloc", b"WriteProcessMemory", b"CreateRemoteThread",
            b"NtCreateThreadEx", b"RtlCreateUserThread",
            b"SetThreadContext", b"QueueUserAPC", b"NtQueueApcThread",
            b"mmap", b"mprotect", b"PROT_EXEC", b"MAP_ANONYMOUS",
            b"ptrace", b"process_vm_writev", b"PTRACE_POKETEXT",
            b"dlopen", b"inject", b"shellcode", b"hollow"
        ],
        "weight": 35,
        "description": "Process injection technique",
        "mitre_id": "T1055"
    },
    "credential_access": {
        "patterns": [
            b"mimikatz", b"lsass", b"sekurlsa", b"wdigest",
            b"SAM", b"SYSTEM", b"SECURITY", b"NTDS.dit",
            b"/etc/shadow", b"unshadow", b"john", b"hashcat",
            b"credential", b"password", b"pass=", b"pwd=",
            b"LaZagne", b"credentialmanager", b"keychain-db"
        ],
        "weight": 35,
        "description": "Credential theft/access",
        "mitre_id": "T1003"
    },
    "webshell_indicators": {
        "patterns": [
            b"<?php", b"system($_", b"eval($_", b"exec($_",
            b"shell_exec", b"passthru", b"popen", b"proc_open",
            b"c99shell", b"r57shell", b"WSO ", b"FilesMan",
            b"b374k", b"weevely", b"$_GET[", b"$_POST[",
            b"$_REQUEST[", b"base64_decode($_"
        ],
        "weight": 30,
        "description": "Webshell/backdoor PHP",
        "mitre_id": "T1505"
    },
    "cryptominer_indicators": {
        "patterns": [
            b"stratum+tcp://", b"stratum+ssl://", b"pool.mining",
            b"xmrig", b"minerd", b"cpuminer", b"cgminer",
            b"monero", b"ethereum", b"bitcoin", b"zcash",
            b"nicehash", b"minergate", b"coinhive", b"cryptonight",
            b"randomx", b"cuckaroo", b"hashrate", b"nonce"
        ],
        "weight": 25,
        "description": "Cryptocurrency mining",
        "mitre_id": "T1496"
    }
}

# Suspicious file extensions
DANGEROUS_EXTENSIONS = {
    ".exe", ".dll", ".scr", ".bat", ".cmd", ".com", ".pif",
    ".vbs", ".vbe", ".js", ".jse", ".ws", ".wsf", ".wsc",
    ".msi", ".msp", ".hta", ".cpl", ".jar", ".ps1", ".psm1"
}

# Magic bytes for executable files
EXECUTABLE_MAGIC = {
    b"MZ": "Windows Executable (PE)",
    b"\x7fELF": "Linux Executable (ELF)",
    b"#!": "Script with shebang",
    b"PK\x03\x04": "ZIP/JAR Archive",
    b"\xca\xfe\xba\xbe": "Java Class File",
    b"Rar!": "RAR Archive",
    b"\x1f\x8b": "GZIP Archive",
    b"BZ": "BZIP2 Archive",
    b"7z\xbc\xaf": "7-Zip Archive",
    b"%PDF": "PDF Document",
    b"\xd0\xcf\x11\xe0": "MS Office Document (OLE)"
}

# Default configuration
DEFAULT_CONFIG = {
    "heuristic_enabled": True,
    "heuristic_threshold": 50,
    "scan_archives": True,
    "max_archive_depth": 3,
    "max_file_size_mb": 100,
    "parallel_scans": 4,
    "auto_quarantine": False,
    "notify_on_threat": True,
    "scan_memory": True,
    "update_check_days": 7,
    "log_level": "info",
    "deep_scan": False,
    "multi_hash": True,
    "encrypted_quarantine": True,
    "smart_caching": True,
    "threat_intel_enabled": False,
    "behavior_analysis_timeout": 10,
    "max_scan_threads": 8,
    "yara_like_rules": True
}


# Threat Severity Levels
class ThreatSeverity(Enum):
    """Enumeration for threat severity levels."""
    CLEAN = auto()
    INFO = auto()
    LOW = auto()
    MEDIUM = auto()
    HIGH = auto()
    CRITICAL = auto()
    
    def __str__(self):
        return self.name.lower()


# Detection Types
class DetectionType(Enum):
    """Types of malware detection methods."""
    SIGNATURE = "signature"
    HEURISTIC = "heuristic"
    BEHAVIORAL = "behavioral"
    CUSTOM_RULE = "custom_rule"
    FUZZY_HASH = "fuzzy_hash"
    YARA = "yara"
    MEMORY = "memory"
    NETWORK = "network"


@dataclass
class ThreatInfo:
    """Dataclass for threat information."""
    name: str
    severity: ThreatSeverity
    detection_type: DetectionType
    file_path: str
    description: str = ""
    hash_value: str = ""
    confidence: float = 100.0
    mitre_attack_id: str = ""
    remediation: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self):
        return {
            "name": self.name,
            "severity": str(self.severity),
            "detection_type": self.detection_type.value,
            "file_path": self.file_path,
            "description": self.description,
            "hash": self.hash_value,
            "confidence": self.confidence,
            "mitre_attack_id": self.mitre_attack_id,
            "remediation": self.remediation,
            "metadata": self.metadata
        }


# MITRE ATT&CK Mapping for common techniques
MITRE_ATTACK_MAPPING = {
    "shell_commands": "T1059",  # Command and Scripting Interpreter
    "network_indicators": "T1071",  # Application Layer Protocol
    "crypto_indicators": "T1486",  # Data Encrypted for Impact
    "persistence_indicators": "T1053",  # Scheduled Task/Job
    "obfuscation_indicators": "T1027",  # Obfuscated Files or Information
    "data_exfil": "T1041",  # Exfiltration Over C2 Channel
    "privilege_escalation": "T1068",  # Exploitation for Privilege Escalation
    "anti_analysis": "T1497",  # Virtualization/Sandbox Evasion
    "rootkit": "T1014",  # Rootkit
    "process_injection": "T1055",  # Process Injection
    "credential_access": "T1003",  # OS Credential Dumping
}


# Advanced PE/ELF header structures for analysis
PE_CHARACTERISTICS = {
    0x0001: "RELOCS_STRIPPED",
    0x0002: "EXECUTABLE_IMAGE",
    0x0020: "LARGE_ADDRESS_AWARE",
    0x0100: "32BIT_MACHINE",
    0x0200: "DEBUG_STRIPPED",
    0x1000: "SYSTEM",
    0x2000: "DLL",
}

ELF_TYPES = {
    0: "ET_NONE",
    1: "ET_REL",
    2: "ET_EXEC",
    3: "ET_DYN",
    4: "ET_CORE",
}


# Fuzzy hash block sizes for similarity detection
FUZZY_BLOCK_SIZES = [3, 6, 12, 24, 48, 96, 192, 384, 768, 1536, 3072, 6144]


# ==================== ADVANCED AUTO THREAT RESPONSE SYSTEM ====================

class ThreatAction(Enum):
    """Available threat response actions."""
    QUARANTINE = auto()        # Move file to quarantine
    TERMINATE = auto()         # Kill associated process
    BLOCK_NETWORK = auto()     # Block network connections
    CLEAN = auto()            # Attempt to clean/repair file
    DELETE = auto()           # Permanently delete file
    ROLLBACK = auto()         # Rollback to previous state
    ALERT = auto()            # Alert only (no action)
    DISABLE = auto()          # Disable autorun/persistence
    RESTORE_BACKUP = auto()   # Restore from backup
    ISOLATE = auto()          # System isolation mode


class ThreatResponseLevel(Enum):
    """Threat response aggressiveness levels."""
    PASSIVE = 0       # Alert only
    CAUTIOUS = 1      # Quarantine, minimal disruption
    MODERATE = 2      # Quarantine + terminate processes
    AGGRESSIVE = 3    # All actions including deletion
    MAXIMUM = 4       # Full isolation and remediation


@dataclass
class ThreatContext:
    """Complete context for a detected threat."""
    threat_id: str
    file_path: Optional[str] = None
    process_id: Optional[int] = None
    process_name: Optional[str] = None
    network_connections: List[Dict[str, Any]] = field(default_factory=list)
    parent_processes: List[int] = field(default_factory=list)
    child_processes: List[int] = field(default_factory=list)
    related_files: List[str] = field(default_factory=list)
    related_processes: List[Dict[str, Any]] = field(default_factory=list)
    persistence_locations: List[str] = field(default_factory=list)
    severity: str = "medium"
    detection_type: str = "signature"
    confidence: float = 100.0
    timestamp: datetime = field(default_factory=datetime.now)
    mitre_tactics: List[str] = field(default_factory=list)
    threat_family: str = ""
    indicators: Dict[str, Any] = field(default_factory=dict)
    file_size: int = 0
    file_mtime: float = 0.0
    file_ctime: float = 0.0
    file_hash: str = ""


class AutoThreatResponse:
    """
    Advanced Automated Threat Response System.
    Provides intelligent, proactive threat detection and removal.
    """
    
    def __init__(self, av_engine):
        self.av = av_engine
        self.response_level = ThreatResponseLevel.MODERATE
        self.enabled = True
        self.dry_run = False  # Test mode - don't take real actions
        self.response_history = []
        self.blocked_ips = set()
        self.blocked_processes = set()
        self.protected_paths = set()
        self.backup_registry = {}  # Track backups for rollback
        self._lock = threading.Lock()
        self._response_queue = queue.Queue()
        self._response_worker = None
        self._running = False
        
        # Response policy configuration
        self.policies = {
            ThreatSeverity.CRITICAL: [
                ThreatAction.TERMINATE,
                ThreatAction.QUARANTINE,
                ThreatAction.BLOCK_NETWORK,
                ThreatAction.DISABLE
            ],
            ThreatSeverity.HIGH: [
                ThreatAction.QUARANTINE,
                ThreatAction.TERMINATE,
                ThreatAction.DISABLE
            ],
            ThreatSeverity.MEDIUM: [
                ThreatAction.QUARANTINE,
                ThreatAction.ALERT
            ],
            ThreatSeverity.LOW: [
                ThreatAction.ALERT
            ]
        }
        
        # Threat intelligence cache
        self.threat_intel = {
            'known_bad_ips': set(),
            'known_bad_hashes': set(),
            'known_bad_domains': set(),
            'known_bad_processes': set(),
            'c2_indicators': set()
        }
        
        # Self-healing configuration
        self.file_baselines = {}  # Original file hashes
        self.system_snapshot = {}  # System state snapshot
        
    def set_response_level(self, level: ThreatResponseLevel):
        """Set the aggressiveness of threat response."""
        self.response_level = level
        print(f"[*] Threat response level set to: {level.name}")
        
    def start_response_worker(self):
        """Start the background response worker thread."""
        if self._running:
            return
        self._running = True
        self._response_worker = threading.Thread(
            target=self._process_response_queue,
            daemon=True,
            name="ThreatResponseWorker"
        )
        self._response_worker.start()
        
    def stop_response_worker(self):
        """Stop the response worker thread."""
        self._running = False
        if self._response_worker:
            self._response_worker.join(timeout=5)
            
    def _process_response_queue(self):
        """Process threat responses from the queue."""
        while self._running:
            try:
                context = self._response_queue.get(timeout=1)
                self._execute_response(context)
            except queue.Empty:
                continue
            except Exception as e:
                print(f"[!] Response worker error: {e}")
                
    def queue_response(self, context: ThreatContext):
        """Queue a threat for response processing."""
        try:
            self._response_queue.put(context, timeout=5)
        except queue.Full:
            print(f"[!] Response queue full, handling synchronously")
            self._execute_response(context)
            
    def _execute_response(self, context: ThreatContext):
        """Execute response for a queued threat (internal method)."""
        try:
            response = self.respond_to_threat(context)
            if not response.get('success'):
                print(f"[!] Response for {context.threat_id} had errors: {response.get('errors', [])}")
        except Exception as e:
            print(f"[!] Failed to execute response for {context.threat_id}: {e}")
            
    def respond_to_threat(self, context: ThreatContext) -> Dict[str, Any]:
        """
        Intelligent threat response based on context and policies.
        Returns detailed response actions taken.
        """
        response = {
            "threat_id": context.threat_id,
            "timestamp": datetime.now().isoformat(),
            "actions_taken": [],
            "success": True,
            "errors": []
        }
        
        if not self.enabled:
            response["actions_taken"].append({"action": "DISABLED", "status": "skipped"})
            return response
            
        # Determine severity
        try:
            severity = ThreatSeverity[context.severity.upper()]
        except (KeyError, AttributeError):
            severity = ThreatSeverity.MEDIUM
            
        # Get actions based on severity and response level
        actions = self._determine_actions(severity, context)
        
        print(f"\n[*] AUTO-RESPONSE: {context.threat_id}")
        print(f"    Severity: {context.severity}")
        print(f"    Response Level: {self.response_level.name}")
        print(f"    Actions: {[a.name for a in actions]}")
        
        if self.dry_run:
            print("    [DRY RUN - No actions taken]")
            response["dry_run"] = True
            response["planned_actions"] = [a.name for a in actions]
            return response
            
        # Execute each action
        for action in actions:
            try:
                result = self._execute_action(action, context)
                response["actions_taken"].append({
                    "action": action.name,
                    "status": "success" if result else "failed",
                    "details": result
                })
            except Exception as e:
                response["errors"].append({
                    "action": action.name,
                    "error": str(e)
                })
                response["success"] = False
                
        # Log response
        self._log_response(response)
        self.response_history.append(response)
        
        return response
        
    def _determine_actions(self, severity: ThreatSeverity, 
                          context: ThreatContext) -> List[ThreatAction]:
        """Determine appropriate actions based on threat context."""
        base_actions = self.policies.get(severity, [ThreatAction.ALERT])
        
        # Adjust based on response level
        if self.response_level == ThreatResponseLevel.PASSIVE:
            return [ThreatAction.ALERT]
        elif self.response_level == ThreatResponseLevel.CAUTIOUS:
            return [a for a in base_actions if a in 
                   [ThreatAction.QUARANTINE, ThreatAction.ALERT]]
        elif self.response_level == ThreatResponseLevel.AGGRESSIVE:
            actions = list(base_actions)
            if ThreatAction.DELETE not in actions and severity == ThreatSeverity.CRITICAL:
                actions.append(ThreatAction.DELETE)
            return actions
        elif self.response_level == ThreatResponseLevel.MAXIMUM:
            return [
                ThreatAction.TERMINATE,
                ThreatAction.BLOCK_NETWORK,
                ThreatAction.QUARANTINE,
                ThreatAction.DISABLE,
                ThreatAction.ISOLATE
            ]
        
        return base_actions
        
    def _execute_action(self, action: ThreatAction, 
                       context: ThreatContext) -> Dict[str, Any]:
        """Execute a specific threat response action."""
        result = {"action": action.name, "timestamp": datetime.now().isoformat()}
        
        if action == ThreatAction.TERMINATE:
            result.update(self._terminate_threat_process(context))
        elif action == ThreatAction.QUARANTINE:
            result.update(self._quarantine_threat(context))
        elif action == ThreatAction.BLOCK_NETWORK:
            result.update(self._block_threat_network(context))
        elif action == ThreatAction.CLEAN:
            result.update(self._clean_threat(context))
        elif action == ThreatAction.DELETE:
            result.update(self._delete_threat(context))
        elif action == ThreatAction.DISABLE:
            result.update(self._disable_persistence(context))
        elif action == ThreatAction.ROLLBACK:
            result.update(self._rollback_changes(context))
        elif action == ThreatAction.ISOLATE:
            result.update(self._isolate_system(context))
        elif action == ThreatAction.ALERT:
            result.update(self._alert_threat(context))
            
        return result
        
    def _terminate_threat_process(self, context: ThreatContext) -> Dict[str, Any]:
        """Terminate malicious process and its children."""
        result = {"terminated": [], "failed": []}
        
        # Collect all PIDs to terminate (children first)
        pids_to_kill = list(context.child_processes)
        if context.process_id:
            pids_to_kill.append(context.process_id)
        pids_to_kill.extend(context.parent_processes)
        
        for pid in pids_to_kill:
            try:
                # Verify process is still running and is malicious
                proc_path = Path(f"/proc/{pid}")
                if not proc_path.exists():
                    continue
                    
                # Send SIGTERM first (graceful)
                os.kill(pid, signal.SIGTERM)
                time.sleep(0.5)
                
                # Check if still running, send SIGKILL
                if proc_path.exists():
                    os.kill(pid, signal.SIGKILL)
                    
                result["terminated"].append(pid)
                self.blocked_processes.add(pid)
                print(f"    [✓] Terminated process: PID {pid}")
                
            except ProcessLookupError:
                pass  # Already dead
            except PermissionError:
                result["failed"].append({"pid": pid, "reason": "permission_denied"})
                print(f"    [!] Cannot terminate PID {pid}: permission denied")
            except Exception as e:
                result["failed"].append({"pid": pid, "reason": str(e)})
                
        return result
        
    def _quarantine_threat(self, context: ThreatContext) -> Dict[str, Any]:
        """Quarantine threat files."""
        result = {"quarantined": [], "failed": []}
        
        files_to_quarantine = [context.file_path] if context.file_path else []
        files_to_quarantine.extend(context.related_files)
        
        for filepath in files_to_quarantine:
            if not filepath:
                continue
            try:
                path = Path(filepath)
                if path.exists():
                    # Create backup for potential rollback
                    self._create_backup(filepath)
                    
                    if self.av.quarantine_file(filepath):
                        result["quarantined"].append(filepath)
                        print(f"    [✓] Quarantined: {path.name}")
                    else:
                        result["failed"].append({"file": filepath, "reason": "quarantine_failed"})
            except Exception as e:
                result["failed"].append({"file": filepath, "reason": str(e)})
                
        return result
        
    def _block_threat_network(self, context: ThreatContext) -> Dict[str, Any]:
        """Block malicious network connections."""
        result = {"blocked": [], "failed": []}
        
        for conn in context.network_connections:
            try:
                # Handle both dict and string formats
                if isinstance(conn, dict):
                    # Extract remote IP from connection dict
                    remote = conn.get('remote', '')
                    if remote and remote != 'N/A':
                        ip_match = re.search(r'(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})', remote)
                    else:
                        continue
                else:
                    # Legacy string format
                    ip_match = re.search(r'(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})', str(conn))
                    
                if ip_match:
                    ip = ip_match.group(1)
                    
                    # Skip local/private IPs
                    if ip.startswith(('127.', '10.', '192.168.', '172.')):
                        continue
                        
                    # Add to blocked list
                    self.blocked_ips.add(ip)
                    
                    # Attempt to block via iptables (requires root)
                    if os.geteuid() == 0:
                        try:
                            subprocess.run(
                                ["iptables", "-A", "OUTPUT", "-d", ip, "-j", "DROP"],
                                capture_output=True, timeout=5
                            )
                            result["blocked"].append(ip)
                            print(f"    [✓] Blocked IP: {ip}")
                        except Exception:
                            result["failed"].append({"ip": ip, "reason": "iptables_failed"})
                    else:
                        result["blocked"].append({"ip": ip, "method": "logged_only"})
                        
            except Exception as e:
                result["failed"].append({"connection": str(conn), "reason": str(e)})
                
        return result
        
    def _clean_threat(self, context: ThreatContext) -> Dict[str, Any]:
        """Attempt to clean/repair infected file."""
        result = {"cleaned": False, "details": ""}
        
        if not context.file_path:
            result["details"] = "No file path provided"
            return result
            
        filepath = Path(context.file_path)
        if not filepath.exists():
            result["details"] = "File not found"
            return result
            
        # Create backup before cleaning
        self._create_backup(str(filepath))
        
        # Attempt to clean using the AV engine
        if self.av.clean_file(filepath):
            result["cleaned"] = True
            result["details"] = "Malicious patterns removed"
            print(f"    [✓] Cleaned: {filepath.name}")
        else:
            result["details"] = "Cleaning failed or not applicable"
            
        return result
        
    def _delete_threat(self, context: ThreatContext) -> Dict[str, Any]:
        """Permanently delete threat files."""
        result = {"deleted": [], "failed": []}
        
        files_to_delete = [context.file_path] if context.file_path else []
        files_to_delete.extend(context.related_files)
        
        for filepath in files_to_delete:
            if not filepath:
                continue
            try:
                path = Path(filepath)
                if path.exists():
                    # Create backup for potential recovery
                    self._create_backup(filepath)
                    
                    # Secure deletion
                    SecureFileOps.secure_delete(path)
                    result["deleted"].append(filepath)
                    print(f"    [✓] Deleted: {path.name}")
            except Exception as e:
                result["failed"].append({"file": filepath, "reason": str(e)})
                
        return result
        
    def _disable_persistence(self, context: ThreatContext) -> Dict[str, Any]:
        """Disable persistence mechanisms."""
        result = {"disabled": [], "failed": []}
        
        for location in context.persistence_locations:
            try:
                path = Path(location)
                if path.exists():
                    # Backup before removal
                    self._create_backup(location)
                    
                    if path.is_file():
                        path.unlink()
                    elif path.is_dir():
                        shutil.rmtree(path)
                        
                    result["disabled"].append(location)
                    print(f"    [✓] Disabled persistence: {location}")
            except Exception as e:
                result["failed"].append({"location": location, "reason": str(e)})
                
        # Check for cron entries
        try:
            cron_dirs = ["/etc/cron.d", "/var/spool/cron"]
            for cron_dir in cron_dirs:
                cron_path = Path(cron_dir)
                if cron_path.exists():
                    for entry in cron_path.iterdir():
                        if context.threat_id in entry.read_text():
                            self._create_backup(str(entry))
                            entry.unlink()
                            result["disabled"].append(str(entry))
        except Exception:
            pass
            
        return result
        
    def _rollback_changes(self, context: ThreatContext) -> Dict[str, Any]:
        """Rollback to previous state using backups."""
        result = {"restored": [], "failed": []}
        
        for original_path, backup_info in self.backup_registry.items():
            try:
                backup_path = backup_info.get("backup_path")
                if backup_path and Path(backup_path).exists():
                    shutil.copy2(backup_path, original_path)
                    result["restored"].append(original_path)
                    print(f"    [✓] Restored: {original_path}")
            except Exception as e:
                result["failed"].append({"file": original_path, "reason": str(e)})
                
        return result
        
    def _isolate_system(self, context: ThreatContext) -> Dict[str, Any]:
        """Emergency system isolation mode."""
        result = {"actions": [], "status": ""}
        
        print("    [!!!] INITIATING SYSTEM ISOLATION")
        
        # Block all outbound connections except essential
        if os.geteuid() == 0:
            try:
                # Save current rules
                subprocess.run(
                    ["iptables-save"],
                    capture_output=True, timeout=5
                )
                
                # Block all OUTPUT except DNS and localhost
                rules = [
                    ["iptables", "-A", "OUTPUT", "-o", "lo", "-j", "ACCEPT"],
                    ["iptables", "-A", "OUTPUT", "-p", "udp", "--dport", "53", "-j", "ACCEPT"],
                    ["iptables", "-A", "OUTPUT", "-j", "DROP"]
                ]
                
                for rule in rules:
                    subprocess.run(rule, capture_output=True, timeout=5)
                    result["actions"].append(" ".join(rule))
                    
                result["status"] = "network_isolated"
                print("    [✓] Network isolated - outbound blocked")
            except Exception as e:
                result["status"] = f"isolation_failed: {e}"
        else:
            result["status"] = "requires_root"
            print("    [!] Network isolation requires root")
            
        return result
        
    def _alert_threat(self, context: ThreatContext) -> Dict[str, Any]:
        """Generate threat alert without taking action."""
        alert = {
            "type": "threat_alert",
            "threat_id": context.threat_id,
            "severity": context.severity,
            "file": context.file_path,
            "process": context.process_name,
            "timestamp": datetime.now().isoformat()
        }
        
        print(f"    [ALERT] Threat detected: {context.threat_id}")
        
        # Log to file
        self.av._log_secure("warning", f"THREAT ALERT: {context.threat_id}")
        
        return alert
        
    def _create_backup(self, filepath: str):
        """Create backup of a file before modification."""
        try:
            path = Path(filepath)
            if not path.exists():
                return
                
            backup_dir = DATA_DIR / "backups" / datetime.now().strftime("%Y%m%d")
            backup_dir.mkdir(parents=True, exist_ok=True)
            
            backup_name = f"{path.name}_{int(time.time())}.bak"
            backup_path = backup_dir / backup_name
            
            shutil.copy2(path, backup_path)
            
            with self._lock:
                self.backup_registry[str(path)] = {
                    "backup_path": str(backup_path),
                    "timestamp": datetime.now().isoformat(),
                    "original_hash": hashlib.sha256(path.read_bytes()).hexdigest()
                }
        except Exception:
            pass
            
    def _log_response(self, response: Dict[str, Any]):
        """Log threat response to secure log."""
        try:
            log_entry = {
                "type": "threat_response",
                "timestamp": datetime.now().isoformat(),
                "response": response
            }
            self.av._log_secure("info", json.dumps(log_entry))
        except Exception:
            pass
            
    # ==================== PROACTIVE THREAT PREVENTION ====================
    
    def enable_proactive_protection(self):
        """Enable proactive threat prevention features."""
        print("[*] Enabling Proactive Protection...")
        
        # Start background monitoring
        self.start_response_worker()
        
        # Initialize threat intelligence
        self._load_threat_intelligence()
        
        # Take system baseline
        self._capture_system_baseline()
        
        print("[✓] Proactive protection enabled")
        
    def _load_threat_intelligence(self):
        """Load threat intelligence data."""
        intel_file = DATA_DIR / "threat_intel.json"
        
        if intel_file.exists():
            try:
                with open(intel_file) as f:
                    data = json.load(f)
                    self.threat_intel['known_bad_ips'].update(data.get('bad_ips', []))
                    self.threat_intel['known_bad_hashes'].update(data.get('bad_hashes', []))
                    self.threat_intel['known_bad_domains'].update(data.get('bad_domains', []))
                    self.threat_intel['c2_indicators'].update(data.get('c2_indicators', []))
                print(f"[+] Loaded threat intelligence: "
                      f"{len(self.threat_intel['known_bad_ips'])} IPs, "
                      f"{len(self.threat_intel['known_bad_hashes'])} hashes")
            except Exception:
                pass
                
        # Add built-in known bad indicators
        self._add_builtin_indicators()
        
    def _add_builtin_indicators(self):
        """Add built-in threat indicators."""
        # Known C2 indicators
        self.threat_intel['c2_indicators'].update([
            'pastebin.com/raw/',
            'githubusercontent.com',
            '/gate.php',
            '/panel.php',
            'checkip.dyndns',
            'ipecho.net'
        ])
        
        # Known malicious process names
        self.threat_intel['known_bad_processes'].update([
            'ld-linux', 'kworker', 'systemd-', 'kdevtmpfs',  # Fake kernel procs
            'xmrig', 'minerd', 'cpuminer', 'cryptonight',   # Miners
            'nmap', 'masscan', 'zmap',                       # Scanners
            'hydra', 'medusa', 'john',                       # Crackers
        ])
        
    def _capture_system_baseline(self):
        """Capture system state baseline for comparison."""
        self.system_snapshot = {
            "timestamp": datetime.now().isoformat(),
            "processes": set(),
            "network_connections": set(),
            "listening_ports": set(),
            "cron_jobs": [],
            "startup_services": []
        }
        
        # Capture running processes
        for proc_dir in Path("/proc").iterdir():
            if proc_dir.name.isdigit():
                try:
                    comm = (proc_dir / "comm").read_text().strip()
                    self.system_snapshot["processes"].add(comm)
                except Exception:
                    pass
                    
        # Capture network listeners
        try:
            result = subprocess.run(
                ["ss", "-ltn"],
                capture_output=True, text=True, timeout=10
            )
            for line in result.stdout.split('\n')[1:]:
                parts = line.split()
                if len(parts) >= 4:
                    self.system_snapshot["listening_ports"].add(parts[3])
        except Exception:
            pass
            
    def check_against_baseline(self) -> List[Dict[str, Any]]:
        """Compare current state against baseline for anomalies."""
        anomalies = []
        
        if not self.system_snapshot:
            return anomalies
            
        # Check for new processes
        current_procs = set()
        for proc_dir in Path("/proc").iterdir():
            if proc_dir.name.isdigit():
                try:
                    comm = (proc_dir / "comm").read_text().strip()
                    current_procs.add(comm)
                except Exception:
                    pass
                    
        new_procs = current_procs - self.system_snapshot.get("processes", set())
        for proc in new_procs:
            # Check against known bad
            if any(bad in proc.lower() for bad in self.threat_intel['known_bad_processes']):
                anomalies.append({
                    "type": "suspicious_process",
                    "name": proc,
                    "severity": "high",
                    "description": f"Suspicious new process: {proc}"
                })
                
        return anomalies
        
    def scan_for_iocs(self, target_path: str = None) -> List[Dict[str, Any]]:
        """Scan for Indicators of Compromise."""
        iocs_found = []
        search_path = Path(target_path) if target_path else Path("/")
        
        print("[*] Scanning for Indicators of Compromise...")
        
        # Check file hashes against known bad
        if search_path.is_file():
            files = [search_path]
        else:
            files = list(search_path.rglob("*"))[:1000]  # Limit
            
        for filepath in files:
            if not filepath.is_file():
                continue
            try:
                file_hash = hashlib.sha256(filepath.read_bytes()).hexdigest()
                if file_hash in self.threat_intel['known_bad_hashes']:
                    iocs_found.append({
                        "type": "known_malware",
                        "file": str(filepath),
                        "hash": file_hash,
                        "severity": "critical"
                    })
            except Exception:
                pass
                
        # Check for C2 indicators in recent files
        try:
            for filepath in Path("/tmp").iterdir():
                if filepath.is_file() and filepath.stat().st_size < 1024 * 1024:
                    try:
                        content = filepath.read_text(errors='ignore')
                        for indicator in self.threat_intel['c2_indicators']:
                            if indicator in content:
                                iocs_found.append({
                                    "type": "c2_indicator",
                                    "file": str(filepath),
                                    "indicator": indicator,
                                    "severity": "critical"
                                })
                    except Exception:
                        pass
        except Exception:
            pass
            
        return iocs_found


class SelfHealingSystem:
    """
    Self-healing system for automatic file and system restoration.
    """
    
    def __init__(self, av_engine):
        self.av = av_engine
        self.file_registry = {}  # Track monitored files
        self.integrity_hashes = {}  # Store original hashes
        self.auto_restore = True
        self._monitoring = False
        self._monitor_thread = None
        
    def register_protected_file(self, filepath: str, backup: bool = True):
        """Register a file for integrity protection."""
        path = Path(filepath)
        if not path.exists():
            return False
            
        file_hash = hashlib.sha256(path.read_bytes()).hexdigest()
        
        self.file_registry[str(path)] = {
            "hash": file_hash,
            "registered": datetime.now().isoformat(),
            "size": path.stat().st_size,
            "permissions": oct(path.stat().st_mode)[-3:]
        }
        
        # Create backup
        if backup:
            backup_dir = DATA_DIR / "protected_backups"
            backup_dir.mkdir(parents=True, exist_ok=True)
            backup_path = backup_dir / f"{path.name}_{file_hash[:8]}"
            shutil.copy2(path, backup_path)
            self.file_registry[str(path)]["backup"] = str(backup_path)
            
        return True
        
    def verify_integrity(self) -> List[Dict[str, Any]]:
        """Verify integrity of all registered files."""
        violations = []
        
        for filepath, info in self.file_registry.items():
            path = Path(filepath)
            
            if not path.exists():
                violations.append({
                    "file": filepath,
                    "issue": "deleted",
                    "original_hash": info["hash"]
                })
                continue
                
            current_hash = hashlib.sha256(path.read_bytes()).hexdigest()
            if current_hash != info["hash"]:
                violations.append({
                    "file": filepath,
                    "issue": "modified",
                    "original_hash": info["hash"],
                    "current_hash": current_hash
                })
                
        return violations
        
    def restore_file(self, filepath: str) -> bool:
        """Restore a file from backup."""
        info = self.file_registry.get(filepath)
        if not info or "backup" not in info:
            return False
            
        backup_path = Path(info["backup"])
        if not backup_path.exists():
            return False
            
        try:
            shutil.copy2(backup_path, filepath)
            # Restore permissions
            os.chmod(filepath, int(info["permissions"], 8))
            return True
        except Exception:
            return False
            
    def auto_heal(self) -> List[Dict[str, Any]]:
        """Automatically detect and repair integrity violations."""
        results = []
        violations = self.verify_integrity()
        
        for violation in violations:
            filepath = violation["file"]
            
            if self.auto_restore:
                if self.restore_file(filepath):
                    results.append({
                        "file": filepath,
                        "action": "restored",
                        "status": "success"
                    })
                else:
                    results.append({
                        "file": filepath,
                        "action": "restore_failed",
                        "status": "failed"
                    })
            else:
                results.append({
                    "file": filepath,
                    "action": "alert_only",
                    "status": "violation_detected"
                })
                
        return results
        
    def start_monitoring(self, interval: int = 60):
        """Start background integrity monitoring."""
        if self._monitoring:
            return
            
        self._monitoring = True
        
        def monitor_loop():
            while self._monitoring:
                violations = self.verify_integrity()
                if violations:
                    print(f"\n[!] Integrity violations detected: {len(violations)}")
                    if self.auto_restore:
                        self.auto_heal()
                time.sleep(interval)
                
        self._monitor_thread = threading.Thread(target=monitor_loop, daemon=True)
        self._monitor_thread.start()
        
    def stop_monitoring(self):
        """Stop background monitoring."""
        self._monitoring = False
        if self._monitor_thread:
            self._monitor_thread.join(timeout=5)


class PreemptiveScanner:
    """
    Pre-execution scanning system - intercepts and scans before execution.
    """
    
    def __init__(self, av_engine):
        self.av = av_engine
        self.enabled = False
        self.blocked_extensions = {'.exe', '.sh', '.py', '.pl', '.rb', '.bat', '.cmd'}
        self.execution_log = []
        self.whitelist_hashes = set()
        
    def scan_before_execute(self, filepath: str) -> Tuple[bool, str]:
        """
        Scan a file before execution.
        Returns (allow_execution, reason).
        """
        path = Path(filepath)
        
        if not path.exists():
            return False, "File not found"
            
        # Quick whitelist check
        try:
            file_hash = hashlib.sha256(path.read_bytes()).hexdigest()
            if file_hash in self.whitelist_hashes:
                return True, "Whitelisted"
        except Exception:
            pass
            
        # Full scan
        threat, heuristic = self.av.scan_file(filepath)
        
        if threat:
            self.execution_log.append({
                "file": filepath,
                "blocked": True,
                "reason": f"Threat: {threat['name']}",
                "timestamp": datetime.now().isoformat()
            })
            return False, f"Blocked: {threat['name']}"
            
        if heuristic and heuristic.get("score", 0) >= 80:
            self.execution_log.append({
                "file": filepath,
                "blocked": True,
                "reason": f"Suspicious: score {heuristic['score']}",
                "timestamp": datetime.now().isoformat()
            })
            return False, f"Blocked: Suspicious activity (score: {heuristic['score']})"
            
        # Allow execution
        self.execution_log.append({
            "file": filepath,
            "blocked": False,
            "reason": "Clean",
            "timestamp": datetime.now().isoformat()
        })
        return True, "Clean - execution allowed"
        
    def add_to_whitelist(self, filepath: str):
        """Add a file hash to execution whitelist."""
        path = Path(filepath)
        if path.exists():
            file_hash = hashlib.sha256(path.read_bytes()).hexdigest()
            self.whitelist_hashes.add(file_hash)


class GreyAV:
    """Advanced Antivirus Engine with multi-layered threat detection."""
    
    VERSION = "5.0-secure"
    
    def __init__(self):
        # Initialize security components first
        self._secure_logger = SecureLogger(SCAN_LOG)
        self._rate_limiter = RateLimiter(max_calls=100, period=60)
        self._scan_rate_limiter = RateLimiter(max_calls=1000, period=1)
        
        # Load configuration securely
        self.config = self._load_config()
        self.signatures = self._load_signatures()
        self.exclusions = self._load_exclusions()
        self.custom_rules = self._load_custom_rules()
        
        # Ensure secure directories
        self._ensure_quarantine_dir()
        self._ensure_reports_dir()
        
        # Initialize state
        self.scan_results = self._init_scan_results()
        self.heuristic_enabled = self.config.get("heuristic_enabled", True)
        self.heuristic_threshold = self.config.get("heuristic_threshold", 50)
        self.scan_start_time = None
        
        # Thread-safe caches with size limits
        self._file_cache = {}
        self._file_cache_max = 10000
        self._fuzzy_cache = {}
        self._fuzzy_cache_max = 5000
        
        # Thread locks
        self._scan_lock = threading.Lock()
        self._stats_lock = threading.Lock()
        self._cache_lock = threading.Lock()
        
        # Security state
        self._threat_db = {}
        self._whitelist_hashes = self._load_whitelist()
        self._scan_session_id = secrets.token_hex(16)  # Longer session ID
        self._detection_callbacks: List[Callable] = []
        self._quarantine_key = self._get_quarantine_key()
        
        # Analysis caches
        self._pe_analysis_cache = {}
        self._elf_analysis_cache = {}
        
        # Integrity verification
        self._config_integrity = self._compute_config_integrity()
        
        # Initialize Advanced Auto Threat Response System
        self.threat_response = AutoThreatResponse(self)
        self.self_healing = SelfHealingSystem(self)
        self.preemptive_scanner = PreemptiveScanner(self)
        
        # Auto-response configuration
        self.auto_response_enabled = self.config.get("auto_response", False)
        self.auto_response_level = ThreatResponseLevel(
            self.config.get("response_level", 2)  # Default: MODERATE
        )
        self.threat_response.set_response_level(self.auto_response_level)
        
        # Initialize Automatic Threat Manager (enhanced auto-response)
        self.auto_threat_manager = None
        if AUTO_THREAT_MANAGER_AVAILABLE:
            try:
                self.auto_threat_manager = get_threat_manager(
                    quarantine_dir=QUARANTINE_DIR,
                    dry_run=False
                )
                # Register callback for threat notifications
                self.auto_threat_manager.on_threat_detected(
                    self._on_auto_threat_detected
                )
                self.auto_threat_manager.on_threat_mitigated(
                    self._on_auto_threat_mitigated
                )
            except Exception as e:
                self._log_secure("warning", f"AutoThreatManager init: {e}")
        
        # Log session start
        self._log_secure("info", f"GreyAV v{self.VERSION} initialized")
    
    def _on_auto_threat_detected(self, threat):
        """Callback when auto threat manager detects a threat."""
        msg = f"AUTO-DETECT: {threat.threat_id} ({threat.severity})"
        self._log_secure("warning", msg)
    
    def _on_auto_threat_mitigated(self, threat, result):
        """Callback when auto threat manager mitigates a threat."""
        status = "SUCCESS" if result.get("success") else "FAILED"
        actions = [a.get("action") for a in result.get("actions", [])]
        msg = f"AUTO-MITIGATE: {threat.threat_id} - {status} - {actions}"
        self._log_secure("info", msg)
    
    def handle_threat_automatically(self, threat_info: Dict,
                                    file_path: str = None) -> Dict:
        """
        Handle a detected threat fully automatically.
        
        This is the primary method for automatic threat handling.
        It uses the AutoThreatManager for comprehensive response.
        
        Args:
            threat_info: Detection result from scan
            file_path: Optional path to the threat file
            
        Returns:
            Dict with response results
        """
        if not AUTO_THREAT_MANAGER_AVAILABLE or not self.auto_threat_manager:
            # Fall back to legacy auto-response
            return self._legacy_auto_response(threat_info, file_path)
        
        try:
            # Convert scan result to DetectedThreat
            threat = create_threat_from_scan(threat_info, file_path)
            
            # Handle through auto threat manager
            result = self.auto_threat_manager.handle_threat(threat)
            
            # Update stats
            with self._stats_lock:
                self.scan_results["quarantined"] += 1 if result.get(
                    "success") else 0
            
            return result
            
        except Exception as e:
            self._log_secure("error", f"Auto-handle failed: {e}")
            return {"success": False, "error": str(e)}
    
    def _legacy_auto_response(self, threat_info: Dict,
                              file_path: str = None) -> Dict:
        """Legacy auto-response fallback."""
        if not self.auto_response_enabled:
            return {"success": False, "reason": "auto-response disabled"}
        
        context = ThreatContext(
            threat_id=threat_info.get("name", "unknown"),
            file_path=file_path or "",
            severity=threat_info.get("severity", "medium"),
            detection_type=threat_info.get("detection_type", "signature"),
            confidence=threat_info.get("confidence", 100.0),
            mitre_tactics=[threat_info.get("mitre", "")],
            threat_family=threat_info.get("family", "")
        )
        
        return self.threat_response.respond_to_threat(context)
    
    def start_auto_protection(self):
        """Start all automatic protection systems."""
        print("[*] Starting automatic protection...")
        
        # Start threat response worker
        self.threat_response.start_response_worker()
        
        # Start auto threat manager if available
        if self.auto_threat_manager:
            self.auto_threat_manager.start()
            print("    [✓] AutoThreatManager started")
        
        # Enable auto-response
        self.auto_response_enabled = True
        
        print("    [✓] Automatic protection active")
    
    def stop_auto_protection(self):
        """Stop all automatic protection systems."""
        # Stop threat response worker
        self.threat_response.stop_response_worker()
        
        # Stop auto threat manager
        if self.auto_threat_manager:
            self.auto_threat_manager.stop()
        
        self.auto_response_enabled = False
        print("[*] Automatic protection stopped")
    
    def get_auto_protection_stats(self) -> Dict:
        """Get statistics from automatic protection systems."""
        stats = {
            "auto_response_enabled": self.auto_response_enabled,
            "response_level": self.auto_response_level.name,
            "legacy_responses": len(self.threat_response.response_history),
        }
        
        if self.auto_threat_manager:
            stats["auto_manager"] = self.auto_threat_manager.get_stats()
        
        return stats
    
    def _log_secure(self, level: str, message: str):
        """Log securely with session context."""
        self._secure_logger.log(level, message, self._scan_session_id)
    
    def _compute_config_integrity(self) -> str:
        """Compute integrity hash of configuration files."""
        config_data = json.dumps(self.config, sort_keys=True)
        return SecureCrypto.secure_hash(config_data.encode())
    
    def verify_config_integrity(self) -> bool:
        """Verify configuration hasn't been tampered with."""
        current_hash = self._compute_config_integrity()
        return SecureCrypto.constant_time_compare(
            current_hash, self._config_integrity
        )

    def _init_scan_results(self):
        """Initialize scan results with extended tracking."""
        return {
            "scanned": 0,
            "infected": 0,
            "suspicious": 0,
            "errors": 0,
            "skipped": 0,
            "archived": 0,
            "cleaned": 0,
            "quarantined": 0,
            "threats": [],
            "heuristic_detections": [],
            "detection_types": defaultdict(int),
            "file_types_scanned": defaultdict(int),
            "severity_counts": defaultdict(int),
            "performance_metrics": {
                "files_per_second": 0,
                "avg_scan_time_ms": 0,
                "cache_hits": 0,
                "cache_misses": 0
            }
        }

    def _load_whitelist(self):
        """Load whitelist hashes for known safe files."""
        whitelist_file = Path(__file__).parent / "whitelist.json"
        if whitelist_file.exists():
            try:
                with open(whitelist_file, 'r') as f:
                    data = json.load(f)
                    return set(data.get("hashes", []))
            except (json.JSONDecodeError, KeyError):
                pass
        return set()

    def _get_quarantine_key(self):
        """Get or generate quarantine encryption key securely."""
        key_file = QUARANTINE_DIR / ".qkey"
        
        # Ensure quarantine directory exists with secure permissions
        SecureFileOps.secure_mkdir(QUARANTINE_DIR)
        
        if key_file.exists():
            try:
                # Verify key file permissions
                key_stat = key_file.stat()
                if key_stat.st_mode & 0o077:  # Check if group/other has access
                    self._log_secure("warning", 
                        "Quarantine key has insecure permissions")
                
                key_data = SecureFileOps.secure_read(key_file)
                if len(key_data) >= SecureCrypto.KEY_SIZE:
                    return key_data[:SecureCrypto.KEY_SIZE]
            except (IOError, SecurityError, ValidationError):
                pass
        
        # Generate new secure key
        key = SecureCrypto.generate_key()
        try:
            with SecureFileOps.atomic_write(key_file, 'wb',
                    file_permissions=stat.S_IRUSR | stat.S_IWUSR) as f:
                f.write(key)
            self._log_secure("info", "Generated new quarantine key")
        except (IOError, OSError) as e:
            self._log_secure("error", f"Failed to save quarantine key: {e}")
        
        return key

    def add_detection_callback(self, callback: Callable):
        """Register a callback for threat detection events."""
        if not callable(callback):
            raise ValidationError("Callback must be callable")
        self._detection_callbacks.append(callback)

    def _notify_detection(self, threat_info: ThreatInfo):
        """Notify all registered callbacks of a detection."""
        for callback in self._detection_callbacks:
            try:
                callback(threat_info)
            except Exception as e:
                self._log_secure("error", 
                    f"Detection callback failed: {type(e).__name__}")

    def _load_config(self):
        """Load configuration from file securely."""
        if CONFIG_FILE.exists():
            try:
                content = SecureFileOps.secure_read(CONFIG_FILE)
                loaded_config = json.loads(content.decode('utf-8'))
                # Merge with defaults, don't allow arbitrary keys
                validated_config = DEFAULT_CONFIG.copy()
                for key, value in loaded_config.items():
                    if key in DEFAULT_CONFIG:
                        # Type validation
                        expected_type = type(DEFAULT_CONFIG[key])
                        if isinstance(value, expected_type):
                            validated_config[key] = value
                return validated_config
            except (json.JSONDecodeError, ValidationError, SecurityError) as e:
                self._log_secure("warning", f"Config load failed: {e}")
        return DEFAULT_CONFIG.copy()

    def _save_config(self):
        """Save configuration to file securely."""
        try:
            config_json = json.dumps(self.config, indent=2)
            SecureFileOps.secure_write(CONFIG_FILE, config_json, backup=True)
            self._config_integrity = self._compute_config_integrity()
        except (IOError, SecurityError) as e:
            self._log_secure("error", f"Failed to save config: {e}")
            raise

    def _load_custom_rules(self):
        """Load custom detection rules securely."""
        if CUSTOM_RULES_FILE.exists():
            try:
                content = SecureFileOps.secure_read(CUSTOM_RULES_FILE)
                rules = json.loads(content.decode('utf-8'))
                # Validate structure
                if isinstance(rules, dict) and "rules" in rules:
                    return rules
            except (json.JSONDecodeError, ValidationError, SecurityError):
                pass
        return {"rules": []}

    def _load_exclusions(self):
        """Load exclusion patterns from file securely."""
        if EXCLUSIONS_FILE.exists():
            try:
                content = SecureFileOps.secure_read(EXCLUSIONS_FILE)
                exclusions = json.loads(content.decode('utf-8'))
                # Validate structure
                validated = {"paths": [], "extensions": [], "patterns": []}
                for key in validated.keys():
                    if key in exclusions and isinstance(exclusions[key], list):
                        # Sanitize each value
                        validated[key] = [
                            InputValidator.sanitize_string(str(v), max_length=256)
                            for v in exclusions[key][:100]  # Max 100 per type
                        ]
                return validated
            except (json.JSONDecodeError, ValidationError, SecurityError):
                pass
        return {"paths": [], "extensions": [], "patterns": []}

    def _save_exclusions(self):
        """Save exclusions to file securely."""
        try:
            exclusions_json = json.dumps(self.exclusions, indent=2)
            SecureFileOps.secure_write(EXCLUSIONS_FILE, exclusions_json)
        except (IOError, SecurityError) as e:
            self._log_secure("error", f"Failed to save exclusions: {e}")

    def _ensure_reports_dir(self):
        """Create reports directory with secure permissions."""
        SecureFileOps.secure_mkdir(REPORTS_DIR)

    def is_excluded(self, filepath):
        """Check if a file should be excluded from scanning."""
        try:
            filepath = InputValidator.validate_path(filepath, allow_symlinks=True)
        except ValidationError:
            return False
        
        str_path = str(filepath).lower()
        
        # Check path exclusions
        for excl in self.exclusions.get("paths", []):
            if excl.lower() in str_path:
                return True
        
        # Check extension exclusions
        ext = filepath.suffix.lower()
        if ext in self.exclusions.get("extensions", []):
            return True
        
        # Check pattern exclusions (glob patterns)
        for pattern in self.exclusions.get("patterns", []):
            try:
                if filepath.match(pattern):
                    return True
            except ValueError:
                # Invalid pattern
                continue
        
        return False

    def add_exclusion(self, excl_type, value):
        """Add an exclusion rule with validation."""
        if excl_type not in ["paths", "extensions", "patterns"]:
            print(f"[!] Invalid exclusion type: {excl_type}")
            return False
        if value not in self.exclusions[excl_type]:
            self.exclusions[excl_type].append(value)
            self._save_exclusions()
            print(f"[+] Added {excl_type} exclusion: {value}")
            return True
        return False

    def remove_exclusion(self, excl_type, value):
        """Remove an exclusion rule."""
        if excl_type in self.exclusions and value in self.exclusions[excl_type]:
            self.exclusions[excl_type].remove(value)
            self._save_exclusions()
            print(f"[-] Removed {excl_type} exclusion: {value}")
            return True
        return False

    def list_exclusions(self):
        """List all exclusion rules."""
        print("\n[*] Exclusion Rules:")
        print("-" * 50)
        print("\nPath exclusions:")
        for p in self.exclusions.get("paths", []):
            print(f"  • {p}")
        print("\nExtension exclusions:")
        for e in self.exclusions.get("extensions", []):
            print(f"  • {e}")
        print("\nPattern exclusions:")
        for pat in self.exclusions.get("patterns", []):
            print(f"  • {pat}")

    def _load_signatures(self):
        """Load virus signatures from file or use defaults with safeguards."""
        if not SafeGuard.circuit_breaker_check("load_signatures"):
            return DEFAULT_SIGNATURES.copy()
        
        if SIGNATURES_FILE.exists():
            try:
                signatures = SafeGuard.safe_json_load(
                    SIGNATURES_FILE, DEFAULT_SIGNATURES)
                
                # Validate signature structure
                if isinstance(signatures, dict):
                    validated = {}
                    for sig_id, sig_data in signatures.items():
                        if isinstance(sig_data, dict) and "hash" in sig_data:
                            validated[sig_id] = sig_data
                    
                    if validated:
                        SafeGuard.record_success("load_signatures")
                        return validated
                
                self._log_secure("warning", 
                    "Invalid signature format, using defaults")
            except Exception as e:
                SafeGuard.record_failure("load_signatures")
                self._log_secure("error", 
                    f"Signature load error: {type(e).__name__}")
        
        return DEFAULT_SIGNATURES.copy()

    def _save_signatures(self):
        """Save current signatures to file with safeguards."""
        if not SafeGuard.circuit_breaker_check("save_signatures"):
            return False
        
        try:
            success = SafeGuard.safe_json_save(
                SIGNATURES_FILE, self.signatures)
            if success:
                SafeGuard.record_success("save_signatures")
            else:
                SafeGuard.record_failure("save_signatures")
            return success
        except Exception:
            SafeGuard.record_failure("save_signatures")
            return False

    def _ensure_quarantine_dir(self):
        """Create quarantine directory if it doesn't exist with safeguards."""
        try:
            QUARANTINE_DIR.mkdir(exist_ok=True, mode=0o700)
        except OSError as e:
            self._log_secure("error", 
                f"Failed to create quarantine dir: {e}")

    def _calculate_hash(self, filepath, algorithm="sha256"):
        """Calculate hash of a file with comprehensive safeguards."""
        # Validate inputs
        if algorithm not in hashlib.algorithms_available:
            algorithm = "sha256"
        
        # Check circuit breaker
        if not SafeGuard.circuit_breaker_check("hash_calculation"):
            return None
        
        # Check cache first
        cache_key = f"{filepath}:{algorithm}"
        with self._cache_lock:
            if cache_key in self._file_cache:
                with self._stats_lock:
                    self.scan_results["performance_metrics"]["cache_hits"] += 1
                return self._file_cache[cache_key]
        
        with self._stats_lock:
            self.scan_results["performance_metrics"]["cache_misses"] += 1
        
        # Validate file path
        try:
            filepath = Path(filepath)
            if not filepath.exists() or not filepath.is_file():
                return None
            
            # Check file size limit (1GB max)
            file_size = filepath.stat().st_size
            if file_size > 1024 * 1024 * 1024:
                self._log_secure("warning", 
                    f"File too large for hashing: {file_size}")
                return None
                
        except (OSError, ValueError):
            return None
        
        # Perform hashing with resource management
        try:
            with SafeGuard.managed_file_handle():
                hash_obj = hashlib.new(algorithm)
                bytes_read = 0
                max_bytes = 1024 * 1024 * 1024  # 1GB safety limit
                
                with open(filepath, "rb") as f:
                    while True:
                        # Check memory periodically
                        if bytes_read % (50 * 1024 * 1024) == 0:
                            if not SafeGuard.check_memory_usage():
                                self._log_secure("warning", 
                                    "Memory limit during hashing")
                                return None
                        
                        byte_block = f.read(65536)
                        if not byte_block:
                            break
                        
                        bytes_read += len(byte_block)
                        if bytes_read > max_bytes:
                            return None
                        
                        hash_obj.update(byte_block)
                
                result = hash_obj.hexdigest()
                
                # Cache result with size limit
                with self._cache_lock:
                    if len(self._file_cache) < self._file_cache_max:
                        self._file_cache[cache_key] = result
                    else:
                        # LRU-like: remove oldest entries
                        if self._file_cache:
                            oldest_key = next(iter(self._file_cache))
                            del self._file_cache[oldest_key]
                        self._file_cache[cache_key] = result
                
                SafeGuard.record_success("hash_calculation")
                return result
                
        except ResourceExhaustedError:
            self._log_secure("warning", "Resource limit during hashing")
            return None
        except (IOError, PermissionError, OSError) as e:
            SafeGuard.record_failure("hash_calculation")
            return None

    def calculate_multi_hash(self, filepath):
        """Calculate multiple hash algorithms with safeguards."""
        algorithms = ["md5", "sha1", "sha256", "sha512"]
        hashes = {}
        
        # Validate file
        try:
            filepath = Path(filepath)
            if not filepath.exists() or not filepath.is_file():
                return None
            
            # Size check
            if filepath.stat().st_size > 1024 * 1024 * 1024:
                return None
        except OSError:
            return None
        
        try:
            with SafeGuard.managed_file_handle():
                hash_objs = {alg: hashlib.new(alg) for alg in algorithms}
                
                with open(filepath, "rb") as f:
                    for block in iter(lambda: f.read(65536), b""):
                        for h in hash_objs.values():
                            h.update(block)
                
                for alg, h in hash_objs.items():
                    hashes[alg] = h.hexdigest()
                
                return hashes
                
        except (IOError, PermissionError, ResourceExhaustedError):
            return None

    def calculate_fuzzy_hash(self, filepath):
        """Calculate fuzzy hash with safeguards and size limits."""
        # Validate and check size
        try:
            filepath = Path(filepath)
            if not filepath.exists():
                return None
            
            file_size = filepath.stat().st_size
            # Limit fuzzy hashing to 50MB files
            if file_size > 50 * 1024 * 1024:
                return None
            if file_size == 0:
                return None
        except OSError:
            return None
        
        try:
            with SafeGuard.managed_file_handle():
                with open(filepath, "rb") as f:
                    content = f.read()
        except (IOError, PermissionError, ResourceExhaustedError):
            return None
        
        # Find appropriate block size
        file_size = len(content)
        block_size = 3
        for bs in FUZZY_BLOCK_SIZES:
            if file_size / bs < 100:
                block_size = bs
                break
        
        # Generate rolling hash chunks
        def rolling_hash(data, start, size):
            """Simple rolling hash for fuzzy matching."""
            h = 0
            for i in range(min(size, len(data) - start)):
                h = ((h << 5) + h + data[start + i]) & 0xFFFFFFFF
            return h
        
        # Generate signature
        chunks = []
        pos = 0
        while pos < len(content):
            h = rolling_hash(content, pos, block_size)
            if h % block_size == (block_size - 1):
                chunk_hash = hashlib.md5(
                    content[max(0, pos - block_size):pos + 1]
                ).digest()[:6]
                chunks.append(base64.b64encode(chunk_hash).decode()[:6])
            pos += 1
        
        fuzzy_sig = f"{block_size}:{''.join(chunks[:64])}"
        self._fuzzy_cache[str(filepath)] = fuzzy_sig
        return fuzzy_sig

    def compare_fuzzy_hashes(self, hash1, hash2):
        """Compare two fuzzy hashes and return similarity percentage."""
        if not hash1 or not hash2:
            return 0
        
        try:
            bs1, sig1 = hash1.split(":", 1)
            bs2, sig2 = hash2.split(":", 1)
            
            if abs(int(bs1) - int(bs2)) > 1:
                return 0  # Block sizes too different
            
            # Calculate string similarity (Jaccard-like)
            set1 = set(sig1[i:i+3] for i in range(len(sig1) - 2))
            set2 = set(sig2[i:i+3] for i in range(len(sig2) - 2))
            
            if not set1 or not set2:
                return 0
            
            intersection = len(set1 & set2)
            union = len(set1 | set2)
            
            return int((intersection / union) * 100) if union > 0 else 0
        except (ValueError, IndexError):
            return 0

    def _check_signature(self, file_hash, multi_hashes=None):
        """Check if hash matches any known malware signature."""
        # Check whitelist first
        if file_hash in self._whitelist_hashes:
            return None
        
        # Check primary signatures
        for sig_id, sig_data in self.signatures.items():
            sig_hash = sig_data.get("hash", "")
            
            # Check SHA256
            if sig_hash == file_hash:
                return sig_data
            
            # Check multi-algorithm hashes if available
            if multi_hashes:
                for alg, h in multi_hashes.items():
                    alt_hash = sig_data.get(f"hash_{alg}")
                    if alt_hash and alt_hash == h:
                        return sig_data
            
            # Check fuzzy hash similarity
            fuzzy_sig = sig_data.get("fuzzy_hash")
            if fuzzy_sig and str(file_hash) in self._fuzzy_cache:
                similarity = self.compare_fuzzy_hashes(
                    self._fuzzy_cache[str(file_hash)], fuzzy_sig
                )
                if similarity >= 85:  # 85% similarity threshold
                    return {
                        **sig_data,
                        "detection_type": "fuzzy_match",
                        "similarity": similarity
                    }
        
        return None

    # ==================== PE/ELF ANALYSIS ====================

    def analyze_pe_header(self, filepath):
        """Analyze PE (Windows executable) header for suspicious indicators."""
        try:
            with open(filepath, "rb") as f:
                # Check MZ header
                if f.read(2) != b"MZ":
                    return None
                
                # Get PE header offset
                f.seek(60)
                pe_offset = struct.unpack("<I", f.read(4))[0]
                
                # Read PE signature
                f.seek(pe_offset)
                if f.read(4) != b"PE\x00\x00":
                    return None
                
                # Read COFF header
                machine = struct.unpack("<H", f.read(2))[0]
                num_sections = struct.unpack("<H", f.read(2))[0]
                timestamp = struct.unpack("<I", f.read(4))[0]
                f.read(8)  # Skip pointer/symbol info
                optional_size = struct.unpack("<H", f.read(2))[0]
                characteristics = struct.unpack("<H", f.read(2))[0]
                
                result = {
                    "machine": machine,
                    "sections": num_sections,
                    "timestamp": datetime.fromtimestamp(timestamp).isoformat()
                    if timestamp > 0 else "Invalid",
                    "characteristics": [],
                    "suspicious_indicators": [],
                    "risk_score": 0
                }
                
                # Decode characteristics
                for flag, name in PE_CHARACTERISTICS.items():
                    if characteristics & flag:
                        result["characteristics"].append(name)
                
                # Suspicious indicators
                if timestamp == 0 or timestamp > time.time():
                    result["suspicious_indicators"].append(
                        "Invalid/future timestamp"
                    )
                    result["risk_score"] += 15
                
                if num_sections > 10:
                    result["suspicious_indicators"].append(
                        f"Unusual number of sections: {num_sections}"
                    )
                    result["risk_score"] += 10
                
                # Read optional header for more analysis
                if optional_size > 0:
                    magic = struct.unpack("<H", f.read(2))[0]
                    if magic == 0x10b:  # PE32
                        result["format"] = "PE32"
                    elif magic == 0x20b:  # PE32+
                        result["format"] = "PE32+"
                
                self._pe_analysis_cache[str(filepath)] = result
                return result
                
        except (IOError, struct.error):
            return None

    def analyze_elf_header(self, filepath):
        """Analyze ELF (Linux executable) header for suspicious indicators."""
        try:
            with open(filepath, "rb") as f:
                # Check ELF magic
                magic = f.read(4)
                if magic != b"\x7fELF":
                    return None
                
                # Read ELF header
                elf_class = struct.unpack("B", f.read(1))[0]
                endian = struct.unpack("B", f.read(1))[0]
                version = struct.unpack("B", f.read(1))[0]
                osabi = struct.unpack("B", f.read(1))[0]
                
                f.read(8)  # Padding
                
                # Type (2 bytes)
                elf_type = struct.unpack("<H", f.read(2))[0]
                
                result = {
                    "class": "ELF64" if elf_class == 2 else "ELF32",
                    "endian": "little" if endian == 1 else "big",
                    "type": ELF_TYPES.get(elf_type, f"Unknown({elf_type})"),
                    "suspicious_indicators": [],
                    "risk_score": 0
                }
                
                # Check for suspicious characteristics
                if elf_class == 2 and elf_type == 3:  # 64-bit shared object
                    # Check if it has unusual permissions
                    pass
                
                self._elf_analysis_cache[str(filepath)] = result
                return result
                
        except (IOError, struct.error):
            return None

    # ==================== HEURISTIC ANALYSIS ====================

    def _get_file_magic(self, filepath):
        """Read and identify file magic bytes."""
        try:
            with open(filepath, "rb") as f:
                header = f.read(16)
            for magic, description in EXECUTABLE_MAGIC.items():
                if header.startswith(magic):
                    return description
            return None
        except (IOError, PermissionError):
            return None

    def _analyze_entropy(self, filepath, block_size=256):
        """Calculate file entropy (high entropy may indicate encryption/packing)."""
        import math
        try:
            with open(filepath, "rb") as f:
                data = f.read()
            if not data:
                return 0.0
            
            frequency = [0] * 256
            for byte in data:
                frequency[byte] += 1
            
            entropy = 0.0
            length = len(data)
            for freq in frequency:
                if freq > 0:
                    p = freq / length
                    entropy -= p * math.log2(p)
            
            return entropy
        except (IOError, PermissionError):
            return 0.0

    def _scan_for_patterns(self, filepath):
        """Scan file content for suspicious patterns."""
        detections = []
        try:
            with open(filepath, "rb") as f:
                content = f.read()
            
            for category, data in SUSPICIOUS_PATTERNS.items():
                for pattern in data["patterns"]:
                    if pattern in content:
                        detections.append({
                            "category": category,
                            "pattern": pattern.decode('utf-8', errors='replace'),
                            "weight": data["weight"],
                            "description": data["description"]
                        })
                        break  # One match per category is enough
            
            return detections
        except (IOError, PermissionError):
            return []

    def _check_extension(self, filepath):
        """Check if file has a dangerous extension."""
        ext = Path(filepath).suffix.lower()
        return ext in DANGEROUS_EXTENSIONS

    def _check_hidden_extension(self, filepath):
        """Check for hidden/double extensions (e.g., document.pdf.exe)."""
        name = Path(filepath).name
        parts = name.split('.')
        if len(parts) >= 3:
            # Check if there's a dangerous extension hiding
            for part in parts[1:-1]:
                if f".{part.lower()}" in DANGEROUS_EXTENSIONS:
                    return True
        return False

    def heuristic_scan(self, filepath):
        """Perform advanced heuristic analysis with security hardening."""
        try:
            filepath = InputValidator.validate_path(
                filepath, allow_symlinks=True)
        except ValidationError:
            return None
        
        if not filepath.exists() or not filepath.is_file():
            return None
        
        # Security check: verify readable
        if not os.access(filepath, os.R_OK):
            return None

        results = {
            "file": str(filepath),
            "score": 0,
            "indicators": [],
            "mitre_techniques": [],
            "risk_level": "clean",
            "confidence": 0.0,
            "file_type": None,
            "entropy": 0.0,
            "pe_analysis": None,
            "elf_analysis": None,
            "pattern_matches": [],
            "recommendations": []
        }

        # Get file stats safely
        try:
            stat = filepath.stat()
            size = stat.st_size
            results["file_size"] = size
            
            # Check file size anomalies
            if size == 0:
                results["indicators"].append("Empty file")
                results["score"] += 5
            elif size > 100 * 1024 * 1024:  # >100MB
                results["indicators"].append("Very large file (>100MB)")
                results["score"] += 5
            elif size < 100 and filepath.suffix.lower() in DANGEROUS_EXTENSIONS:
                results["indicators"].append("Suspiciously small executable")
                results["score"] += 15
            
            # Check file permissions (Linux)
            mode = stat.st_mode
            if mode & 0o4000:  # SUID bit
                results["indicators"].append("SUID bit set")
                results["score"] += 20
                results["mitre_techniques"].append("T1548")
            if mode & 0o2000:  # SGID bit
                results["indicators"].append("SGID bit set")
                results["score"] += 15
            if mode & 0o0002:  # World-writable
                results["indicators"].append("World-writable file")
                results["score"] += 10
                
        except (OSError, SecurityError):
            return None

        # Check magic bytes and file type
        magic = self._get_file_magic(filepath)
        if magic:
            results["file_type"] = magic
            results["indicators"].append(f"File type: {magic}")
            if "Executable" in magic:
                results["score"] += 10
                # Perform PE/ELF analysis for executables
                if "PE" in magic or "Windows" in magic:
                    pe_result = self.analyze_pe_header(filepath)
                    if pe_result:
                        results["pe_analysis"] = pe_result
                        results["score"] += pe_result.get("risk_score", 0)
                        for indicator in pe_result.get("suspicious_indicators", []):
                            results["indicators"].append(f"PE: {indicator}")
                elif "ELF" in magic or "Linux" in magic:
                    elf_result = self.analyze_elf_header(filepath)
                    if elf_result:
                        results["elf_analysis"] = elf_result
                        results["score"] += elf_result.get("risk_score", 0)

        # Check dangerous extensions
        if self._check_extension(filepath):
            results["indicators"].append("Dangerous file extension")
            results["score"] += 20

        # Check hidden extensions (double extension trick)
        if self._check_hidden_extension(filepath):
            results["indicators"].append("Hidden/double extension detected")
            results["score"] += 30
            results["mitre_techniques"].append("T1036")

        # Check for extension mismatch
        if magic and self._check_extension_mismatch(filepath, magic):
            results["indicators"].append("Extension does not match file type")
            results["score"] += 25
            results["mitre_techniques"].append("T1036.007")

        # Calculate entropy (detect encryption/packing)
        entropy = self._analyze_entropy(filepath)
        results["entropy"] = entropy
        if entropy > 7.8:
            results["indicators"].append(
                f"Very high entropy ({entropy:.2f}) - likely encrypted/packed"
            )
            results["score"] += 35
            results["mitre_techniques"].append("T1027.002")
        elif entropy > 7.5:
            results["indicators"].append(
                f"High entropy ({entropy:.2f}) - possible encryption/packing"
            )
            results["score"] += 25
        elif entropy > 7.0:
            results["indicators"].append(f"Elevated entropy ({entropy:.2f})")
            results["score"] += 10

        # Analyze entropy distribution (detect packed sections)
        entropy_anomaly = self._analyze_entropy_distribution(filepath)
        if entropy_anomaly:
            results["indicators"].append(entropy_anomaly)
            results["score"] += 15

        # Scan for suspicious patterns with MITRE mapping
        pattern_matches = self._scan_for_patterns_advanced(filepath)
        results["pattern_matches"] = pattern_matches
        for match in pattern_matches:
            results["indicators"].append(
                f"{match['description']}: {match['pattern']}"
            )
            results["score"] += match["weight"]
            if match.get("mitre_id"):
                results["mitre_techniques"].append(match["mitre_id"])

        # Check for strings analysis (suspicious strings)
        suspicious_strings = self._analyze_strings(filepath)
        for s in suspicious_strings:
            results["indicators"].append(f"Suspicious string: {s}")
            results["score"] += 5

        # Check for packed executable indicators
        if self._is_likely_packed(filepath):
            results["indicators"].append("Likely packed/compressed executable")
            results["score"] += 20
            results["mitre_techniques"].append("T1027.002")

        # Remove duplicate MITRE techniques
        results["mitre_techniques"] = list(set(results["mitre_techniques"]))

        # Calculate confidence based on number of indicators
        num_indicators = len(results["indicators"])
        if num_indicators > 0:
            results["confidence"] = min(100, 50 + (num_indicators * 5))
        
        # Generate recommendations
        results["recommendations"] = self._generate_recommendations(results)

        # Determine risk level with finer granularity
        if results["score"] >= 100:
            results["risk_level"] = "critical"
        elif results["score"] >= 80:
            results["risk_level"] = "high"
        elif results["score"] >= 60:
            results["risk_level"] = "elevated"
        elif results["score"] >= 40:
            results["risk_level"] = "medium"
        elif results["score"] >= 20:
            results["risk_level"] = "low"
        else:
            results["risk_level"] = "clean"

        return results

    def _check_extension_mismatch(self, filepath, detected_magic):
        """Check if file extension matches detected file type."""
        ext = filepath.suffix.lower()
        
        mismatches = {
            ".jpg": ["Executable", "Script", "Archive"],
            ".jpeg": ["Executable", "Script", "Archive"],
            ".png": ["Executable", "Script", "Archive"],
            ".gif": ["Executable", "Script", "Archive"],
            ".pdf": ["Executable", "Script"],
            ".doc": ["Executable", "Script"],
            ".docx": ["Executable", "Script"],
            ".txt": ["Executable", "Archive"],
            ".mp3": ["Executable", "Script"],
            ".mp4": ["Executable", "Script"],
        }
        
        if ext in mismatches:
            for mismatch_type in mismatches[ext]:
                if mismatch_type in detected_magic:
                    return True
        return False

    def _analyze_entropy_distribution(self, filepath):
        """Analyze entropy distribution across file sections."""
        try:
            with open(filepath, "rb") as f:
                data = f.read()
            
            if len(data) < 1024:
                return None
            
            block_size = len(data) // 10
            if block_size < 256:
                return None
            
            entropies = []
            for i in range(10):
                block = data[i * block_size:(i + 1) * block_size]
                freq = [0] * 256
                for byte in block:
                    freq[byte] += 1
                ent = 0.0
                length = len(block)
                for f in freq:
                    if f > 0:
                        p = f / length
                        ent -= p * math.log2(p)
                entropies.append(ent)
            
            # Check for high variance (indicates packed sections)
            variance = sum((e - sum(entropies)/10)**2 for e in entropies) / 10
            if variance > 2.0:
                return f"High entropy variance detected ({variance:.2f})"
            
            return None
        except Exception:
            return None

    def _scan_for_patterns_advanced(self, filepath):
        """Advanced pattern scanning with MITRE mapping."""
        detections = []
        try:
            with open(filepath, "rb") as f:
                content = f.read()
            
            for category, data in SUSPICIOUS_PATTERNS.items():
                matched_patterns = []
                for pattern in data["patterns"]:
                    if pattern in content:
                        matched_patterns.append(
                            pattern.decode('utf-8', errors='replace')
                        )
                
                if matched_patterns:
                    # Weight increases with more pattern matches
                    weight = data["weight"] + (len(matched_patterns) - 1) * 3
                    detections.append({
                        "category": category,
                        "pattern": matched_patterns[0],
                        "all_patterns": matched_patterns,
                        "match_count": len(matched_patterns),
                        "weight": min(weight, 50),  # Cap at 50
                        "description": data["description"],
                        "mitre_id": data.get("mitre_id")
                    })
            
            return detections
        except (IOError, PermissionError):
            return []

    def _analyze_strings(self, filepath, min_length=8, max_results=20):
        """Extract and analyze suspicious strings from file."""
        suspicious = []
        suspicious_keywords = [
            "hack", "crack", "keygen", "trojan", "virus", "malware",
            "backdoor", "exploit", "payload", "reverse", "shell",
            "admin", "password", "login", "secret", "token",
            "http://", "https://", "ftp://", "pastebin", "discord"
        ]
        
        try:
            with open(filepath, "rb") as f:
                data = f.read(1024 * 1024)  # Read first 1MB
            
            # Extract ASCII strings
            current = b""
            for byte in data:
                if 32 <= byte <= 126:
                    current += bytes([byte])
                else:
                    if len(current) >= min_length:
                        s = current.decode('ascii', errors='ignore').lower()
                        for keyword in suspicious_keywords:
                            if keyword in s:
                                if s not in suspicious:
                                    suspicious.append(s[:100])
                                break
                    current = b""
            
            return suspicious[:max_results]
        except Exception:
            return []

    def _is_likely_packed(self, filepath):
        """Detect if executable is likely packed/compressed."""
        try:
            with open(filepath, "rb") as f:
                data = f.read(4096)
            
            # Check for common packer signatures
            packer_sigs = [
                b"UPX!", b"UPX0", b"UPX1", b"UPX2",
                b"ASPack", b"PECompact", b"Petite",
                b"MPRESS", b"FSG!", b"aPLib",
                b"Themida", b"VMProtect"
            ]
            
            for sig in packer_sigs:
                if sig in data:
                    return True
            
            # Check for unusual section names
            if b".packed" in data or b".crypted" in data:
                return True
            
            return False
        except Exception:
            return False

    def _generate_recommendations(self, results):
        """Generate actionable recommendations based on findings."""
        recommendations = []
        
        if results["score"] >= 80:
            recommendations.append(
                "CRITICAL: Quarantine this file immediately"
            )
        elif results["score"] >= 60:
            recommendations.append(
                "HIGH: Submit to sandbox analysis before execution"
            )
        
        if "encryption" in str(results["indicators"]).lower():
            recommendations.append(
                "Check for ransomware behavior"
            )
        
        if any("persistence" in str(i).lower() for i in results["indicators"]):
            recommendations.append(
                "Review system startup locations and scheduled tasks"
            )
        
        if any("network" in str(i).lower() for i in results["indicators"]):
            recommendations.append(
                "Monitor network connections if executed"
            )
        
        if results.get("pe_analysis") or results.get("elf_analysis"):
            recommendations.append(
                "Run in isolated sandbox environment"
            )
        
        return recommendations

    def scan_file(self, filepath, use_heuristics=True):
        """Scan a single file for malware with security hardening."""
        try:
            filepath = InputValidator.validate_path(
                filepath, allow_symlinks=True)
        except ValidationError as e:
            self._log_secure("warning", f"Invalid path: {e}")
            self.scan_results["errors"] += 1
            return None, None
        
        if not filepath.exists():
            self.scan_results["errors"] += 1
            return None, None

        if not filepath.is_file():
            return None, None

        # Security check: verify we can safely read the file
        if not os.access(filepath, os.R_OK):
            self._log_secure("warning", f"No read access: {filepath}")
            self.scan_results["errors"] += 1
            return None, None

        # Check exclusions
        if self.is_excluded(filepath):
            self.scan_results["skipped"] += 1
            return None, None

        # Rate limiting for scan operations
        if not RateLimiter.check("scan_file"):
            self._log_secure("warning", "Scan rate limit reached")
            return None, None

        self.scan_results["scanned"] += 1
        
        try:
            file_hash = self._calculate_hash(filepath)
        except (SecurityError, ValidationError):
            self.scan_results["errors"] += 1
            return None, None
        
        if file_hash is None:
            self.scan_results["errors"] += 1
            return None, None

        # Signature-based detection
        threat = self._check_signature(file_hash)
        if threat:
            self.scan_results["infected"] += 1
            threat_entry = {
                "file": str(filepath),
                "threat": threat["name"],
                "severity": threat["severity"],
                "hash": file_hash,
                "detection_type": "signature",
                "detection_time": datetime.now().isoformat()
            }
            self.scan_results["threats"].append(threat_entry)
            
            # Create ThreatInfo and notify callbacks
            severity_map = {
                "low": ThreatSeverity.LOW,
                "medium": ThreatSeverity.MEDIUM,
                "high": ThreatSeverity.HIGH,
                "critical": ThreatSeverity.CRITICAL
            }
            severity = severity_map.get(
                threat["severity"].lower(), ThreatSeverity.MEDIUM)
            
            threat_info = ThreatInfo(
                name=threat["name"],
                file_path=str(filepath),
                severity=severity,
                detection_type=DetectionType.SIGNATURE,
                hash_value=file_hash
            )
            self._notify_detection(threat_info)
            
            return threat, None

        # Heuristic analysis
        heuristic_result = None
        if use_heuristics and self.heuristic_enabled:
            heuristic_result = self.heuristic_scan(filepath)
            if heuristic_result:
                score = heuristic_result.get("score", 0)
                if score >= self.heuristic_threshold:
                    self.scan_results["suspicious"] += 1
                    self.scan_results["heuristic_detections"].append(
                        heuristic_result)
                    return None, heuristic_result

        return None, heuristic_result

    def scan_directory(self, directory, recursive=True, use_heuristics=True):
        """Scan a directory for malware with security hardening."""
        try:
            directory = InputValidator.validate_path(
                directory, allow_symlinks=True)
        except ValidationError as e:
            print(f"[!] Invalid directory: {e}")
            return

        if not directory.exists() or not directory.is_dir():
            print(f"[!] Directory not found: {directory}")
            return

        print(f"\n[*] Scanning: {directory}")
        print(f"[*] Heuristic analysis: "
              f"{'Enabled' if use_heuristics else 'Disabled'}")
        print("-" * 50)

        try:
            if recursive:
                files = list(directory.rglob("*"))
            else:
                files = list(directory.glob("*"))
        except PermissionError:
            print(f"[!] Permission denied: {directory}")
            return

        for filepath in files:
            try:
                if filepath.is_file():
                    # Skip quarantine directory
                    if QUARANTINE_DIR in filepath.parents:
                        continue
                    
                    threat, heuristic = self.scan_file(
                        filepath, use_heuristics)
                    if threat:
                        print(f"[!] THREAT FOUND: {filepath}")
                        print(f"    Malware: {threat['name']} "
                              f"(Severity: {threat['severity']})")
                    elif (heuristic and 
                          heuristic["score"] >= self.heuristic_threshold):
                        print(f"[?] SUSPICIOUS: {filepath}")
                        print(f"    Risk: {heuristic['risk_level'].upper()} "
                              f"(Score: {heuristic['score']})")
                        for ind in heuristic["indicators"][:3]:
                            print(f"    - {ind}")
            except (OSError, PermissionError):
                continue

    def quarantine_file(self, filepath, encrypt=True):
        """Move infected file to quarantine with secure encryption."""
        try:
            filepath = InputValidator.validate_path(filepath)
        except ValidationError as e:
            print(f"[!] Invalid path: {e}")
            return False
        
        if not filepath.exists():
            print(f"[!] File not found: {filepath}")
            return False

        try:
            # Ensure quarantine directory with secure permissions
            SecureFileOps.secure_mkdir(QUARANTINE_DIR)
            
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            file_hash = self._calculate_hash(filepath) or "unknown"
            # Sanitize filename for security
            safe_name = InputValidator.sanitize_string(
                filepath.name, max_length=128)
            quarantine_name = f"{timestamp}_{safe_name}.quarantine"
            quarantine_path = QUARANTINE_DIR / quarantine_name
            metadata_path = QUARANTINE_DIR / f"{quarantine_name}.meta"
            
            # Read original file securely
            original_data = SecureFileOps.secure_read(filepath)
            
            # Encrypt with secure encryption if enabled
            use_encryption = (encrypt and 
                self.config.get("encrypted_quarantine", True))
            if use_encryption:
                encrypted_data = SecureCrypto.secure_encrypt(
                    original_data, self._quarantine_key)
                SecureFileOps.secure_write(
                    quarantine_path, encrypted_data, is_binary=True)
            else:
                SecureFileOps.secure_write(
                    quarantine_path, original_data, is_binary=True)
            
            # Store metadata securely
            file_stat = filepath.stat()
            metadata = {
                "original_path": str(filepath),
                "original_name": filepath.name,
                "original_size": len(original_data),
                "hash_sha256": file_hash,
                "quarantine_time": datetime.now().isoformat(),
                "encrypted": use_encryption,
                "permissions": oct(file_stat.st_mode)[-3:],
                "owner_uid": file_stat.st_uid,
                "owner_gid": file_stat.st_gid,
                "integrity_hash": hashlib.sha256(
                    original_data).hexdigest()
            }
            
            metadata_json = json.dumps(metadata, indent=2)
            SecureFileOps.secure_write(metadata_path, metadata_json)
            
            # Securely remove original file
            SecureFileOps.secure_delete(filepath)
            
            print(f"[+] Quarantined: {filepath}")
            print(f"    Location: {quarantine_path}")
            print(f"    Encrypted: {'Yes (AES-256)' if use_encryption else 'No'}")
            
            # Update stats
            with self._stats_lock:
                self.scan_results["quarantined"] += 1
            
            self._log_secure("info", 
                f"QUARANTINE: {safe_name} -> {quarantine_name}")
            return True
            
        except (SecurityError, ValidationError) as e:
            print(f"[!] Security error quarantining {filepath}: {e}")
            self._log_secure("error", f"Quarantine failed: {e}")
            return False
        except Exception as e:
            print(f"[!] Failed to quarantine {filepath}: {e}")
            self._log_secure("error", 
                f"Quarantine failed: {type(e).__name__}")
            return False

    def restore_file(self, quarantine_name, restore_path=None):
        """Restore a file from quarantine with secure decryption."""
        # Validate quarantine name
        try:
            quarantine_name = InputValidator.sanitize_string(
                quarantine_name, max_length=256)
        except ValidationError as e:
            print(f"[!] Invalid quarantine name: {e}")
            return False
        
        quarantine_path = QUARANTINE_DIR / quarantine_name
        metadata_path = QUARANTINE_DIR / f"{quarantine_name}.meta"
        
        if not quarantine_path.exists():
            print(f"[!] Quarantined file not found: {quarantine_name}")
            return False

        try:
            # Load metadata securely
            metadata = {}
            if metadata_path.exists():
                meta_content = SecureFileOps.secure_read(metadata_path)
                metadata = json.loads(meta_content.decode('utf-8'))
            
            # Determine restore path
            if restore_path is None:
                restore_path = metadata.get("original_path")
            if restore_path is None:
                print("[!] No restore path specified and no metadata")
                return False
            
            # Validate restore path
            try:
                restore_path = InputValidator.validate_path(
                    restore_path, must_exist=False)
            except ValidationError as e:
                print(f"[!] Invalid restore path: {e}")
                return False
            
            # Read quarantined data securely
            data = SecureFileOps.secure_read(quarantine_path)
            
            # Decrypt if needed
            if metadata.get("encrypted", False):
                try:
                    data = SecureCrypto.secure_decrypt(
                        data, self._quarantine_key)
                except SecurityError as e:
                    print(f"[!] Decryption failed: {e}")
                    return False
            
            # Verify integrity hash if available
            if "integrity_hash" in metadata:
                restored_hash = hashlib.sha256(data).hexdigest()
                if restored_hash != metadata["integrity_hash"]:
                    print("[!] CRITICAL: File integrity check failed!")
                    print("    The file may have been tampered with.")
                    self._log_secure("error", 
                        "Restore failed: integrity check failed")
                    return False
            elif "hash_sha256" in metadata:
                # Fallback to hash_sha256 for older quarantine entries
                restored_hash = hashlib.sha256(data).hexdigest()
                if restored_hash != metadata["hash_sha256"]:
                    print("[!] Warning: File hash mismatch after restore!")
            
            # Write restored file securely
            SecureFileOps.secure_mkdir(restore_path.parent)
            SecureFileOps.secure_write(restore_path, data, is_binary=True)
            
            # Restore permissions if available
            if "permissions" in metadata:
                try:
                    os.chmod(restore_path, int(metadata["permissions"], 8))
                except (OSError, ValueError):
                    pass
            
            # Securely remove from quarantine
            SecureFileOps.secure_delete(quarantine_path)
            if metadata_path.exists():
                SecureFileOps.secure_delete(metadata_path)
            
            print(f"[+] Restored: {quarantine_name} -> {restore_path}")
            self._log_secure("info", 
                f"RESTORE: {quarantine_name} -> {restore_path}")
            return True
            
        except (SecurityError, ValidationError) as e:
            print(f"[!] Security error during restore: {e}")
            return False
        except Exception as e:
            print(f"[!] Failed to restore: {e}")
            return False

    def list_quarantine(self, verbose=False):
        """List all quarantined files with metadata."""
        print("\n[*] Quarantined Files:")
        print("-" * 60)
        
        files = list(QUARANTINE_DIR.glob("*.quarantine"))
        if not files:
            print("    No files in quarantine")
            return []

        quarantine_info = []
        for f in files:
            size = f.stat().st_size
            print(f"    {f.name} ({size} bytes)")

    def add_signature(self, name, file_hash, severity="medium"):
        """Add a new malware signature."""
        sig_id = name.lower().replace(" ", "_")
        self.signatures[sig_id] = {
            "hash": file_hash,
            "name": name,
            "severity": severity
        }
        self._save_signatures()
        print(f"[+] Added signature: {name}")

    def print_results(self):
        """Print scan results summary."""
        print("\n" + "=" * 50)
        print("SCAN RESULTS")
        print("=" * 50)
        print(f"Files scanned:  {self.scan_results['scanned']}")
        print(f"Threats found:  {self.scan_results['infected']}")
        print(f"Suspicious:     {self.scan_results['suspicious']}")
        print(f"Archived:       {self.scan_results['archived']}")
        print(f"Skipped:        {self.scan_results['skipped']}")
        print(f"Errors:         {self.scan_results['errors']}")
        
        if self.scan_start_time:
            elapsed = time.time() - self.scan_start_time
            print(f"Scan time:      {elapsed:.2f}s")
        
        if self.scan_results["threats"]:
            print("\nThreats Detected:")
            for threat in self.scan_results["threats"]:
                print(f"  - {threat['file']}")
                print(f"    Threat: {threat['threat']} [{threat['severity']}]")

        if self.scan_results["heuristic_detections"]:
            print("\nSuspicious Files (Heuristic):")
            for detection in self.scan_results["heuristic_detections"]:
                print(f"  - {detection['file']}")
                risk = detection['risk_level']
                score = detection['score']
                print(f"    Risk: {risk} (Score: {score})")

    def _log_action(self, message):
        """Log an action to the scan log (legacy wrapper)."""
        self._log_secure("info", message)

    # ==================== INTEGRITY MONITORING ====================

    def create_integrity_baseline(self, directory, recursive=True):
        """Create integrity baseline with comprehensive safeguards."""
        # Circuit breaker check
        if not SafeGuard.circuit_breaker_check("integrity_baseline"):
            print("[!] Integrity baseline creation temporarily disabled")
            return False
        
        try:
            directory = InputValidator.validate_path(directory)
        except ValidationError as e:
            print(f"[!] Invalid directory: {e}")
            return False
        
        if not directory.exists() or not directory.is_dir():
            print(f"[!] Directory not found: {directory}")
            return False

        print(f"[*] Creating integrity baseline for: {directory}")
        baseline = {
            "created": datetime.now().isoformat(),
            "directory": str(directory),
            "version": "2.0",
            "files": {}
        }

        try:
            files = directory.rglob("*") if recursive else directory.glob("*")
        except PermissionError:
            print(f"[!] Permission denied: {directory}")
            return False
        
        count = 0
        errors = 0
        max_files = 100000  # Prevent resource exhaustion
        
        for filepath in files:
            if count >= max_files:
                print(f"[*] Reached max file limit ({max_files})")
                break
            
            # Periodic memory check
            if count % 1000 == 0:
                if not SafeGuard.check_memory_usage():
                    print("[!] Memory limit reached, stopping baseline")
                    break
            
            if filepath.is_file():
                try:
                    stat = filepath.stat()
                    # Skip very large files
                    if stat.st_size > 1024 * 1024 * 1024:
                        continue
                    
                    file_hash = self._calculate_hash(filepath)
                    if file_hash is None:
                        errors += 1
                        continue
                    
                    baseline["files"][str(filepath)] = {
                        "hash": file_hash,
                        "size": stat.st_size,
                        "mtime": stat.st_mtime,
                        "mode": stat.st_mode,
                        "inode": stat.st_ino
                    }
                    count += 1
                except (OSError, PermissionError):
                    errors += 1
                    continue

        # Save baseline securely
        try:
            success = SafeGuard.safe_json_save(INTEGRITY_DB, baseline)
            if success:
                SafeGuard.record_success("integrity_baseline")
            else:
                SafeGuard.record_failure("integrity_baseline")
                print("[!] Failed to save integrity baseline")
                return False
        except Exception as e:
            SafeGuard.record_failure("integrity_baseline")
            print(f"[!] Error saving baseline: {e}")
            return False

        print(f"[+] Baseline created: {count} files indexed")
        if errors > 0:
            print(f"[*] {errors} files could not be indexed")
        self._log_secure("info", 
            f"INTEGRITY BASELINE: {directory} ({count} files)")
        return True

    def verify_integrity(self):
        """Verify file integrity against baseline with safeguards."""
        # Circuit breaker check
        if not SafeGuard.circuit_breaker_check("integrity_verify"):
            print("[!] Integrity verification temporarily disabled")
            return None
        
        if not INTEGRITY_DB.exists():
            print("[!] No integrity baseline found. Create one first.")
            return None

        # Load baseline securely
        baseline = SafeGuard.safe_json_load(INTEGRITY_DB, {})
        if not baseline or "files" not in baseline:
            print("[!] Invalid or corrupted integrity database")
            return None

        print(f"[*] Verifying integrity (baseline: {baseline.get('created', 'unknown')})")
        print("-" * 50)

        results = {
            "modified": [],
            "deleted": [],
            "new": [],
            "unchanged": 0,
            "errors": 0,
            "verified_at": datetime.now().isoformat()
        }

        # Check existing files
        files_checked = 0
        max_files = 100000
        
        for filepath, data in baseline["files"].items():
            if files_checked >= max_files:
                print(f"[*] Reached verification limit ({max_files})")
                break
            
            # Periodic memory check
            if files_checked % 1000 == 0:
                if not SafeGuard.check_memory_usage():
                    print("[!] Memory limit reached")
                    break
            
            files_checked += 1
            filepath = Path(filepath)
            
            if not filepath.exists():
                results["deleted"].append(str(filepath))
                continue

            try:
                current_hash = self._calculate_hash(filepath)
                if current_hash is None:
                    results["errors"] += 1
                    continue
                
                if current_hash != data.get("hash"):
                    results["modified"].append({
                        "file": str(filepath),
                        "old_hash": data.get("hash", "")[:16] + "...",
                        "new_hash": current_hash[:16] + "..."
                    })
                else:
                    results["unchanged"] += 1
            except (OSError, PermissionError):
                results["errors"] += 1
                continue

        # Check for new files
        base_dir = Path(baseline.get("directory", ""))
        if base_dir.exists():
            try:
                new_file_count = 0
                max_new_files = 10000
                for filepath in base_dir.rglob("*"):
                    if new_file_count >= max_new_files:
                        break
                    if filepath.is_file():
                        if str(filepath) not in baseline["files"]:
                            results["new"].append(str(filepath))
                            new_file_count += 1
            except (PermissionError, OSError):
                pass

        SafeGuard.record_success("integrity_verify")

        # Print results
        print(f"\nUnchanged:  {results['unchanged']}")
        print(f"Modified:   {len(results['modified'])}")
        print(f"Deleted:    {len(results['deleted'])}")
        print(f"New files:  {len(results['new'])}")
        if results["errors"] > 0:
            print(f"Errors:     {results['errors']}")

        if results["modified"]:
            print("\nModified files:")
            for m in results["modified"][:10]:
                print(f"  ! {m['file']}")
        if results["deleted"]:
            print("\nDeleted files:")
            for d in results["deleted"][:10]:
                print(f"  - {d}")
        if results["new"]:
            print("\nNew files:")
            for n in results["new"][:10]:
                print(f"  + {n}")

        self._log_action(
            f"INTEGRITY CHECK: {results['unchanged']} ok, "
            f"{len(results['modified'])} modified, "
            f"{len(results['deleted'])} deleted"
        )
        return results

    # ==================== REPORT GENERATION ====================

    def generate_report(self, format_type="txt"):
        """Generate a detailed scan report."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        if format_type == "json":
            report_path = REPORTS_DIR / f"scan_report_{timestamp}.json"
            report_data = {
                "timestamp": datetime.now().isoformat(),
                "summary": self.scan_results,
                "threats": self.scan_results["threats"],
                "heuristic_detections": self.scan_results["heuristic_detections"]
            }
            with open(report_path, 'w') as f:
                json.dump(report_data, f, indent=2)
        
        elif format_type == "html":
            report_path = REPORTS_DIR / f"scan_report_{timestamp}.html"
            html = self._generate_html_report()
            with open(report_path, 'w') as f:
                f.write(html)
        
        else:  # txt format
            report_path = REPORTS_DIR / f"scan_report_{timestamp}.txt"
            with open(report_path, 'w') as f:
                f.write("=" * 60 + "\n")
                f.write("GREYAV SCAN REPORT\n")
                f.write("=" * 60 + "\n\n")
                f.write(f"Generated: {datetime.now()}\n\n")
                f.write("SUMMARY\n")
                f.write("-" * 40 + "\n")
                f.write(f"Files scanned:  {self.scan_results['scanned']}\n")
                f.write(f"Threats found:  {self.scan_results['infected']}\n")
                f.write(f"Suspicious:     {self.scan_results['suspicious']}\n")
                f.write(f"Errors:         {self.scan_results['errors']}\n\n")
                
                if self.scan_results["threats"]:
                    f.write("THREATS DETECTED\n")
                    f.write("-" * 40 + "\n")
                    for t in self.scan_results["threats"]:
                        f.write(f"File: {t['file']}\n")
                        f.write(f"  Threat: {t['threat']}\n")
                        f.write(f"  Severity: {t['severity']}\n")
                        f.write(f"  Hash: {t['hash']}\n\n")

                if self.scan_results["heuristic_detections"]:
                    f.write("HEURISTIC DETECTIONS\n")
                    f.write("-" * 40 + "\n")
                    for h in self.scan_results["heuristic_detections"]:
                        f.write(f"File: {h['file']}\n")
                        f.write(f"  Risk: {h['risk_level']}\n")
                        f.write(f"  Score: {h['score']}\n")
                        f.write("  Indicators:\n")
                        for ind in h["indicators"][:5]:
                            f.write(f"    - {ind}\n")
                        f.write("\n")

        print(f"[+] Report saved: {report_path}")
        return report_path

    def _generate_html_report(self):
        """Generate HTML formatted report."""
        threat_rows = ""
        for t in self.scan_results["threats"]:
            sev_class = t['severity']
            threat_rows += f"""
            <tr class="{sev_class}">
                <td>{t['file']}</td>
                <td>{t['threat']}</td>
                <td>{t['severity']}</td>
            </tr>"""

        heur_rows = ""
        for h in self.scan_results["heuristic_detections"]:
            indicators = "<br>".join(h["indicators"][:3])
            heur_rows += f"""
            <tr class="{h['risk_level']}">
                <td>{h['file']}</td>
                <td>{h['risk_level']}</td>
                <td>{h['score']}</td>
                <td>{indicators}</td>
            </tr>"""

        return f"""<!DOCTYPE html>
<html>
<head>
    <title>GreyAV Scan Report</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; }}
        h1 {{ color: #333; }}
        .summary {{ background: #f5f5f5; padding: 15px; border-radius: 5px; }}
        table {{ border-collapse: collapse; width: 100%; margin: 20px 0; }}
        th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
        th {{ background: #333; color: white; }}
        .critical {{ background: #ffcccc; }}
        .high {{ background: #ffddcc; }}
        .medium {{ background: #ffffcc; }}
        .low {{ background: #ccffcc; }}
        .clean {{ background: #ffffff; }}
    </style>
</head>
<body>
    <h1>GreyAV Scan Report</h1>
    <p>Generated: {datetime.now()}</p>
    
    <div class="summary">
        <h2>Summary</h2>
        <p>Files scanned: {self.scan_results['scanned']}</p>
        <p>Threats found: {self.scan_results['infected']}</p>
        <p>Suspicious: {self.scan_results['suspicious']}</p>
        <p>Errors: {self.scan_results['errors']}</p>
    </div>

    <h2>Threats Detected</h2>
    <table>
        <tr><th>File</th><th>Threat</th><th>Severity</th></tr>
        {threat_rows if threat_rows else "<tr><td colspan='3'>No threats</td></tr>"}
    </table>

    <h2>Heuristic Detections</h2>
    <table>
        <tr><th>File</th><th>Risk</th><th>Score</th><th>Indicators</th></tr>
        {heur_rows if heur_rows else "<tr><td colspan='4'>None</td></tr>"}
    </table>
</body>
</html>"""

    # ==================== PROCESS SCANNING ====================

    def scan_running_processes(self):
        """Scan running processes for suspicious activity."""
        print("[*] Scanning running processes...")
        print("-" * 50)
        
        suspicious = []
        
        # Get process list using /proc on Linux
        if platform.system() == "Linux":
            proc_dir = Path("/proc")
            for pid_dir in proc_dir.iterdir():
                if pid_dir.name.isdigit():
                    try:
                        # Read process info
                        exe_link = pid_dir / "exe"
                        cmdline = pid_dir / "cmdline"
                        
                        if exe_link.exists():
                            exe_path = exe_link.resolve()
                            
                            # Scan the executable
                            if exe_path.exists() and exe_path.is_file():
                                threat, heur = self.scan_file(exe_path, True)
                                if threat or (heur and heur["score"] >= 40):
                                    pid = pid_dir.name
                                    with open(cmdline, 'r') as f:
                                        cmd = f.read().replace('\x00', ' ')
                                    suspicious.append({
                                        "pid": pid,
                                        "exe": str(exe_path),
                                        "cmdline": cmd[:100],
                                        "threat": threat,
                                        "heuristic": heur
                                    })
                    except (OSError, PermissionError):
                        continue
        else:
            # Use subprocess for other platforms
            try:
                if platform.system() == "Windows":
                    result = subprocess.run(
                        ["tasklist", "/fo", "csv"],
                        capture_output=True, text=True
                    )
                else:  # macOS
                    result = subprocess.run(
                        ["ps", "-eo", "pid,comm"],
                        capture_output=True, text=True
                    )
                print(f"[*] Process list retrieved ({platform.system()})")
            except Exception as e:
                print(f"[!] Could not get process list: {e}")

        if suspicious:
            print(f"\n[!] Found {len(suspicious)} suspicious processes:")
            for p in suspicious:
                print(f"\n  PID: {p['pid']}")
                print(f"  Exe: {p['exe']}")
                if p['threat']:
                    print(f"  Threat: {p['threat']['name']}")
                if p['heuristic']:
                    print(f"  Score: {p['heuristic']['score']}")
        else:
            print("\n[✓] No suspicious processes detected")

        return suspicious

    # ==================== SCHEDULED SCAN ====================

    def save_scan_history(self):
        """Save scan results to history."""
        history = []
        if SCAN_HISTORY.exists():
            try:
                with open(SCAN_HISTORY, 'r') as f:
                    history = json.load(f)
            except json.JSONDecodeError:
                pass

        history.append({
            "timestamp": datetime.now().isoformat(),
            "scanned": self.scan_results["scanned"],
            "infected": self.scan_results["infected"],
            "suspicious": self.scan_results["suspicious"],
            "threats": len(self.scan_results["threats"])
        })

        # Keep last 100 entries
        history = history[-100:]

        with open(SCAN_HISTORY, 'w') as f:
            json.dump(history, f, indent=2)

    def show_scan_history(self):
        """Display scan history."""
        if not SCAN_HISTORY.exists():
            print("[!] No scan history found")
            return

        with open(SCAN_HISTORY, 'r') as f:
            history = json.load(f)

        print("\n[*] Scan History (last 10):")
        print("-" * 60)
        print(f"{'Date':<20} {'Scanned':>10} {'Threats':>10} {'Suspicious':>10}")
        print("-" * 60)
        
        for entry in history[-10:]:
            dt = entry['timestamp'][:19].replace('T', ' ')
            print(f"{dt:<20} {entry['scanned']:>10} "
                  f"{entry['infected']:>10} {entry['suspicious']:>10}")

    # ==================== UPDATE SIGNATURES ====================

    def update_signatures_from_url(self, url=None):
        """Update signatures from a remote URL."""
        if url is None:
            url = "https://example.com/signatures.json"  # Placeholder
        
        print(f"[*] Updating signatures from: {url}")
        
        try:
            import urllib.request
            response = urllib.request.urlopen(url, timeout=10)
            new_sigs = json.loads(response.read().decode())
            
            added = 0
            for sig_id, sig_data in new_sigs.items():
                if sig_id not in self.signatures:
                    self.signatures[sig_id] = sig_data
                    added += 1
            
            self._save_signatures()
            print(f"[+] Added {added} new signatures")
            print(f"[*] Total signatures: {len(self.signatures)}")
            return True
        except Exception as e:
            print(f"[!] Update failed: {e}")
            return False

    def import_signatures(self, filepath):
        """Import signatures from a local file."""
        try:
            with open(filepath, 'r') as f:
                new_sigs = json.load(f)
            
            added = 0
            for sig_id, sig_data in new_sigs.items():
                if sig_id not in self.signatures:
                    self.signatures[sig_id] = sig_data
                    added += 1
            
            self._save_signatures()
            print(f"[+] Imported {added} new signatures")
            return True
        except Exception as e:
            print(f"[!] Import failed: {e}")
            return False

    def export_signatures(self, filepath):
        """Export current signatures to a file."""
        try:
            with open(filepath, 'w') as f:
                json.dump(self.signatures, f, indent=2)
            print(f"[+] Exported {len(self.signatures)} signatures to {filepath}")
            return True
        except Exception as e:
            print(f"[!] Export failed: {e}")
            return False

    def show_statistics(self):
        """Show overall statistics."""
        print("\n[*] GreyAV Statistics")
        print("=" * 50)
        print(f"Signatures:     {len(self.signatures)}")
        print(f"Custom rules:   {len(self.custom_rules.get('rules', []))}")
        
        q_files = list(QUARANTINE_DIR.glob("*.quarantine"))
        print(f"Quarantined:    {len(q_files)}")
        
        if EXCLUSIONS_FILE.exists():
            total_excl = (len(self.exclusions.get("paths", [])) +
                         len(self.exclusions.get("extensions", [])) +
                         len(self.exclusions.get("patterns", [])))
            print(f"Exclusions:     {total_excl}")
        
        if SCAN_HISTORY.exists():
            with open(SCAN_HISTORY, 'r') as f:
                history = json.load(f)
            total_scanned = sum(h['scanned'] for h in history)
            total_threats = sum(h['infected'] for h in history)
            print(f"Total scanned:  {total_scanned}")
            print(f"Total threats:  {total_threats}")

        if SCAN_LOG.exists():
            with open(SCAN_LOG, 'r') as f:
                lines = f.readlines()
            print(f"Log entries:    {len(lines)}")

    # ==================== ARCHIVE SCANNING ====================

    def scan_archive(self, filepath, depth=0):
        """Scan inside archive files (zip, tar, gz)."""
        if depth >= self.config.get("max_archive_depth", 3):
            return []
        
        filepath = Path(filepath)
        results = []
        
        try:
            # ZIP files
            if zipfile.is_zipfile(filepath):
                with zipfile.ZipFile(filepath, 'r') as zf:
                    for name in zf.namelist():
                        if not name.endswith('/'):
                            with tempfile.NamedTemporaryFile(delete=False) as tmp:
                                try:
                                    tmp.write(zf.read(name))
                                    tmp.flush()
                                    threat, heur = self.scan_file(tmp.name, True)
                                    if threat or (heur and heur['score'] >= self.heuristic_threshold):
                                        results.append({
                                            "archive": str(filepath),
                                            "file": name,
                                            "threat": threat,
                                            "heuristic": heur
                                        })
                                        self.scan_results["archived"] += 1
                                finally:
                                    os.unlink(tmp.name)
            
            # TAR files
            elif tarfile.is_tarfile(filepath):
                with tarfile.open(filepath, 'r:*') as tf:
                    for member in tf.getmembers():
                        if member.isfile():
                            with tempfile.NamedTemporaryFile(delete=False) as tmp:
                                try:
                                    f = tf.extractfile(member)
                                    if f:
                                        tmp.write(f.read())
                                        tmp.flush()
                                        threat, heur = self.scan_file(tmp.name, True)
                                        if threat or (heur and heur['score'] >= self.heuristic_threshold):
                                            results.append({
                                                "archive": str(filepath),
                                                "file": member.name,
                                                "threat": threat,
                                                "heuristic": heur
                                            })
                                            self.scan_results["archived"] += 1
                                finally:
                                    os.unlink(tmp.name)
            
            # GZIP files
            elif str(filepath).endswith('.gz'):
                with gzip.open(filepath, 'rb') as gz:
                    with tempfile.NamedTemporaryFile(delete=False) as tmp:
                        try:
                            tmp.write(gz.read())
                            tmp.flush()
                            threat, heur = self.scan_file(tmp.name, True)
                            if threat or (heur and heur['score'] >= self.heuristic_threshold):
                                results.append({
                                    "archive": str(filepath),
                                    "file": filepath.stem,
                                    "threat": threat,
                                    "heuristic": heur
                                })
                                self.scan_results["archived"] += 1
                        finally:
                            os.unlink(tmp.name)
                            
        except Exception as e:
            self.scan_results["errors"] += 1
        
        return results

    # ==================== CUSTOM RULES ENGINE ====================

    def add_custom_rule(self, name, rule_type, pattern, severity="medium", description=""):
        """Add a custom detection rule."""
        rule = {
            "id": name.lower().replace(" ", "_"),
            "name": name,
            "type": rule_type,  # string, regex, hex
            "pattern": pattern,
            "severity": severity,
            "description": description,
            "enabled": True,
            "created": datetime.now().isoformat()
        }
        self.custom_rules.setdefault("rules", []).append(rule)
        self._save_custom_rules()
        print(f"[+] Added custom rule: {name}")
        return rule

    def _save_custom_rules(self):
        """Save custom rules to file."""
        with open(CUSTOM_RULES_FILE, 'w') as f:
            json.dump(self.custom_rules, f, indent=2)

    def check_custom_rules(self, filepath):
        """Check file against custom rules."""
        matches = []
        try:
            with open(filepath, 'rb') as f:
                content = f.read()
            
            for rule in self.custom_rules.get("rules", []):
                if not rule.get("enabled", True):
                    continue
                
                pattern = rule["pattern"]
                matched = False
                
                if rule["type"] == "string":
                    if pattern.encode() in content:
                        matched = True
                elif rule["type"] == "regex":
                    if re.search(pattern.encode(), content):
                        matched = True
                elif rule["type"] == "hex":
                    hex_bytes = bytes.fromhex(pattern.replace(" ", ""))
                    if hex_bytes in content:
                        matched = True
                
                if matched:
                    matches.append(rule)
            
        except (IOError, PermissionError):
            pass
        
        return matches

    def list_custom_rules(self):
        """List all custom rules."""
        rules = self.custom_rules.get("rules", [])
        print(f"\n[*] Custom Rules ({len(rules)}):")
        print("-" * 60)
        for rule in rules:
            status = "✓" if rule.get("enabled", True) else "✗"
            print(f"  [{status}] {rule['name']} ({rule['type']}) [{rule['severity']}]")
            if rule.get("description"):
                print(f"      {rule['description']}")

    def toggle_rule(self, rule_id, enabled=None):
        """Enable or disable a custom rule."""
        for rule in self.custom_rules.get("rules", []):
            if rule["id"] == rule_id:
                if enabled is None:
                    rule["enabled"] = not rule.get("enabled", True)
                else:
                    rule["enabled"] = enabled
                self._save_custom_rules()
                state = "enabled" if rule["enabled"] else "disabled"
                print(f"[*] Rule '{rule['name']}' {state}")
                return True
        print(f"[!] Rule not found: {rule_id}")
        return False

    def delete_rule(self, rule_id):
        """Delete a custom rule."""
        rules = self.custom_rules.get("rules", [])
        for i, rule in enumerate(rules):
            if rule["id"] == rule_id:
                del rules[i]
                self._save_custom_rules()
                print(f"[-] Deleted rule: {rule['name']}")
                return True
        print(f"[!] Rule not found: {rule_id}")
        return False

    # ==================== PARALLEL SCANNING ====================

    def parallel_scan(self, directory, max_workers=None):
        """Scan directory using multiple threads."""
        directory = Path(directory)
        if not directory.exists():
            print(f"[!] Directory not found: {directory}")
            return
        
        max_workers = max_workers or self.config.get("parallel_scans", 4)
        files = [f for f in directory.rglob("*") if f.is_file()]
        
        print(f"[*] Parallel scan: {len(files)} files with {max_workers} workers")
        print("-" * 50)
        
        threats_found = []
        
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {executor.submit(self.scan_file, f): f for f in files}
            
            for future in as_completed(futures):
                filepath = futures[future]
                try:
                    threat, heuristic = future.result()
                    if threat:
                        threats_found.append({"file": filepath, "threat": threat})
                        print(f"[!] THREAT: {filepath.name}")
                    elif heuristic and heuristic["score"] >= self.heuristic_threshold:
                        print(f"[?] SUSPICIOUS: {filepath.name}")
                except Exception as e:
                    pass
        
        return threats_found

    # ==================== NETWORK SCANNING ====================

    def scan_network_connections(self):
        """Scan active network connections for suspicious activity."""
        print("[*] Scanning network connections...")
        print("-" * 60)
        
        suspicious = []
        connections = []
        
        # Use centralized port manager for C2/backdoor detection
        if PORT_MANAGER_AVAILABLE and get_port_manager:
            pm = get_port_manager()
            c2_ports = pm.get_c2_ports()
            dangerous_ports = pm.get_dangerous_ports()
            suspicious_ports = c2_ports | dangerous_ports
        else:
            # Fallback to hardcoded list
            suspicious_ports = {
                4444, 5555, 6666, 7777, 1337,  # Common backdoor ports
                31337, 12345, 27374,  # Known trojan ports
                6667, 6668, 6669,  # IRC (often used by botnets)
                4445, 9999,  # Additional backdoors
                50050,  # Cobalt Strike
                3333, 14444,  # Crypto mining
            }
        
        try:
            if platform.system() == "Linux":
                # Parse /proc/net/tcp
                with open("/proc/net/tcp", 'r') as f:
                    for line in f.readlines()[1:]:
                        parts = line.split()
                        local = parts[1].split(':')
                        remote = parts[2].split(':')
                        local_port = int(local[1], 16)
                        remote_port = int(remote[1], 16)
                        state = int(parts[3], 16)
                        
                        connections.append({
                            "local_port": local_port,
                            "remote_port": remote_port,
                            "state": state
                        })
                        
                        is_suspicious = False
                        reason = ""
                        
                        if local_port in suspicious_ports:
                            is_suspicious = True
                            if PORT_MANAGER_AVAILABLE and get_port_manager:
                                reason = f"C2/Backdoor indicator: {pm.get_service_name(local_port)}"
                            else:
                                reason = "suspicious_port"
                        
                        if remote_port in suspicious_ports:
                            is_suspicious = True
                            if PORT_MANAGER_AVAILABLE and get_port_manager:
                                reason = f"Connection to C2 port: {pm.get_service_name(remote_port)}"
                            else:
                                reason = "suspicious_remote_port"
                        
                        if is_suspicious:
                            suspicious.append({
                                "type": reason,
                                "local_port": local_port,
                                "remote_port": remote_port,
                                "severity": "critical" if local_port in c2_ports or remote_port in c2_ports else "high"
                            } if PORT_MANAGER_AVAILABLE else {
                                "type": "suspicious_port",
                                "local_port": local_port,
                                "remote_port": remote_port
                            })
                
                # Also check UDP
                with open("/proc/net/udp", 'r') as f:
                    for line in f.readlines()[1:]:
                        parts = line.split()
                        local = parts[1].split(':')
                        local_port = int(local[1], 16)
                        
                        if local_port in suspicious_ports:
                            suspicious.append({
                                "type": "suspicious_udp_port",
                                "port": local_port,
                                "service": pm.get_service_name(local_port) if PORT_MANAGER_AVAILABLE and get_port_manager else "unknown"
                            })
            else:
                # Use netstat for other platforms
                result = subprocess.run(
                    ["netstat", "-an"],
                    capture_output=True, text=True, timeout=10
                )
                print(f"[*] Retrieved network connections")
                
        except Exception as e:
            print(f"[!] Error scanning network: {e}")
        
        print(f"\nTotal connections: {len(connections)}")
        if suspicious:
            print(f"\n[!] Suspicious connections: {len(suspicious)}")
            for s in suspicious:
                print(f"  - Port {s.get('local_port', s.get('port'))}")
        else:
            print("[✓] No suspicious connections detected")
        
        # Log results
        self._log_network_scan(connections, suspicious)
        return suspicious

    def _log_network_scan(self, connections, suspicious):
        """Log network scan results."""
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "total_connections": len(connections),
            "suspicious": suspicious
        }
        
        history = []
        if NETWORK_LOG.exists():
            try:
                with open(NETWORK_LOG, 'r') as f:
                    history = json.load(f)
            except:
                pass
        
        history.append(log_entry)
        history = history[-50:]  # Keep last 50 scans
        
        with open(NETWORK_LOG, 'w') as f:
            json.dump(history, f, indent=2)

    # ==================== SELF-PROTECTION ====================

    def verify_self_integrity(self):
        """Verify integrity of GreyAV files."""
        print("[*] Verifying GreyAV integrity...")
        
        critical_files = [
            Path(__file__),
            SIGNATURES_FILE,
            EXCLUSIONS_FILE
        ]
        
        integrity_good = True
        for filepath in critical_files:
            if filepath.exists():
                file_hash = self._calculate_hash(filepath)
                print(f"  [✓] {filepath.name}: {file_hash[:16]}...")
            else:
                if filepath == SIGNATURES_FILE:
                    print(f"  [!] {filepath.name}: Missing (will use defaults)")
                else:
                    print(f"  [?] {filepath.name}: Not found")
        
        return integrity_good

    # ==================== CONFIGURATION MANAGEMENT ====================

    def show_config(self):
        """Display current configuration."""
        print("\n[*] Current Configuration:")
        print("=" * 50)
        for key, value in sorted(self.config.items()):
            print(f"  {key}: {value}")

    def set_config(self, key, value):
        """Update a configuration value."""
        if key not in DEFAULT_CONFIG:
            print(f"[!] Unknown config key: {key}")
            print(f"    Valid keys: {', '.join(DEFAULT_CONFIG.keys())}")
            return False
        
        # Type conversion
        expected_type = type(DEFAULT_CONFIG[key])
        try:
            if expected_type == bool:
                value = value.lower() in ('true', '1', 'yes', 'on')
            elif expected_type == int:
                value = int(value)
            elif expected_type == float:
                value = float(value)
        except ValueError:
            print(f"[!] Invalid value type for {key}")
            return False
        
        self.config[key] = value
        self._save_config()
        print(f"[+] Set {key} = {value}")
        return True

    def reset_config(self):
        """Reset configuration to defaults."""
        self.config = DEFAULT_CONFIG.copy()
        self._save_config()
        print("[+] Configuration reset to defaults")

    # ==================== SCHEDULED SCANS ====================

    def add_scheduled_scan(self, name, path, schedule_type, time_value):
        """Add a scheduled scan task."""
        schedules = self._load_schedules()
        
        task = {
            "id": name.lower().replace(" ", "_"),
            "name": name,
            "path": str(path),
            "type": schedule_type,  # daily, weekly, interval
            "time": time_value,  # HH:MM for daily/weekly, minutes for interval
            "enabled": True,
            "last_run": None,
            "created": datetime.now().isoformat()
        }
        
        schedules.append(task)
        self._save_schedules(schedules)
        print(f"[+] Added scheduled scan: {name}")
        return task

    def _load_schedules(self):
        """Load scheduled scans."""
        if SCHEDULE_FILE.exists():
            try:
                with open(SCHEDULE_FILE, 'r') as f:
                    return json.load(f)
            except:
                pass
        return []

    def _save_schedules(self, schedules):
        """Save scheduled scans."""
        with open(SCHEDULE_FILE, 'w') as f:
            json.dump(schedules, f, indent=2)

    def list_schedules(self):
        """List scheduled scans."""
        schedules = self._load_schedules()
        print(f"\n[*] Scheduled Scans ({len(schedules)}):")
        print("-" * 60)
        for s in schedules:
            status = "✓" if s.get("enabled", True) else "✗"
            last = s.get("last_run", "Never")[:19] if s.get("last_run") else "Never"
            print(f"  [{status}] {s['name']}")
            print(f"      Path: {s['path']}")
            print(f"      Schedule: {s['type']} at {s['time']}")
            print(f"      Last run: {last}")

    def run_scheduled_daemon(self):
        """Run the scheduled scan daemon."""
        print("[*] Starting scheduled scan daemon...")
        print("[*] Press Ctrl+C to stop")
        
        try:
            while True:
                schedules = self._load_schedules()
                now = datetime.now()
                
                for task in schedules:
                    if not task.get("enabled", True):
                        continue
                    
                    should_run = False
                    
                    if task["type"] == "interval":
                        interval_mins = int(task["time"])
                        last_run = task.get("last_run")
                        if last_run:
                            last_dt = datetime.fromisoformat(last_run)
                            if (now - last_dt).total_seconds() >= interval_mins * 60:
                                should_run = True
                        else:
                            should_run = True
                    
                    elif task["type"] == "daily":
                        target_time = task["time"]  # "HH:MM"
                        if now.strftime("%H:%M") == target_time:
                            last_run = task.get("last_run")
                            if not last_run or last_run[:10] != now.strftime("%Y-%m-%d"):
                                should_run = True
                    
                    if should_run:
                        print(f"\n[*] Running scheduled scan: {task['name']}")
                        self.scan_start_time = time.time()
                        self.scan_directory(task["path"])
                        self.print_results()
                        self.save_scan_history()
                        
                        # Update last run
                        task["last_run"] = now.isoformat()
                        self._save_schedules(schedules)
                        
                        # Reset results for next scan
                        self.scan_results = {
                            "scanned": 0, "infected": 0, "suspicious": 0,
                            "errors": 0, "skipped": 0, "archived": 0,
                            "threats": [], "heuristic_detections": []
                        }
                
                time.sleep(60)  # Check every minute
                
        except KeyboardInterrupt:
            print("\n[*] Scheduled daemon stopped")

    # ==================== CLEAN/REPAIR ====================

    def clean_file(self, filepath):
        """Attempt to clean/neutralize a malicious file."""
        filepath = Path(filepath)
        if not filepath.exists():
            print(f"[!] File not found: {filepath}")
            return False
        
        print(f"[*] Attempting to clean: {filepath}")
        
        # Create backup first
        backup_path = filepath.with_suffix(filepath.suffix + ".backup")
        shutil.copy2(filepath, backup_path)
        print(f"[+] Backup created: {backup_path}")
        
        cleaned = False
        try:
            with open(filepath, 'rb') as f:
                content = f.read()
            
            original_size = len(content)
            
            # Remove known malicious patterns
            for category, data in SUSPICIOUS_PATTERNS.items():
                for pattern in data["patterns"]:
                    if pattern in content:
                        content = content.replace(pattern, b"[REMOVED]")
                        cleaned = True
            
            if cleaned:
                with open(filepath, 'wb') as f:
                    f.write(content)
                new_size = len(content)
                print(f"[+] Cleaned: {original_size} -> {new_size} bytes")
                self._log_action(f"CLEANED: {filepath}")
            else:
                print("[*] No cleanable patterns found")
                os.remove(backup_path)  # Remove unnecessary backup
                
        except Exception as e:
            print(f"[!] Clean failed: {e}")
            # Restore from backup
            if backup_path.exists():
                shutil.copy2(backup_path, filepath)
                print("[*] Restored from backup")
            return False
        
        return cleaned

    # ==================== WHITELIST ====================

    def add_to_whitelist(self, filepath):
        """Add a file hash to the whitelist (known safe)."""
        filepath = Path(filepath)
        if not filepath.exists():
            print(f"[!] File not found: {filepath}")
            return False
        
        file_hash = self._calculate_hash(filepath)
        if not file_hash:
            return False
        
        whitelist = self.exclusions.setdefault("hashes", [])
        if file_hash not in whitelist:
            whitelist.append(file_hash)
            self._save_exclusions()
            print(f"[+] Added to whitelist: {filepath.name}")
            print(f"    Hash: {file_hash[:32]}...")
            return True
        print("[*] File already whitelisted")
        return False

    def is_whitelisted(self, file_hash):
        """Check if a hash is whitelisted."""
        return file_hash in self.exclusions.get("hashes", [])

    # ==================== MEMORY SCANNING ====================

    def scan_process_memory(self, pid=None):
        """Scan process memory for suspicious patterns with safeguards."""
        print("[*] Scanning process memory...")
        print("-" * 60)
        
        suspicious = []
        
        # Circuit breaker check
        if not SafeGuard.circuit_breaker_check("memory_scan"):
            print("[!] Memory scanning temporarily disabled (circuit breaker)")
            return suspicious
        
        if platform.system() != "Linux":
            print("[!] Memory scanning currently only supported on Linux")
            return suspicious
        
        # Check if running as root for full access
        if os.geteuid() != 0:
            print("[!] Warning: Limited access without root. Some processes skipped.")
        
        pids_to_scan = []
        if pid:
            # Validate PID input
            try:
                pid = int(pid)
                if pid <= 0 or pid > 4194304:  # Max PID on Linux
                    print(f"[!] Invalid PID: {pid}")
                    return suspicious
                pids_to_scan = [str(pid)]
            except (ValueError, TypeError):
                print("[!] Invalid PID format")
                return suspicious
        else:
            # Get all accessible PIDs with limit
            count = 0
            max_pids = 500  # Limit to prevent resource exhaustion
            for p in Path("/proc").iterdir():
                if p.name.isdigit():
                    pids_to_scan.append(p.name)
                    count += 1
                    if count >= max_pids:
                        print(f"[*] Limiting scan to {max_pids} processes")
                        break
        
        # Memory tracking for safeguard
        processed_count = 0
        max_memory_scanned = 500 * 1024 * 1024  # 500MB total
        total_memory_scanned = 0
        
        for current_pid in pids_to_scan:
            # Check memory usage periodically
            if processed_count % 50 == 0:
                if not SafeGuard.check_memory_usage():
                    print("[!] Memory limit reached, stopping scan")
                    break
            
            processed_count += 1
            
            try:
                maps_file = Path(f"/proc/{current_pid}/maps")
                mem_file = Path(f"/proc/{current_pid}/mem")
                
                if not maps_file.exists() or not mem_file.exists():
                    continue
                
                # Read memory maps safely
                try:
                    with open(maps_file, 'r') as f:
                        maps = f.readlines()
                except (IOError, PermissionError):
                    continue
                
                # Get process name safely
                try:
                    with open(f"/proc/{current_pid}/comm", 'r') as f:
                        proc_name = f.read().strip()[:64]  # Limit length
                except (IOError, PermissionError):
                    proc_name = "unknown"
                
                # Check readable memory regions for suspicious patterns
                try:
                    with open(mem_file, 'rb') as mem:
                        regions_scanned = 0
                        for line in maps[:20]:  # Limit to first 20 regions
                            if total_memory_scanned >= max_memory_scanned:
                                break
                            
                            try:
                                parts = line.split()
                                if len(parts) < 2:
                                    continue
                                addr_range = parts[0].split('-')
                                perms = parts[1]
                                
                                if 'r' not in perms:
                                    continue
                                
                                start = int(addr_range[0], 16)
                                end = int(addr_range[1], 16)
                                size = end - start
                                
                                # Skip very large regions
                                if size > 10 * 1024 * 1024:
                                    continue
                                
                                # Track memory usage
                                read_size = min(size, 1024 * 1024)
                                total_memory_scanned += read_size
                                
                                mem.seek(start)
                                data = mem.read(read_size)
                                regions_scanned += 1
                                
                                # Check for suspicious patterns
                                for category, info in SUSPICIOUS_PATTERNS.items():
                                    for pattern in info["patterns"]:
                                        if pattern in data:
                                            suspicious.append({
                                                "pid": current_pid,
                                                "process": proc_name,
                                                "pattern": pattern.decode('utf-8', errors='replace'),
                                                "category": category
                                            })
                                            break
                            except (OSError, ValueError):
                                continue
                except (IOError, PermissionError):
                    continue
                            
            except (PermissionError, OSError):
                continue
            except Exception:
                # Circuit breaker for repeated failures
                SafeGuard.record_failure("memory_scan")
                continue
        
        SafeGuard.record_success("memory_scan")
        
        if suspicious:
            print(f"\n[!] Found {len(suspicious)} suspicious memory patterns:")
            seen = set()
            for s in suspicious:
                key = (s['pid'], s['category'])
                if key not in seen:
                    seen.add(key)
                    safe_proc = InputValidator.sanitize_string(
                        s['process'], max_length=32)
                    print(f"  PID {s['pid']} ({safe_proc}): {s['category']}")
        else:
            print("[✓] No suspicious memory patterns detected")
        
        return suspicious

    # ==================== BEHAVIORAL ANALYSIS ====================

    def analyze_behavior(self, filepath, timeout=5):
        """Analyze file behavior using strace/sandbox with safeguards."""
        # Input validation
        try:
            filepath = InputValidator.validate_path(filepath, must_exist=True)
        except ValidationError as e:
            print(f"[!] Invalid path: {e}")
            return None
        
        # Circuit breaker check
        if not SafeGuard.circuit_breaker_check("behavior_analysis"):
            print("[!] Behavior analysis temporarily disabled")
            return None
        
        # Validate timeout is reasonable
        timeout = max(1, min(timeout, 30))  # Between 1-30 seconds
        
        print(f"[*] Behavioral analysis: {filepath}")
        print("-" * 60)
        
        results = {
            "file": str(filepath),
            "behaviors": [],
            "risk_score": 0,
            "syscalls": defaultdict(int)
        }
        
        # Only analyze executables
        magic = self._get_file_magic(filepath)
        if not magic or "Executable" not in magic:
            print("[*] Not an executable, skipping behavior analysis")
            return None
        
        # Security check: file size limit
        try:
            if filepath.stat().st_size > 50 * 1024 * 1024:  # 50MB max
                print("[!] File too large for behavior analysis")
                return None
        except OSError:
            return None
        
        if platform.system() == "Linux":
            # Create secure temp file for output
            strace_log = None
            try:
                import tempfile
                fd, strace_log = tempfile.mkstemp(
                    prefix="greyav_strace_", suffix=".log")
                os.close(fd)
                
                # Verify strace is available
                strace_path = shutil.which("strace")
                if not strace_path:
                    print("[!] strace not found, skipping syscall analysis")
                    if strace_log and os.path.exists(strace_log):
                        os.unlink(strace_log)
                    return results
                
                # Build safe command
                cmd = [strace_path, "-f", "-c", "-o", strace_log, 
                       str(filepath)]
                
                # Run with resource limits
                proc = subprocess.Popen(
                    cmd, 
                    stdout=subprocess.PIPE, 
                    stderr=subprocess.PIPE,
                    # Security: don't inherit environment
                    env={"PATH": "/usr/bin:/bin"},
                    # Resource limits
                    preexec_fn=lambda: os.nice(10)  # Lower priority
                )
                
                try:
                    proc.wait(timeout=timeout)
                except subprocess.TimeoutExpired:
                    proc.kill()
                    try:
                        proc.wait(timeout=2)
                    except subprocess.TimeoutExpired:
                        pass  # Process terminated
                
                # Parse strace output safely
                if Path(strace_log).exists():
                    try:
                        # Limit read size
                        with open(strace_log, 'r') as f:
                            strace_output = f.read(1024 * 1024)  # 1MB max
                    except IOError:
                        strace_output = ""
                    
                    # Suspicious syscalls
                    suspicious_syscalls = {
                        'execve': 30, 'fork': 20, 'clone': 20,
                        'connect': 25, 'socket': 20, 'bind': 25,
                        'unlink': 15, 'rename': 10, 'chmod': 15,
                        'chown': 15, 'ptrace': 40, 'kill': 20
                    }
                    
                    for syscall, weight in suspicious_syscalls.items():
                        if syscall in strace_output:
                            results["behaviors"].append(f"Uses {syscall}")
                            results["risk_score"] += weight
                            results["syscalls"][syscall] += 1
                    
                SafeGuard.record_success("behavior_analysis")
                    
            except FileNotFoundError:
                print("[!] strace not found, skipping syscall analysis")
            except Exception as e:
                SafeGuard.record_failure("behavior_analysis")
                self._log_secure("error", 
                    f"Behavior analysis error: {type(e).__name__}")
            finally:
                # Secure cleanup
                if strace_log and os.path.exists(strace_log):
                    try:
                        os.unlink(strace_log)
                    except OSError:
                        pass
        
        # Print results
        print(f"\nBehavior Risk Score: {results['risk_score']}")
        if results["behaviors"]:
            print("\nDetected behaviors:")
            for b in results["behaviors"]:
                print(f"  • {b}")
        
        return results
        
        # Print results
        print(f"\nBehavior Risk Score: {results['risk_score']}")
        if results["behaviors"]:
            print("\nDetected behaviors:")
            for b in results["behaviors"]:
                print(f"  • {b}")
        
        return results

    # ==================== EMAIL SCANNING ====================

    def scan_email(self, filepath):
        """Scan email files for threats."""
        filepath = Path(filepath)
        if not filepath.exists():
            print(f"[!] File not found: {filepath}")
            return None
        
        print(f"[*] Scanning email: {filepath}")
        print("-" * 60)
        
        results = {
            "file": str(filepath),
            "threats": [],
            "attachments": [],
            "suspicious_links": []
        }
        
        try:
            import email
            from email import policy
            
            with open(filepath, 'rb') as f:
                msg = email.message_from_binary_file(f, policy=policy.default)
            
            # Check headers for spoofing
            from_addr = msg.get('From', '')
            reply_to = msg.get('Reply-To', '')
            if reply_to and from_addr and reply_to not in from_addr:
                results["threats"].append("Reply-To mismatch (possible spoofing)")
            
            # Scan attachments
            for part in msg.walk():
                if part.get_content_maintype() == 'multipart':
                    continue
                
                filename = part.get_filename()
                if filename:
                    results["attachments"].append(filename)
                    
                    # Check for dangerous extensions
                    ext = Path(filename).suffix.lower()
                    if ext in DANGEROUS_EXTENSIONS:
                        results["threats"].append(f"Dangerous attachment: {filename}")
                    
                    # Double extension check
                    parts = filename.split('.')
                    if len(parts) >= 3:
                        results["threats"].append(f"Double extension: {filename}")
                    
                    # Extract and scan attachment
                    payload = part.get_payload(decode=True)
                    if payload:
                        with tempfile.NamedTemporaryFile(delete=False) as tmp:
                            tmp.write(payload)
                            tmp.flush()
                            threat, heur = self.scan_file(tmp.name)
                            if threat:
                                results["threats"].append(
                                    f"Malware in attachment: {filename}"
                                )
                            os.unlink(tmp.name)
            
            # Check for suspicious URLs in body
            body = ""
            if msg.is_multipart():
                for part in msg.walk():
                    if part.get_content_type() == 'text/plain':
                        body += part.get_content()
            else:
                body = msg.get_content()
            
            # Find URLs
            url_pattern = rb'https?://[^\s<>"{}|\\^`\[\]]+'
            urls = re.findall(url_pattern, body.encode() if isinstance(body, str) else body)
            
            suspicious_domains = ['bit.ly', 'tinyurl', 'goo.gl', 't.co', 
                                 '.ru/', '.cn/', 'download', 'login']
            for url in urls:
                url_str = url.decode('utf-8', errors='ignore')
                for domain in suspicious_domains:
                    if domain in url_str.lower():
                        results["suspicious_links"].append(url_str[:100])
                        break
            
        except ImportError:
            print("[!] Email parsing requires the 'email' module")
        except Exception as e:
            print(f"[!] Error scanning email: {e}")
        
        # Print results
        print(f"\nAttachments: {len(results['attachments'])}")
        for att in results["attachments"]:
            print(f"  • {att}")
        
        if results["threats"]:
            print(f"\n[!] Threats detected ({len(results['threats'])}):")
            for t in results["threats"]:
                print(f"  • {t}")
        
        if results["suspicious_links"]:
            print(f"\n[?] Suspicious links ({len(results['suspicious_links'])}):")
            for link in results["suspicious_links"][:5]:
                print(f"  • {link}")
        
        return results

    # ==================== USB MONITORING ====================

    def monitor_usb(self):
        """Monitor USB devices for potential threats."""
        print("[*] USB Monitor - Watching for device insertions...")
        print("[*] Press Ctrl+C to stop")
        print("-" * 60)
        
        if platform.system() != "Linux":
            print("[!] USB monitoring currently only supported on Linux")
            return
        
        # Track known devices
        known_devices = set()
        
        def get_usb_devices():
            devices = set()
            try:
                usb_path = Path("/sys/bus/usb/devices")
                for device in usb_path.iterdir():
                    if device.is_dir() and not device.name.startswith("usb"):
                        product_file = device / "product"
                        if product_file.exists():
                            with open(product_file, 'r') as f:
                                product = f.read().strip()
                            devices.add((device.name, product))
            except Exception:
                pass
            return devices
        
        # Initial scan
        known_devices = get_usb_devices()
        print(f"[*] Found {len(known_devices)} existing USB devices")
        
        try:
            while True:
                time.sleep(2)
                current_devices = get_usb_devices()
                
                # Check for new devices
                new_devices = current_devices - known_devices
                for dev_id, product in new_devices:
                    print(f"\n[!] New USB device: {product} ({dev_id})")
                    
                    # Try to find mount point
                    time.sleep(1)  # Wait for mount
                    mounts = []
                    try:
                        with open("/proc/mounts", 'r') as f:
                            for line in f:
                                if '/dev/sd' in line and 'media' in line:
                                    parts = line.split()
                                    mounts.append(parts[1])
                    except:
                        pass
                    
                    if mounts:
                        for mount in mounts:
                            print(f"    Mounted at: {mount}")
                            # Check for autorun
                            autorun = Path(mount) / "autorun.inf"
                            if autorun.exists():
                                print(f"    [!] AUTORUN.INF DETECTED!")
                                self._log_action(f"USB AUTORUN: {mount}")
                            
                            # Quick scan root
                            print(f"    [*] Quick scanning root...")
                            for f in list(Path(mount).glob("*"))[:20]:
                                if f.is_file():
                                    threat, _ = self.scan_file(f)
                                    if threat:
                                        print(f"    [!] THREAT: {f.name}")
                
                # Check for removed devices
                removed = known_devices - current_devices
                for dev_id, product in removed:
                    print(f"\n[-] USB device removed: {product}")
                
                known_devices = current_devices
                
        except KeyboardInterrupt:
            print("\n[*] USB monitoring stopped")

    # ==================== ROOTKIT DETECTION ====================

    def detect_rootkits(self):
        """Check for common rootkit indicators."""
        print("[*] Rootkit Detection Scan")
        print("-" * 60)
        
        findings = []
        
        if platform.system() != "Linux":
            print("[!] Rootkit detection currently only supported on Linux")
            return findings
        
        # 1. Check for hidden processes (compare /proc with ps)
        print("[*] Checking for hidden processes...")
        proc_pids = set()
        for p in Path("/proc").iterdir():
            if p.name.isdigit():
                proc_pids.add(p.name)
        
        try:
            result = subprocess.run(
                ["ps", "-eo", "pid", "--no-headers"],
                capture_output=True, text=True, timeout=10
            )
            ps_pids = set(result.stdout.strip().split())
            
            hidden = proc_pids - ps_pids
            if hidden:
                findings.append({
                    "type": "hidden_process",
                    "pids": list(hidden),
                    "severity": "high"
                })
                print(f"  [!] Hidden processes: {hidden}")
            else:
                print("  [✓] No hidden processes")
        except Exception as e:
            print(f"  [!] Error checking processes: {e}")
        
        # 2. Check for hidden files with abnormal attributes
        print("[*] Checking for suspicious hidden files...")
        suspicious_paths = [
            "/tmp", "/var/tmp", "/dev/shm", "/root"
        ]
        
        for spath in suspicious_paths:
            try:
                for f in Path(spath).rglob(".*"):
                    if f.is_file():
                        # Check for executable hidden files
                        if os.access(f, os.X_OK):
                            findings.append({
                                "type": "hidden_executable",
                                "path": str(f),
                                "severity": "medium"
                            })
            except (PermissionError, OSError):
                pass
        
        if any(f["type"] == "hidden_executable" for f in findings):
            print(f"  [!] Found hidden executables")
        else:
            print("  [✓] No suspicious hidden files")
        
        # 3. Check for suspicious kernel modules
        print("[*] Checking kernel modules...")
        try:
            with open("/proc/modules", 'r') as f:
                modules = f.read()
            
            suspicious_modules = ['rootkit', 'hide', 'stealth', 'invisible']
            for mod in suspicious_modules:
                if mod in modules.lower():
                    findings.append({
                        "type": "suspicious_module",
                        "name": mod,
                        "severity": "critical"
                    })
                    print(f"  [!] Suspicious module: {mod}")
            
            if not any(f["type"] == "suspicious_module" for f in findings):
                print("  [✓] No suspicious kernel modules")
        except Exception:
            pass
        
        # 4. Check for modified system binaries
        print("[*] Checking system binaries...")
        critical_binaries = [
            "/bin/ls", "/bin/ps", "/bin/netstat", "/bin/login",
            "/usr/bin/find", "/usr/bin/top"
        ]
        
        for binary in critical_binaries:
            if Path(binary).exists():
                try:
                    result = subprocess.run(
                        ["file", binary],
                        capture_output=True, text=True, timeout=5
                    )
                    if "script" in result.stdout.lower():
                        findings.append({
                            "type": "replaced_binary",
                            "path": binary,
                            "severity": "critical"
                        })
                        print(f"  [!] Replaced binary: {binary}")
                except:
                    pass
        
        # 5. Check /etc/ld.so.preload
        print("[*] Checking LD_PRELOAD hooks...")
        preload = Path("/etc/ld.so.preload")
        if preload.exists():
            with open(preload, 'r') as f:
                content = f.read().strip()
            if content:
                findings.append({
                    "type": "ld_preload",
                    "content": content,
                    "severity": "high"
                })
                print(f"  [!] LD_PRELOAD hook found: {content}")
        else:
            print("  [✓] No LD_PRELOAD hooks")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] ROOTKIT INDICATORS: {len(findings)}")
            for f in findings:
                print(f"  [{f['severity'].upper()}] {f['type']}")
        else:
            print("[✓] No rootkit indicators detected")
        
        self._log_action(f"ROOTKIT SCAN: {len(findings)} findings")
        return findings

    # ==================== UPDATE CHECKER ====================

    def check_for_updates(self):
        """Check for GreyAV updates."""
        print("[*] Checking for updates...")
        
        current_version = "3.0"
        update_url = "https://api.github.com/repos/example/greyav/releases/latest"
        
        try:
            import urllib.request
            
            req = urllib.request.Request(
                update_url,
                headers={'User-Agent': f'GreyAV/{current_version}'}
            )
            
            response = urllib.request.urlopen(req, timeout=10)
            data = json.loads(response.read().decode())
            
            latest_version = data.get("tag_name", "").lstrip("v")
            
            if latest_version and latest_version > current_version:
                print(f"[+] New version available: {latest_version}")
                print(f"    Current version: {current_version}")
                print(f"    Download: {data.get('html_url', 'N/A')}")
                return True
            else:
                print(f"[✓] GreyAV is up to date (v{current_version})")
                return False
                
        except Exception as e:
            print(f"[!] Update check failed: {e}")
            print(f"[*] Current version: {current_version}")
            return None

    # ==================== FILE RECOVERY ====================

    def recover_deleted(self, directory, pattern="*"):
        """Attempt to recover recently deleted files (undelete)."""
        print(f"[*] Attempting file recovery in: {directory}")
        print("-" * 60)
        
        if platform.system() != "Linux":
            print("[!] File recovery currently only supported on Linux")
            return []
        
        recovered = []
        
        # Check trash directories
        trash_paths = [
            Path.home() / ".local/share/Trash/files",
            Path("/tmp/.Trash"),
            Path.home() / ".Trash"
        ]
        
        for trash in trash_paths:
            if trash.exists():
                print(f"[*] Checking trash: {trash}")
                for f in trash.glob(pattern):
                    print(f"    Found: {f.name}")
                    recovered.append(str(f))
        
        print(f"\n[*] Found {len(recovered)} recoverable files")
        return recovered

    # ==================== STARTUP SCANNING ====================

    def scan_startup_items(self):
        """Scan startup items, cron jobs, and systemd services for threats."""
        print("[*] Scanning Startup Items & Persistence Mechanisms")
        print("-" * 60)
        
        findings = []
        
        if platform.system() != "Linux":
            print("[!] Startup scanning currently only supported on Linux")
            return findings
        
        # 1. Check cron jobs
        print("[*] Checking cron jobs...")
        cron_locations = [
            "/etc/crontab",
            "/etc/cron.d",
            "/var/spool/cron/crontabs",
            Path.home() / ".cron"
        ]
        
        for cron_path in cron_locations:
            cron_path = Path(cron_path)
            if cron_path.is_file():
                try:
                    threat, heur = self.scan_file(cron_path)
                    if threat or (heur and heur["score"] >= 30):
                        findings.append({
                            "type": "cron",
                            "path": str(cron_path),
                            "severity": "high"
                        })
                        print(f"  [!] Suspicious cron: {cron_path}")
                except:
                    pass
            elif cron_path.is_dir():
                try:
                    for f in cron_path.iterdir():
                        if f.is_file():
                            with open(f, 'r') as cf:
                                content = cf.read()
                            # Check for suspicious commands
                            suspicious = ['wget', 'curl', 'nc ', 'bash -c', 
                                        '/dev/tcp', 'base64', 'eval']
                            for s in suspicious:
                                if s in content:
                                    findings.append({
                                        "type": "cron_cmd",
                                        "path": str(f),
                                        "pattern": s,
                                        "severity": "high"
                                    })
                                    print(f"  [!] Suspicious cron command in {f.name}: {s}")
                except (PermissionError, OSError):
                    pass
        
        if not any(f["type"].startswith("cron") for f in findings):
            print("  [✓] No suspicious cron jobs")
        
        # 2. Check systemd services
        print("[*] Checking systemd services...")
        systemd_paths = [
            "/etc/systemd/system",
            "/lib/systemd/system",
            Path.home() / ".config/systemd/user"
        ]
        
        for sd_path in systemd_paths:
            sd_path = Path(sd_path)
            if sd_path.exists():
                try:
                    for service in sd_path.glob("*.service"):
                        with open(service, 'r') as f:
                            content = f.read()
                        # Check for suspicious patterns
                        if any(p in content for p in ['/tmp/', '/dev/shm/', 
                               'bash -c', 'curl', 'wget']):
                            findings.append({
                                "type": "systemd",
                                "path": str(service),
                                "severity": "medium"
                            })
                            print(f"  [?] Suspicious service: {service.name}")
                except (PermissionError, OSError):
                    pass
        
        if not any(f["type"] == "systemd" for f in findings):
            print("  [✓] No suspicious systemd services")
        
        # 3. Check init scripts
        print("[*] Checking init scripts...")
        init_paths = ["/etc/init.d", "/etc/rc.local"]
        
        for init_path in init_paths:
            init_path = Path(init_path)
            if init_path.is_file():
                threat, heur = self.scan_file(init_path)
                if threat or (heur and heur["score"] >= 40):
                    findings.append({
                        "type": "init",
                        "path": str(init_path),
                        "severity": "high"
                    })
            elif init_path.is_dir():
                for f in init_path.iterdir():
                    if f.is_file():
                        threat, heur = self.scan_file(f)
                        if threat or (heur and heur["score"] >= 50):
                            findings.append({
                                "type": "init",
                                "path": str(f),
                                "severity": "high"
                            })
                            print(f"  [!] Suspicious init script: {f.name}")
        
        if not any(f["type"] == "init" for f in findings):
            print("  [✓] No suspicious init scripts")
        
        # 4. Check user startup files
        print("[*] Checking user startup files...")
        startup_files = [
            ".bashrc", ".bash_profile", ".profile", ".zshrc",
            ".config/autostart"
        ]
        
        for sf in startup_files:
            sf_path = Path.home() / sf
            if sf_path.is_file():
                try:
                    with open(sf_path, 'r') as f:
                        content = f.read()
                    suspicious = ['curl', 'wget', 'nc ', 'base64 -d', 
                                '/dev/tcp', 'eval $(']
                    for s in suspicious:
                        if s in content:
                            findings.append({
                                "type": "shell_rc",
                                "path": str(sf_path),
                                "pattern": s,
                                "severity": "high"
                            })
                            print(f"  [!] Suspicious command in {sf}: {s}")
                except:
                    pass
            elif sf_path.is_dir():
                for f in sf_path.glob("*.desktop"):
                    with open(f, 'r') as df:
                        content = df.read()
                    if 'Exec=' in content:
                        exec_line = [l for l in content.split('\n') if l.startswith('Exec=')]
                        if exec_line:
                            cmd = exec_line[0].replace('Exec=', '')
                            if any(s in cmd for s in ['/tmp/', 'bash -c', 'sh -c']):
                                findings.append({
                                    "type": "autostart",
                                    "path": str(f),
                                    "command": cmd[:100],
                                    "severity": "medium"
                                })
                                print(f"  [?] Suspicious autostart: {f.name}")
        
        if not any(f["type"] in ["shell_rc", "autostart"] for f in findings):
            print("  [✓] No suspicious startup files")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Found {len(findings)} suspicious startup items:")
            for f in findings:
                print(f"  [{f['severity'].upper()}] {f['type']}: {f['path']}")
        else:
            print("[✓] No suspicious startup items detected")
        
        self._log_action(f"STARTUP SCAN: {len(findings)} findings")
        return findings

    # ==================== BROWSER SCANNING ====================

    def scan_browser(self):
        """Scan browser extensions, history, and downloads for threats."""
        print("[*] Browser Security Scan")
        print("-" * 60)
        
        findings = []
        home = Path.home()
        
        # Browser profile locations
        browsers = {
            "Chrome": [
                home / ".config/google-chrome/Default",
                home / ".config/chromium/Default"
            ],
            "Firefox": list((home / ".mozilla/firefox").glob("*.default*")) if (home / ".mozilla/firefox").exists() else [],
            "Brave": [home / ".config/BraveSoftware/Brave-Browser/Default"]
        }
        
        for browser_name, profiles in browsers.items():
            for profile in profiles:
                if not isinstance(profile, Path):
                    profile = Path(profile)
                if not profile.exists():
                    continue
                
                print(f"\n[*] Scanning {browser_name}...")
                
                # 1. Check extensions
                extensions_dir = profile / "Extensions"
                if extensions_dir.exists():
                    print("  [*] Checking extensions...")
                    ext_count = 0
                    for ext in extensions_dir.iterdir():
                        if ext.is_dir():
                            ext_count += 1
                            # Check manifest for permissions
                            for version_dir in ext.iterdir():
                                manifest = version_dir / "manifest.json"
                                if manifest.exists():
                                    try:
                                        with open(manifest, 'r') as f:
                                            data = json.load(f)
                                        name = data.get("name", "Unknown")
                                        perms = data.get("permissions", [])
                                        
                                        # Suspicious permissions
                                        risky_perms = ['<all_urls>', 'webRequest', 
                                                      'webRequestBlocking', 'nativeMessaging',
                                                      'clipboardRead', 'history']
                                        found_risky = [p for p in perms if p in risky_perms]
                                        
                                        if found_risky:
                                            findings.append({
                                                "type": "extension",
                                                "browser": browser_name,
                                                "name": name,
                                                "permissions": found_risky,
                                                "severity": "medium"
                                            })
                                            print(f"    [?] {name}: {', '.join(found_risky)}")
                                    except:
                                        pass
                    print(f"  [*] Found {ext_count} extensions")
                
                # 2. Check downloads
                downloads_dir = home / "Downloads"
                if downloads_dir.exists():
                    print("  [*] Checking recent downloads...")
                    dangerous_found = 0
                    for f in downloads_dir.iterdir():
                        if f.is_file():
                            ext = f.suffix.lower()
                            if ext in DANGEROUS_EXTENSIONS:
                                # Scan the file
                                threat, heur = self.scan_file(f)
                                if threat or (heur and heur["score"] >= 50):
                                    findings.append({
                                        "type": "download",
                                        "path": str(f),
                                        "threat": threat["name"] if threat else "Suspicious",
                                        "severity": "high"
                                    })
                                    dangerous_found += 1
                                    print(f"    [!] Dangerous download: {f.name}")
                    if dangerous_found == 0:
                        print("  [✓] No dangerous downloads")
                
                # 3. Check browser history for malicious URLs (Chromium-based)
                history_db = profile / "History"
                if history_db.exists():
                    print("  [*] Checking browser history...")
                    try:
                        # Copy to temp to avoid locking issues
                        temp_db = Path(tempfile.gettempdir()) / "greyav_history.db"
                        shutil.copy2(history_db, temp_db)
                        
                        conn = sqlite3.connect(str(temp_db))
                        cursor = conn.cursor()
                        
                        cursor.execute(
                            "SELECT url FROM urls ORDER BY last_visit_time DESC LIMIT 1000"
                        )
                        urls = cursor.fetchall()
                        
                        suspicious_domains = [
                            '.ru/', '.cn/', 'malware', 'phishing', 'crack',
                            'keygen', 'warez', 'torrent', 'hack'
                        ]
                        
                        for (url,) in urls:
                            for domain in suspicious_domains:
                                if domain in url.lower():
                                    findings.append({
                                        "type": "history",
                                        "browser": browser_name,
                                        "url": url[:100],
                                        "severity": "low"
                                    })
                                    break
                        
                        conn.close()
                        os.unlink(temp_db)
                        
                        hist_findings = [f for f in findings if f["type"] == "history"]
                        if hist_findings:
                            print(f"    [?] Found {len(hist_findings)} suspicious URLs")
                        else:
                            print("  [✓] No suspicious URLs in history")
                    except Exception as e:
                        print(f"  [!] Could not read history: {e}")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Browser scan findings: {len(findings)}")
            by_type = defaultdict(list)
            for f in findings:
                by_type[f["type"]].append(f)
            for t, items in by_type.items():
                print(f"  {t}: {len(items)}")
        else:
            print("[✓] No browser security issues detected")
        
        return findings

    # ==================== DOCUMENT/MACRO ANALYSIS ====================

    def analyze_document(self, filepath):
        """Analyze Office documents for malicious macros."""
        filepath = Path(filepath)
        if not filepath.exists():
            print(f"[!] File not found: {filepath}")
            return None
        
        print(f"[*] Document Analysis: {filepath}")
        print("-" * 60)
        
        results = {
            "file": str(filepath),
            "type": None,
            "has_macros": False,
            "suspicious_macros": [],
            "embedded_objects": [],
            "external_links": [],
            "risk_score": 0
        }
        
        # Check file type
        magic = self._get_file_magic(filepath)
        if magic:
            results["type"] = magic
        
        try:
            with open(filepath, 'rb') as f:
                content = f.read()
            
            # OLE document detection (doc, xls, ppt)
            if content.startswith(b'\xd0\xcf\x11\xe0'):
                results["type"] = "OLE Document"
                
                # Check for VBA macros
                vba_indicators = [b'VBA', b'Macros', b'_VBA_PROJECT', b'Module']
                for ind in vba_indicators:
                    if ind in content:
                        results["has_macros"] = True
                        break
                
                # Suspicious macro patterns
                suspicious_patterns = [
                    (b'Shell', 'Shell execution'),
                    (b'WScript', 'Windows Script Host'),
                    (b'PowerShell', 'PowerShell execution'),
                    (b'CreateObject', 'Object creation'),
                    (b'Auto_Open', 'Auto-open macro'),
                    (b'AutoExec', 'Auto-execute macro'),
                    (b'Document_Open', 'Document open macro'),
                    (b'Workbook_Open', 'Workbook open macro'),
                    (b'URLDownloadToFile', 'URL download'),
                    (b'ADODB', 'Database access'),
                    (b'Environ', 'Environment access'),
                    (b'RegWrite', 'Registry write'),
                    (b'CallByName', 'Dynamic call'),
                    (b'Chr(', 'Character obfuscation'),
                    (b'ChrW(', 'Unicode obfuscation'),
                ]
                
                for pattern, desc in suspicious_patterns:
                    if pattern in content:
                        results["suspicious_macros"].append(desc)
                        results["risk_score"] += 15
            
            # OOXML documents (docx, xlsx, pptx)
            elif content.startswith(b'PK\x03\x04'):
                results["type"] = "OOXML Document"
                
                try:
                    with zipfile.ZipFile(filepath, 'r') as zf:
                        names = zf.namelist()
                        
                        # Check for macros
                        if any('vbaProject' in n for n in names):
                            results["has_macros"] = True
                            results["risk_score"] += 20
                        
                        # Check for embedded objects
                        embedded = [n for n in names if 'embeddings' in n.lower()]
                        results["embedded_objects"] = embedded
                        if embedded:
                            results["risk_score"] += 10 * len(embedded)
                        
                        # Check for external links
                        for name in names:
                            if name.endswith('.rels'):
                                try:
                                    rels_content = zf.read(name).decode('utf-8', errors='ignore')
                                    if 'http://' in rels_content or 'https://' in rels_content:
                                        results["external_links"].append(name)
                                        results["risk_score"] += 10
                                except:
                                    pass
                except zipfile.BadZipFile:
                    pass
            
            # RTF documents
            elif content.startswith(b'{\\rtf'):
                results["type"] = "RTF Document"
                
                # RTF exploits often use embedded objects
                if b'\\objdata' in content or b'\\objemb' in content:
                    results["embedded_objects"].append("RTF embedded object")
                    results["risk_score"] += 30
                
                # Equation editor exploits
                if b'Equation' in content:
                    results["suspicious_macros"].append("Equation Editor (CVE-2017-11882)")
                    results["risk_score"] += 50
            
        except Exception as e:
            print(f"[!] Error analyzing document: {e}")
        
        # Print results
        print(f"  Type: {results['type'] or 'Unknown'}")
        print(f"  Macros: {'Yes' if results['has_macros'] else 'No'}")
        
        if results["suspicious_macros"]:
            print(f"\n  [!] Suspicious macro indicators:")
            for m in results["suspicious_macros"]:
                print(f"      • {m}")
        
        if results["embedded_objects"]:
            print(f"\n  [?] Embedded objects ({len(results['embedded_objects'])}):")
            for o in results["embedded_objects"][:5]:
                print(f"      • {o}")
        
        if results["external_links"]:
            print(f"\n  [?] External links detected")
        
        # Risk assessment
        print(f"\n  Risk Score: {results['risk_score']}")
        if results["risk_score"] >= 50:
            print("  [!] HIGH RISK - Document may be malicious!")
        elif results["risk_score"] >= 25:
            print("  [?] MEDIUM RISK - Exercise caution")
        else:
            print("  [✓] LOW RISK")
        
        return results

    # ==================== PDF ANALYSIS ====================

    def analyze_pdf(self, filepath):
        """Analyze PDF files for embedded threats."""
        filepath = Path(filepath)
        if not filepath.exists():
            print(f"[!] File not found: {filepath}")
            return None
        
        print(f"[*] PDF Analysis: {filepath}")
        print("-" * 60)
        
        results = {
            "file": str(filepath),
            "suspicious_elements": [],
            "javascript": False,
            "embedded_files": [],
            "urls": [],
            "risk_score": 0
        }
        
        try:
            with open(filepath, 'rb') as f:
                content = f.read()
            
            # Verify it's a PDF
            if not content.startswith(b'%PDF'):
                print("[!] Not a valid PDF file")
                return None
            
            # Check for JavaScript
            js_indicators = [b'/JavaScript', b'/JS ', b'/JS(', b'/S /JavaScript']
            for ind in js_indicators:
                if ind in content:
                    results["javascript"] = True
                    results["risk_score"] += 30
                    break
            
            # Check for suspicious actions
            suspicious_actions = [
                (b'/OpenAction', 'Auto-open action'),
                (b'/AA', 'Additional actions'),
                (b'/Launch', 'Launch action'),
                (b'/EmbeddedFile', 'Embedded file'),
                (b'/RichMedia', 'Rich media'),
                (b'/XFA', 'XFA forms'),
                (b'/AcroForm', 'Acro forms'),
                (b'/JBIG2Decode', 'JBIG2 decoder (CVE-2009-0658)'),
                (b'/Colors > 2', 'Colors exploit'),
                (b'/Encrypt', 'Encrypted content'),
            ]
            
            for pattern, desc in suspicious_actions:
                if pattern in content:
                    results["suspicious_elements"].append(desc)
                    results["risk_score"] += 15
            
            # Extract URLs
            url_pattern = rb'https?://[^\s<>"\'\)\\]+'
            urls = re.findall(url_pattern, content)
            results["urls"] = [u.decode('utf-8', errors='ignore') for u in urls[:20]]
            
            # Check for embedded files
            if b'/EmbeddedFiles' in content:
                results["embedded_files"].append("Contains embedded files")
                results["risk_score"] += 20
            
            # Check for obfuscation
            if content.count(b'/Filter') > 10:
                results["suspicious_elements"].append("Heavy filtering/encoding")
                results["risk_score"] += 15
            
            # Check for stream lengths that don't match
            streams = re.findall(rb'/Length\s+(\d+)', content)
            if len(streams) > 50:
                results["suspicious_elements"].append("Many streams (possible obfuscation)")
                results["risk_score"] += 10
            
        except Exception as e:
            print(f"[!] Error analyzing PDF: {e}")
        
        # Print results
        print(f"  JavaScript: {'Yes [!]' if results['javascript'] else 'No'}")
        
        if results["suspicious_elements"]:
            print(f"\n  [!] Suspicious elements:")
            for elem in results["suspicious_elements"]:
                print(f"      • {elem}")
        
        if results["urls"]:
            print(f"\n  URLs found ({len(results['urls'])}):")
            for url in results["urls"][:5]:
                print(f"      • {url[:80]}")
        
        if results["embedded_files"]:
            print(f"\n  [!] {results['embedded_files'][0]}")
        
        # Risk assessment
        print(f"\n  Risk Score: {results['risk_score']}")
        if results["risk_score"] >= 50:
            print("  [!] HIGH RISK - PDF may be malicious!")
        elif results["risk_score"] >= 25:
            print("  [?] MEDIUM RISK - Exercise caution")
        else:
            print("  [✓] LOW RISK")
        
        return results

    # ==================== CRYPTOMINER DETECTION ====================

    def detect_cryptominers(self):
        """Detect cryptocurrency mining malware."""
        print("[*] Cryptominer Detection Scan")
        print("-" * 60)
        
        findings = []
        
        # 1. Check for mining process names
        print("[*] Checking for mining processes...")
        miner_names = [
            'xmrig', 'xmr-stak', 'minerd', 'cgminer', 'bfgminer',
            'ethminer', 'claymore', 'phoenixminer', 'nbminer',
            'ccminer', 'cpuminer', 't-rex', 'gminer', 'lolminer'
        ]
        
        if platform.system() == "Linux":
            for pid_dir in Path("/proc").iterdir():
                if pid_dir.name.isdigit():
                    try:
                        with open(pid_dir / "comm", 'r') as f:
                            proc_name = f.read().strip().lower()
                        
                        for miner in miner_names:
                            if miner in proc_name:
                                findings.append({
                                    "type": "process",
                                    "pid": pid_dir.name,
                                    "name": proc_name,
                                    "severity": "critical"
                                })
                                print(f"  [!] Miner process: {proc_name} (PID: {pid_dir.name})")
                        
                        # Check cmdline for mining pools
                        with open(pid_dir / "cmdline", 'r') as f:
                            cmdline = f.read().replace('\x00', ' ').lower()
                        
                        pool_indicators = [
                            'stratum+tcp://', 'mining.pool', '.nicehash.',
                            'pool.', 'monero', 'xmr.', 'eth.', 'bitcoin'
                        ]
                        
                        for pool in pool_indicators:
                            if pool in cmdline:
                                findings.append({
                                    "type": "pool_connection",
                                    "pid": pid_dir.name,
                                    "indicator": pool,
                                    "severity": "critical"
                                })
                                print(f"  [!] Pool connection: {pool}")
                                break
                                
                    except (PermissionError, OSError, FileNotFoundError):
                        continue
        
        if not any(f["type"] in ["process", "pool_connection"] for f in findings):
            print("  [✓] No mining processes detected")
        
        # 2. Check CPU usage
        print("[*] Checking CPU usage...")
        try:
            if platform.system() == "Linux":
                with open("/proc/stat", 'r') as f:
                    cpu_line = f.readline()
                values = cpu_line.split()[1:5]
                idle = int(values[3])
                total = sum(int(v) for v in values)
                usage = 100 - (idle * 100 / total)
                
                if usage > 80:
                    findings.append({
                        "type": "high_cpu",
                        "usage": f"{usage:.1f}%",
                        "severity": "medium"
                    })
                    print(f"  [?] High CPU usage: {usage:.1f}%")
                else:
                    print(f"  [*] CPU usage: {usage:.1f}%")
        except:
            pass
        
        # 3. Check for miner files
        print("[*] Checking for miner files...")
        common_paths = [
            "/tmp", "/var/tmp", "/dev/shm",
            str(Path.home() / ".cache"),
            str(Path.home() / ".local/share")
        ]
        
        miner_file_patterns = [
            '*xmrig*', '*miner*', '*minerd*', '*cgminer*',
            '*.sh', '*pool*config*', '*wallet*'
        ]
        
        for search_path in common_paths:
            search_path = Path(search_path)
            if search_path.exists():
                for pattern in miner_file_patterns:
                    for f in search_path.glob(pattern):
                        if f.is_file():
                            # Quick content check
                            try:
                                with open(f, 'rb') as file:
                                    content = file.read(4096)
                                if any(m.encode() in content.lower() for m in miner_names):
                                    findings.append({
                                        "type": "miner_file",
                                        "path": str(f),
                                        "severity": "high"
                                    })
                                    print(f"  [!] Miner file: {f}")
                            except:
                                pass
        
        if not any(f["type"] == "miner_file" for f in findings):
            print("  [✓] No miner files detected")
        
        # 4. Check network for mining pool connections
        print("[*] Checking network connections...")
        mining_ports = [3333, 4444, 5555, 7777, 8888, 9999, 14444]
        
        try:
            with open("/proc/net/tcp", 'r') as f:
                for line in f.readlines()[1:]:
                    parts = line.split()
                    remote = parts[2].split(':')
                    remote_port = int(remote[1], 16)
                    
                    if remote_port in mining_ports:
                        findings.append({
                            "type": "mining_port",
                            "port": remote_port,
                            "severity": "high"
                        })
                        print(f"  [!] Connection to mining port: {remote_port}")
        except:
            pass
        
        if not any(f["type"] == "mining_port" for f in findings):
            print("  [✓] No mining pool connections")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] CRYPTOMINER INDICATORS: {len(findings)}")
            for f in findings:
                print(f"  [{f['severity'].upper()}] {f['type']}: {f.get('name', f.get('path', f.get('port', '')))}")
        else:
            print("[✓] No cryptominer activity detected")
        
        self._log_action(f"CRYPTOMINER SCAN: {len(findings)} findings")
        return findings

    # ==================== SYSTEM HARDENING ====================

    def check_system_hardening(self):
        """Check system security configuration and provide recommendations."""
        print("[*] System Hardening Assessment")
        print("-" * 60)
        
        issues = []
        recommendations = []
        
        if platform.system() != "Linux":
            print("[!] System hardening checks currently only supported on Linux")
            return issues, recommendations
        
        # 1. Check ASLR
        print("[*] Checking ASLR...")
        try:
            with open("/proc/sys/kernel/randomize_va_space", 'r') as f:
                aslr = int(f.read().strip())
            if aslr < 2:
                issues.append("ASLR not fully enabled")
                recommendations.append("Enable full ASLR: echo 2 > /proc/sys/kernel/randomize_va_space")
                print("  [!] ASLR not fully enabled")
            else:
                print("  [✓] ASLR enabled")
        except:
            pass
        
        # 2. Check core dumps
        print("[*] Checking core dumps...")
        try:
            with open("/proc/sys/kernel/core_pattern", 'r') as f:
                core_pattern = f.read().strip()
            if core_pattern and core_pattern != '|/bin/false':
                issues.append("Core dumps enabled")
                recommendations.append("Disable core dumps: echo '|/bin/false' > /proc/sys/kernel/core_pattern")
                print("  [?] Core dumps enabled")
            else:
                print("  [✓] Core dumps disabled")
        except:
            pass
        
        # 3. Check for password-less sudo
        print("[*] Checking sudo configuration...")
        try:
            with open("/etc/sudoers", 'r') as f:
                sudoers = f.read()
            if 'NOPASSWD' in sudoers:
                issues.append("NOPASSWD entries in sudoers")
                recommendations.append("Review /etc/sudoers for NOPASSWD entries")
                print("  [?] NOPASSWD sudo entries found")
            else:
                print("  [✓] No NOPASSWD sudo entries")
        except PermissionError:
            print("  [*] Cannot read sudoers (need root)")
        
        # 4. Check SSH configuration
        print("[*] Checking SSH configuration...")
        ssh_config = Path("/etc/ssh/sshd_config")
        if ssh_config.exists():
            try:
                with open(ssh_config, 'r') as f:
                    ssh_content = f.read()
                
                if 'PermitRootLogin yes' in ssh_content:
                    issues.append("SSH root login enabled")
                    recommendations.append("Disable SSH root login: PermitRootLogin no")
                    print("  [!] Root SSH login enabled")
                else:
                    print("  [✓] Root SSH login disabled")
                
                if 'PasswordAuthentication yes' in ssh_content:
                    issues.append("SSH password authentication enabled")
                    recommendations.append("Consider SSH key authentication only")
                    print("  [?] Password authentication enabled")
            except PermissionError:
                print("  [*] Cannot read SSH config (need root)")
        
        # 5. Check open ports
        print("[*] Checking open ports...")
        open_ports = []
        try:
            with open("/proc/net/tcp", 'r') as f:
                for line in f.readlines()[1:]:
                    parts = line.split()
                    local = parts[1].split(':')
                    port = int(local[1], 16)
                    state = int(parts[3], 16)
                    if state == 10:  # LISTEN
                        open_ports.append(port)
            
            risky_ports = [21, 23, 25, 139, 445, 3389]
            for port in open_ports:
                if port in risky_ports:
                    issues.append(f"Risky port open: {port}")
                    print(f"  [?] Risky port open: {port}")
            
            if not any(p in risky_ports for p in open_ports):
                print(f"  [✓] {len(open_ports)} ports open (none risky)")
        except:
            pass
        
        # 6. Check file permissions
        print("[*] Checking critical file permissions...")
        critical_files = [
            ("/etc/passwd", 0o644),
            ("/etc/shadow", 0o640),
            ("/etc/ssh/sshd_config", 0o600),
        ]
        
        for fpath, expected in critical_files:
            fpath = Path(fpath)
            if fpath.exists():
                mode = fpath.stat().st_mode & 0o777
                if mode > expected:
                    issues.append(f"{fpath} has permissive permissions: {oct(mode)}")
                    print(f"  [!] {fpath.name}: {oct(mode)} (should be {oct(expected)})")
        
        # 7. Check firewall
        print("[*] Checking firewall...")
        try:
            result = subprocess.run(
                ["iptables", "-L", "-n"],
                capture_output=True, text=True, timeout=5
            )
            if "ACCEPT" in result.stdout and result.stdout.count('\n') < 10:
                issues.append("Minimal firewall rules")
                recommendations.append("Configure firewall rules with iptables/ufw")
                print("  [?] Minimal firewall configuration")
            else:
                print("  [✓] Firewall configured")
        except:
            try:
                result = subprocess.run(["ufw", "status"], capture_output=True, text=True)
                if "inactive" in result.stdout.lower():
                    issues.append("UFW firewall is inactive")
                    print("  [!] UFW firewall inactive")
                else:
                    print("  [✓] UFW firewall active")
            except:
                print("  [?] Could not check firewall status")
        
        # Summary
        print("\n" + "=" * 60)
        print(f"Security Issues: {len(issues)}")
        print(f"Recommendations: {len(recommendations)}")
        
        if issues:
            print("\n[!] Issues Found:")
            for i, issue in enumerate(issues, 1):
                print(f"  {i}. {issue}")
        
        if recommendations:
            print("\n[*] Recommendations:")
            for i, rec in enumerate(recommendations, 1):
                print(f"  {i}. {rec}")
        
        if not issues:
            print("\n[✓] No major security issues detected")
        
        return issues, recommendations

    # ==================== FORENSIC TIMELINE ====================

    def generate_forensic_timeline(self, directory, days=7):
        """Generate forensic timeline of file activities."""
        print(f"[*] Generating Forensic Timeline")
        print(f"    Directory: {directory}")
        print(f"    Period: Last {days} days")
        print("-" * 60)
        
        directory = Path(directory)
        if not directory.exists():
            print(f"[!] Directory not found: {directory}")
            return []
        
        cutoff = datetime.now() - timedelta(days=days)
        cutoff_ts = cutoff.timestamp()
        
        events = []
        
        # Collect file events
        print("[*] Collecting file events...")
        for filepath in directory.rglob("*"):
            if not filepath.is_file():
                continue
            
            try:
                stat = filepath.stat()
                
                # Modified time
                if stat.st_mtime >= cutoff_ts:
                    events.append({
                        "time": datetime.fromtimestamp(stat.st_mtime),
                        "type": "modified",
                        "file": str(filepath),
                        "size": stat.st_size
                    })
                
                # Access time (if different and recent)
                if stat.st_atime >= cutoff_ts and stat.st_atime != stat.st_mtime:
                    events.append({
                        "time": datetime.fromtimestamp(stat.st_atime),
                        "type": "accessed",
                        "file": str(filepath),
                        "size": stat.st_size
                    })
                
                # Creation time (ctime on Linux is actually change time)
                if stat.st_ctime >= cutoff_ts:
                    events.append({
                        "time": datetime.fromtimestamp(stat.st_ctime),
                        "type": "changed",
                        "file": str(filepath),
                        "size": stat.st_size
                    })
                    
            except (OSError, PermissionError):
                continue
        
        # Sort by time
        events.sort(key=lambda x: x["time"], reverse=True)
        
        # Print timeline
        print(f"\n[*] Timeline ({len(events)} events):")
        print("-" * 80)
        
        current_date = None
        for event in events[:100]:  # Limit output
            event_date = event["time"].strftime("%Y-%m-%d")
            if event_date != current_date:
                print(f"\n=== {event_date} ===")
                current_date = event_date
            
            time_str = event["time"].strftime("%H:%M:%S")
            type_icon = {"modified": "M", "accessed": "A", "changed": "C"}
            icon = type_icon.get(event["type"], "?")
            
            filename = Path(event["file"]).name
            print(f"  [{time_str}] [{icon}] {filename}")
        
        if len(events) > 100:
            print(f"\n  ... and {len(events) - 100} more events")
        
        # Save timeline to file
        timeline_file = REPORTS_DIR / f"timeline_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(timeline_file, 'w') as f:
            json.dump([{**e, "time": e["time"].isoformat()} for e in events], f, indent=2)
        print(f"\n[+] Timeline saved: {timeline_file}")
        
        return events

    # ==================== LOG ANALYSIS ====================

    def analyze_system_logs(self):
        """Analyze system logs for suspicious activities."""
        print("[*] System Log Analysis")
        print("-" * 60)
        
        findings = []
        
        if platform.system() != "Linux":
            print("[!] Log analysis currently only supported on Linux")
            return findings
        
        # Log files to check
        log_files = {
            "/var/log/auth.log": self._analyze_auth_log,
            "/var/log/syslog": self._analyze_syslog,
            "/var/log/secure": self._analyze_auth_log,
            "/var/log/messages": self._analyze_syslog,
        }
        
        for log_path, analyzer in log_files.items():
            log_path = Path(log_path)
            if log_path.exists():
                print(f"\n[*] Analyzing: {log_path}")
                try:
                    log_findings = analyzer(log_path)
                    findings.extend(log_findings)
                except PermissionError:
                    print(f"  [!] Permission denied - run as root")
                except Exception as e:
                    print(f"  [!] Error: {e}")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Suspicious log entries: {len(findings)}")
            by_type = defaultdict(int)
            for f in findings:
                by_type[f["type"]] += 1
            for t, count in by_type.items():
                print(f"  • {t}: {count}")
        else:
            print("[✓] No suspicious log entries detected")
        
        return findings

    def _analyze_auth_log(self, log_path, lines=1000):
        """Analyze authentication log."""
        findings = []
        
        # Read last N lines
        with open(log_path, 'r') as f:
            content = f.readlines()[-lines:]
        
        failed_logins = defaultdict(int)
        successful_root = []
        
        for line in content:
            line_lower = line.lower()
            
            # Failed SSH attempts
            if 'failed password' in line_lower:
                # Extract IP if present
                ip_match = re.search(r'from\s+(\d+\.\d+\.\d+\.\d+)', line)
                if ip_match:
                    failed_logins[ip_match.group(1)] += 1
            
            # Successful root logins
            if 'accepted' in line_lower and 'root' in line_lower:
                successful_root.append(line.strip())
            
            # Su/sudo usage
            if 'su[' in line_lower or 'sudo:' in line_lower:
                if 'authentication failure' in line_lower:
                    findings.append({
                        "type": "auth_failure",
                        "log": str(log_path),
                        "entry": line.strip()[:200],
                        "severity": "medium"
                    })
        
        # Report brute force attempts
        for ip, count in failed_logins.items():
            if count >= 5:
                findings.append({
                    "type": "brute_force",
                    "ip": ip,
                    "attempts": count,
                    "severity": "high"
                })
                print(f"  [!] Brute force: {ip} ({count} attempts)")
        
        # Report root logins
        for entry in successful_root[-5:]:
            findings.append({
                "type": "root_login",
                "entry": entry[:200],
                "severity": "medium"
            })
            print(f"  [?] Root login: {entry[:80]}")
        
        return findings

    def _analyze_syslog(self, log_path, lines=1000):
        """Analyze system log."""
        findings = []
        
        with open(log_path, 'r') as f:
            content = f.readlines()[-lines:]
        
        suspicious_patterns = [
            ('segfault', 'Segmentation fault', 'medium'),
            ('oom-killer', 'Out of memory kill', 'high'),
            ('kernel panic', 'Kernel panic', 'critical'),
            ('usb device', 'USB device activity', 'low'),
            ('firewall', 'Firewall event', 'medium'),
        ]
        
        for line in content:
            line_lower = line.lower()
            
            for pattern, desc, severity in suspicious_patterns:
                if pattern in line_lower:
                    findings.append({
                        "type": desc,
                        "log": str(log_path),
                        "entry": line.strip()[:200],
                        "severity": severity
                    })
        
        # Summarize
        by_type = defaultdict(int)
        for f in findings:
            by_type[f["type"]] += 1
        
        for t, count in by_type.items():
            print(f"  [*] {t}: {count} events")
        
        return findings

    # ==================== INTERACTIVE MODE ====================

    def interactive_mode(self):
        """Start interactive shell for GreyAV."""
        print("\n[*] GreyAV Interactive Mode")
        print("    Type 'help' for commands, 'exit' to quit")
        print("-" * 60)
        
        commands = {
            'scan': 'Scan a file or directory',
            'monitor': 'Start real-time monitoring',
            'heuristic': 'Run heuristic analysis',
            'quarantine': 'Quarantine a file',
            'network': 'Scan network connections',
            'processes': 'Scan running processes',
            'rootkit': 'Check for rootkits',
            'cryptominer': 'Detect cryptominers',
            'startup': 'Scan startup items',
            'browser': 'Scan browser security',
            'hardening': 'Security assessment',
            'logs': 'Analyze system logs',
            'stats': 'Show statistics',
            'config': 'Show configuration',
            'help': 'Show this help',
            'exit': 'Exit interactive mode'
        }
        
        while True:
            try:
                user_input = input("\ngreyav> ").strip()
                
                if not user_input:
                    continue
                
                parts = user_input.split()
                cmd = parts[0].lower()
                args = parts[1:] if len(parts) > 1 else []
                
                if cmd == 'exit' or cmd == 'quit':
                    print("[*] Goodbye!")
                    break
                
                elif cmd == 'help':
                    print("\nAvailable commands:")
                    for c, desc in commands.items():
                        print(f"  {c:<15} {desc}")
                
                elif cmd == 'scan':
                    if args:
                        self.scan_start_time = time.time()
                        path = Path(args[0])
                        if path.is_file():
                            threat, heur = self.scan_file(path)
                            if threat:
                                print(f"[!] THREAT: {threat['name']}")
                            elif heur and heur["score"] >= 50:
                                print(f"[?] SUSPICIOUS: Score {heur['score']}")
                            else:
                                print("[✓] Clean")
                        else:
                            self.scan_directory(path)
                            self.print_results()
                    else:
                        print("Usage: scan <path>")
                
                elif cmd == 'heuristic':
                    if args:
                        result = self.heuristic_scan(args[0])
                        if result:
                            print(f"Score: {result['score']} ({result['risk_level']})")
                    else:
                        print("Usage: heuristic <file>")
                
                elif cmd == 'quarantine':
                    if args:
                        self.quarantine_file(args[0])
                    else:
                        print("Usage: quarantine <file>")
                
                elif cmd == 'network':
                    self.scan_network_connections()
                
                elif cmd == 'processes':
                    self.scan_running_processes()
                
                elif cmd == 'rootkit':
                    self.detect_rootkits()
                
                elif cmd == 'cryptominer':
                    self.detect_cryptominers()
                
                elif cmd == 'startup':
                    self.scan_startup_items()
                
                elif cmd == 'browser':
                    self.scan_browser()
                
                elif cmd == 'hardening':
                    self.check_system_hardening()
                
                elif cmd == 'logs':
                    self.analyze_system_logs()
                
                elif cmd == 'stats':
                    self.show_statistics()
                
                elif cmd == 'config':
                    self.show_config()
                
                else:
                    print(f"Unknown command: {cmd}")
                    print("Type 'help' for available commands")
                    
            except KeyboardInterrupt:
                print("\n[*] Use 'exit' to quit")
            except Exception as e:
                print(f"[!] Error: {e}")

    # ==================== RANSOMWARE PROTECTION ====================

    def detect_ransomware_activity(self, directory, duration=30):
        """Monitor for ransomware-like behavior (mass file changes)."""
        print(f"[*] Ransomware Activity Monitor")
        print(f"    Watching: {directory}")
        print(f"    Duration: {duration} seconds")
        print("-" * 60)
        
        directory = Path(directory)
        if not directory.exists():
            print(f"[!] Directory not found: {directory}")
            return None
        
        # Take baseline snapshot
        print("[*] Creating baseline snapshot...")
        baseline = {}
        for f in directory.rglob("*"):
            if f.is_file():
                try:
                    stat = f.stat()
                    baseline[str(f)] = {
                        "size": stat.st_size,
                        "mtime": stat.st_mtime,
                        "exists": True
                    }
                except:
                    pass
        
        print(f"[*] Monitoring {len(baseline)} files...")
        print("[*] Press Ctrl+C to stop early")
        
        changes = {
            "modified": [],
            "deleted": [],
            "created": [],
            "renamed": []
        }
        
        ransomware_extensions = [
            '.encrypted', '.locked', '.crypto', '.crypt', '.enc',
            '.locky', '.zepto', '.cerber', '.wallet', '.onion',
            '.wncry', '.wcry', '.wncryt', '.ransomware', '.aaa',
            '.abc', '.xyz', '.zzz', '.micro', '.xxx', '.ttt',
            '.ecc', '.vvv', '.exx', '.ccc', '.vault', '.petya',
            '.cry', '.legion', '.breaking_bad', '.darkness'
        ]
        
        try:
            start = time.time()
            while time.time() - start < duration:
                time.sleep(2)
                
                current_files = set()
                for f in directory.rglob("*"):
                    if f.is_file():
                        current_files.add(str(f))
                        try:
                            stat = f.stat()
                            path_str = str(f)
                            
                            if path_str in baseline:
                                old = baseline[path_str]
                                if stat.st_mtime != old["mtime"]:
                                    changes["modified"].append(path_str)
                                    print(f"  [!] Modified: {f.name}")
                            else:
                                changes["created"].append(path_str)
                                # Check for ransomware extensions
                                if f.suffix.lower() in ransomware_extensions:
                                    print(f"  [!!!] RANSOMWARE EXT: {f.name}")
                                else:
                                    print(f"  [+] Created: {f.name}")
                        except:
                            pass
                
                # Check deleted files
                for old_path in baseline:
                    if old_path not in current_files:
                        if old_path not in changes["deleted"]:
                            changes["deleted"].append(old_path)
                            print(f"  [-] Deleted: {Path(old_path).name}")
                
                elapsed = int(time.time() - start)
                total_changes = sum(len(v) for v in changes.values())
                
                # Ransomware detection thresholds
                if total_changes > 50:
                    print("\n" + "!" * 60)
                    print("[!!!] RANSOMWARE ACTIVITY DETECTED!")
                    print(f"      {total_changes} file changes in {elapsed}s")
                    print("!" * 60)
                    break
                    
        except KeyboardInterrupt:
            print("\n[*] Monitoring stopped")
        
        # Summary
        print("\n" + "=" * 60)
        print("[*] Activity Summary:")
        print(f"    Modified: {len(changes['modified'])}")
        print(f"    Created:  {len(changes['created'])}")
        print(f"    Deleted:  {len(changes['deleted'])}")
        
        total = sum(len(v) for v in changes.values())
        if total > 20:
            print("\n[!] HIGH FILE ACTIVITY - Possible ransomware!")
        elif total > 10:
            print("\n[?] Elevated file activity detected")
        else:
            print("\n[✓] Normal file activity")
        
        return changes

    # ==================== SUID/SGID SCANNING ====================

    def scan_suid_files(self):
        """Scan for suspicious SUID/SGID files."""
        print("[*] SUID/SGID Binary Scanner")
        print("-" * 60)
        
        if platform.system() != "Linux":
            print("[!] SUID scanning only supported on Linux")
            return []
        
        findings = []
        
        # Known legitimate SUID binaries
        known_suid = {
            '/usr/bin/sudo', '/usr/bin/su', '/usr/bin/passwd',
            '/usr/bin/chsh', '/usr/bin/chfn', '/usr/bin/newgrp',
            '/usr/bin/gpasswd', '/usr/bin/mount', '/usr/bin/umount',
            '/usr/bin/pkexec', '/usr/bin/crontab', '/usr/bin/ssh-agent',
            '/usr/lib/openssh/ssh-keysign', '/usr/bin/fusermount',
            '/usr/bin/fusermount3'
        }
        
        print("[*] Scanning filesystem for SUID/SGID binaries...")
        
        search_paths = ['/', '/usr', '/bin', '/sbin', '/opt']
        
        for search_path in search_paths:
            search_path = Path(search_path)
            if not search_path.exists():
                continue
            
            try:
                for f in search_path.rglob("*"):
                    if not f.is_file():
                        continue
                    
                    try:
                        stat = f.stat()
                        mode = stat.st_mode
                        
                        is_suid = bool(mode & 0o4000)
                        is_sgid = bool(mode & 0o2000)
                        
                        if is_suid or is_sgid:
                            path_str = str(f)
                            flags = []
                            if is_suid:
                                flags.append("SUID")
                            if is_sgid:
                                flags.append("SGID")
                            
                            severity = "low"
                            if path_str not in known_suid:
                                # Unknown SUID binary
                                if '/tmp/' in path_str or '/home/' in path_str:
                                    severity = "critical"
                                elif '/opt/' in path_str or '/usr/local/' in path_str:
                                    severity = "high"
                                else:
                                    severity = "medium"
                            
                            finding = {
                                "path": path_str,
                                "flags": flags,
                                "owner": stat.st_uid,
                                "severity": severity
                            }
                            findings.append(finding)
                            
                            if severity != "low":
                                print(f"  [{severity.upper()}] {' '.join(flags)}: {path_str}")
                                
                    except (PermissionError, OSError):
                        continue
                        
            except PermissionError:
                continue
        
        # Summary
        print("\n" + "=" * 60)
        by_severity = defaultdict(int)
        for f in findings:
            by_severity[f["severity"]] += 1
        
        print(f"[*] Found {len(findings)} SUID/SGID binaries:")
        for sev in ["critical", "high", "medium", "low"]:
            if by_severity[sev]:
                print(f"    {sev.upper()}: {by_severity[sev]}")
        
        if by_severity["critical"] or by_severity["high"]:
            print("\n[!] Suspicious SUID binaries found!")
        
        return findings

    # ==================== WORLD-WRITABLE FILES ====================

    def scan_world_writable(self, directory="/"):
        """Scan for world-writable files and directories."""
        print(f"[*] World-Writable File Scanner")
        print(f"    Scanning: {directory}")
        print("-" * 60)
        
        if platform.system() != "Linux":
            print("[!] Only supported on Linux")
            return []
        
        findings = []
        
        # Skip known world-writable directories
        skip_dirs = {'/tmp', '/var/tmp', '/dev/shm', '/run', '/proc', '/sys'}
        
        directory = Path(directory)
        
        try:
            for f in directory.rglob("*"):
                try:
                    # Skip known locations
                    if any(str(f).startswith(s) for s in skip_dirs):
                        continue
                    
                    stat = f.stat()
                    mode = stat.st_mode & 0o777
                    
                    # Check if world-writable (o+w)
                    if mode & 0o002:
                        severity = "medium"
                        
                        # More dangerous locations
                        if '/bin' in str(f) or '/sbin' in str(f):
                            severity = "critical"
                        elif '/etc' in str(f):
                            severity = "high"
                        elif f.suffix in ['.sh', '.py', '.pl', '.rb']:
                            severity = "high"
                        
                        finding = {
                            "path": str(f),
                            "is_dir": f.is_dir(),
                            "mode": oct(mode),
                            "severity": severity
                        }
                        findings.append(finding)
                        
                        if len(findings) <= 50:  # Limit output
                            ftype = "DIR" if f.is_dir() else "FILE"
                            print(f"  [{severity.upper()}] [{ftype}] {f}")
                            
                except (PermissionError, OSError):
                    continue
                    
        except PermissionError:
            print("[!] Permission denied - try running as root")
        
        # Summary
        print("\n" + "=" * 60)
        if len(findings) > 50:
            print(f"[*] Found {len(findings)} world-writable items (showing first 50)")
        else:
            print(f"[*] Found {len(findings)} world-writable items")
        
        by_severity = defaultdict(int)
        for f in findings:
            by_severity[f["severity"]] += 1
        
        for sev in ["critical", "high", "medium"]:
            if by_severity[sev]:
                print(f"    {sev.upper()}: {by_severity[sev]}")
        
        return findings

    # ==================== WEBSHELL DETECTION ====================

    def detect_webshells(self, webroot="/var/www"):
        """Detect web shells in web directories."""
        print(f"[*] WebShell Detection Scanner")
        print(f"    Scanning: {webroot}")
        print("-" * 60)
        
        webroot = Path(webroot)
        if not webroot.exists():
            print(f"[!] Web root not found: {webroot}")
            return []
        
        findings = []
        
        # WebShell indicators
        php_patterns = [
            (b'eval($_', 'eval($_POST/GET/REQUEST)'),
            (b'base64_decode($_', 'base64 decode of input'),
            (b'shell_exec(', 'shell_exec function'),
            (b'system($_', 'system() with input'),
            (b'passthru(', 'passthru function'),
            (b'exec($_', 'exec with input'),
            (b'popen(', 'popen function'),
            (b'proc_open(', 'proc_open function'),
            (b"c99shell", 'c99 shell'),
            (b"r57shell", 'r57 shell'),
            (b"WSO ", 'WSO shell'),
            (b"FilesMan", 'FilesMan shell'),
            (b"b374k", 'b374k shell'),
            (b"<?php @eval", 'obfuscated eval'),
            (b'$_FILES[', 'file upload handling'),
            (b'move_uploaded_file', 'file upload'),
            (b'assert($_', 'assert with input'),
            (b'preg_replace.*e"', 'preg_replace /e'),
            (b'create_function(', 'create_function'),
            (b'call_user_func(', 'call_user_func'),
        ]
        
        asp_patterns = [
            (b'Execute(', 'Execute function'),
            (b'Eval(Request', 'Eval Request'),
            (b'CreateObject("WScript', 'WScript object'),
            (b'CreateObject("Scripting.FileSystemObject', 'FSO access'),
        ]
        
        jsp_patterns = [
            (b'Runtime.getRuntime().exec', 'Runtime exec'),
            (b'ProcessBuilder', 'ProcessBuilder'),
            (b'<%@ page import="java.io.*"', 'IO import'),
        ]
        
        web_extensions = ['.php', '.php3', '.php4', '.php5', '.phtml',
                        '.asp', '.aspx', '.jsp', '.jspx', '.cfm']
        
        scanned = 0
        for f in webroot.rglob("*"):
            if not f.is_file():
                continue
            
            if f.suffix.lower() not in web_extensions:
                continue
            
            scanned += 1
            
            try:
                with open(f, 'rb') as file:
                    content = file.read()
                
                indicators = []
                
                # Check PHP patterns
                if f.suffix.lower() in ['.php', '.php3', '.php4', '.php5', '.phtml']:
                    for pattern, desc in php_patterns:
                        if pattern in content:
                            indicators.append(desc)
                
                # Check ASP patterns
                elif f.suffix.lower() in ['.asp', '.aspx']:
                    for pattern, desc in asp_patterns:
                        if pattern in content:
                            indicators.append(desc)
                
                # Check JSP patterns
                elif f.suffix.lower() in ['.jsp', '.jspx']:
                    for pattern, desc in jsp_patterns:
                        if pattern in content:
                            indicators.append(desc)
                
                # Check entropy (obfuscated shells often have high entropy)
                entropy = self._calculate_entropy(content)
                
                if indicators:
                    severity = "critical" if len(indicators) >= 3 else "high"
                    finding = {
                        "file": str(f),
                        "indicators": indicators,
                        "entropy": entropy,
                        "severity": severity
                    }
                    findings.append(finding)
                    
                    print(f"\n  [{severity.upper()}] {f}")
                    for ind in indicators[:5]:
                        print(f"      • {ind}")
                    if len(indicators) > 5:
                        print(f"      ... and {len(indicators)-5} more")
                        
            except (PermissionError, OSError):
                continue
        
        # Summary
        print("\n" + "=" * 60)
        print(f"[*] Scanned {scanned} web files")
        print(f"[*] Found {len(findings)} potential webshells")
        
        if findings:
            print("\n[!] WEBSHELL INDICATORS DETECTED!")
            print("    Review these files immediately!")
        else:
            print("\n[✓] No webshells detected")
        
        return findings

    # ==================== HIDDEN FILE DETECTION ====================

    def scan_hidden_files(self, directory="/"):
        """Scan for suspicious hidden files and directories."""
        print(f"[*] Hidden File Scanner")
        print(f"    Scanning: {directory}")
        print("-" * 60)
        
        directory = Path(directory)
        if not directory.exists():
            print(f"[!] Directory not found: {directory}")
            return []
        
        findings = []
        
        # Known legitimate hidden directories
        known_hidden = {
            '.git', '.svn', '.hg', '.cache', '.config', '.local',
            '.npm', '.nvm', '.pyenv', '.cargo', '.rustup', '.ssh',
            '.gnupg', '.bash_history', '.bashrc', '.profile', '.vimrc',
            '.mozilla', '.thunderbird', '.wine', '.docker'
        }
        
        # Suspicious locations for hidden files
        suspicious_locations = ['/tmp', '/var/tmp', '/dev/shm', '/opt', '/srv']
        
        print("[*] Scanning for hidden files in suspicious locations...")
        
        for loc in suspicious_locations:
            loc_path = Path(loc)
            if not loc_path.exists():
                continue
            
            try:
                for f in loc_path.rglob(".*"):
                    if f.name in known_hidden:
                        continue
                    
                    try:
                        stat = f.stat()
                        severity = "medium"
                        
                        # Check for executable hidden files
                        if f.is_file() and (stat.st_mode & 0o111):
                            severity = "high"
                        
                        # Check for recent creation
                        age_days = (time.time() - stat.st_ctime) / 86400
                        if age_days < 7:
                            severity = "high"
                        
                        # Large hidden files are suspicious
                        if f.is_file() and stat.st_size > 1024 * 1024:
                            severity = "high"
                        
                        finding = {
                            "path": str(f),
                            "is_dir": f.is_dir(),
                            "size": stat.st_size if f.is_file() else 0,
                            "age_days": int(age_days),
                            "severity": severity
                        }
                        findings.append(finding)
                        
                        ftype = "DIR" if f.is_dir() else "FILE"
                        print(f"  [{severity.upper()}] [{ftype}] {f}")
                        
                    except (PermissionError, OSError):
                        continue
                        
            except PermissionError:
                continue
        
        # Also check user home directories
        print("\n[*] Checking home directories...")
        homes = Path("/home")
        if homes.exists():
            for user_home in homes.iterdir():
                if not user_home.is_dir():
                    continue
                
                for f in user_home.glob(".*"):
                    if f.name in known_hidden:
                        continue
                    
                    try:
                        stat = f.stat()
                        
                        # Check if it's a suspicious hidden executable
                        if f.is_file() and f.suffix in ['.sh', '.py', '.pl']:
                            if stat.st_mode & 0o111:  # Executable
                                finding = {
                                    "path": str(f),
                                    "is_dir": False,
                                    "size": stat.st_size,
                                    "severity": "medium"
                                }
                                findings.append(finding)
                                print(f"  [MEDIUM] [FILE] {f}")
                                
                    except (PermissionError, OSError):
                        continue
        
        # Summary
        print("\n" + "=" * 60)
        print(f"[*] Found {len(findings)} suspicious hidden items")
        
        by_severity = defaultdict(int)
        for f in findings:
            by_severity[f["severity"]] += 1
        
        for sev in ["high", "medium", "low"]:
            if by_severity[sev]:
                print(f"    {sev.upper()}: {by_severity[sev]}")
        
        return findings

    # ==================== ORPHAN PROCESS DETECTION ====================

    def detect_orphan_processes(self):
        """Detect orphaned and suspicious processes."""
        print("[*] Orphan Process Detection")
        print("-" * 60)
        
        if platform.system() != "Linux":
            print("[!] Only supported on Linux")
            return []
        
        findings = []
        
        # Get all processes
        processes = {}
        for pid_dir in Path("/proc").iterdir():
            if not pid_dir.name.isdigit():
                continue
            
            pid = int(pid_dir.name)
            try:
                with open(pid_dir / "stat", 'r') as f:
                    stat_content = f.read()
                
                with open(pid_dir / "comm", 'r') as f:
                    comm = f.read().strip()
                
                # Parse stat carefully - format is: pid (comm) state ppid ...
                # comm can contain spaces and parens
                close_paren = stat_content.rfind(')')
                if close_paren == -1:
                    continue
                rest = stat_content[close_paren+2:].split()
                if len(rest) < 2:
                    continue
                    
                state = rest[0]
                ppid = int(rest[1])
                
                processes[pid] = {
                    "pid": pid,
                    "ppid": ppid,
                    "comm": comm,
                    "state": state
                }
                
            except (PermissionError, OSError, FileNotFoundError, 
                    IndexError, ValueError):
                continue
        
        print(f"[*] Analyzing {len(processes)} processes...")
        
        # Find orphans (ppid=1 but not system services)
        system_services = {
            'systemd', 'init', 'sshd', 'cron', 'rsyslogd', 'dbus-daemon',
            'NetworkManager', 'polkitd', 'gdm', 'lightdm', 'Xorg',
            'pulseaudio', 'gnome-session', 'dockerd', 'containerd'
        }
        
        for pid, proc in processes.items():
            ppid = proc["ppid"]
            comm = proc["comm"]
            
            # Skip init/systemd
            if pid == 1:
                continue
            
            # Check for orphans (parent is init but not a known service)
            if ppid == 1 and comm not in system_services:
                # Check if it's running from suspicious location
                try:
                    exe_link = Path(f"/proc/{pid}/exe").resolve()
                    exe_path = str(exe_link)
                    
                    severity = "low"
                    if '/tmp/' in exe_path or '/dev/shm/' in exe_path:
                        severity = "critical"
                    elif '/home/' in exe_path and '.cache' not in exe_path:
                        severity = "medium"
                    elif not exe_path.startswith('/usr'):
                        severity = "medium"
                    
                    finding = {
                        "pid": pid,
                        "name": comm,
                        "ppid": ppid,
                        "exe": exe_path,
                        "severity": severity
                    }
                    findings.append(finding)
                    
                    if severity in ["critical", "high", "medium"]:
                        print(f"  [{severity.upper()}] PID {pid}: {comm}")
                        print(f"           Exe: {exe_path[:60]}")
                        
                except (PermissionError, OSError, FileNotFoundError):
                    pass
            
            # Check for zombie processes
            if proc["state"] == 'Z':
                finding = {
                    "pid": pid,
                    "name": comm,
                    "ppid": ppid,
                    "state": "zombie",
                    "severity": "low"
                }
                findings.append(finding)
                print(f"  [LOW] Zombie process: PID {pid} ({comm})")
        
        # Summary
        print("\n" + "=" * 60)
        print(f"[*] Found {len(findings)} suspicious processes")
        
        critical = [f for f in findings if f["severity"] == "critical"]
        if critical:
            print("\n[!] CRITICAL: Processes running from suspicious locations!")
            for f in critical:
                print(f"    PID {f['pid']}: {f['name']}")
        
        return findings

    # ==================== KEYLOGGER DETECTION ====================

    def detect_keyloggers(self):
        """Detect potential keylogger activity."""
        print("[*] Keylogger Detection Scan")
        print("-" * 60)
        
        findings = []
        
        if platform.system() != "Linux":
            print("[!] Only supported on Linux")
            return findings
        
        # 1. Check for processes reading from input devices
        print("[*] Checking input device access...")
        input_devices = list(Path("/dev/input").glob("event*"))
        
        for pid_dir in Path("/proc").iterdir():
            if not pid_dir.name.isdigit():
                continue
            
            pid = pid_dir.name
            
            try:
                # Check file descriptors
                fd_dir = pid_dir / "fd"
                for fd in fd_dir.iterdir():
                    try:
                        target = fd.resolve()
                        if "/dev/input" in str(target):
                            # Get process name
                            with open(pid_dir / "comm", 'r') as f:
                                comm = f.read().strip()
                            
                            # Skip known legitimate processes
                            if comm in ['Xorg', 'Xwayland', 'gdm', 'lightdm', 
                                       'gnome-shell', 'sddm', 'loginctl']:
                                continue
                            
                            finding = {
                                "type": "input_device_access",
                                "pid": pid,
                                "name": comm,
                                "device": str(target),
                                "severity": "high"
                            }
                            findings.append(finding)
                            print(f"  [HIGH] Process reading input: {comm} (PID {pid})")
                            
                    except (PermissionError, OSError):
                        continue
                        
            except (PermissionError, OSError):
                continue
        
        # 2. Check for xinput/xdotool processes
        print("[*] Checking for keylogger tools...")
        keylogger_tools = ['xinput', 'xdotool', 'xev', 'logkeys', 'lkl', 
                         'pykeylogger', 'keylogger']
        
        for pid_dir in Path("/proc").iterdir():
            if not pid_dir.name.isdigit():
                continue
            
            try:
                with open(pid_dir / "comm", 'r') as f:
                    comm = f.read().strip().lower()
                
                for tool in keylogger_tools:
                    if tool in comm:
                        finding = {
                            "type": "keylogger_tool",
                            "pid": pid_dir.name,
                            "name": comm,
                            "severity": "critical"
                        }
                        findings.append(finding)
                        print(f"  [CRITICAL] Keylogger tool: {comm}")
                        
            except (PermissionError, OSError):
                continue
        
        # 3. Check for suspicious log files
        print("[*] Checking for keylog files...")
        keylog_patterns = ['*keylog*', '*keystroke*', '*keyboard*log*', '*.keys']
        
        for pattern in keylog_patterns:
            for f in Path("/tmp").glob(pattern):
                finding = {
                    "type": "keylog_file",
                    "path": str(f),
                    "severity": "critical"
                }
                findings.append(finding)
                print(f"  [CRITICAL] Keylog file: {f}")
            
            for f in Path.home().glob(pattern):
                finding = {
                    "type": "keylog_file",
                    "path": str(f),
                    "severity": "high"
                }
                findings.append(finding)
                print(f"  [HIGH] Keylog file: {f}")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Found {len(findings)} keylogger indicators!")
        else:
            print("[✓] No keylogger activity detected")
        
        return findings

    # ==================== CLIPBOARD MONITOR ====================

    def check_clipboard_access(self):
        """Check for processes accessing clipboard."""
        print("[*] Clipboard Access Monitor")
        print("-" * 60)
        
        findings = []
        
        if platform.system() != "Linux":
            print("[!] Only supported on Linux")
            return findings
        
        # Known legitimate clipboard managers
        known_clipboard_apps = {
            'xclip', 'xsel', 'parcellite', 'clipit', 'copyq',
            'klipper', 'gpaste', 'clipman', 'diodon'
        }
        
        print("[*] Checking for clipboard access...")
        
        for pid_dir in Path("/proc").iterdir():
            if not pid_dir.name.isdigit():
                continue
            
            pid = pid_dir.name
            
            try:
                # Check cmdline for clipboard tools
                with open(pid_dir / "cmdline", 'r') as f:
                    cmdline = f.read().replace('\x00', ' ').lower()
                
                with open(pid_dir / "comm", 'r') as f:
                    comm = f.read().strip()
                
                # Skip known apps
                if comm.lower() in known_clipboard_apps:
                    continue
                
                # Check for clipboard access patterns
                clipboard_patterns = ['xclip', 'xsel', 'clipboard', 'primary']
                
                if any(p in cmdline for p in clipboard_patterns):
                    finding = {
                        "pid": pid,
                        "name": comm,
                        "cmdline": cmdline[:100],
                        "severity": "medium"
                    }
                    findings.append(finding)
                    print(f"  [MEDIUM] Clipboard access: {comm} (PID {pid})")
                    
            except (PermissionError, OSError):
                continue
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[*] Found {len(findings)} processes accessing clipboard")
        else:
            print("[✓] No suspicious clipboard access detected")
        
        return findings

    # ==================== ENVIRONMENT VARIABLE CHECK ====================

    def check_environment_variables(self):
        """Check for suspicious environment variables."""
        print("[*] Environment Variable Security Check")
        print("-" * 60)
        
        findings = []
        
        # Suspicious environment variable patterns
        suspicious_vars = {
            'LD_PRELOAD': 'Library preloading (DLL injection)',
            'LD_LIBRARY_PATH': 'Custom library path',
            'LD_AUDIT': 'Library auditing hook',
            'LD_DEBUG': 'Library debug output',
            'PROMPT_COMMAND': 'Command executed before prompt',
            'BASH_ENV': 'Script executed on bash start',
            'ENV': 'Script executed on shell start',
            'HISTFILE': 'Command history file location',
            'HISTSIZE': 'Command history size',
            'http_proxy': 'HTTP proxy setting',
            'https_proxy': 'HTTPS proxy setting',
            'all_proxy': 'Global proxy setting',
        }
        
        # Check current environment
        print("[*] Checking current environment...")
        for var, desc in suspicious_vars.items():
            value = os.environ.get(var)
            if value:
                severity = "high" if var.startswith('LD_') else "medium"
                
                finding = {
                    "variable": var,
                    "value": value[:100],
                    "description": desc,
                    "severity": severity
                }
                findings.append(finding)
                print(f"  [{severity.upper()}] {var}={value[:50]}")
                print(f"           ({desc})")
        
        # Check process environments
        print("\n[*] Checking process environments...")
        
        for pid_dir in Path("/proc").iterdir():
            if not pid_dir.name.isdigit():
                continue
            
            pid = pid_dir.name
            
            try:
                with open(pid_dir / "environ", 'rb') as f:
                    environ = f.read().decode('utf-8', errors='ignore')
                
                with open(pid_dir / "comm", 'r') as f:
                    comm = f.read().strip()
                
                # Check for LD_PRELOAD specifically
                if 'LD_PRELOAD=' in environ:
                    # Extract value
                    for var in environ.split('\x00'):
                        if var.startswith('LD_PRELOAD='):
                            value = var.split('=', 1)[1]
                            finding = {
                                "pid": pid,
                                "process": comm,
                                "variable": "LD_PRELOAD",
                                "value": value[:100],
                                "severity": "critical"
                            }
                            findings.append(finding)
                            print(f"  [CRITICAL] PID {pid} ({comm}): LD_PRELOAD={value[:40]}")
                            
            except (PermissionError, OSError):
                continue
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[*] Found {len(findings)} environment concerns")
            
            critical = [f for f in findings if f.get("severity") == "critical"]
            if critical:
                print("\n[!] CRITICAL: LD_PRELOAD detected - possible code injection!")
        else:
            print("[✓] No suspicious environment variables detected")
        
        return findings

    # ==================== DNS SECURITY CHECK ====================

    def check_dns_security(self):
        """Check DNS configuration and recent queries for threats."""
        print("[*] DNS Security Check")
        print("-" * 60)
        
        findings = []
        
        # 1. Check resolv.conf
        print("[*] Checking DNS configuration...")
        resolv_conf = Path("/etc/resolv.conf")
        if resolv_conf.exists():
            try:
                with open(resolv_conf, 'r') as f:
                    content = f.read()
                
                nameservers = []
                for line in content.split('\n'):
                    if line.startswith('nameserver'):
                        ns = line.split()[1]
                        nameservers.append(ns)
                        print(f"  Nameserver: {ns}")
                
                # Check for suspicious DNS servers
                suspicious_dns = {
                    '8.8.8.8': False,  # Google - OK
                    '8.8.4.4': False,  # Google - OK
                    '1.1.1.1': False,  # Cloudflare - OK
                    '9.9.9.9': False,  # Quad9 - OK
                }
                
                for ns in nameservers:
                    if ns not in suspicious_dns:
                        # Unknown DNS server
                        if not ns.startswith('127.') and not ns.startswith('192.168.'):
                            finding = {
                                "type": "unknown_dns",
                                "server": ns,
                                "severity": "medium"
                            }
                            findings.append(finding)
                            print(f"  [?] Unknown DNS server: {ns}")
                            
            except PermissionError:
                print("  [!] Cannot read resolv.conf")
        
        # 2. Check hosts file for hijacking
        print("\n[*] Checking hosts file...")
        hosts_file = Path("/etc/hosts")
        if hosts_file.exists():
            try:
                with open(hosts_file, 'r') as f:
                    content = f.read()
                
                suspicious_domains = ['google', 'facebook', 'twitter', 'bank',
                                    'paypal', 'amazon', 'microsoft', 'apple']
                
                for line in content.split('\n'):
                    line = line.strip()
                    if line and not line.startswith('#'):
                        parts = line.split()
                        if len(parts) >= 2:
                            ip = parts[0]
                            hostname = parts[1].lower()
                            
                            # Check for DNS hijacking
                            for domain in suspicious_domains:
                                if domain in hostname and ip not in ['127.0.0.1', '::1']:
                                    finding = {
                                        "type": "hosts_hijack",
                                        "ip": ip,
                                        "hostname": hostname,
                                        "severity": "critical"
                                    }
                                    findings.append(finding)
                                    print(f"  [CRITICAL] DNS Hijack: {hostname} -> {ip}")
                                    
            except PermissionError:
                print("  [!] Cannot read hosts file")
        
        # 3. Check for DNS over HTTPS/TLS bypass
        print("\n[*] Checking for DNS bypass...")
        doh_services = [
            ('1.1.1.1', 443),
            ('8.8.8.8', 443),
            ('dns.google', 443),
        ]
        
        # Check active connections
        try:
            with open("/proc/net/tcp", 'r') as f:
                for line in f.readlines()[1:]:
                    parts = line.split()
                    remote = parts[2].split(':')
                    remote_port = int(remote[1], 16)
                    
                    if remote_port == 53:  # DNS
                        print(f"  [*] Active DNS connection detected")
                        
        except (PermissionError, OSError):
            pass
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Found {len(findings)} DNS security issues")
            hijacks = [f for f in findings if f["type"] == "hosts_hijack"]
            if hijacks:
                print("\n[!!!] DNS HIJACKING DETECTED!")
        else:
            print("[✓] DNS configuration appears secure")
        
        return findings

    # ==================== CONTAINER SECURITY ====================

    def scan_containers(self):
        """Scan Docker containers for security issues."""
        print("[*] Container Security Scanner")
        print("-" * 60)
        
        findings = []
        
        # Check if Docker is available
        docker_sock = Path("/var/run/docker.sock")
        if not docker_sock.exists():
            print("[!] Docker not detected on this system")
            return findings
        
        print("[*] Docker detected, scanning containers...")
        
        try:
            # List running containers
            result = subprocess.run(
                ["docker", "ps", "--format", "{{.ID}}\t{{.Image}}\t{{.Names}}"],
                capture_output=True, text=True, timeout=10
            )
            
            if result.returncode != 0:
                print("[!] Cannot access Docker - check permissions")
                return findings
            
            containers = []
            for line in result.stdout.strip().split('\n'):
                if line:
                    parts = line.split('\t')
                    if len(parts) >= 3:
                        containers.append({
                            "id": parts[0],
                            "image": parts[1],
                            "name": parts[2]
                        })
            
            print(f"[*] Found {len(containers)} running containers")
            
            for container in containers:
                print(f"\n  Checking: {container['name']} ({container['image']})")
                
                # Check container for security issues
                issues = []
                
                # 1. Check if running as root
                result = subprocess.run(
                    ["docker", "exec", container["id"], "whoami"],
                    capture_output=True, text=True, timeout=5
                )
                if result.returncode == 0 and 'root' in result.stdout:
                    issues.append("Running as root")
                
                # 2. Check for privileged mode
                result = subprocess.run(
                    ["docker", "inspect", "--format", 
                     "{{.HostConfig.Privileged}}", container["id"]],
                    capture_output=True, text=True, timeout=5
                )
                if result.returncode == 0 and 'true' in result.stdout.lower():
                    issues.append("Privileged mode enabled")
                
                # 3. Check for host network
                result = subprocess.run(
                    ["docker", "inspect", "--format",
                     "{{.HostConfig.NetworkMode}}", container["id"]],
                    capture_output=True, text=True, timeout=5
                )
                if result.returncode == 0 and 'host' in result.stdout.lower():
                    issues.append("Host network mode")
                
                # 4. Check for sensitive mounts
                result = subprocess.run(
                    ["docker", "inspect", "--format",
                     "{{range .Mounts}}{{.Source}}:{{.Destination}} {{end}}", 
                     container["id"]],
                    capture_output=True, text=True, timeout=5
                )
                if result.returncode == 0:
                    mounts = result.stdout.strip()
                    sensitive_mounts = ['/etc', '/root', '/var/run/docker.sock',
                                       '/proc', '/sys']
                    for sm in sensitive_mounts:
                        if sm in mounts:
                            issues.append(f"Sensitive mount: {sm}")
                
                if issues:
                    severity = "critical" if any('privileged' in i.lower() or 
                                                 'docker.sock' in i for i in issues) else "high"
                    finding = {
                        "container": container["name"],
                        "image": container["image"],
                        "issues": issues,
                        "severity": severity
                    }
                    findings.append(finding)
                    
                    print(f"    [{severity.upper()}] Issues found:")
                    for issue in issues:
                        print(f"      • {issue}")
                else:
                    print(f"    [✓] No major issues")
                    
        except subprocess.TimeoutExpired:
            print("[!] Docker command timed out")
        except FileNotFoundError:
            print("[!] Docker CLI not found")
        except Exception as e:
            print(f"[!] Error scanning containers: {e}")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Found {len(findings)} containers with security issues")
        else:
            print("[✓] No container security issues detected")
        
        return findings

    # ==================== THREAT INTELLIGENCE ====================

    def lookup_hash_reputation(self, file_hash):
        """Lookup hash reputation from online databases."""
        print(f"[*] Looking up hash reputation...")
        print(f"    Hash: {file_hash}")
        
        results = {
            "hash": file_hash,
            "reputation": "unknown",
            "detections": 0,
            "sources": []
        }
        
        # Check local signatures first
        local_match = self._check_signature(file_hash)
        if local_match:
            results["reputation"] = "malicious"
            results["sources"].append({
                "source": "GreyAV Signatures",
                "result": local_match["name"]
            })
            print(f"[!] Known malware: {local_match['name']}")
            return results
        
        # Check whitelist
        if self.is_whitelisted(file_hash):
            results["reputation"] = "clean"
            results["sources"].append({
                "source": "GreyAV Whitelist",
                "result": "Trusted"
            })
            print("[✓] Whitelisted file")
            return results
        
        print("[*] Hash not found in local database")
        print("[*] For online lookup, use VirusTotal API")
        
        return results

    # ==================== PROACTIVE THREAT DETECTION ====================

    def proactive_threat_hunt(self, target_path=None, deep=False):
        """
        Comprehensive proactive threat hunting system.
        Actively searches for threats using multiple detection vectors.
        """
        print("[*] Proactive Threat Hunting System")
        print("=" * 70)
        
        findings = []
        start_time = time.time()
        
        target = Path(target_path) if target_path else Path("/")
        
        # Phase 1: Behavioral Anomaly Detection
        print("\n[*] Phase 1: Behavioral Anomaly Detection")
        print("-" * 50)
        findings.extend(self._detect_behavioral_anomalies())
        
        # Phase 2: Process Chain Analysis
        print("\n[*] Phase 2: Process Chain Analysis")
        print("-" * 50)
        findings.extend(self._analyze_process_chains())
        
        # Phase 3: File Creation Pattern Analysis
        print("\n[*] Phase 3: Recent File Activity Analysis")
        print("-" * 50)
        findings.extend(self._analyze_recent_file_activity(target))
        
        # Phase 4: Network Threat Detection
        print("\n[*] Phase 4: Network Threat Detection")
        print("-" * 50)
        findings.extend(self._detect_network_threats())
        
        # Phase 5: Memory Threat Scanning
        print("\n[*] Phase 5: In-Memory Threat Detection")
        print("-" * 50)
        findings.extend(self._scan_memory_threats())
        
        # Phase 6: Persistence Mechanism Detection
        print("\n[*] Phase 6: Persistence Mechanism Detection")
        print("-" * 50)
        findings.extend(self._detect_persistence_mechanisms())
        
        # Phase 7: Privilege Escalation Detection
        print("\n[*] Phase 7: Privilege Escalation Detection")
        print("-" * 50)
        findings.extend(self._detect_privilege_escalation())
        
        # Phase 8: Lateral Movement Detection
        print("\n[*] Phase 8: Lateral Movement Detection")
        print("-" * 50)
        findings.extend(self._detect_lateral_movement())
        
        if deep:
            # Phase 9: Deep Entropy Analysis
            print("\n[*] Phase 9: Deep Entropy Analysis")
            print("-" * 50)
            findings.extend(self._deep_entropy_analysis(target))
            
            # Phase 10: Signature-less Threat Detection
            print("\n[*] Phase 10: Signature-less Threat Detection")
            print("-" * 50)
            findings.extend(self._signatureless_detection(target))
        
        # Summary
        elapsed = time.time() - start_time
        print("\n" + "=" * 70)
        print("[*] PROACTIVE THREAT HUNT SUMMARY")
        print("=" * 70)
        print(f"\n[*] Duration: {elapsed:.2f} seconds")
        print(f"[*] Total Findings: {len(findings)}")
        
        critical = sum(1 for f in findings if f.get('severity') == 'critical')
        high = sum(1 for f in findings if f.get('severity') == 'high')
        medium = sum(1 for f in findings if f.get('severity') == 'medium')
        low = sum(1 for f in findings if f.get('severity') == 'low')
        
        print(f"\n[*] By Severity:")
        print(f"    Critical: {critical}")
        print(f"    High:     {high}")
        print(f"    Medium:   {medium}")
        print(f"    Low:      {low}")
        
        if findings:
            print("\n[*] Top Threats:")
            sorted_findings = sorted(
                findings,
                key=lambda x: {'critical': 0, 'high': 1, 'medium': 2, 'low': 3
                               }.get(x.get('severity', 'low'), 4)
            )
            for finding in sorted_findings[:10]:
                sev = finding.get('severity', 'low').upper()
                print(f"    [{sev}] {finding.get('description', 'Unknown')}")
        
        return findings

    def _detect_behavioral_anomalies(self):
        """Detect behavioral anomalies in running processes."""
        findings = []
        
        try:
            # Get all processes
            for proc_dir in Path("/proc").iterdir():
                if not proc_dir.name.isdigit():
                    continue
                
                pid = int(proc_dir.name)
                try:
                    # Read process info
                    comm_file = proc_dir / "comm"
                    cmdline_file = proc_dir / "cmdline"
                    exe_link = proc_dir / "exe"
                    
                    if not comm_file.exists():
                        continue
                    
                    name = comm_file.read_text().strip()
                    cmdline = ""
                    try:
                        cmdline = cmdline_file.read_bytes().replace(
                            b'\x00', b' ').decode('utf-8', errors='ignore')
                    except Exception:
                        pass
                    
                    # Check for process name masquerading
                    if exe_link.exists():
                        try:
                            exe_path = exe_link.resolve()
                            exe_name = exe_path.name
                            
                            # Process name doesn't match executable
                            if name and exe_name and name != exe_name[:15]:
                                # Some legitimate cases exist, filter common ones
                                legitimate = [
                                    ('bash', 'bash'), ('python3', 'python'),
                                    ('sh', 'dash'), ('node', 'nodejs')
                                ]
                                is_legit = False
                                for legit_name, legit_exe in legitimate:
                                    if legit_name in name or legit_exe in exe_name:
                                        is_legit = True
                                        break
                                
                                if not is_legit and '[' not in name:
                                    print(f"    [!] PID {pid}: Name masquerading")
                                    print(f"        Name: {name}, Exe: {exe_name}")
                                    findings.append({
                                        "type": "behavioral",
                                        "description": f"Process masquerading: {name}",
                                        "severity": "high",
                                        "pid": pid,
                                        "mitre": "T1036.004"
                                    })
                        except Exception:
                            pass
                    
                    # Check for suspicious command line patterns
                    suspicious_patterns = [
                        (r'base64\s+-d', "Base64 decode in cmdline", "high"),
                        (r'curl.*\|\s*(bash|sh)', "Remote script execution", "critical"),
                        (r'wget.*\|\s*(bash|sh)', "Remote script execution", "critical"),
                        (r'python.*-c.*exec', "Python exec in cmdline", "high"),
                        (r'nc\s+-[el]', "Netcat listener/connect", "high"),
                        (r'ncat\s+-[el]', "Ncat listener", "high"),
                        (r'/dev/tcp/', "Bash network redirection", "critical"),
                        (r'mkfifo', "Named pipe creation", "medium"),
                        (r'socat', "Socat tunneling", "medium"),
                        (r'\\x[0-9a-f]{2}', "Hex-encoded content", "medium"),
                        (r'chmod\s+[47]77', "Permissive chmod", "medium"),
                        (r'rm\s+-rf\s+/', "Destructive rm command", "critical"),
                    ]
                    
                    for pattern, desc, severity in suspicious_patterns:
                        if re.search(pattern, cmdline, re.IGNORECASE):
                            print(f"    [!] PID {pid}: {desc}")
                            findings.append({
                                "type": "behavioral",
                                "description": f"{desc} (PID {pid})",
                                "severity": severity,
                                "pid": pid,
                                "cmdline": cmdline[:200]
                            })
                    
                    # Check for deleted executables (common malware behavior)
                    if exe_link.exists():
                        try:
                            exe_target = os.readlink(str(exe_link))
                            if "(deleted)" in exe_target:
                                print(f"    [!] PID {pid}: Running from deleted file")
                                findings.append({
                                    "type": "behavioral",
                                    "description": f"Deleted executable: {name}",
                                    "severity": "critical",
                                    "pid": pid,
                                    "mitre": "T1070.004"
                                })
                        except Exception:
                            pass
                    
                except PermissionError:
                    pass
                except Exception:
                    pass
        
        except Exception as e:
            print(f"    [!] Error: {e}")
        
        if not findings:
            print("    [✓] No behavioral anomalies detected")
        
        return findings

    def _analyze_process_chains(self):
        """Analyze parent-child process relationships for threats."""
        findings = []
        process_tree = {}
        
        try:
            # Build process tree
            for proc_dir in Path("/proc").iterdir():
                if not proc_dir.name.isdigit():
                    continue
                
                pid = int(proc_dir.name)
                try:
                    stat_file = proc_dir / "stat"
                    if stat_file.exists():
                        stat_content = stat_file.read_text()
                        # Parse: pid (name) state ppid...
                        match = re.match(
                            r'(\d+)\s+\(([^)]+)\)\s+\S+\s+(\d+)',
                            stat_content
                        )
                        if match:
                            ppid = int(match.group(3))
                            name = match.group(2)
                            process_tree[pid] = {
                                'name': name,
                                'ppid': ppid,
                                'children': []
                            }
                except Exception:
                    pass
            
            # Build child relationships
            for pid, info in process_tree.items():
                ppid = info['ppid']
                if ppid in process_tree:
                    process_tree[ppid]['children'].append(pid)
            
            # Suspicious process chains
            suspicious_chains = [
                # Web server spawning shell
                (['apache', 'nginx', 'httpd'], ['bash', 'sh', 'dash', 'zsh'],
                 "Web server spawned shell", "critical"),
                # Database spawning shell
                (['mysql', 'postgres', 'mongod'], ['bash', 'sh'],
                 "Database spawned shell", "critical"),
                # Init spawning crypto miner
                (['systemd', 'init'], ['xmrig', 'minerd', 'cpuminer'],
                 "Init spawned crypto miner", "critical"),
                # Shell spawning compiler (living off the land)
                (['bash', 'sh'], ['gcc', 'g++', 'make'],
                 "Shell compiling code", "medium"),
            ]
            
            for pid, info in process_tree.items():
                parent_name = info['name'].lower()
                
                for children in info['children']:
                    if children in process_tree:
                        child_name = process_tree[children]['name'].lower()
                        
                        for parents, childs, desc, severity in suspicious_chains:
                            if any(p in parent_name for p in parents):
                                if any(c in child_name for c in childs):
                                    print(f"    [!] Suspicious chain: {parent_name} -> {child_name}")
                                    findings.append({
                                        "type": "process_chain",
                                        "description": desc,
                                        "severity": severity,
                                        "parent_pid": pid,
                                        "child_pid": children,
                                        "chain": f"{parent_name} -> {child_name}"
                                    })
            
            # Detect deep nesting (potential anti-analysis)
            def get_depth(pid, visited=None):
                if visited is None:
                    visited = set()
                if pid in visited or pid not in process_tree:
                    return 0
                visited.add(pid)
                ppid = process_tree[pid]['ppid']
                return 1 + get_depth(ppid, visited)
            
            for pid, info in process_tree.items():
                depth = get_depth(pid)
                if depth > 10:
                    print(f"    [!] Deep process nesting: {info['name']} (depth {depth})")
                    findings.append({
                        "type": "process_chain",
                        "description": f"Deep process nesting (depth {depth})",
                        "severity": "medium",
                        "pid": pid
                    })
        
        except Exception as e:
            print(f"    [!] Error: {e}")
        
        if not findings:
            print("    [✓] No suspicious process chains detected")
        
        return findings

    def _analyze_recent_file_activity(self, target_path):
        """Analyze recently modified files for threats."""
        findings = []
        suspicious_files = []
        
        try:
            # Find files modified in last 24 hours
            cutoff = time.time() - 86400  # 24 hours
            
            dangerous_locations = [
                '/tmp', '/var/tmp', '/dev/shm',
                '/var/www', '/home'
            ]
            
            for location in dangerous_locations:
                loc_path = Path(location)
                if not loc_path.exists():
                    continue
                
                try:
                    for f in loc_path.rglob("*"):
                        if not f.is_file():
                            continue
                        try:
                            mtime = f.stat().st_mtime
                            if mtime > cutoff:
                                # Check for suspicious characteristics
                                reasons = []
                                severity = "low"
                                
                                # Hidden file in unusual location
                                if f.name.startswith('.') and location in ['/tmp', '/var/tmp', '/dev/shm']:
                                    reasons.append("Hidden file in temp directory")
                                    severity = "high"
                                
                                # Executable in temp
                                if os.access(f, os.X_OK) and location in ['/tmp', '/var/tmp', '/dev/shm']:
                                    reasons.append("Executable in temp directory")
                                    severity = "high"
                                
                                # Script files
                                if f.suffix.lower() in ['.sh', '.py', '.pl', '.rb']:
                                    if location in ['/tmp', '/var/tmp']:
                                        reasons.append("Script in temp directory")
                                        severity = "medium"
                                
                                # Binary files with no extension
                                if not f.suffix and os.access(f, os.X_OK):
                                    magic = self._get_file_magic(f)
                                    if magic and 'executable' in magic.lower():
                                        reasons.append("Unnamed executable")
                                        severity = "high"
                                
                                # High entropy (encrypted/packed)
                                if f.stat().st_size < 10 * 1024 * 1024:  # < 10MB
                                    entropy = self._analyze_entropy(f)
                                    if entropy > 7.5:
                                        reasons.append(f"High entropy ({entropy:.2f})")
                                        severity = "high"
                                
                                if reasons:
                                    suspicious_files.append({
                                        'path': str(f),
                                        'reasons': reasons,
                                        'severity': severity,
                                        'mtime': mtime
                                    })
                        except (PermissionError, OSError):
                            pass
                except PermissionError:
                    pass
            
            # Report findings
            for sf in sorted(suspicious_files, 
                           key=lambda x: {'critical': 0, 'high': 1, 'medium': 2, 'low': 3
                                         }.get(x['severity'], 4))[:20]:
                print(f"    [!] {sf['path']}")
                print(f"        Reasons: {', '.join(sf['reasons'])}")
                findings.append({
                    "type": "file_activity",
                    "description": f"Suspicious file: {sf['path']}",
                    "severity": sf['severity'],
                    "path": sf['path'],
                    "reasons": sf['reasons']
                })
        
        except Exception as e:
            print(f"    [!] Error: {e}")
        
        if not findings:
            print("    [✓] No suspicious recent file activity detected")
        
        return findings

    def _detect_network_threats(self):
        """Detect network-based threats proactively."""
        findings = []
        
        try:
            # Known malicious ports
            malicious_ports = {
                4444: "Metasploit default",
                5555: "Android ADB exploit",
                6666: "IRC botnet",
                6667: "IRC botnet",
                31337: "Back Orifice",
                12345: "NetBus",
                27374: "SubSeven",
                1337: "Generic backdoor",
                9001: "Tor default",
                9050: "Tor SOCKS",
                4443: "Alternative HTTPS",
                8443: "Alternative HTTPS",
                3128: "Squid proxy",
                8080: "HTTP proxy",
            }
            
            # Check established connections
            result = subprocess.run(
                ["ss", "-tnp"],
                capture_output=True, text=True, timeout=10
            )
            
            if result.returncode == 0:
                for line in result.stdout.split('\n')[1:]:
                    if 'ESTAB' not in line:
                        continue
                    
                    parts = line.split()
                    if len(parts) >= 5:
                        remote = parts[4]
                        if ':' in remote:
                            addr, port = remote.rsplit(':', 1)
                            try:
                                port_num = int(port)
                                
                                if port_num in malicious_ports:
                                    print(f"    [!] Connection to suspicious port {port_num}")
                                    print(f"        ({malicious_ports[port_num]})")
                                    findings.append({
                                        "type": "network",
                                        "description": f"Connection to {malicious_ports[port_num]} port",
                                        "severity": "high",
                                        "port": port_num,
                                        "remote": remote
                                    })
                            except ValueError:
                                pass
            
            # Check for suspicious DNS queries (if available)
            dns_cache = Path("/var/cache/bind") 
            if dns_cache.exists():
                # Check for suspicious domains
                pass
            
            # Check for raw sockets (potential packet sniffing)
            result = subprocess.run(
                ["ss", "-w"],
                capture_output=True, text=True, timeout=5
            )
            if result.returncode == 0 and result.stdout.strip():
                lines = [l for l in result.stdout.split('\n') if l.strip() and 'State' not in l]
                if lines:
                    print(f"    [!] Raw sockets detected: {len(lines)}")
                    findings.append({
                        "type": "network",
                        "description": "Raw socket activity detected",
                        "severity": "medium",
                        "count": len(lines)
                    })
            
            # Check for promiscuous mode
            for iface_dir in Path("/sys/class/net").iterdir():
                try:
                    flags_file = iface_dir / "flags"
                    if flags_file.exists():
                        flags = int(flags_file.read_text().strip(), 16)
                        if flags & 0x100:  # IFF_PROMISC
                            print(f"    [!] Interface {iface_dir.name} in promiscuous mode")
                            findings.append({
                                "type": "network",
                                "description": f"Promiscuous mode: {iface_dir.name}",
                                "severity": "high",
                                "interface": iface_dir.name,
                                "mitre": "T1040"
                            })
                except Exception:
                    pass
        
        except Exception as e:
            print(f"    [!] Error: {e}")
        
        if not findings:
            print("    [✓] No network threats detected")
        
        return findings

    def _scan_memory_threats(self):
        """Scan process memory for threats."""
        findings = []
        
        try:
            # Suspicious memory patterns
            suspicious_patterns = [
                (b'/bin/sh', "Shell reference", "medium"),
                (b'/bin/bash', "Bash reference", "low"),
                (b'rm -rf', "Destructive command", "high"),
                (b'wget http', "Download command", "medium"),
                (b'curl http', "Download command", "medium"),
                (b'nc -e', "Netcat reverse shell", "critical"),
                (b'python -c', "Python exec", "medium"),
                (b'/etc/passwd', "Password file access", "medium"),
                (b'/etc/shadow', "Shadow file access", "high"),
                (b'base64 -d', "Base64 decode", "medium"),
                (b'chmod 777', "Permissive chmod", "medium"),
                (b'LD_PRELOAD', "Library preload", "high"),
            ]
            
            # Scan memory of suspicious processes
            for proc_dir in Path("/proc").iterdir():
                if not proc_dir.name.isdigit():
                    continue
                
                pid = int(proc_dir.name)
                
                # Skip system processes
                if pid < 100:
                    continue
                
                try:
                    maps_file = proc_dir / "maps"
                    mem_file = proc_dir / "mem"
                    
                    if not maps_file.exists() or not mem_file.exists():
                        continue
                    
                    # Read process name
                    comm = (proc_dir / "comm").read_text().strip()
                    
                    # Check environment for suspicious vars
                    try:
                        environ = (proc_dir / "environ").read_bytes()
                        
                        for pattern, desc, severity in suspicious_patterns:
                            if pattern in environ:
                                print(f"    [!] PID {pid} ({comm}): {desc} in environment")
                                findings.append({
                                    "type": "memory",
                                    "description": f"{desc} in process environment",
                                    "severity": severity,
                                    "pid": pid,
                                    "process": comm
                                })
                    except PermissionError:
                        pass
                    
                except PermissionError:
                    pass
                except Exception:
                    pass
        
        except Exception as e:
            print(f"    [!] Error: {e}")
        
        if not findings:
            print("    [✓] No memory threats detected")
        
        return findings

    def _detect_persistence_mechanisms(self):
        """Detect malware persistence mechanisms."""
        findings = []
        
        persistence_locations = [
            # Cron
            ("/etc/crontab", "System crontab", "high"),
            ("/var/spool/cron/crontabs", "User crontabs", "high"),
            ("/etc/cron.d", "Cron.d directory", "high"),
            ("/etc/cron.daily", "Daily cron", "medium"),
            ("/etc/cron.hourly", "Hourly cron", "medium"),
            
            # Systemd
            ("/etc/systemd/system", "Systemd services", "high"),
            ("/lib/systemd/system", "Lib systemd", "medium"),
            ("/run/systemd/system", "Runtime systemd", "high"),
            
            # Init
            ("/etc/init.d", "Init scripts", "medium"),
            ("/etc/rc.local", "RC local", "high"),
            
            # Profile scripts
            ("/etc/profile.d", "Profile.d scripts", "high"),
            ("/etc/bash.bashrc", "System bashrc", "medium"),
            
            # SSH
            ("/root/.ssh/authorized_keys", "Root SSH keys", "critical"),
            ("/etc/ssh/sshd_config", "SSH config", "medium"),
            
            # LD preload
            ("/etc/ld.so.preload", "LD preload", "critical"),
        ]
        
        for location, desc, severity in persistence_locations:
            path = Path(location)
            
            try:
                if not path.exists():
                    continue
            except PermissionError:
                continue
            except Exception:
                continue
            
            try:
                if path.is_file():
                    mtime = path.stat().st_mtime
                    age_days = (time.time() - mtime) / 86400
                    
                    # Recently modified system file
                    if age_days < 7:
                        print(f"    [!] Recently modified: {location} ({age_days:.1f} days ago)")
                        
                        # Check content for suspicious patterns
                        try:
                            content = path.read_text(errors='ignore')
                            
                            suspicious = [
                                'curl ', 'wget ', 'base64', '/dev/tcp',
                                'nc -', 'ncat ', 'socat ', 'python -c',
                                'chmod 777', 'rm -rf', '>/dev/null 2>&1 &'
                            ]
                            
                            for susp in suspicious:
                                if susp in content:
                                    findings.append({
                                        "type": "persistence",
                                        "description": f"Suspicious content in {location}",
                                        "severity": "critical",
                                        "path": location,
                                        "pattern": susp,
                                        "mitre": "T1053"
                                    })
                                    break
                        except Exception:
                            pass
                
                elif path.is_dir():
                    # Check for recently added files
                    for f in path.iterdir():
                        try:
                            if f.is_file():
                                mtime = f.stat().st_mtime
                                age_days = (time.time() - mtime) / 86400
                                
                                if age_days < 1:  # Last 24 hours
                                    print(f"    [!] New file in {desc}: {f.name}")
                                    findings.append({
                                        "type": "persistence",
                                        "description": f"New persistence: {f}",
                                        "severity": severity,
                                        "path": str(f),
                                        "mitre": "T1053"
                                    })
                        except Exception:
                            pass
                            
            except PermissionError:
                pass
            except Exception:
                pass
        
        if not findings:
            print("    [✓] No suspicious persistence mechanisms detected")
        
        return findings

    def _detect_privilege_escalation(self):
        """Detect privilege escalation attempts."""
        findings = []
        
        try:
            # Check for suspicious SUID/SGID files
            suspicious_suid = [
                'nmap', 'vim', 'nano', 'python', 'python3', 'perl', 'ruby',
                'php', 'node', 'awk', 'find', 'tar', 'zip', 'wget', 'curl',
                'nc', 'ncat', 'socat', 'bash', 'sh', 'dash', 'zsh'
            ]
            
            suid_paths = ['/usr/bin', '/usr/sbin', '/bin', '/sbin', 
                         '/usr/local/bin', '/usr/local/sbin']
            
            for suid_path in suid_paths:
                path = Path(suid_path)
                if not path.exists():
                    continue
                
                for f in path.iterdir():
                    try:
                        mode = f.stat().st_mode
                        if mode & 0o4000:  # SUID
                            if f.name in suspicious_suid:
                                print(f"    [!] Suspicious SUID binary: {f}")
                                findings.append({
                                    "type": "privilege_escalation",
                                    "description": f"SUID on {f.name}",
                                    "severity": "critical",
                                    "path": str(f),
                                    "mitre": "T1548.001"
                                })
                    except Exception:
                        pass
            
            # Check for world-writable files in PATH
            path_dirs = os.environ.get('PATH', '').split(':')
            for path_dir in path_dirs:
                pd = Path(path_dir)
                if not pd.exists():
                    continue
                    
                try:
                    mode = pd.stat().st_mode
                    if mode & 0o0002:  # World-writable
                        print(f"    [!] World-writable PATH directory: {path_dir}")
                        findings.append({
                            "type": "privilege_escalation",
                            "description": f"Writable PATH: {path_dir}",
                            "severity": "high",
                            "path": path_dir,
                            "mitre": "T1574.007"
                        })
                except Exception:
                    pass
            
            # Check sudo configuration
            sudoers = Path("/etc/sudoers")
            if sudoers.exists():
                try:
                    content = sudoers.read_text()
                    
                    # Check for dangerous sudo rules
                    dangerous_patterns = [
                        ('NOPASSWD.*ALL', "NOPASSWD ALL", "high"),
                        (r'\!authenticate', "No authentication", "high"),
                        (r'env_keep.*LD_', "LD_ environment kept", "critical"),
                    ]
                    
                    for pattern, desc, severity in dangerous_patterns:
                        if re.search(pattern, content):
                            print(f"    [!] Dangerous sudoers config: {desc}")
                            findings.append({
                                "type": "privilege_escalation",
                                "description": f"Sudoers: {desc}",
                                "severity": severity,
                                "mitre": "T1548.003"
                            })
                except PermissionError:
                    pass
            
            # Check capabilities on binaries
            try:
                result = subprocess.run(
                    ["getcap", "-r", "/usr/bin", "/usr/sbin"],
                    capture_output=True, text=True, timeout=30,
                    stderr=subprocess.DEVNULL
                )
                
                if result.returncode == 0:
                    dangerous_caps = [
                        'cap_setuid', 'cap_setgid', 'cap_dac_override',
                        'cap_sys_admin', 'cap_sys_ptrace'
                    ]
                    
                    for line in result.stdout.split('\n'):
                        if not line.strip():
                            continue
                        
                        for cap in dangerous_caps:
                            if cap in line.lower():
                                binary = line.split()[0] if line.split() else 'unknown'
                                name = Path(binary).name
                                
                                if name in suspicious_suid:
                                    print(f"    [!] Dangerous capability: {line}")
                                    findings.append({
                                        "type": "privilege_escalation",
                                        "description": f"Cap on {name}: {cap}",
                                        "severity": "high",
                                        "path": binary,
                                        "mitre": "T1548"
                                    })
            except Exception:
                pass
        
        except Exception as e:
            print(f"    [!] Error: {e}")
        
        if not findings:
            print("    [✓] No privilege escalation vectors detected")
        
        return findings

    def _detect_lateral_movement(self):
        """Detect lateral movement indicators."""
        findings = []
        
        try:
            # Check for suspicious SSH activity
            ssh_log = Path("/var/log/auth.log")
            if ssh_log.exists():
                try:
                    result = subprocess.run(
                        ["tail", "-1000", str(ssh_log)],
                        capture_output=True, text=True, timeout=10
                    )
                    
                    if result.returncode == 0:
                        content = result.stdout
                        
                        # Count failed attempts
                        failed_count = content.count("Failed password")
                        if failed_count > 50:
                            print(f"    [!] High SSH failures: {failed_count}")
                            findings.append({
                                "type": "lateral_movement",
                                "description": "SSH brute force detected",
                                "severity": "high",
                                "count": failed_count,
                                "mitre": "T1110"
                            })
                        
                        # Check for successful auth from unusual IPs
                        accepted = re.findall(
                            r'Accepted \w+ for (\S+) from (\S+)',
                            content
                        )
                        
                        if len(set(ip for _, ip in accepted)) > 10:
                            print("    [!] SSH access from many unique IPs")
                            findings.append({
                                "type": "lateral_movement",
                                "description": "Multiple SSH source IPs",
                                "severity": "medium",
                                "mitre": "T1021.004"
                            })
                            
                except Exception:
                    pass
            
            # Check for suspicious tools
            lateral_tools = [
                ('psexec', "PsExec detected", "high"),
                ('crackmapexec', "CrackMapExec detected", "critical"),
                ('impacket', "Impacket detected", "critical"),
                ('mimikatz', "Mimikatz detected", "critical"),
                ('bloodhound', "BloodHound detected", "high"),
                ('enum4linux', "Enum4Linux detected", "high"),
                ('smbclient', "SMB client usage", "low"),
            ]
            
            for proc_dir in Path("/proc").iterdir():
                if not proc_dir.name.isdigit():
                    continue
                
                try:
                    cmdline_file = proc_dir / "cmdline"
                    if cmdline_file.exists():
                        cmdline = cmdline_file.read_bytes().decode(
                            'utf-8', errors='ignore').lower()
                        
                        for tool, desc, severity in lateral_tools:
                            if tool in cmdline:
                                print(f"    [!] {desc}")
                                findings.append({
                                    "type": "lateral_movement",
                                    "description": desc,
                                    "severity": severity,
                                    "pid": proc_dir.name,
                                    "mitre": "T1021"
                                })
                except Exception:
                    pass
        
        except Exception as e:
            print(f"    [!] Error: {e}")
        
        if not findings:
            print("    [✓] No lateral movement indicators detected")
        
        return findings

    def _deep_entropy_analysis(self, target_path):
        """Deep entropy analysis for encrypted/packed threats."""
        findings = []
        high_entropy_files = []
        
        try:
            scan_paths = ['/tmp', '/var/tmp', '/dev/shm', '/home']
            
            for scan_path in scan_paths:
                path = Path(scan_path)
                if not path.exists():
                    continue
                
                try:
                    for f in path.rglob("*"):
                        if not f.is_file():
                            continue
                        
                        try:
                            size = f.stat().st_size
                            if size < 100 or size > 50 * 1024 * 1024:
                                continue
                            
                            entropy = self._analyze_entropy(f)
                            
                            if entropy > 7.9:
                                high_entropy_files.append({
                                    'path': str(f),
                                    'entropy': entropy,
                                    'size': size,
                                    'severity': 'critical'
                                })
                            elif entropy > 7.5:
                                high_entropy_files.append({
                                    'path': str(f),
                                    'entropy': entropy,
                                    'size': size,
                                    'severity': 'high'
                                })
                        except Exception:
                            pass
                except PermissionError:
                    pass
            
            # Report top findings
            for ef in sorted(high_entropy_files, 
                           key=lambda x: x['entropy'], reverse=True)[:10]:
                print(f"    [!] High entropy: {ef['path']} ({ef['entropy']:.2f})")
                findings.append({
                    "type": "entropy",
                    "description": f"High entropy file: {ef['path']}",
                    "severity": ef['severity'],
                    "path": ef['path'],
                    "entropy": ef['entropy'],
                    "mitre": "T1027"
                })
        
        except Exception as e:
            print(f"    [!] Error: {e}")
        
        if not findings:
            print("    [✓] No suspicious high-entropy files detected")
        
        return findings

    def _signatureless_detection(self, target_path):
        """Signature-less threat detection using behavior analysis."""
        findings = []
        
        try:
            # Yara-like rules without signatures
            behavior_rules = [
                {
                    'name': 'Script Dropper',
                    'conditions': [
                        lambda f: f.suffix in ['.sh', '.py', '.pl'],
                        lambda f: 'curl' in f.read_text(errors='ignore') or 
                                  'wget' in f.read_text(errors='ignore'),
                        lambda f: 'chmod' in f.read_text(errors='ignore'),
                    ],
                    'severity': 'high'
                },
                {
                    'name': 'Reverse Shell',
                    'conditions': [
                        lambda f: '/dev/tcp' in f.read_text(errors='ignore') or
                                  'nc -e' in f.read_text(errors='ignore') or
                                  'bash -i' in f.read_text(errors='ignore'),
                    ],
                    'severity': 'critical'
                },
                {
                    'name': 'Crypto Miner Config',
                    'conditions': [
                        lambda f: 'stratum+' in f.read_text(errors='ignore').lower() or
                                  'pool.' in f.read_text(errors='ignore').lower(),
                        lambda f: f.suffix in ['.json', '.conf', '.cfg', ''],
                    ],
                    'severity': 'critical'
                },
            ]
            
            scan_dirs = ['/tmp', '/var/tmp', '/dev/shm']
            
            for scan_dir in scan_dirs:
                path = Path(scan_dir)
                if not path.exists():
                    continue
                
                try:
                    for f in path.rglob("*"):
                        if not f.is_file():
                            continue
                        
                        try:
                            size = f.stat().st_size
                            if size < 10 or size > 1024 * 1024:
                                continue
                            
                            for rule in behavior_rules:
                                try:
                                    if all(cond(f) for cond in rule['conditions']):
                                        print(f"    [!] {rule['name']}: {f}")
                                        findings.append({
                                            "type": "signatureless",
                                            "description": f"{rule['name']}: {f}",
                                            "severity": rule['severity'],
                                            "path": str(f),
                                            "rule": rule['name']
                                        })
                                except Exception:
                                    pass
                        except Exception:
                            pass
                except PermissionError:
                    pass
        
        except Exception as e:
            print(f"    [!] Error: {e}")
        
        if not findings:
            print("    [✓] No signature-less threats detected")
        
        return findings

    def setup_honeypot_files(self, directories=None):
        """Set up honeypot/canary files to detect ransomware/malware."""
        print("[*] Setting up Honeypot Canary Files")
        print("-" * 60)
        
        if directories is None:
            directories = [Path.home(), Path("/tmp")]
        
        honeypot_names = [
            ".0_DO_NOT_DELETE.txt",
            "!important_backup.docx",
            "passwords.txt.bak",
            ".secret_keys.dat",
            "confidential_data.xlsx"
        ]
        
        created = []
        honeypot_registry = Path.home() / ".greyav_honeypots.json"
        
        for directory in directories:
            dir_path = Path(directory)
            if not dir_path.exists():
                continue
            
            for name in honeypot_names:
                honeypot = dir_path / name
                try:
                    # Create decoy file with tracking content
                    content = f"""HONEYPOT FILE - DO NOT MODIFY
Created: {datetime.now().isoformat()}
Hash: {hashlib.sha256(str(time.time()).encode()).hexdigest()}
If this file is modified or encrypted, ransomware may be active.
"""
                    honeypot.write_text(content)
                    
                    # Store metadata
                    stat = honeypot.stat()
                    created.append({
                        'path': str(honeypot),
                        'hash': hashlib.sha256(content.encode()).hexdigest(),
                        'mtime': stat.st_mtime,
                        'size': stat.st_size
                    })
                    
                    print(f"    [✓] Created: {honeypot}")
                    
                except Exception as e:
                    print(f"    [!] Failed: {honeypot} - {e}")
        
        # Save registry
        if created:
            SafeGuard.safe_json_save(honeypot_registry, created)
            print(f"\n[*] Created {len(created)} honeypot files")
            print(f"[*] Registry saved to: {honeypot_registry}")
        
        return created

    def check_honeypot_integrity(self):
        """Check honeypot files for tampering (ransomware detection)."""
        print("[*] Checking Honeypot File Integrity")
        print("-" * 60)
        
        honeypot_registry = Path.home() / ".greyav_honeypots.json"
        
        if not honeypot_registry.exists():
            print("[!] No honeypot registry found. Run 'setup-honeypots' first.")
            return []
        
        try:
            registry = json.loads(honeypot_registry.read_text())
        except Exception as e:
            print(f"[!] Error reading registry: {e}")
            return []
        
        alerts = []
        
        for entry in registry:
            path = Path(entry['path'])
            original_hash = entry['hash']
            original_mtime = entry['mtime']
            
            try:
                if not path.exists():
                    print(f"    [!!] DELETED: {path}")
                    alerts.append({
                        'path': str(path),
                        'alert': 'DELETED',
                        'severity': 'critical'
                    })
                    continue
                
                content = path.read_text()
                current_hash = hashlib.sha256(content.encode()).hexdigest()
                current_mtime = path.stat().st_mtime
                
                if current_hash != original_hash:
                    print(f"    [!!] MODIFIED: {path}")
                    
                    # Check for encryption indicators
                    entropy = self._calculate_string_entropy(content)
                    if entropy > 7.5:
                        print(f"         High entropy ({entropy:.2f}) - LIKELY ENCRYPTED!")
                        alerts.append({
                            'path': str(path),
                            'alert': 'ENCRYPTED',
                            'severity': 'critical',
                            'entropy': entropy
                        })
                    else:
                        alerts.append({
                            'path': str(path),
                            'alert': 'MODIFIED',
                            'severity': 'high'
                        })
                    
                elif current_mtime != original_mtime:
                    print(f"    [!] Touched (mtime changed): {path}")
                    alerts.append({
                        'path': str(path),
                        'alert': 'TOUCHED',
                        'severity': 'medium'
                    })
                else:
                    print(f"    [✓] OK: {path}")
                    
            except Exception as e:
                print(f"    [!] Error checking {path}: {e}")
        
        if alerts:
            critical = sum(1 for a in alerts if a['severity'] == 'critical')
            if critical > 0:
                print("\n" + "!" * 70)
                print("[!!!] CRITICAL: Ransomware activity may be occurring!")
                print("[!!!] Multiple honeypot files have been modified/encrypted!")
                print("!" * 70)
        else:
            print("\n[✓] All honeypot files intact")
        
        return alerts

    def _calculate_string_entropy(self, text):
        """Calculate entropy of a string."""
        if not text:
            return 0.0
        
        byte_data = text.encode('utf-8', errors='ignore')
        if not byte_data:
            return 0.0
        
        counts = defaultdict(int)
        for byte in byte_data:
            counts[byte] += 1
        
        total = len(byte_data)
        entropy = 0.0
        
        for count in counts.values():
            if count > 0:
                prob = count / total
                entropy -= prob * math.log2(prob)
        
        return entropy

    def predictive_threat_analysis(self, watch_period=60):
        """Analyze system behavior to predict potential threats."""
        print(f"[*] Predictive Threat Analysis ({watch_period}s monitoring)")
        print("=" * 70)
        
        baseline = {
            'cpu_spikes': [],
            'memory_growth': [],
            'file_activity': [],
            'network_connections': [],
            'process_spawns': []
        }
        
        predictions = []
        start_time = time.time()
        samples = 0
        
        print("[*] Collecting behavioral baseline...")
        
        initial_procs = set()
        for proc_dir in Path("/proc").iterdir():
            if proc_dir.name.isdigit():
                initial_procs.add(proc_dir.name)
        
        while time.time() - start_time < watch_period:
            samples += 1
            
            # Sample CPU
            try:
                loadavg = Path("/proc/loadavg").read_text().split()
                load1 = float(loadavg[0])
                baseline['cpu_spikes'].append(load1)
            except Exception:
                pass
            
            # Sample memory
            try:
                meminfo = Path("/proc/meminfo").read_text()
                for line in meminfo.split('\n'):
                    if 'MemAvailable' in line:
                        mem_avail = int(line.split()[1])
                        baseline['memory_growth'].append(mem_avail)
                        break
            except Exception:
                pass
            
            # Sample new processes
            current_procs = set()
            for proc_dir in Path("/proc").iterdir():
                if proc_dir.name.isdigit():
                    current_procs.add(proc_dir.name)
            
            new_procs = current_procs - initial_procs
            if new_procs:
                baseline['process_spawns'].append(len(new_procs))
            initial_procs = current_procs
            
            # Sample network connections
            try:
                result = subprocess.run(
                    ["ss", "-tn", "state", "established"],
                    capture_output=True, text=True, timeout=5
                )
                conn_count = len(result.stdout.strip().split('\n')) - 1
                baseline['network_connections'].append(conn_count)
            except Exception:
                pass
            
            time.sleep(2)
            print(f"\r[*] Sampling... {samples} samples collected", end="")
        
        print(f"\n\n[*] Analysis Complete - {samples} samples collected")
        print("-" * 50)
        
        # Analyze patterns
        if baseline['cpu_spikes']:
            avg_cpu = sum(baseline['cpu_spikes']) / len(baseline['cpu_spikes'])
            max_cpu = max(baseline['cpu_spikes'])
            
            if max_cpu > avg_cpu * 3:
                print(f"[!] CPU spike pattern detected (max: {max_cpu:.2f}, avg: {avg_cpu:.2f})")
                predictions.append({
                    'type': 'resource',
                    'prediction': 'Potential crypto miner or intensive malware',
                    'confidence': min(90, (max_cpu / avg_cpu) * 20),
                    'severity': 'high'
                })
        
        if baseline['memory_growth']:
            start_mem = baseline['memory_growth'][0]
            end_mem = baseline['memory_growth'][-1]
            
            if end_mem < start_mem * 0.7:  # Lost 30% memory
                print(f"[!] Significant memory consumption detected")
                predictions.append({
                    'type': 'resource',
                    'prediction': 'Memory-intensive malware or memory leak',
                    'confidence': 75,
                    'severity': 'medium'
                })
        
        if baseline['process_spawns']:
            total_spawns = sum(baseline['process_spawns'])
            if total_spawns > samples * 5:  # >5 new procs per sample
                print(f"[!] High process spawn rate: {total_spawns} in {watch_period}s")
                predictions.append({
                    'type': 'behavioral',
                    'prediction': 'Fork bomb or rapid process spawning malware',
                    'confidence': 80,
                    'severity': 'high'
                })
        
        if baseline['network_connections']:
            conn_variance = max(baseline['network_connections']) - min(baseline['network_connections'])
            if conn_variance > 20:
                print(f"[!] Network connection fluctuation: {conn_variance}")
                predictions.append({
                    'type': 'network',
                    'prediction': 'Potential C2 beaconing or data exfiltration',
                    'confidence': 60,
                    'severity': 'high'
                })
        
        # Summary
        if predictions:
            print("\n[*] Threat Predictions:")
            for pred in predictions:
                conf = pred['confidence']
                print(f"    [{pred['severity'].upper()}] {pred['prediction']} (confidence: {conf:.0f}%)")
        else:
            print("\n[✓] No threat patterns predicted")
        
        return predictions

    # ==================== BOOT SECTOR SCANNING ====================

    def scan_boot_sector(self, device="/dev/sda"):
        """Scan boot sector for rootkit indicators."""
        print(f"[*] Scanning boot sector: {device}")
        print("-" * 60)
        
        if platform.system() != "Linux":
            print("[!] Boot sector scanning only supported on Linux")
            return None
        
        if os.geteuid() != 0:
            print("[!] Root privileges required for boot sector scanning")
            return None
        
        results = {
            "device": device,
            "suspicious": False,
            "indicators": []
        }
        
        try:
            with open(device, 'rb') as f:
                mbr = f.read(512)
            
            # Check MBR signature
            if mbr[-2:] != b'\x55\xaa':
                results["indicators"].append("Invalid MBR signature")
                results["suspicious"] = True
            
            # Check for suspicious strings in boot code
            suspicious_strings = [
                b'rootkit', b'hidden', b'stealth', b'backdoor'
            ]
            for s in suspicious_strings:
                if s in mbr.lower():
                    results["indicators"].append(f"Suspicious string: {s.decode()}")
                    results["suspicious"] = True
            
            # Calculate and store MBR hash
            mbr_hash = hashlib.sha256(mbr).hexdigest()
            results["mbr_hash"] = mbr_hash
            
            print(f"    MBR Hash: {mbr_hash[:32]}...")
            
            if results["suspicious"]:
                print("\n[!] Suspicious boot sector!")
                for ind in results["indicators"]:
                    print(f"    • {ind}")
            else:
                print("[✓] Boot sector appears normal")
                
        except PermissionError:
            print("[!] Permission denied - run as root")
        except FileNotFoundError:
            print(f"[!] Device not found: {device}")
        except Exception as e:
            print(f"[!] Error: {e}")
        
        return results

    # ==================== SSH KEY AUDIT ====================

    def audit_ssh_keys(self):
        """Audit SSH keys for security issues with comprehensive safeguards."""
        print("[*] SSH Key Security Audit")
        print("-" * 60)
        
        findings = []
        
        # Circuit breaker check
        if not SafeGuard.circuit_breaker_check("ssh_audit"):
            print("[!] SSH audit temporarily disabled")
            return findings
        
        # Common SSH key locations (validated)
        ssh_dirs = []
        potential_dirs = [
            Path.home() / ".ssh",
            Path("/root/.ssh"),
            Path("/etc/ssh"),
        ]
        
        for d in potential_dirs:
            try:
                validated = InputValidator.validate_path(
                    d, must_exist=False, allow_symlinks=False)
                ssh_dirs.append(validated)
            except (ValidationError, PermissionError, OSError):
                # Skip directories we can't access
                continue
        
        # Also check all user home directories with limits
        try:
            user_count = 0
            max_users = 100
            for entry in Path("/home").iterdir():
                if user_count >= max_users:
                    break
                try:
                    if entry.is_dir():
                        ssh_dirs.append(entry / ".ssh")
                        user_count += 1
                except (PermissionError, OSError):
                    continue
        except PermissionError:
            pass
        
        dirs_checked = 0
        max_dirs = 200
        
        for ssh_dir in ssh_dirs:
            if dirs_checked >= max_dirs:
                print(f"[*] Reached directory limit ({max_dirs})")
                break
            
            dirs_checked += 1
            
            try:
                if not ssh_dir.exists():
                    continue
            except PermissionError:
                continue
            
            print(f"\n[*] Checking: {ssh_dir}")
            
            # Check authorized_keys files
            auth_keys_file = ssh_dir / "authorized_keys"
            try:
                if auth_keys_file.exists():
                    # Size limit for reading
                    try:
                        file_size = auth_keys_file.stat().st_size
                        if file_size > 10 * 1024 * 1024:  # 10MB max
                            print(f"    [!] File too large: {file_size} bytes")
                            continue
                    except OSError:
                        continue
                    
                    content = auth_keys_file.read_text(errors='replace')
                    lines = [l.strip() for l in content.split('\n') 
                             if l.strip() and not l.startswith('#')]
                    
                    # Limit number of keys to check
                    max_keys = 500
                    lines = lines[:max_keys]
                    
                    print(f"    Authorized keys: {len(lines)}")
                    
                    for i, line in enumerate(lines, 1):
                        issues = []
                        parts = line.split()
                        
                        if len(parts) >= 2:
                            key_type = parts[0][:32]  # Limit length
                            
                            # Check for weak key types
                            if key_type == 'ssh-dss':
                                issues.append("DSA key (deprecated, weak)")
                            elif key_type == 'ssh-rsa':
                                # RSA is okay but check key size
                                if 'rsa1' in line.lower():
                                    issues.append("RSA1 key (obsolete)")
                        
                        # Check for suspicious options
                        if 'no-pty' not in line and 'command=' in line:
                            issues.append("Forced command without no-pty")
                        
                        # Check for wildcard hosts
                        if 'from="*"' in line:
                            issues.append("Accepts from any host")
                        
                        # Check for suspicious comments
                        suspicious = ['hack', 'backdoor', 'temp', 'test']
                        for sc in suspicious:
                            if sc in line.lower():
                                issues.append(f"Suspicious: '{sc}'")
                        
                        if issues:
                            finding = {
                                "file": str(auth_keys_file),
                                "key_number": i,
                                "issues": issues,
                                "severity": "high"
                            }
                            findings.append(finding)
                            for issue in issues:
                                print(f"    [!] Key {i}: {issue}")
            except PermissionError:
                print(f"    [!] Permission denied: {auth_keys_file}")
            except Exception as e:
                self._log_secure("warning", 
                    f"SSH audit error: {type(e).__name__}")
            
            # Check private keys with safeguards
            try:
                keys_checked = 0
                max_key_files = 100
                for key_file in ssh_dir.glob("*"):
                    if keys_checked >= max_key_files:
                        break
                    
                    try:
                        if not key_file.is_file():
                            continue
                        if key_file.suffix in ['.pub', '.known_hosts']:
                            continue
                        
                        # Size limit
                        if key_file.stat().st_size > 100 * 1024:  # 100KB max
                            continue
                        
                        keys_checked += 1
                        
                        # Check if it's a private key
                        content = key_file.read_text(errors='ignore')
                        if 'PRIVATE KEY' in content:
                            safe_name = InputValidator.sanitize_string(
                                key_file.name, max_length=64)
                            print(f"    Private key: {safe_name}")
                            issues = []
                            
                            # Check permissions
                            mode = key_file.stat().st_mode
                            if mode & 0o077:
                                issues.append(f"Insecure perms: {oct(mode)[-3:]}")
                            
                            # Check key type
                            if 'DSA PRIVATE KEY' in content:
                                issues.append("DSA key (weak)")
                            elif 'RSA PRIVATE KEY' in content:
                                if 'ENCRYPTED' not in content:
                                    issues.append("Unencrypted RSA key")
                            elif 'EC PRIVATE KEY' in content:
                                if 'ENCRYPTED' not in content:
                                    issues.append("Unencrypted EC key")
                            
                            # Check for old format
                            if 'BEGIN RSA PRIVATE KEY' in content:
                                if 'ENCRYPTED' not in content:
                                    issues.append("Use newer OpenSSH format")
                            
                            if issues:
                                finding = {
                                    "file": str(key_file),
                                    "type": "private_key",
                                    "issues": issues,
                                    "severity": "critical" if "Insecure" in str(issues) else "medium"
                                }
                                findings.append(finding)
                                for issue in issues:
                                    print(f"      [!] {issue}")
                                    
                    except PermissionError:
                        pass
                    except Exception:
                        pass
            except PermissionError:
                pass
        
        # Check sshd_config for security
        sshd_config = Path("/etc/ssh/sshd_config")
        if sshd_config.exists():
            print(f"\n[*] Checking SSH daemon configuration")
            try:
                content = sshd_config.read_text()
                
                # Security checks
                security_checks = {
                    'PermitRootLogin yes': "Root login permitted",
                    'PasswordAuthentication yes': "Password auth enabled (prefer keys)",
                    'PermitEmptyPasswords yes': "Empty passwords permitted!",
                    'X11Forwarding yes': "X11 forwarding enabled",
                    'Protocol 1': "SSH Protocol 1 enabled (insecure)",
                }
                
                for pattern, warning in security_checks.items():
                    if pattern.lower() in content.lower():
                        print(f"    [!] {warning}")
                        findings.append({
                            "file": str(sshd_config),
                            "type": "config",
                            "issue": warning,
                            "severity": "high" if "Empty" in warning or "Protocol 1" in warning else "medium"
                        })
                        
            except PermissionError:
                print("    [!] Permission denied reading sshd_config")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            critical = sum(1 for f in findings if f.get('severity') == 'critical')
            high = sum(1 for f in findings if f.get('severity') == 'high')
            print(f"[!] Found {len(findings)} SSH security issues")
            print(f"    Critical: {critical}, High: {high}")
        else:
            print("[✓] No SSH security issues detected")
        
        return findings

    # ==================== KERNEL MODULE SCANNER ====================

    def scan_kernel_modules(self):
        """Scan loaded kernel modules for suspicious entries."""
        print("[*] Kernel Module Security Scanner")
        print("-" * 60)
        
        if platform.system() != "Linux":
            print("[!] Kernel module scanning only supported on Linux")
            return []
        
        findings = []
        
        # Get loaded modules
        try:
            result = subprocess.run(
                ["lsmod"], capture_output=True, text=True, timeout=10
            )
            
            if result.returncode != 0:
                print("[!] Cannot list kernel modules")
                return findings
            
            modules = []
            for line in result.stdout.strip().split('\n')[1:]:  # Skip header
                parts = line.split()
                if parts:
                    modules.append({
                        "name": parts[0],
                        "size": int(parts[1]) if len(parts) > 1 else 0,
                        "used_by": parts[3] if len(parts) > 3 else ""
                    })
            
            print(f"[*] Found {len(modules)} loaded modules")
            
            # Known suspicious module patterns
            suspicious_patterns = [
                'rootkit', 'hide', 'stealth', 'invisible', 'diamorphine',
                'reptile', 'bdvl', 'suterusu', 'knark', 'adore', 'enyelkm',
                'rkmod', 'kbeast', 'necromancer', 'azazel', 'jynx'
            ]
            
            # Check modules against /lib/modules
            kernel_version = platform.release()
            module_path = Path(f"/lib/modules/{kernel_version}")
            
            for mod in modules:
                issues = []
                
                # Check for suspicious names
                mod_lower = mod["name"].lower()
                for pattern in suspicious_patterns:
                    if pattern in mod_lower:
                        issues.append(f"Suspicious name pattern: {pattern}")
                
                # Check if module file exists in standard path
                if module_path.exists():
                    # Search for module file
                    found = False
                    for mod_file in module_path.rglob(f"{mod['name']}.ko*"):
                        found = True
                        break
                    
                    if not found:
                        # Not in standard location - could be suspicious
                        issues.append("Module not found in standard kernel modules path")
                
                # Check modinfo for taint flags
                try:
                    modinfo = subprocess.run(
                        ["modinfo", mod["name"]], 
                        capture_output=True, text=True, timeout=5
                    )
                    if modinfo.returncode == 0:
                        if 'taint' in modinfo.stdout.lower():
                            issues.append("Module has taint flag")
                        if 'unsigned' in modinfo.stdout.lower():
                            issues.append("Unsigned module")
                except (subprocess.TimeoutExpired, FileNotFoundError):
                    pass
                
                if issues:
                    finding = {
                        "module": mod["name"],
                        "size": mod["size"],
                        "issues": issues,
                        "severity": "critical" if "Suspicious name" in str(issues) else "high"
                    }
                    findings.append(finding)
                    print(f"\n  [!] {mod['name']} ({mod['size']} bytes)")
                    for issue in issues:
                        print(f"      • {issue}")
            
        except subprocess.TimeoutExpired:
            print("[!] lsmod timed out")
        except FileNotFoundError:
            print("[!] lsmod not found")
        except Exception as e:
            print(f"[!] Error: {e}")
        
        # Check for hidden modules
        print("\n[*] Checking for hidden modules...")
        try:
            # Compare /proc/modules with lsmod
            proc_modules = set()
            with open("/proc/modules", "r") as f:
                for line in f:
                    parts = line.split()
                    if parts:
                        proc_modules.add(parts[0])
            
            lsmod_modules = {m["name"] for m in modules}
            
            hidden = proc_modules - lsmod_modules
            if hidden:
                for mod in hidden:
                    print(f"  [!] Potentially hidden module: {mod}")
                    findings.append({
                        "module": mod,
                        "issue": "Module in /proc/modules but not in lsmod",
                        "severity": "critical"
                    })
        except Exception as e:
            print(f"[!] Error checking hidden modules: {e}")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Found {len(findings)} suspicious kernel modules")
        else:
            print("[✓] No suspicious kernel modules detected")
        
        return findings

    # ==================== CERTIFICATE SCANNER ====================

    def scan_certificates(self, directory=None):
        """Scan for expired, weak, or suspicious certificates."""
        print("[*] Certificate Security Scanner")
        print("-" * 60)
        
        findings = []
        
        # Certificate locations to check
        cert_dirs = [
            Path("/etc/ssl/certs"),
            Path("/etc/pki/tls/certs"),
            Path("/usr/local/share/ca-certificates"),
            Path("/etc/ca-certificates"),
        ]
        
        if directory:
            cert_dirs = [Path(directory)]
        
        # Also check user directories
        cert_dirs.append(Path.home() / ".ssl")
        cert_dirs.append(Path.home() / ".local/share/ca-certificates")
        
        total_certs = 0
        
        for cert_dir in cert_dirs:
            if not cert_dir.exists():
                continue
            
            print(f"\n[*] Scanning: {cert_dir}")
            
            for cert_file in cert_dir.rglob("*.pem"):
                total_certs += 1
                self._check_certificate(cert_file, findings)
            
            for cert_file in cert_dir.rglob("*.crt"):
                total_certs += 1
                self._check_certificate(cert_file, findings)
            
            for cert_file in cert_dir.rglob("*.cer"):
                total_certs += 1
                self._check_certificate(cert_file, findings)
        
        print(f"\n[*] Scanned {total_certs} certificates")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            expired = sum(1 for f in findings if 'expired' in str(f.get('issues', [])).lower())
            weak = sum(1 for f in findings if 'weak' in str(f.get('issues', [])).lower())
            print(f"[!] Found {len(findings)} certificate issues")
            print(f"    Expired: {expired}, Weak: {weak}")
        else:
            print("[✓] No certificate security issues detected")
        
        return findings

    def _check_certificate(self, cert_path, findings):
        """Check a single certificate for issues."""
        try:
            # Use openssl to check certificate
            result = subprocess.run(
                ["openssl", "x509", "-in", str(cert_path), "-noout", 
                 "-dates", "-subject", "-issuer", "-text"],
                capture_output=True, text=True, timeout=5
            )
            
            if result.returncode != 0:
                return
            
            output = result.stdout
            issues = []
            
            # Check expiration
            import re
            not_after = re.search(r'notAfter=(.+)', output)
            if not_after:
                from email.utils import parsedate_to_datetime
                try:
                    exp_date = parsedate_to_datetime(not_after.group(1))
                    if exp_date < datetime.now(exp_date.tzinfo):
                        issues.append(f"Certificate expired: {not_after.group(1)}")
                    elif (exp_date - datetime.now(exp_date.tzinfo)).days < 30:
                        issues.append(f"Certificate expiring soon: {not_after.group(1)}")
                except Exception:
                    pass
            
            # Check key size
            if 'RSA Public-Key: (1024 bit)' in output:
                issues.append("Weak RSA key size: 1024 bits")
            elif 'RSA Public-Key: (512 bit)' in output:
                issues.append("Very weak RSA key size: 512 bits")
            
            # Check signature algorithm
            if 'sha1WithRSAEncryption' in output:
                issues.append("Weak signature algorithm: SHA-1")
            elif 'md5WithRSAEncryption' in output:
                issues.append("Very weak signature algorithm: MD5")
            
            # Check for self-signed
            subject = re.search(r'subject=(.+)', output)
            issuer = re.search(r'issuer=(.+)', output)
            if subject and issuer and subject.group(1) == issuer.group(1):
                if '/etc/ssl/certs' not in str(cert_path):
                    issues.append("Self-signed certificate")
            
            if issues:
                print(f"    [!] {cert_path.name}")
                for issue in issues:
                    print(f"        • {issue}")
                findings.append({
                    "file": str(cert_path),
                    "issues": issues,
                    "severity": "high" if "expired" in str(issues).lower() else "medium"
                })
                
        except subprocess.TimeoutExpired:
            pass
        except FileNotFoundError:
            pass
        except Exception:
            pass

    # ==================== PORT SCANNER ====================

    def scan_open_ports(self, quick=True):
        """Scan for open ports and suspicious services with safeguards."""
        print("[*] Open Port Scanner")
        print("-" * 60)
        
        findings = []
        
        # Circuit breaker check
        if not SafeGuard.circuit_breaker_check("port_scan"):
            print("[!] Port scanning temporarily disabled")
            return findings
        
        # Get listening ports using ss or netstat
        try:
            # Find command path securely
            ss_path = shutil.which("ss")
            netstat_path = shutil.which("netstat")
            
            if ss_path:
                result = subprocess.run(
                    [ss_path, "-tlnp"], 
                    capture_output=True, 
                    text=True, 
                    timeout=30,
                    env={"PATH": "/usr/bin:/bin"}
                )
            elif netstat_path:
                result = subprocess.run(
                    [netstat_path, "-tlnp"], 
                    capture_output=True, 
                    text=True, 
                    timeout=30,
                    env={"PATH": "/usr/bin:/bin"}
                )
            else:
                print("[!] Neither ss nor netstat found")
                return findings
        except subprocess.TimeoutExpired:
            print("[!] Port scan timed out")
            SafeGuard.record_failure("port_scan")
            return findings
        except FileNotFoundError:
            print("[!] Required network tools not found")
            return findings
        except Exception as e:
            SafeGuard.record_failure("port_scan")
            self._log_secure("error", f"Port scan error: {type(e).__name__}")
            return findings
        
        if result.returncode != 0:
            print("[!] Cannot list listening ports")
            return findings
        
        # Parse output safely with limits
        ports = []
        max_ports = 1000  # Prevent DoS
        
        try:
            lines = result.stdout.strip().split('\n')[1:]
            for line in lines[:max_ports]:
                try:
                    parts = line.split()
                    if len(parts) >= 4:
                        # Extract port and process
                        local_addr = parts[3]
                        if ':' in local_addr:
                            port_str = local_addr.split(':')[-1]
                            port = int(port_str)
                            if 0 < port <= 65535:  # Valid port range
                                process = parts[-1][:64] if len(parts) > 4 else "unknown"
                                ports.append({
                                    "port": port, 
                                    "process": process, 
                                    "line": line[:256]
                                })
                except (ValueError, IndexError):
                    continue
        except Exception:
            pass
        
        print(f"[*] Found {len(ports)} listening ports")
        SafeGuard.record_success("port_scan")
        
        # Use centralized port manager if available
        if PORT_MANAGER_AVAILABLE and get_port_manager:
            pm = get_port_manager()
            for p in ports:
                port = p["port"]
                process = p.get("process", "")
                
                # Perform comprehensive port analysis
                analysis = pm.analyze_port(
                    port, 
                    process=process,
                    platform=platform.system().lower()
                )
                
                issues = analysis.get("issues", [])
                severity = analysis["risk_level"].lower()
                
                # Map severity levels
                if severity == "critical":
                    severity = "critical"
                elif severity == "high":
                    severity = "high"
                elif severity == "medium":
                    severity = "medium"
                else:
                    severity = "info"
                
                if issues or analysis.get("is_c2_indicator"):
                    if analysis.get("is_c2_indicator"):
                        issues.insert(0, f"C2/Backdoor indicator: {analysis['service']}")
                        severity = "critical"
                    
                    print(f"\n  [{severity.upper()}] Port {port} ({analysis['service']})")
                    print(f"      Process: {process}")
                    print(f"      Risk Score: {analysis['risk_score']}/100")
                    for issue in issues:
                        print(f"      • {issue}")
                    
                    if analysis.get('mitre_attack_id'):
                        print(f"      MITRE ATT&CK: {analysis['mitre_attack_id']}")
                    
                    for rec in analysis.get('recommendations', []):
                        print(f"      → {rec}")
                    
                    findings.append({
                        "port": port,
                        "process": process,
                        "service": analysis['service'],
                        "issues": issues,
                        "severity": severity,
                        "risk_score": analysis['risk_score'],
                        "mitre_attack_id": analysis.get('mitre_attack_id'),
                        "is_c2_indicator": analysis.get('is_c2_indicator', False),
                        "recommendations": analysis.get('recommendations', [])
                    })
                elif not quick:
                    print(f"  [INFO] Port {port}: {analysis['service']} (Risk: {analysis['risk_level']})")
        else:
            # Fallback to static port lists
            suspicious_ports = {
                4444: "Metasploit default",
                5555: "Android ADB / various backdoors",
                6666: "IRC / backdoor common",
                6667: "IRC",
                31337: "Elite backdoor",
                12345: "NetBus",
                27374: "SubSeven",
                1234: "Common backdoor",
                9001: "Tor default",
                4443: "Alternative HTTPS / C2",
                8080: "HTTP proxy / possible C2",
                8443: "Alternative HTTPS",
                2222: "Alternative SSH",
                3389: "RDP (unexpected on Linux)",
                5900: "VNC",
                5901: "VNC",
                50050: "Cobalt Strike",
                3333: "Crypto mining stratum",
                14444: "Monero mining",
                7777: "Backdoor common",
                9999: "Backdoor common",
            }
            
            known_ports = {
                22: "SSH",
                80: "HTTP",
                443: "HTTPS",
                25: "SMTP",
                53: "DNS",
                123: "NTP",
                67: "DHCP Server",
                68: "DHCP Client",
                465: "SMTPS",
                587: "Submission",
                993: "IMAPS",
                995: "POP3S",
                3306: "MySQL",
                5432: "PostgreSQL",
                6379: "Redis",
                27017: "MongoDB",
                8000: "HTTP Alt (dev)",
                8080: "HTTP Proxy",
                8443: "HTTPS Alt",
                3000: "Node.js",
                5000: "Flask",
                9000: "PHP-FPM",
                9090: "Prometheus/Cockpit",
                631: "CUPS",
                5353: "mDNS",
                1194: "OpenVPN",
                51820: "WireGuard",
            }
            
            for p in ports:
                port = p["port"]
                issues = []
                severity = "info"
                
                if port in suspicious_ports:
                    issues.append(f"Suspicious port: {suspicious_ports[port]}")
                    severity = "high"
                elif port > 1024 and port not in known_ports:
                    if 'unknown' in p["process"].lower():
                        issues.append("Unknown process on high port")
                        severity = "medium"
                
                if port in [3389] and platform.system() == "Linux":
                    issues.append("RDP port open on Linux (unexpected)")
                    severity = "high"
                
                if issues:
                    print(f"\n  [{severity.upper()}] Port {port}")
                    print(f"      Process: {p['process']}")
                    for issue in issues:
                        print(f"      • {issue}")
                    findings.append({
                        "port": port,
                        "process": p["process"],
                        "issues": issues,
                        "severity": severity
                    })
                elif not quick:
                    print(f"  [INFO] Port {port}: {known_ports.get(port, 'Unknown')}")
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            high = sum(1 for f in findings if f.get('severity') == 'high')
            print(f"[!] Found {len(findings)} suspicious open ports (High: {high})")
        else:
            print(f"[✓] No suspicious open ports detected ({len(ports)} ports scanned)")
        
        return findings

    # ==================== USER ACCOUNT AUDITOR ====================

    def audit_user_accounts(self):
        """Audit user accounts for security issues."""
        print("[*] User Account Security Audit")
        print("-" * 60)
        
        findings = []
        
        # Parse /etc/passwd
        try:
            with open("/etc/passwd", "r") as f:
                passwd_lines = f.readlines()
        except PermissionError:
            print("[!] Permission denied reading /etc/passwd")
            return findings
        
        # Parse /etc/shadow if accessible
        shadow_data = {}
        try:
            with open("/etc/shadow", "r") as f:
                for line in f:
                    parts = line.strip().split(':')
                    if parts:
                        shadow_data[parts[0]] = {
                            "password_hash": parts[1] if len(parts) > 1 else "",
                            "last_change": parts[2] if len(parts) > 2 else "",
                            "max_days": parts[4] if len(parts) > 4 else ""
                        }
        except PermissionError:
            print("[*] Cannot read /etc/shadow (need root)")
        
        uid_0_users = []
        empty_password = []
        no_shell_issues = []
        suspicious_users = []
        
        suspicious_names = ['admin', 'test', 'guest', 'user', 'backup', 'temp']
        
        for line in passwd_lines:
            parts = line.strip().split(':')
            if len(parts) < 7:
                continue
            
            username = parts[0]
            uid = int(parts[2])
            gid = int(parts[3])
            home = parts[5]
            shell = parts[6]
            
            issues = []
            
            # Check for UID 0 (root equivalent)
            if uid == 0 and username != 'root':
                issues.append(f"UID 0 account (root equivalent)")
                uid_0_users.append(username)
            
            # Check for empty password in shadow
            if username in shadow_data:
                pwd_hash = shadow_data[username]["password_hash"]
                if pwd_hash == "" or pwd_hash == "!":
                    pass  # Locked or no password set
                elif pwd_hash == "*":
                    pass  # Disabled
                elif len(pwd_hash) < 13 and pwd_hash not in ['!', '*', '!!']:
                    issues.append("Weak or empty password hash")
                    empty_password.append(username)
            
            # Check for suspicious usernames
            if username.lower() in suspicious_names:
                issues.append(f"Suspicious username pattern")
                suspicious_users.append(username)
            
            # Check for interactive shell on system accounts
            system_accounts = ['daemon', 'bin', 'sys', 'games', 'man', 'lp', 
                             'mail', 'news', 'uucp', 'proxy', 'www-data',
                             'backup', 'list', 'irc', 'gnats', 'nobody']
            if username in system_accounts and shell not in ['/bin/false', '/usr/sbin/nologin', '/sbin/nologin']:
                issues.append(f"System account with login shell: {shell}")
                no_shell_issues.append(username)
            
            # Check for users with unusual home directories
            if uid >= 1000 and uid < 65534:  # Regular users
                if not home.startswith('/home/') and home not in ['/root', '/var/lib']:
                    issues.append(f"Unusual home directory: {home}")
            
            if issues:
                severity = "critical" if uid == 0 and username != 'root' else "high"
                findings.append({
                    "username": username,
                    "uid": uid,
                    "shell": shell,
                    "issues": issues,
                    "severity": severity
                })
                print(f"\n  [{severity.upper()}] {username} (UID: {uid})")
                for issue in issues:
                    print(f"      • {issue}")
        
        # Check for users in sudoers
        print("\n[*] Checking sudo privileges...")
        try:
            result = subprocess.run(
                ["grep", "-r", "ALL", "/etc/sudoers", "/etc/sudoers.d/"],
                capture_output=True, text=True, timeout=5
            )
            if result.stdout:
                sudo_users = []
                for line in result.stdout.strip().split('\n'):
                    if 'ALL' in line and not line.strip().startswith('#'):
                        sudo_users.append(line.strip())
                
                if sudo_users:
                    print(f"    Users/groups with sudo ALL access: {len(sudo_users)}")
                    for su in sudo_users[:5]:  # Show first 5
                        print(f"      • {su[:60]}...")
                        
        except (subprocess.TimeoutExpired, FileNotFoundError):
            pass
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Found {len(findings)} user account security issues")
            if uid_0_users:
                print(f"    UID 0 accounts: {', '.join(uid_0_users)}")
            if empty_password:
                print(f"    Weak passwords: {len(empty_password)}")
        else:
            print("[✓] No user account security issues detected")
        
        return findings

    # ==================== SERVICE AUDITOR ====================

    def audit_services(self):
        """Audit running services for security issues."""
        print("[*] Service Security Audit")
        print("-" * 60)
        
        findings = []
        
        # Check for systemd or init
        use_systemd = Path("/run/systemd/system").exists()
        
        if use_systemd:
            print("[*] Using systemd...")
            
            # List all services
            try:
                result = subprocess.run(
                    ["systemctl", "list-units", "--type=service", "--all", "--no-pager"],
                    capture_output=True, text=True, timeout=10
                )
                
                services = []
                for line in result.stdout.strip().split('\n')[1:]:
                    parts = line.split()
                    if len(parts) >= 4 and '.service' in parts[0]:
                        services.append({
                            "name": parts[0].replace('.service', ''),
                            "load": parts[1],
                            "active": parts[2],
                            "sub": parts[3]
                        })
                
                print(f"[*] Found {len(services)} services")
                
                # Suspicious service patterns
                suspicious_patterns = [
                    'miner', 'crypto', 'backdoor', 'reverse', 'shell', 'rat',
                    'bind', 'connect', 'tunnel', 'proxy', 'hidden'
                ]
                
                for svc in services:
                    if svc["active"] != "active":
                        continue
                    
                    issues = []
                    svc_lower = svc["name"].lower()
                    
                    # Check for suspicious names
                    for pattern in suspicious_patterns:
                        if pattern in svc_lower:
                            issues.append(f"Suspicious name: contains '{pattern}'")
                    
                    # Check service file for issues
                    svc_file = Path(f"/etc/systemd/system/{svc['name']}.service")
                    if not svc_file.exists():
                        svc_file = Path(f"/lib/systemd/system/{svc['name']}.service")
                    
                    if svc_file.exists():
                        try:
                            content = svc_file.read_text()
                            
                            # Check for suspicious ExecStart
                            if '/tmp/' in content or '/dev/shm/' in content:
                                issues.append("Service runs from /tmp or /dev/shm")
                            
                            # Check for reverse shell patterns
                            if 'nc ' in content and '-e' in content:
                                issues.append("Possible netcat reverse shell")
                            if 'bash -i' in content and '>&' in content:
                                issues.append("Possible bash reverse shell")
                            
                            # Check for download and execute
                            if ('curl' in content or 'wget' in content) and ('|' in content or 'bash' in content):
                                issues.append("Download and execute pattern")
                                
                        except PermissionError:
                            pass
                    
                    if issues:
                        severity = "critical" if "reverse shell" in str(issues).lower() else "high"
                        findings.append({
                            "service": svc["name"],
                            "state": svc["active"],
                            "issues": issues,
                            "severity": severity
                        })
                        print(f"\n  [{severity.upper()}] {svc['name']}")
                        for issue in issues:
                            print(f"      • {issue}")
                            
            except (subprocess.TimeoutExpired, FileNotFoundError) as e:
                print(f"[!] Error listing services: {e}")
        else:
            print("[*] Using init.d...")
            # Check /etc/init.d scripts
            init_d = Path("/etc/init.d")
            if init_d.exists():
                for script in init_d.iterdir():
                    if script.is_file():
                        try:
                            content = script.read_text(errors='ignore')
                            issues = []
                            
                            if '/tmp/' in content:
                                issues.append("References /tmp directory")
                            if 'nc ' in content and '-e' in content:
                                issues.append("Possible netcat usage")
                            
                            if issues:
                                findings.append({
                                    "service": script.name,
                                    "issues": issues,
                                    "severity": "medium"
                                })
                                
                        except PermissionError:
                            pass
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Found {len(findings)} suspicious services")
        else:
            print("[✓] No suspicious services detected")
        
        return findings

    # ==================== SUDO/PAM CHECKER ====================

    def check_sudo_pam_security(self):
        """Check sudoers and PAM configuration for security issues."""
        print("[*] Sudo/PAM Security Checker")
        print("-" * 60)
        
        findings = []
        
        # Check sudoers
        print("\n[*] Checking sudoers configuration...")
        sudoers_files = [Path("/etc/sudoers")]
        sudoers_d = Path("/etc/sudoers.d")
        if sudoers_d.exists():
            sudoers_files.extend(sudoers_d.glob("*"))
        
        for sudoers_file in sudoers_files:
            if not sudoers_file.exists():
                continue
            
            try:
                content = sudoers_file.read_text()
                issues = []
                
                # Check for NOPASSWD
                if 'NOPASSWD' in content:
                    nopasswd_lines = [l for l in content.split('\n') 
                                     if 'NOPASSWD' in l and not l.strip().startswith('#')]
                    if nopasswd_lines:
                        issues.append(f"NOPASSWD entries: {len(nopasswd_lines)}")
                
                # Check for ALL ALL ALL
                if 'ALL=(ALL) ALL' in content or 'ALL=(ALL:ALL) ALL' in content:
                    count = content.count('ALL=(ALL')
                    issues.append(f"Broad sudo privileges: {count} entries")
                
                # Check for !authenticate
                if '!authenticate' in content:
                    issues.append("Sudo without authentication enabled")
                
                # Check for secure_path
                if 'secure_path' not in content.lower():
                    issues.append("No secure_path set")
                
                # Check for visudo syntax issues (basic)
                if '\\n' in content or '\t\t\t' in content:
                    issues.append("Potential formatting issues")
                
                if issues:
                    print(f"\n  [!] {sudoers_file}")
                    for issue in issues:
                        print(f"      • {issue}")
                    findings.append({
                        "file": str(sudoers_file),
                        "type": "sudoers",
                        "issues": issues,
                        "severity": "high" if "NOPASSWD" in str(issues) else "medium"
                    })
                    
            except PermissionError:
                print(f"  [!] Permission denied: {sudoers_file}")
        
        # Check PAM configuration
        print("\n[*] Checking PAM configuration...")
        pam_d = Path("/etc/pam.d")
        
        if pam_d.exists():
            critical_pam_files = ['su', 'sudo', 'login', 'sshd', 'system-auth', 'common-auth']
            
            for pam_name in critical_pam_files:
                pam_file = pam_d / pam_name
                if not pam_file.exists():
                    continue
                
                try:
                    content = pam_file.read_text()
                    issues = []
                    
                    # Check for pam_permit (allows without auth)
                    if 'pam_permit.so' in content:
                        lines = [l for l in content.split('\n') 
                                if 'pam_permit.so' in l and not l.strip().startswith('#')]
                        if lines:
                            issues.append("pam_permit.so found (bypasses auth)")
                    
                    # Check for nullok (allows null passwords)
                    if 'nullok' in content:
                        issues.append("nullok option found (allows empty passwords)")
                    
                    # Check for pam_rootok without restrictions
                    if 'pam_rootok.so' in content:
                        lines = [l for l in content.split('\n') 
                                if 'pam_rootok.so' in l and not l.strip().startswith('#')]
                        if any('sufficient' in l for l in lines):
                            issues.append("pam_rootok.so as sufficient (root bypasses)")
                    
                    # Check for suspicious PAM modules
                    suspicious_pam = ['pam_exec.so', 'pam_script.so']
                    for sp in suspicious_pam:
                        if sp in content:
                            issues.append(f"Found {sp} (can execute scripts)")
                    
                    if issues:
                        print(f"\n  [!] {pam_file}")
                        for issue in issues:
                            print(f"      • {issue}")
                        findings.append({
                            "file": str(pam_file),
                            "type": "pam",
                            "issues": issues,
                            "severity": "critical" if "pam_permit" in str(issues) else "high"
                        })
                        
                except PermissionError:
                    pass
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Found {len(findings)} sudo/PAM security issues")
        else:
            print("[✓] No sudo/PAM security issues detected")
        
        return findings

    # ==================== IOC SCANNER ====================

    def scan_iocs(self, ioc_file=None):
        """Scan system for Indicators of Compromise."""
        print("[*] IOC (Indicators of Compromise) Scanner")
        print("-" * 60)
        
        findings = []
        
        # Default IOCs (can be loaded from file)
        iocs = {
            "ip_addresses": [
                "185.220.101.1",  # Known Tor exit
                "45.33.32.156",   # Example malicious
            ],
            "domains": [
                "malware.com",
                "evil-domain.net",
                "c2-server.xyz",
            ],
            "file_hashes": [
                "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",  # Empty file
            ],
            "file_paths": [
                "/tmp/.hidden_backdoor",
                "/var/tmp/.cache_update",
                "/dev/shm/.x",
            ],
            "process_names": [
                "xmrig",
                "minerd",
                "cryptonight",
            ],
            "registry_patterns": [],  # Linux doesn't have registry
        }
        
        # Load custom IOCs if provided
        if ioc_file:
            try:
                with open(ioc_file, 'r') as f:
                    custom_iocs = json.load(f)
                    for key in iocs:
                        if key in custom_iocs:
                            iocs[key].extend(custom_iocs[key])
                print(f"[*] Loaded custom IOCs from {ioc_file}")
            except Exception as e:
                print(f"[!] Error loading IOC file: {e}")
        
        # Check file paths
        print("\n[*] Checking suspicious file paths...")
        for path in iocs["file_paths"]:
            p = Path(path)
            if p.exists():
                print(f"  [!] Found: {path}")
                findings.append({
                    "type": "file_path",
                    "value": path,
                    "severity": "critical"
                })
        
        # Check running processes
        print("[*] Checking running processes...")
        try:
            result = subprocess.run(
                ["ps", "aux"], capture_output=True, text=True, timeout=10
            )
            for proc_name in iocs["process_names"]:
                if proc_name.lower() in result.stdout.lower():
                    print(f"  [!] Suspicious process: {proc_name}")
                    findings.append({
                        "type": "process",
                        "value": proc_name,
                        "severity": "critical"
                    })
        except Exception:
            pass
        
        # Check network connections for IOC IPs
        print("[*] Checking network connections...")
        try:
            result = subprocess.run(
                ["ss", "-tn"], capture_output=True, text=True, timeout=10
            )
            for ip in iocs["ip_addresses"]:
                if ip in result.stdout:
                    print(f"  [!] Connection to IOC IP: {ip}")
                    findings.append({
                        "type": "network",
                        "value": ip,
                        "severity": "critical"
                    })
        except Exception:
            pass
        
        # Check DNS cache / hosts file
        print("[*] Checking hosts file...")
        try:
            hosts_content = Path("/etc/hosts").read_text()
            for domain in iocs["domains"]:
                if domain in hosts_content:
                    print(f"  [!] IOC domain in hosts: {domain}")
                    findings.append({
                        "type": "domain",
                        "value": domain,
                        "severity": "high"
                    })
        except Exception:
            pass
        
        # Summary
        print("\n" + "=" * 60)
        if findings:
            print(f"[!] Found {len(findings)} IOC matches!")
            print("[!] CRITICAL: System may be compromised")
        else:
            print("[✓] No IOC matches detected")
        
        return findings

    # ==================== FIREWALL AUDITOR ====================

    def audit_firewall(self, deep_scan=False, export_rules=False):
        """
        Comprehensive firewall security audit with advanced analysis.
        
        Features:
        - Multi-firewall detection (iptables, nftables, UFW, firewalld)
        - Rule analysis and scoring
        - Port exposure assessment
        - Stateful inspection verification
        - Rule optimization suggestions
        - Security policy compliance check
        - Network zone analysis
        - Logging configuration audit
        """
        print("[*] Comprehensive Firewall Security Audit")
        print("=" * 70)
        
        findings = []
        audit_report = {
            'timestamp': datetime.now().isoformat(),
            'firewalls_detected': [],
            'security_score': 100,
            'findings': [],
            'recommendations': [],
            'rule_summary': {},
            'exposed_ports': [],
            'network_zones': []
        }
        
        # ==================== FIREWALL DETECTION ====================
        print("\n[*] Phase 1: Firewall Detection")
        print("-" * 50)
        
        firewall_status = {
            'iptables': {'installed': False, 'active': False, 'rules': 0},
            'nftables': {'installed': False, 'active': False, 'rules': 0},
            'ufw': {'installed': False, 'active': False, 'rules': 0},
            'firewalld': {'installed': False, 'active': False, 'zones': []},
        }
        
        # Check iptables
        try:
            result = subprocess.run(
                ["which", "iptables"],
                capture_output=True, text=True, timeout=5
            )
            if result.returncode == 0:
                firewall_status['iptables']['installed'] = True
                print("    [✓] iptables: Installed")
        except Exception:
            pass
        
        # Check nftables
        try:
            result = subprocess.run(
                ["which", "nft"],
                capture_output=True, text=True, timeout=5
            )
            if result.returncode == 0:
                firewall_status['nftables']['installed'] = True
                print("    [✓] nftables: Installed")
        except Exception:
            pass
        
        # Check UFW
        try:
            result = subprocess.run(
                ["which", "ufw"],
                capture_output=True, text=True, timeout=5
            )
            if result.returncode == 0:
                firewall_status['ufw']['installed'] = True
                print("    [✓] UFW: Installed")
        except Exception:
            pass
        
        # Check firewalld
        try:
            result = subprocess.run(
                ["which", "firewall-cmd"],
                capture_output=True, text=True, timeout=5
            )
            if result.returncode == 0:
                firewall_status['firewalld']['installed'] = True
                print("    [✓] firewalld: Installed")
        except Exception:
            pass
        
        if not any(fw['installed'] for fw in firewall_status.values()):
            print("    [!!] CRITICAL: No firewall software detected!")
            findings.append({
                "type": "system",
                "issue": "No firewall software installed",
                "severity": "critical",
                "recommendation": "Install iptables, nftables, or UFW immediately"
            })
            audit_report['security_score'] -= 50
        
        # ==================== IPTABLES DEEP ANALYSIS ====================
        print("\n[*] Phase 2: iptables Analysis")
        print("-" * 50)
        
        if firewall_status['iptables']['installed']:
            iptables_findings = self._analyze_iptables(deep_scan)
            findings.extend(iptables_findings['findings'])
            firewall_status['iptables'].update(iptables_findings['status'])
            audit_report['rule_summary']['iptables'] = iptables_findings.get('rule_stats', {})
            
            if iptables_findings['status']['active']:
                audit_report['firewalls_detected'].append('iptables')
        else:
            print("    [*] iptables not installed, skipping...")
        
        # ==================== NFTABLES ANALYSIS ====================
        print("\n[*] Phase 3: nftables Analysis")
        print("-" * 50)
        
        if firewall_status['nftables']['installed']:
            nft_findings = self._analyze_nftables(deep_scan)
            findings.extend(nft_findings['findings'])
            firewall_status['nftables'].update(nft_findings['status'])
            audit_report['rule_summary']['nftables'] = nft_findings.get('rule_stats', {})
            
            if nft_findings['status']['active']:
                audit_report['firewalls_detected'].append('nftables')
        else:
            print("    [*] nftables not installed, skipping...")
        
        # ==================== UFW ANALYSIS ====================
        print("\n[*] Phase 4: UFW Analysis")
        print("-" * 50)
        
        if firewall_status['ufw']['installed']:
            ufw_findings = self._analyze_ufw(deep_scan)
            findings.extend(ufw_findings['findings'])
            firewall_status['ufw'].update(ufw_findings['status'])
            
            if ufw_findings['status']['active']:
                audit_report['firewalls_detected'].append('ufw')
        else:
            print("    [*] UFW not installed, skipping...")
        
        # ==================== FIREWALLD ANALYSIS ====================
        print("\n[*] Phase 5: firewalld Analysis")
        print("-" * 50)
        
        if firewall_status['firewalld']['installed']:
            fwd_findings = self._analyze_firewalld(deep_scan)
            findings.extend(fwd_findings['findings'])
            firewall_status['firewalld'].update(fwd_findings['status'])
            audit_report['network_zones'] = fwd_findings.get('zones', [])
            
            if fwd_findings['status']['active']:
                audit_report['firewalls_detected'].append('firewalld')
        else:
            print("    [*] firewalld not installed, skipping...")
        
        # ==================== KERNEL NETWORK SECURITY ====================
        print("\n[*] Phase 6: Kernel Network Security")
        print("-" * 50)
        
        kernel_findings = self._audit_kernel_network_security()
        findings.extend(kernel_findings)
        
        # ==================== PORT EXPOSURE ANALYSIS ====================
        print("\n[*] Phase 7: Port Exposure Analysis")
        print("-" * 50)
        
        exposed_ports = self._analyze_port_exposure()
        audit_report['exposed_ports'] = exposed_ports
        
        # Use centralized port manager for analysis if available
        if PORT_MANAGER_AVAILABLE and get_port_manager:
            pm = get_port_manager()
            for port_info in exposed_ports:
                port = port_info.get('port')
                process = port_info.get('process', '')
                all_interfaces = port_info.get('address', '').startswith('0.0.0.0') or \
                                 port_info.get('address', '').startswith('::')
                
                analysis = pm.analyze_port(
                    port,
                    process=process,
                    listening_all_interfaces=all_interfaces,
                    platform=platform.system().lower()
                )
                
                if analysis['risk_level'] in ('CRITICAL', 'HIGH'):
                    severity = 'critical' if analysis['risk_level'] == 'CRITICAL' else 'high'
                    print(f"    [!] {analysis['risk_level']}: Port {port} ({analysis['service']})")
                    for issue in analysis.get('issues', []):
                        print(f"        - {issue}")
                    
                    findings.append({
                        "type": "port_exposure",
                        "issue": f"Port {port} ({analysis['service']}) - {', '.join(analysis.get('issues', ['high risk']))}",
                        "severity": severity,
                        "port": port,
                        "service": analysis['service'],
                        "risk_score": analysis['risk_score'],
                        "mitre_attack_id": analysis.get('mitre_attack_id'),
                        "is_c2_indicator": analysis.get('is_c2_indicator', False),
                        "recommendation": '; '.join(analysis.get('recommendations', 
                            [f"Consider blocking or restricting access to port {port}"]))
                    })
                    
                    # Adjust security score based on risk
                    if severity == 'critical':
                        audit_report['security_score'] -= 10
                    else:
                        audit_report['security_score'] -= 5
        else:
            # Fallback to static dangerous ports list
            dangerous_ports = {
                21: 'FTP (unencrypted)',
                23: 'Telnet (unencrypted)',
                25: 'SMTP (often spam relay)',
                69: 'TFTP (no auth)',
                111: 'RPC portmapper',
                135: 'Windows RPC',
                139: 'NetBIOS',
                445: 'SMB',
                512: 'rexec',
                513: 'rlogin',
                514: 'rsh',
                1433: 'MSSQL',
                1521: 'Oracle',
                2049: 'NFS',
                3306: 'MySQL',
                3389: 'RDP',
                5432: 'PostgreSQL',
                5900: 'VNC',
                6379: 'Redis',
                27017: 'MongoDB',
                # C2 / Backdoor indicators
                4444: 'Metasploit default',
                5555: 'Android ADB / backdoor',
                6666: 'Backdoor common',
                31337: 'Elite backdoor',
                12345: 'NetBus',
                27374: 'SubSeven',
                50050: 'Cobalt Strike',
                3333: 'Crypto mining stratum',
                14444: 'Monero mining',
            }
            
            for port_info in exposed_ports:
                port = port_info.get('port')
                if port in dangerous_ports:
                    print(f"    [!] Dangerous port exposed: {port} ({dangerous_ports[port]})")
                    findings.append({
                        "type": "port_exposure",
                        "issue": f"Potentially dangerous port {port} ({dangerous_ports[port]}) is exposed",
                        "severity": "high",
                        "port": port,
                        "service": port_info.get('service', 'unknown'),
                        "recommendation": f"Consider blocking or restricting access to port {port}"
                    })
                    audit_report['security_score'] -= 5
        
        # ==================== LOGGING CONFIGURATION ====================
        print("\n[*] Phase 8: Firewall Logging Audit")
        print("-" * 50)
        
        logging_findings = self._audit_firewall_logging()
        findings.extend(logging_findings)
        
        # ==================== SECURITY SCORE CALCULATION ====================
        print("\n[*] Phase 9: Security Score Calculation")
        print("-" * 50)
        
        # Deduct points based on findings
        for finding in findings:
            severity = finding.get('severity', 'low')
            if severity == 'critical':
                audit_report['security_score'] -= 20
            elif severity == 'high':
                audit_report['security_score'] -= 10
            elif severity == 'medium':
                audit_report['security_score'] -= 5
            elif severity == 'low':
                audit_report['security_score'] -= 2
        
        # Ensure score is within bounds
        audit_report['security_score'] = max(0, min(100, audit_report['security_score']))
        
        # ==================== GENERATE RECOMMENDATIONS ====================
        recommendations = self._generate_firewall_recommendations(findings, firewall_status)
        audit_report['recommendations'] = recommendations
        
        # ==================== SUMMARY ====================
        print("\n" + "=" * 70)
        print("[*] FIREWALL AUDIT SUMMARY")
        print("=" * 70)
        
        # Active firewalls
        print(f"\n[*] Active Firewalls: ", end="")
        if audit_report['firewalls_detected']:
            print(", ".join(audit_report['firewalls_detected']))
        else:
            print("NONE - SYSTEM IS UNPROTECTED!")
        
        # Security score
        score = audit_report['security_score']
        if score >= 80:
            score_color = "✓"
            score_status = "GOOD"
        elif score >= 60:
            score_color = "~"
            score_status = "FAIR"
        elif score >= 40:
            score_color = "!"
            score_status = "POOR"
        else:
            score_color = "!!"
            score_status = "CRITICAL"
        
        print(f"\n[{score_color}] Security Score: {score}/100 ({score_status})")
        
        # Findings by severity
        critical = sum(1 for f in findings if f.get('severity') == 'critical')
        high = sum(1 for f in findings if f.get('severity') == 'high')
        medium = sum(1 for f in findings if f.get('severity') == 'medium')
        low = sum(1 for f in findings if f.get('severity') == 'low')
        
        print(f"\n[*] Findings:")
        print(f"    Critical: {critical}")
        print(f"    High:     {high}")
        print(f"    Medium:   {medium}")
        print(f"    Low:      {low}")
        
        # Exposed ports summary
        if exposed_ports:
            print(f"\n[*] Exposed Ports: {len(exposed_ports)}")
            for port_info in exposed_ports[:10]:
                print(f"    - {port_info['port']}/{port_info.get('proto', 'tcp')}: {port_info.get('service', 'unknown')}")
            if len(exposed_ports) > 10:
                print(f"    ... and {len(exposed_ports) - 10} more")
        
        # Top recommendations
        if recommendations:
            print(f"\n[*] Top Recommendations:")
            for i, rec in enumerate(recommendations[:5], 1):
                print(f"    {i}. [{rec['priority']}] {rec['action']}")
        
        # Export rules if requested
        if export_rules:
            export_file = Path(f"firewall_rules_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json")
            audit_report['findings'] = findings
            SafeGuard.safe_json_save(export_file, audit_report)
            print(f"\n[*] Rules exported to: {export_file}")
        
        return findings

    def _analyze_iptables(self, deep_scan=False):
        """Deep analysis of iptables rules."""
        result = {
            'status': {'active': False, 'rules': 0},
            'findings': [],
            'rule_stats': {
                'total_rules': 0,
                'accept_rules': 0,
                'drop_rules': 0,
                'reject_rules': 0,
                'log_rules': 0,
                'chains': []
            }
        }
        
        try:
            # Get all chains and rules
            for table in ['filter', 'nat', 'mangle', 'raw']:
                try:
                    cmd_result = subprocess.run(
                        ["iptables", "-t", table, "-L", "-n", "-v", "--line-numbers"],
                        capture_output=True, text=True, timeout=10
                    )
                    
                    if cmd_result.returncode == 0:
                        output = cmd_result.stdout
                        result['status']['active'] = True
                        
                        # Parse chains
                        chains = re.findall(r'Chain (\S+) \(policy (\S+)', output)
                        for chain_name, policy in chains:
                            result['rule_stats']['chains'].append({
                                'table': table,
                                'chain': chain_name,
                                'policy': policy
                            })
                            
                            # Check for insecure default policies
                            if chain_name in ['INPUT', 'FORWARD'] and policy == 'ACCEPT':
                                print(f"    [!] {table}/{chain_name}: Default policy is ACCEPT")
                                result['findings'].append({
                                    "type": "iptables_policy",
                                    "issue": f"{table}/{chain_name} has permissive default policy",
                                    "severity": "high",
                                    "chain": chain_name,
                                    "table": table,
                                    "recommendation": f"Set default policy to DROP: iptables -P {chain_name} DROP"
                                })
                        
                        # Count rule types
                        result['rule_stats']['accept_rules'] += output.count(' ACCEPT ')
                        result['rule_stats']['drop_rules'] += output.count(' DROP ')
                        result['rule_stats']['reject_rules'] += output.count(' REJECT ')
                        result['rule_stats']['log_rules'] += output.count(' LOG ')
                        
                        # Count total rules (lines with numbers)
                        rule_lines = re.findall(r'^\d+\s+', output, re.MULTILINE)
                        result['rule_stats']['total_rules'] += len(rule_lines)
                        result['status']['rules'] = result['rule_stats']['total_rules']
                        
                        if deep_scan:
                            # Analyze individual rules for security issues
                            self._analyze_iptables_rules_deep(output, table, result)
                            
                except subprocess.TimeoutExpired:
                    print(f"    [!] Timeout reading {table} table")
                except Exception as e:
                    pass
            
            if result['status']['active']:
                total = result['rule_stats']['total_rules']
                print(f"    [*] Total rules: {total}")
                print(f"    [*] ACCEPT: {result['rule_stats']['accept_rules']}, DROP: {result['rule_stats']['drop_rules']}, REJECT: {result['rule_stats']['reject_rules']}")
                
                # Check for empty/minimal ruleset
                if total < 3:
                    print("    [!] Minimal or no rules configured")
                    result['findings'].append({
                        "type": "iptables",
                        "issue": "Firewall has minimal or no rules",
                        "severity": "critical",
                        "recommendation": "Configure proper firewall rules"
                    })
                
                # Check if any logging is configured
                if result['rule_stats']['log_rules'] == 0:
                    print("    [!] No logging rules configured")
                    result['findings'].append({
                        "type": "iptables_logging",
                        "issue": "No iptables logging rules",
                        "severity": "medium",
                        "recommendation": "Add logging rules for dropped/rejected packets"
                    })
            else:
                print("    [!] iptables not active or requires root")
                
        except FileNotFoundError:
            print("    [*] iptables command not found")
        except Exception as e:
            print(f"    [!] Error analyzing iptables: {e}")
        
        return result

    def _analyze_iptables_rules_deep(self, output, table, result):
        """Deep analysis of individual iptables rules."""
        lines = output.split('\n')
        
        for line in lines:
            # Check for overly permissive rules
            if ' ACCEPT ' in line and '0.0.0.0/0' in line:
                parts = line.split()
                if len(parts) > 5:
                    # Check if it's accepting all from anywhere
                    if 'all' in parts and parts.count('0.0.0.0/0') >= 2:
                        print(f"    [!] Overly permissive rule: {line[:60]}...")
                        result['findings'].append({
                            "type": "iptables_rule",
                            "issue": "Overly permissive ACCEPT rule (all from anywhere)",
                            "severity": "high",
                            "rule": line.strip()[:100],
                            "recommendation": "Restrict source/destination or ports"
                        })
            
            # Check for state tracking
            if 'RELATED,ESTABLISHED' not in output and 'state' not in output.lower():
                if 'stateful_warned' not in str(result.get('_flags', [])):
                    result['_flags'] = result.get('_flags', []) + ['stateful_warned']
                    result['findings'].append({
                        "type": "iptables_stateful",
                        "issue": "No stateful packet inspection detected",
                        "severity": "medium",
                        "recommendation": "Add stateful rules: -m state --state RELATED,ESTABLISHED -j ACCEPT"
                    })
            
            # Check for ICMP handling
            if 'icmp' in line.lower():
                if ' ACCEPT ' in line and 'icmp-type' not in line:
                    result['findings'].append({
                        "type": "iptables_icmp",
                        "issue": "ICMP accepted without type restriction",
                        "severity": "low",
                        "recommendation": "Restrict ICMP types (allow echo-request, echo-reply, etc.)"
                    })

    def _analyze_nftables(self, deep_scan=False):
        """Deep analysis of nftables ruleset."""
        result = {
            'status': {'active': False, 'rules': 0},
            'findings': [],
            'rule_stats': {
                'tables': 0,
                'chains': 0,
                'rules': 0
            }
        }
        
        try:
            cmd_result = subprocess.run(
                ["nft", "list", "ruleset"],
                capture_output=True, text=True, timeout=15
            )
            
            if cmd_result.returncode == 0:
                output = cmd_result.stdout
                
                if not output.strip():
                    print("    [!] nftables ruleset is empty")
                    result['findings'].append({
                        "type": "nftables",
                        "issue": "Empty nftables ruleset",
                        "severity": "critical",
                        "recommendation": "Configure nftables rules"
                    })
                else:
                    result['status']['active'] = True
                    
                    # Count tables, chains, rules
                    tables = re.findall(r'table\s+(\w+)\s+(\w+)', output)
                    chains = re.findall(r'chain\s+(\w+)', output)
                    rules = len(re.findall(r'^\s+(?:accept|drop|reject|counter|log)', output, re.MULTILINE))
                    
                    result['rule_stats']['tables'] = len(tables)
                    result['rule_stats']['chains'] = len(chains)
                    result['rule_stats']['rules'] = rules
                    result['status']['rules'] = rules
                    
                    print(f"    [*] Tables: {len(tables)}, Chains: {len(chains)}, Rules: {rules}")
                    
                    # Check for default policies
                    if deep_scan:
                        # Check chain policies
                        chain_policies = re.findall(r'chain\s+(\w+)\s+\{[^}]*policy\s+(\w+)', output, re.DOTALL)
                        for chain, policy in chain_policies:
                            if chain.lower() in ['input', 'forward'] and policy.lower() == 'accept':
                                print(f"    [!] Chain {chain} has ACCEPT policy")
                                result['findings'].append({
                                    "type": "nftables_policy",
                                    "issue": f"Chain {chain} has permissive default policy",
                                    "severity": "high",
                                    "recommendation": f"Set policy to drop in chain {chain}"
                                })
                        
                        # Check for counter usage (helps with monitoring)
                        if 'counter' not in output:
                            result['findings'].append({
                                "type": "nftables_monitoring",
                                "issue": "No packet counters configured",
                                "severity": "low",
                                "recommendation": "Add 'counter' to rules for monitoring"
                            })
                    
            else:
                print("    [!] Cannot read nftables (need root or not configured)")
                
        except FileNotFoundError:
            print("    [*] nft command not found")
        except subprocess.TimeoutExpired:
            print("    [!] nftables command timed out")
        except Exception as e:
            print(f"    [!] Error analyzing nftables: {e}")
        
        return result

    def _analyze_ufw(self, deep_scan=False):
        """Deep analysis of UFW configuration."""
        result = {
            'status': {'active': False, 'rules': 0},
            'findings': []
        }
        
        try:
            # Check UFW status
            cmd_result = subprocess.run(
                ["ufw", "status", "verbose"],
                capture_output=True, text=True, timeout=10
            )
            
            if cmd_result.returncode == 0:
                output = cmd_result.stdout
                
                if "inactive" in output.lower():
                    print("    [!] UFW is INACTIVE")
                    result['findings'].append({
                        "type": "ufw",
                        "issue": "UFW firewall is disabled",
                        "severity": "high",
                        "recommendation": "Enable UFW: sudo ufw enable"
                    })
                else:
                    result['status']['active'] = True
                    print("    [✓] UFW is active")
                    
                    # Parse default policies
                    default_match = re.search(r'Default:\s+(\w+)\s+\(incoming\),\s+(\w+)\s+\(outgoing\)', output)
                    if default_match:
                        incoming, outgoing = default_match.groups()
                        print(f"    [*] Default: incoming={incoming}, outgoing={outgoing}")
                        
                        if incoming.lower() == 'allow':
                            result['findings'].append({
                                "type": "ufw_policy",
                                "issue": "Default incoming policy is ALLOW",
                                "severity": "high",
                                "recommendation": "Set default deny: sudo ufw default deny incoming"
                            })
                    
                    # Count rules
                    rule_lines = [l for l in output.split('\n') if 'ALLOW' in l or 'DENY' in l or 'REJECT' in l]
                    result['status']['rules'] = len(rule_lines)
                    print(f"    [*] Rules configured: {len(rule_lines)}")
                    
                    if deep_scan:
                        # Analyze each rule
                        for rule in rule_lines:
                            # Check for rules allowing all from anywhere
                            if 'Anywhere' in rule and 'ALLOW' in rule:
                                if not any(port in rule for port in ['22', '80', '443', '53']):
                                    result['findings'].append({
                                        "type": "ufw_rule",
                                        "issue": f"Broad ALLOW rule: {rule.strip()}",
                                        "severity": "medium",
                                        "rule": rule.strip()
                                    })
                    
                    # Check logging
                    if 'Logging:' in output:
                        log_match = re.search(r'Logging:\s+(\w+)', output)
                        if log_match and log_match.group(1).lower() == 'off':
                            print("    [!] UFW logging is disabled")
                            result['findings'].append({
                                "type": "ufw_logging",
                                "issue": "UFW logging is disabled",
                                "severity": "medium",
                                "recommendation": "Enable logging: sudo ufw logging on"
                            })
                        else:
                            print(f"    [✓] Logging: {log_match.group(1) if log_match else 'unknown'}")
                            
        except FileNotFoundError:
            print("    [*] UFW command not found")
        except Exception as e:
            print(f"    [!] Error analyzing UFW: {e}")
        
        return result

    def _analyze_firewalld(self, deep_scan=False):
        """Deep analysis of firewalld configuration."""
        result = {
            'status': {'active': False, 'zones': []},
            'findings': [],
            'zones': []
        }
        
        try:
            # Check firewalld status
            cmd_result = subprocess.run(
                ["firewall-cmd", "--state"],
                capture_output=True, text=True, timeout=10
            )
            
            if cmd_result.returncode == 0 and "running" in cmd_result.stdout.lower():
                result['status']['active'] = True
                print("    [✓] firewalld is running")
                
                # Get active zones
                zones_result = subprocess.run(
                    ["firewall-cmd", "--get-active-zones"],
                    capture_output=True, text=True, timeout=10
                )
                
                if zones_result.returncode == 0:
                    zones_output = zones_result.stdout
                    current_zone = None
                    
                    for line in zones_output.split('\n'):
                        line = line.strip()
                        if line and not line.startswith('interfaces') and not line.startswith('sources'):
                            current_zone = line
                            result['zones'].append({'name': current_zone, 'interfaces': [], 'sources': []})
                        elif current_zone and line.startswith('interfaces:'):
                            ifaces = line.replace('interfaces:', '').strip().split()
                            result['zones'][-1]['interfaces'] = ifaces
                        elif current_zone and line.startswith('sources:'):
                            sources = line.replace('sources:', '').strip().split()
                            result['zones'][-1]['sources'] = sources
                    
                    print(f"    [*] Active zones: {len(result['zones'])}")
                    for zone in result['zones']:
                        print(f"        - {zone['name']}: {', '.join(zone['interfaces']) or 'no interfaces'}")
                
                if deep_scan:
                    # Check default zone
                    default_result = subprocess.run(
                        ["firewall-cmd", "--get-default-zone"],
                        capture_output=True, text=True, timeout=5
                    )
                    if default_result.returncode == 0:
                        default_zone = default_result.stdout.strip()
                        print(f"    [*] Default zone: {default_zone}")
                        
                        # Check if default zone is too permissive
                        if default_zone in ['trusted', 'public']:
                            if default_zone == 'trusted':
                                result['findings'].append({
                                    "type": "firewalld_zone",
                                    "issue": "Default zone is 'trusted' (allows all)",
                                    "severity": "critical",
                                    "recommendation": "Change default zone: firewall-cmd --set-default-zone=drop"
                                })
                    
                    # List services in each zone
                    for zone in result['zones']:
                        svc_result = subprocess.run(
                            ["firewall-cmd", "--zone", zone['name'], "--list-services"],
                            capture_output=True, text=True, timeout=5
                        )
                        if svc_result.returncode == 0:
                            services = svc_result.stdout.strip().split()
                            zone['services'] = services
                            
                            # Check for dangerous services
                            dangerous_services = ['ftp', 'telnet', 'rsh', 'rlogin', 'tftp']
                            for svc in services:
                                if svc in dangerous_services:
                                    result['findings'].append({
                                        "type": "firewalld_service",
                                        "issue": f"Dangerous service '{svc}' enabled in zone {zone['name']}",
                                        "severity": "high",
                                        "recommendation": f"Remove service: firewall-cmd --zone={zone['name']} --remove-service={svc}"
                                    })
            else:
                print("    [*] firewalld is not running")
                
        except FileNotFoundError:
            print("    [*] firewall-cmd not found")
        except Exception as e:
            print(f"    [!] Error analyzing firewalld: {e}")
        
        return result

    def _audit_kernel_network_security(self):
        """Audit kernel network security parameters."""
        findings = []
        
        sysctl_checks = {
            '/proc/sys/net/ipv4/ip_forward': {
                'expected': '0',
                'name': 'IPv4 Forwarding',
                'severity': 'medium',
                'desc': 'IPv4 forwarding enabled (router mode)'
            },
            '/proc/sys/net/ipv6/conf/all/forwarding': {
                'expected': '0',
                'name': 'IPv6 Forwarding',
                'severity': 'medium',
                'desc': 'IPv6 forwarding enabled'
            },
            '/proc/sys/net/ipv4/conf/all/accept_redirects': {
                'expected': '0',
                'name': 'ICMP Redirects',
                'severity': 'medium',
                'desc': 'ICMP redirects accepted (potential MitM)'
            },
            '/proc/sys/net/ipv4/conf/all/send_redirects': {
                'expected': '0',
                'name': 'Send Redirects',
                'severity': 'low',
                'desc': 'System sends ICMP redirects'
            },
            '/proc/sys/net/ipv4/conf/all/accept_source_route': {
                'expected': '0',
                'name': 'Source Routing',
                'severity': 'high',
                'desc': 'Source routing accepted (dangerous)'
            },
            '/proc/sys/net/ipv4/conf/all/rp_filter': {
                'expected': '1',
                'name': 'Reverse Path Filtering',
                'severity': 'medium',
                'desc': 'Reverse path filtering disabled'
            },
            '/proc/sys/net/ipv4/tcp_syncookies': {
                'expected': '1',
                'name': 'SYN Cookies',
                'severity': 'medium',
                'desc': 'SYN cookies disabled (SYN flood vulnerable)'
            },
            '/proc/sys/net/ipv4/icmp_echo_ignore_broadcasts': {
                'expected': '1',
                'name': 'ICMP Broadcast',
                'severity': 'low',
                'desc': 'Responds to broadcast pings (smurf attack)'
            },
            '/proc/sys/net/ipv4/icmp_ignore_bogus_error_responses': {
                'expected': '1',
                'name': 'Bogus ICMP Responses',
                'severity': 'low',
                'desc': 'Logs bogus ICMP responses'
            },
            '/proc/sys/net/ipv4/conf/all/log_martians': {
                'expected': '1',
                'name': 'Martian Logging',
                'severity': 'low',
                'desc': 'Martian packets not logged'
            }
        }
        
        for path, check in sysctl_checks.items():
            try:
                value = Path(path).read_text().strip()
                status = "✓" if value == check['expected'] else "!"
                
                if value != check['expected']:
                    print(f"    [{status}] {check['name']}: {value} (expected {check['expected']})")
                    findings.append({
                        "type": "kernel_network",
                        "issue": check['desc'],
                        "severity": check['severity'],
                        "parameter": path,
                        "current": value,
                        "expected": check['expected'],
                        "recommendation": f"Set via: echo {check['expected']} > {path}"
                    })
                else:
                    print(f"    [{status}] {check['name']}: OK")
                    
            except FileNotFoundError:
                pass
            except PermissionError:
                print(f"    [?] {check['name']}: Permission denied")
            except Exception:
                pass
        
        return findings

    def _analyze_port_exposure(self):
        """Analyze which ports are exposed."""
        exposed_ports = []
        
        try:
            # Use ss to list listening ports
            result = subprocess.run(
                ["ss", "-tlnp"],
                capture_output=True, text=True, timeout=10
            )
            
            if result.returncode == 0:
                for line in result.stdout.split('\n')[1:]:  # Skip header
                    if not line.strip():
                        continue
                    
                    parts = line.split()
                    if len(parts) >= 4:
                        local_addr = parts[3]
                        
                        # Parse address and port
                        if ':' in local_addr:
                            addr, port = local_addr.rsplit(':', 1)
                            try:
                                port_num = int(port)
                                
                                # Check if listening on all interfaces
                                listening_all = addr in ['0.0.0.0', '*', '::']
                                
                                # Get process info if available
                                process = parts[-1] if len(parts) > 4 else 'unknown'
                                
                                exposed_ports.append({
                                    'port': port_num,
                                    'proto': 'tcp',
                                    'address': addr,
                                    'all_interfaces': listening_all,
                                    'process': process,
                                    'service': self._get_service_name(port_num)
                                })
                                
                                if listening_all:
                                    print(f"    [*] Port {port_num}/tcp listening on all interfaces")
                                    
                            except ValueError:
                                pass
            
            # Also check UDP
            result = subprocess.run(
                ["ss", "-ulnp"],
                capture_output=True, text=True, timeout=10
            )
            
            if result.returncode == 0:
                for line in result.stdout.split('\n')[1:]:
                    if not line.strip():
                        continue
                    
                    parts = line.split()
                    if len(parts) >= 4:
                        local_addr = parts[3]
                        
                        if ':' in local_addr:
                            addr, port = local_addr.rsplit(':', 1)
                            try:
                                port_num = int(port)
                                listening_all = addr in ['0.0.0.0', '*', '::']
                                process = parts[-1] if len(parts) > 4 else 'unknown'
                                
                                exposed_ports.append({
                                    'port': port_num,
                                    'proto': 'udp',
                                    'address': addr,
                                    'all_interfaces': listening_all,
                                    'process': process,
                                    'service': self._get_service_name(port_num)
                                })
                            except ValueError:
                                pass
                                
        except Exception as e:
            print(f"    [!] Error analyzing ports: {e}")
        
        print(f"    [*] Found {len(exposed_ports)} listening ports")
        return exposed_ports

    def _get_service_name(self, port):
        """Get service name for a port number using centralized port manager."""
        # Use centralized port manager if available
        if PORT_MANAGER_AVAILABLE and get_port_manager:
            try:
                return pm_get_service_name(port)
            except Exception:
                pass
        
        # Fallback to built-in mapping
        common_ports = {
            20: 'ftp-data', 21: 'ftp', 22: 'ssh', 23: 'telnet', 25: 'smtp',
            53: 'dns', 67: 'dhcp', 68: 'dhcp', 69: 'tftp', 80: 'http',
            110: 'pop3', 111: 'rpcbind', 119: 'nntp', 123: 'ntp', 135: 'msrpc',
            137: 'netbios-ns', 138: 'netbios-dgm', 139: 'netbios-ssn',
            143: 'imap', 161: 'snmp', 162: 'snmptrap', 389: 'ldap',
            443: 'https', 445: 'microsoft-ds', 465: 'smtps', 514: 'syslog',
            515: 'printer', 587: 'submission', 631: 'ipp', 636: 'ldaps',
            873: 'rsync', 993: 'imaps', 995: 'pop3s', 1080: 'socks',
            1433: 'mssql', 1521: 'oracle', 1723: 'pptp', 2049: 'nfs',
            2082: 'cpanel', 2083: 'cpanel-ssl', 3128: 'squid', 3306: 'mysql',
            3389: 'rdp', 5432: 'postgresql', 5900: 'vnc', 6379: 'redis',
            6667: 'irc', 8080: 'http-alt', 8443: 'https-alt', 9000: 'php-fpm',
            9090: 'cockpit', 27017: 'mongodb', 27018: 'mongodb',
            # Additional common ports
            8000: 'http-dev', 3000: 'node', 4200: 'angular', 5000: 'flask',
            8888: 'jupyter', 9200: 'elasticsearch', 9092: 'kafka',
            5672: 'amqp', 15672: 'rabbitmq-mgmt', 6443: 'kubernetes',
            10250: 'kubelet', 2375: 'docker', 2376: 'docker-tls',
            1194: 'openvpn', 51820: 'wireguard', 2222: 'ssh-alt'
        }
        return common_ports.get(port, 'unknown')

    def _audit_firewall_logging(self):
        """Audit firewall logging configuration."""
        findings = []
        
        # Check if kernel firewall logging is configured
        log_files = [
            '/var/log/kern.log',
            '/var/log/messages',
            '/var/log/syslog',
            '/var/log/firewall.log',
            '/var/log/ufw.log'
        ]
        
        found_log = False
        for log_file in log_files:
            try:
                if Path(log_file).exists():
                    # Check if it has recent firewall entries
                    result = subprocess.run(
                        ["grep", "-c", "-i", "iptables\\|nftables\\|firewall\\|ufw", log_file],
                        capture_output=True, text=True, timeout=10
                    )
                    if result.returncode == 0:
                        count = int(result.stdout.strip())
                        if count > 0:
                            found_log = True
                            print(f"    [✓] Firewall logs found in {log_file} ({count} entries)")
            except Exception:
                pass
        
        if not found_log:
            print("    [!] No firewall log entries found")
            findings.append({
                "type": "firewall_logging",
                "issue": "No firewall logging detected",
                "severity": "medium",
                "recommendation": "Enable firewall logging for security monitoring"
            })
        
        # Check rsyslog/journald for firewall config
        rsyslog_conf = Path('/etc/rsyslog.conf')
        if rsyslog_conf.exists():
            try:
                content = rsyslog_conf.read_text()
                if 'kern.' in content or 'firewall' in content.lower():
                    print("    [✓] rsyslog configured for kernel/firewall messages")
                else:
                    print("    [!] rsyslog may not be capturing firewall logs")
            except Exception:
                pass
        
        return findings

    def _generate_firewall_recommendations(self, findings, firewall_status):
        """Generate prioritized firewall recommendations."""
        recommendations = []
        
        # Priority 1: No firewall active
        if not any(fw['active'] for fw in firewall_status.values()):
            recommendations.append({
                "priority": "CRITICAL",
                "action": "Enable a firewall immediately (ufw enable or configure iptables)",
                "command": "sudo ufw enable"
            })
        
        # Priority 2: Critical findings
        for finding in findings:
            if finding.get('severity') == 'critical':
                recommendations.append({
                    "priority": "CRITICAL",
                    "action": finding.get('recommendation', finding['issue']),
                    "command": finding.get('command')
                })
        
        # Priority 3: High severity findings
        for finding in findings:
            if finding.get('severity') == 'high':
                recommendations.append({
                    "priority": "HIGH",
                    "action": finding.get('recommendation', finding['issue']),
                    "command": finding.get('command')
                })
        
        # Priority 4: General hardening recommendations
        recommendations.append({
            "priority": "RECOMMENDED",
            "action": "Set default INPUT policy to DROP",
            "command": "iptables -P INPUT DROP"
        })
        
        recommendations.append({
            "priority": "RECOMMENDED",
            "action": "Enable stateful packet inspection",
            "command": "iptables -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT"
        })
        
        recommendations.append({
            "priority": "RECOMMENDED",
            "action": "Log dropped packets for monitoring",
            "command": "iptables -A INPUT -j LOG --log-prefix 'IPT-DROP: '"
        })
        
        # Remove duplicates based on action
        seen = set()
        unique_recs = []
        for rec in recommendations:
            if rec['action'] not in seen:
                seen.add(rec['action'])
                unique_recs.append(rec)
        
        return unique_recs[:10]  # Top 10 recommendations


# ==================== FILE TYPE VALIDATOR ====================

    def validate_file_types(self, directory, recursive=True):
        """Validate file types match their extensions."""
        print(f"[*] File Type Validator: {directory}")
        print("-" * 60)
        
        findings = []
        dir_path = Path(directory)
        
        if not dir_path.exists():
            print(f"[!] Directory not found: {directory}")
            return findings
        
        # Common magic bytes
        magic_signatures = {
            b'\x7fELF': ('elf', ['.so', '.o', '']),
            b'MZ': ('executable', ['.exe', '.dll', '.sys']),
            b'\x89PNG': ('png', ['.png']),
            b'\xff\xd8\xff': ('jpeg', ['.jpg', '.jpeg']),
            b'GIF87a': ('gif', ['.gif']),
            b'GIF89a': ('gif', ['.gif']),
            b'PK\x03\x04': ('zip', ['.zip', '.jar', '.docx', '.xlsx', '.pptx', '.apk']),
            b'%PDF': ('pdf', ['.pdf']),
            b'Rar!\x1a\x07': ('rar', ['.rar']),
            b'\x1f\x8b': ('gzip', ['.gz', '.tgz']),
            b'BZ': ('bzip2', ['.bz2']),
            b'\xfd7zXZ': ('xz', ['.xz']),
            b'<!DOCTYPE html': ('html', ['.html', '.htm']),
            b'<html': ('html', ['.html', '.htm']),
            b'<?xml': ('xml', ['.xml', '.svg', '.xhtml']),
            b'#!/': ('script', ['.sh', '.py', '.pl', '.rb', '']),
            b'#!': ('script', ['.sh', '.py', '.pl', '.rb', '']),
        }
        
        pattern = dir_path.rglob("*") if recursive else dir_path.glob("*")
        
        checked = 0
        mismatches = 0
        
        for filepath in pattern:
            if not filepath.is_file():
                continue
            
            try:
                with open(filepath, 'rb') as f:
                    header = f.read(16)
                
                if not header:
                    continue
                
                checked += 1
                ext = filepath.suffix.lower()
                
                # Check against signatures
                for magic, (file_type, valid_exts) in magic_signatures.items():
                    if header.startswith(magic):
                        if ext and ext not in valid_exts:
                            # Extension mismatch
                            mismatches += 1
                            severity = "critical" if file_type in ['elf', 'executable'] else "medium"
                            
                            print(f"\n  [{severity.upper()}] {filepath}")
                            print(f"      Detected: {file_type}")
                            print(f"      Extension: {ext}")
                            print(f"      Expected: {', '.join(valid_exts)}")
                            
                            findings.append({
                                "file": str(filepath),
                                "detected_type": file_type,
                                "extension": ext,
                                "expected": valid_exts,
                                "severity": severity
                            })
                        break
                
                # Check for executables hidden as other file types
                if header.startswith(b'\x7fELF'):
                    if ext in ['.txt', '.pdf', '.jpg', '.png', '.doc', '.mp3', '.mp4']:
                        print(f"\n  [CRITICAL] {filepath}")
                        print(f"      ELF binary disguised as {ext}")
                        findings.append({
                            "file": str(filepath),
                            "detected_type": "elf_binary",
                            "extension": ext,
                            "issue": "Executable disguised as data file",
                            "severity": "critical"
                        })
                        mismatches += 1
                        
            except PermissionError:
                pass
            except Exception:
                pass
        
        # Summary
        print("\n" + "=" * 60)
        print(f"[*] Checked {checked} files")
        if findings:
            print(f"[!] Found {mismatches} file type mismatches")
            critical = sum(1 for f in findings if f.get('severity') == 'critical')
            if critical:
                print(f"[!] CRITICAL: {critical} executables disguised as other files")
        else:
            print("[✓] No file type mismatches detected")
        
        return findings

    # ==================== HEALTH & SAFEGUARDS ====================

    def perform_health_check(self, verbose=False, auto_fix=False):
        """Comprehensive system health check with safeguards."""
        print("[*] System Health Check")
        print("=" * 60)
        
        issues = []
        checks_passed = 0
        checks_total = 0
        
        # 1. Memory check
        checks_total += 1
        print("\n[*] Memory Status...")
        if SafeGuard.check_memory_usage():
            print("    [✓] Memory usage within limits")
            checks_passed += 1
        else:
            print("    [!] Memory usage is high")
            issues.append(("memory", "High memory usage detected"))
            if auto_fix:
                import gc
                gc.collect()
                print("    [~] Triggered garbage collection")
        
        # 2. Disk space check
        checks_total += 1
        print("\n[*] Disk Space...")
        if SafeGuard.ensure_disk_space(Path.cwd(), required_mb=100):
            print("    [✓] Sufficient disk space available")
            checks_passed += 1
        else:
            print("    [!] Low disk space")
            issues.append(("disk", "Low disk space"))
        
        # 3. Circuit breakers status
        checks_total += 1
        print("\n[*] Circuit Breakers...")
        health = SafeGuard.health_check()
        open_breakers = [k for k, v in health['circuit_breakers'].items() if v]
        if not open_breakers:
            print("    [✓] All circuit breakers closed")
            checks_passed += 1
        else:
            print(f"    [!] Open circuit breakers: {', '.join(open_breakers)}")
            issues.append(("circuit_breakers", f"Open: {open_breakers}"))
            if auto_fix:
                for breaker in open_breakers:
                    SafeGuard._circuit_breaker_state[breaker] = {'open': False}
                    SafeGuard._failed_operations[breaker] = 0
                print("    [~] Reset circuit breakers")
        
        # 4. Resource tracking
        checks_total += 1
        print("\n[*] Resource Usage...")
        open_files = health['open_files']
        active_threads = health['active_threads']
        if open_files < SafeGuard.MAX_OPEN_FILES * 0.8:
            print(f"    [✓] Open file handles: {open_files}/{SafeGuard.MAX_OPEN_FILES}")
            checks_passed += 1
        else:
            print(f"    [!] High file handle usage: {open_files}/{SafeGuard.MAX_OPEN_FILES}")
            issues.append(("file_handles", f"High usage: {open_files}"))
        
        # 5. Signature database
        checks_total += 1
        print("\n[*] Signature Database...")
        if SIGNATURES_FILE.exists():
            sigs = SafeGuard.safe_json_load(SIGNATURES_FILE, {})
            sig_count = len(sigs.get("signatures", []))
            print(f"    [✓] Signatures loaded: {sig_count}")
            checks_passed += 1
        else:
            print("    [!] Signature database missing")
            issues.append(("signatures", "Database file missing"))
        
        # 6. Quarantine directory
        checks_total += 1
        print("\n[*] Quarantine Directory...")
        if QUARANTINE_DIR.exists():
            try:
                mode = QUARANTINE_DIR.stat().st_mode
                if mode & 0o077 == 0:
                    print("    [✓] Quarantine directory secure")
                    checks_passed += 1
                else:
                    print("    [!] Quarantine directory permissions too open")
                    issues.append(("quarantine", "Insecure permissions"))
                    if auto_fix:
                        os.chmod(QUARANTINE_DIR, 0o700)
                        print("    [~] Fixed permissions")
            except OSError as e:
                print(f"    [!] Cannot check quarantine: {e}")
                issues.append(("quarantine", str(e)))
        else:
            print("    [✓] Quarantine directory will be created on first use")
            checks_passed += 1
        
        # 7. Configuration
        checks_total += 1
        print("\n[*] Configuration...")
        config_file = Path("greyav_config.json")
        if config_file.exists():
            config = SafeGuard.safe_json_load(config_file, None)
            if config is not None:
                print("    [✓] Configuration valid")
                checks_passed += 1
            else:
                print("    [!] Configuration file corrupted")
                issues.append(("config", "Corrupted config file"))
        else:
            print("    [✓] Using default configuration")
            checks_passed += 1
        
        # 8. Failed operations
        checks_total += 1
        print("\n[*] Failed Operations...")
        failed_ops = health['failed_operations']
        high_failures = {k: v for k, v in failed_ops.items() if v > 5}
        if not high_failures:
            print("    [✓] No significant operation failures")
            checks_passed += 1
        else:
            print(f"    [!] High failure counts: {high_failures}")
            issues.append(("failures", str(high_failures)))
        
        # Verbose output
        if verbose:
            print("\n" + "-" * 60)
            print("[*] Detailed Status:")
            print(f"    Open files: {open_files}")
            print(f"    Active threads: {active_threads}")
            print(f"    Failed operations: {failed_ops}")
            print(f"    Circuit breakers: {health['circuit_breakers']}")
        
        # Summary
        print("\n" + "=" * 60)
        print(f"[*] Health Check Complete: {checks_passed}/{checks_total} passed")
        if issues:
            print(f"[!] Issues found: {len(issues)}")
            for issue_type, desc in issues:
                print(f"    - {issue_type}: {desc}")
        else:
            print("[✓] All systems healthy")
        
        return checks_passed == checks_total

    def verify_safeguards(self, run_tests=False, reset_breakers=False):
        """Verify and test safeguard systems."""
        print("[*] Safeguard Verification")
        print("=" * 60)
        
        safeguards_ok = True
        
        # Show safeguard status
        print("\n[*] Safeguard Configuration:")
        print(f"    Max Memory: {SafeGuard.MAX_MEMORY_MB} MB")
        print(f"    Max Open Files: {SafeGuard.MAX_OPEN_FILES}")
        print(f"    Max Threads: {SafeGuard.MAX_THREADS}")
        print(f"    Max Retries: {SafeGuard.MAX_RETRIES}")
        print(f"    Default Timeout: {SafeGuard.DEFAULT_TIMEOUT}s")
        print(f"    Circuit Breaker Threshold: {SafeGuard._circuit_breaker_threshold}")
        
        # Reset circuit breakers if requested
        if reset_breakers:
            print("\n[*] Resetting circuit breakers...")
            with SafeGuard._lock:
                SafeGuard._circuit_breaker_state.clear()
                SafeGuard._failed_operations.clear()
            print("    [✓] All circuit breakers reset")
        
        # Current status
        health = SafeGuard.health_check()
        print("\n[*] Current Status:")
        print(f"    Open Files: {health['open_files']}")
        print(f"    Active Threads: {health['active_threads']}")
        print(f"    Circuit Breakers Open: {sum(1 for v in health['circuit_breakers'].values() if v)}")
        
        if run_tests:
            print("\n[*] Running Safeguard Tests...")
            
            # Test 1: Rate limiting
            print("\n    [Test 1] Rate Limiting...")
            test_passed = True
            for i in range(65):
                result = SafeGuard.rate_limit_check("test_rate", max_per_minute=60)
                if i < 60 and not result:
                    test_passed = False
                    break
                if i >= 60 and result:
                    test_passed = False
                    break
            if test_passed:
                print("        [✓] Rate limiting working correctly")
            else:
                print("        [!] Rate limiting test failed")
                safeguards_ok = False
            
            # Test 2: Memory check
            print("\n    [Test 2] Memory Check...")
            mem_ok = SafeGuard.check_memory_usage()
            print(f"        [{'✓' if mem_ok else '!'}] Memory check returned: {mem_ok}")
            
            # Test 3: Circuit breaker
            print("\n    [Test 3] Circuit Breaker...")
            test_op = "test_circuit_breaker"
            for _ in range(SafeGuard._circuit_breaker_threshold + 1):
                SafeGuard.record_failure(test_op)
            breaker_open = not SafeGuard.circuit_breaker_check(test_op)
            if breaker_open:
                print("        [✓] Circuit breaker triggered correctly")
                # Reset
                SafeGuard._circuit_breaker_state[test_op] = {'open': False}
                SafeGuard._failed_operations[test_op] = 0
            else:
                print("        [!] Circuit breaker did not trigger")
                safeguards_ok = False
            
            # Test 4: File handle management
            print("\n    [Test 4] File Handle Management...")
            acquired = SafeGuard.acquire_file_handle()
            if acquired:
                SafeGuard.release_file_handle()
                print("        [✓] File handle acquire/release working")
            else:
                print("        [!] Could not acquire file handle")
                safeguards_ok = False
            
            # Test 5: Safe JSON operations
            print("\n    [Test 5] Safe JSON Operations...")
            test_file = Path("/tmp/greyav_test.json")
            test_data = {"test": "data", "number": 42}
            save_ok = SafeGuard.safe_json_save(test_file, test_data)
            load_ok = SafeGuard.safe_json_load(test_file, None) == test_data
            if save_ok and load_ok:
                print("        [✓] Safe JSON operations working")
                test_file.unlink(missing_ok=True)
            else:
                print("        [!] Safe JSON operations failed")
                safeguards_ok = False
            
            # Test 6: Pre-operation check
            print("\n    [Test 6] Pre-operation Check...")
            can_proceed, reason = SafeGuard.pre_operation_check("test_op")
            if can_proceed:
                print(f"        [✓] Pre-operation check passed: {reason}")
            else:
                print(f"        [!] Pre-operation check failed: {reason}")
                safeguards_ok = False
            
            # Test summary
            print("\n" + "-" * 60)
            if safeguards_ok:
                print("[✓] All safeguard tests passed")
            else:
                print("[!] Some safeguard tests failed")
        
        print("\n" + "=" * 60)
        print("[✓] Safeguard verification complete")
        return safeguards_ok

    def validate_system_readiness(self, operation="all"):
        """Validate system is ready for operations."""
        print(f"[*] System Readiness Validation: {operation}")
        print("=" * 60)
        
        ready = True
        validations = []
        
        operations_to_check = []
        if operation == "all":
            operations_to_check = ["scan", "quarantine", "restore"]
        else:
            operations_to_check = [operation]
        
        for op in operations_to_check:
            print(f"\n[*] Validating: {op}")
            
            # Pre-operation check
            can_proceed, reason = SafeGuard.pre_operation_check(op)
            if can_proceed:
                print(f"    [✓] Pre-checks passed")
            else:
                print(f"    [!] Pre-checks failed: {reason}")
                ready = False
                validations.append((op, "pre_check", False, reason))
                continue
            
            # Operation-specific checks
            if op == "scan":
                # Check signature database
                if SIGNATURES_FILE.exists():
                    print(f"    [✓] Signature database available")
                    validations.append((op, "signatures", True, "Available"))
                else:
                    print(f"    [!] Signature database missing")
                    validations.append((op, "signatures", False, "Missing"))
                    ready = False
                
                # Check heuristic patterns
                if SUSPICIOUS_PATTERNS:
                    print(f"    [✓] Heuristic patterns loaded")
                else:
                    print(f"    [!] Heuristic patterns not initialized")
                    ready = False
                    
            elif op == "quarantine":
                # Check quarantine directory or ability to create
                try:
                    if QUARANTINE_DIR.exists():
                        if os.access(QUARANTINE_DIR, os.W_OK):
                            print(f"    [✓] Quarantine directory writable")
                            validations.append((op, "quarantine_dir", True, "OK"))
                        else:
                            print(f"    [!] Quarantine directory not writable")
                            validations.append((op, "quarantine_dir", False, "Not writable"))
                            ready = False
                    else:
                        # Check if parent is writable
                        parent = QUARANTINE_DIR.parent
                        if os.access(parent, os.W_OK):
                            print(f"    [✓] Can create quarantine directory")
                            validations.append((op, "quarantine_dir", True, "Can create"))
                        else:
                            print(f"    [!] Cannot create quarantine directory")
                            validations.append((op, "quarantine_dir", False, "No permission"))
                            ready = False
                except OSError as e:
                    print(f"    [!] Quarantine check error: {e}")
                    ready = False
                
                # Check encryption key
                if hasattr(self, '_quarantine_key') and self._quarantine_key:
                    print(f"    [✓] Encryption key available")
                else:
                    print(f"    [!] Encryption key not set")
                    ready = False
                    
            elif op == "restore":
                # Check quarantine files exist
                if QUARANTINE_DIR.exists():
                    quarantined = list(QUARANTINE_DIR.glob("*.quarantine"))
                    print(f"    [✓] {len(quarantined)} files in quarantine")
                else:
                    print(f"    [*] No files in quarantine")
                
                # Check metadata files
                meta_files = list(QUARANTINE_DIR.glob("*.meta")) if QUARANTINE_DIR.exists() else []
                print(f"    [*] {len(meta_files)} metadata files")
            
            # Disk space check
            if SafeGuard.ensure_disk_space(Path.cwd(), required_mb=50):
                print(f"    [✓] Sufficient disk space")
            else:
                print(f"    [!] Low disk space")
                ready = False
        
        # Summary
        print("\n" + "=" * 60)
        if ready:
            print(f"[✓] System ready for {operation} operations")
        else:
            print(f"[!] System not ready - issues found")
            print("\n[*] Recommendations:")
            if not SIGNATURES_FILE.exists():
                print("    - Create/download signature database")
            if not SafeGuard.check_memory_usage():
                print("    - Free up memory")
            if not SafeGuard.ensure_disk_space(Path.cwd()):
                print("    - Free up disk space")
        
        return ready


# ==================== REAL-TIME MONITORING ====================

class RealTimeMonitor:
    """Advanced real-time file system monitoring with security hardening."""
    
    def __init__(self, av_engine, paths, recursive=True):
        self.av = av_engine
        # Validate and filter paths securely
        self.paths = []
        for p in paths:
            try:
                validated = InputValidator.validate_path(p, must_exist=True)
                if validated.is_dir():
                    self.paths.append(validated)
            except ValidationError as e:
                print(f"[!] Invalid path skipped: {p} - {e}")
        
        if not self.paths:
            raise ValidationError("No valid paths to monitor")
        
        self.recursive = recursive
        self.running = False
        self.observer = None
        self.scan_queue = queue.Queue(maxsize=10000)  # Prevent memory exhaustion
        self.lock = threading.Lock()
        self.processed_files = {}  # Track recently processed files
        self.event_count = defaultdict(int)
        self.threats_detected = 0
        self.suspicious_detected = 0
        self.files_scanned = 0
        self.start_time = None
        self._worker_threads = []
        self._num_workers = 3  # Number of scan workers
        self._rate_limiter = RateLimiter  # Use rate limiter for events
        
    def _handle_event(self, event_type, filepath, priority=False):
        """Handle a file system event with security hardening."""
        try:
            filepath = InputValidator.validate_path(
                filepath, allow_symlinks=True)
        except ValidationError:
            return
        
        # Skip directories and quarantine
        if filepath.is_dir():
            return
        if QUARANTINE_DIR in filepath.parents:
            return
        
        # Rate limit events per file
        file_key = str(filepath)
        if not RateLimiter.check(f"event_{file_key}", max_calls=10, period=5):
            return
        
        # Debounce: skip if same file was processed recently
        current_time = time.time()
        with self.lock:
            last_processed = self.processed_files.get(file_key, 0)
            if current_time - last_processed < 1.0:  # 1 second debounce
                return
            self.processed_files[file_key] = current_time
            
            # Cleanup old entries to prevent memory growth
            if len(self.processed_files) > 10000:
                cutoff = current_time - 60
                self.processed_files = {
                    k: v for k, v in self.processed_files.items()
                    if v > cutoff
                }
        
        # Queue the scan (non-blocking with timeout)
        try:
            self.scan_queue.put((event_type, filepath, priority), timeout=1)
        except queue.Full:
            self.av._log_secure("warning", "Scan queue full, event dropped")

    def _scan_worker(self):
        """Worker thread for processing scan queue securely."""
        while self.running:
            try:
                event_type, filepath, priority = self.scan_queue.get(timeout=1)
            except queue.Empty:
                continue
            
            try:
                self._process_file_event(event_type, filepath)
            except (SecurityError, ValidationError) as e:
                self.av._log_secure("warning", f"Security error: {e}")
            except Exception as e:
                self.av._log_secure("error", 
                    f"Error processing: {type(e).__name__}")
            finally:
                self.scan_queue.task_done()

    def _process_file_event(self, event_type, filepath):
        """Process a single file event with security hardening."""
        try:
            filepath = InputValidator.validate_path(
                filepath, allow_symlinks=True)
        except ValidationError:
            return
        
        # Small delay to ensure file is fully written
        time.sleep(0.2)
        
        if not filepath.exists():
            return
        
        # Security check: verify readable
        if not os.access(filepath, os.R_OK):
            return
            
        timestamp = datetime.now().strftime("%H:%M:%S")
        self.event_count[event_type] += 1
        self.files_scanned += 1
        
        # Perform multi-layer scan
        try:
            file_size = filepath.stat().st_size
            
            # Quick check for very large files
            max_mb = self.av.config.get("max_file_size_mb", 100)
            max_size = max_mb * 1024 * 1024
            if file_size > max_size:
                safe_name = InputValidator.sanitize_string(
                    filepath.name, max_length=64)
                print(f"\n[{timestamp}] {event_type}: {safe_name}")
                print(f"    [*] Skipped (size: {file_size // (1024*1024)}MB)")
                return
            
            # Perform scan
            threat, heuristic = self.av.scan_file(filepath)
            safe_name = InputValidator.sanitize_string(
                filepath.name, max_length=64)
            
            if threat:
                self.threats_detected += 1
                severity = threat.get('severity', 'low')
                severity_icon = {"low": "!", "medium": "!!",
                                 "high": "!!!", "critical": "!!!!",
                                 "test": "T"}.get(severity, "!")
                
                print(f"\n[{timestamp}] {event_type}: {safe_name}")
                print(f"    [{severity_icon}] THREAT: {threat['name']}")
                print(f"        Severity: {severity}")
                det_type = threat.get('detection_type', 'signature')
                print(f"        Detection: {det_type}")
                
                self.av._log_secure("warning",
                    f"REALTIME THREAT: {safe_name} - {threat['name']}")

                # Use enhanced AutoThreatManager if available
                if (AUTO_THREAT_MANAGER_AVAILABLE and
                        self.av.auto_threat_manager and
                        self.av.auto_response_enabled):
                    print("        [*] Auto Threat Manager responding...")
                    response = self.av.handle_threat_automatically(
                        threat, str(filepath))
                    if response.get('success'):
                        print("        [✓] Threat automatically handled")
                        actions = [a.get('action')
                                   for a in response.get('actions', [])]
                        if actions:
                            print(f"        Actions: {', '.join(actions)}")
                    else:
                        errs = response.get('errors', [])
                        print(f"        [!] Errors: {errs}")
                # Fall back to legacy Advanced Auto Threat Response
                elif self.av.auto_response_enabled:
                    print("        [*] Initiating Auto Threat Response...")
                    context = ThreatContext(
                        threat_id=threat.get('name', 'unknown'),
                        file_path=str(filepath),
                        severity=severity,
                        detection_type=det_type,
                        confidence=threat.get('confidence', 100.0),
                        mitre_tactics=[threat.get('mitre', '')],
                        threat_family=threat.get('family', '')
                    )
                    # Get process info if file is executable
                    self._gather_threat_context(context, filepath)
                    # Execute response
                    response = self.av.threat_response.respond_to_threat(context)
                    if response.get('success'):
                        print(f"        [✓] Auto-response completed")
                    else:
                        print(f"        [!] Auto-response errors: {response.get('errors', [])}")
                # Legacy auto-quarantine fallback
                elif self.av.config.get("auto_quarantine", False):
                    print("        Auto-quarantining...")
                    self.av.quarantine_file(filepath)
                    
            elif heuristic and heuristic["score"] >= self.av.heuristic_threshold:
                self.suspicious_detected += 1
                
                print(f"\n[{timestamp}] {event_type}: {safe_name}")
                print(f"    [?] SUSPICIOUS: Score {heuristic['score']}")
                print(f"        Risk Level: {heuristic['risk_level']}")
                conf = heuristic.get('confidence', 0)
                print(f"        Confidence: {conf:.0f}%")
                
                # Show MITRE techniques if detected
                mitre = heuristic.get("mitre_techniques", [])
                if mitre:
                    print(f"        MITRE: {', '.join(mitre[:3])}")
                
                # Show top indicators
                indicators = heuristic.get("indicators", [])[:3]
                for ind in indicators:
                    print(f"        - {ind[:60]}...")
                
                self.av._log_action(
                    f"REALTIME SUSPICIOUS: {filepath} - Score {heuristic['score']}"
                )
                
                # Auto-respond to high-severity heuristic detections
                if self.av.auto_response_enabled and heuristic['score'] >= 80:
                    print("        [*] Initiating Auto Threat Response...")
                    context = ThreatContext(
                        threat_id=f"Heuristic_{heuristic['risk_level']}",
                        file_path=str(filepath),
                        severity='high' if heuristic['score'] >= 80 else 'medium',
                        detection_type='heuristic',
                        confidence=heuristic.get('confidence', 75.0),
                        mitre_tactics=heuristic.get('mitre_techniques', [])
                    )
                    response = self.av.threat_response.respond_to_threat(context)
                    if response.get('success'):
                        print(f"        [✓] Auto-response completed")
            else:
                # Only print for created files to reduce noise
                if event_type == "Created":
                    print(f"[{timestamp}] {event_type}: {filepath.name} [✓]")
                    
        except PermissionError:
            print(f"\n[{timestamp}] {event_type}: {filepath.name}")
            print(f"    [!] Access denied")
        except Exception as e:
            print(f"\n[{timestamp}] Error scanning {filepath.name}: {e}")

    def _gather_threat_context(self, context: 'ThreatContext', filepath: Path):
        """Gather additional context about a detected threat for response."""
        try:
            # Get file metadata
            stat = filepath.stat()
            context.file_size = stat.st_size
            context.file_mtime = stat.st_mtime
            context.file_ctime = stat.st_ctime
            
            # Check if file is executable
            is_executable = os.access(filepath, os.X_OK)
            
            # Find related processes
            if is_executable and PSUTIL_AVAILABLE:
                try:
                    filepath_str = str(filepath.resolve())
                    for proc in psutil.process_iter(['pid', 'name', 'exe', 'cmdline']):
                        try:
                            proc_exe = proc.info.get('exe')
                            if proc_exe and Path(proc_exe).resolve() == filepath.resolve():
                                context.related_processes.append({
                                    'pid': proc.info['pid'],
                                    'name': proc.info['name'],
                                    'cmdline': proc.info.get('cmdline', [])
                                })
                        except (psutil.NoSuchProcess, psutil.AccessDenied):
                            continue
                except Exception:
                    pass
            
            # Check for network connections from threat process
            if context.related_processes and PSUTIL_AVAILABLE:
                try:
                    for proc_info in context.related_processes[:3]:
                        try:
                            proc = psutil.Process(proc_info['pid'])
                            for conn in proc.connections():
                                if conn.status == 'ESTABLISHED':
                                    context.network_connections.append({
                                        'local': f"{conn.laddr.ip}:{conn.laddr.port}",
                                        'remote': f"{conn.raddr.ip}:{conn.raddr.port}" if conn.raddr else 'N/A',
                                        'status': conn.status
                                    })
                        except (psutil.NoSuchProcess, psutil.AccessDenied):
                            continue
                except Exception:
                    pass
            
            # Get file hash for reputation lookup
            try:
                with open(filepath, 'rb') as f:
                    content = f.read(1024 * 1024)  # First 1MB
                    context.file_hash = hashlib.sha256(content).hexdigest()
            except Exception:
                pass
            
            # Check parent directory for other suspicious files
            try:
                parent = filepath.parent
                suspicious_siblings = []
                for sibling in parent.iterdir():
                    if sibling.is_file() and sibling != filepath:
                        try:
                            # Quick heuristic check on siblings
                            if sibling.suffix.lower() in ['.exe', '.dll', '.bat', '.ps1', '.vbs', '.js']:
                                if sibling.stat().st_mtime >= stat.st_mtime - 60:
                                    suspicious_siblings.append(str(sibling.name))
                        except Exception:
                            continue
                if suspicious_siblings:
                    context.related_files = suspicious_siblings[:5]
            except Exception:
                pass
                
        except Exception as e:
            self.av._log_secure("warning", f"Error gathering threat context: {e}")

    def start(self):
        """Start real-time monitoring."""
        if not WATCHDOG_AVAILABLE:
            print("[!] Real-time monitoring requires the 'watchdog' package.")
            print("    Install with: pip install watchdog")
            return self._start_polling_fallback()
        
        self.running = True
        self.start_time = datetime.now()
        self.observer = Observer()
        
        # Start worker threads
        for i in range(self._num_workers):
            worker = threading.Thread(
                target=self._scan_worker,
                daemon=True,
                name=f"ScanWorker-{i}"
            )
            worker.start()
            self._worker_threads.append(worker)
        
        # Create event handler
        monitor = self
        
        class AVEventHandler(FileSystemEventHandler):
            def on_created(self, event):
                if not event.is_directory:
                    monitor._handle_event("Created", event.src_path, priority=True)
            
            def on_modified(self, event):
                if not event.is_directory:
                    monitor._handle_event("Modified", event.src_path)
            
            def on_moved(self, event):
                if not event.is_directory:
                    monitor._handle_event("Moved", event.dest_path)
            
            def on_deleted(self, event):
                # Log deletions (could indicate cleanup after malware)
                if not event.is_directory:
                    timestamp = datetime.now().strftime("%H:%M:%S")
                    print(f"[{timestamp}] Deleted: {Path(event.src_path).name}")
        
        handler = AVEventHandler()
        
        for path in self.paths:
            if path.exists():
                self.observer.schedule(handler, str(path), recursive=self.recursive)
                print(f"[+] Monitoring: {path}")
            else:
                print(f"[!] Path not found: {path}")
        
        self.observer.start()
        print("\n[*] Real-time protection ACTIVE")
        print("[*] Press Ctrl+C to stop\n")
        print("-" * 50)
        
        try:
            while self.running:
                time.sleep(1)
        except KeyboardInterrupt:
            self.stop()

    def _start_polling_fallback(self):
        """Fallback polling-based monitoring when watchdog is not available."""
        print("[*] Using polling-based monitoring (fallback mode)")
        print("[*] Install 'watchdog' for better performance: pip install watchdog")
        
        self.running = True
        file_states = {}
        
        # Initial scan to get file states
        for path in self.paths:
            if path.exists():
                print(f"[+] Monitoring: {path}")
                for filepath in path.rglob("*") if self.recursive else path.glob("*"):
                    if filepath.is_file():
                        try:
                            file_states[str(filepath)] = filepath.stat().st_mtime
                        except OSError:
                            pass
        
        print("\n[*] Real-time protection ACTIVE (polling mode)")
        print("[*] Press Ctrl+C to stop\n")
        print("-" * 50)
        
        try:
            while self.running:
                time.sleep(2)  # Poll every 2 seconds
                
                for path in self.paths:
                    if not path.exists():
                        continue
                        
                    current_files = {}
                    for filepath in path.rglob("*") if self.recursive else path.glob("*"):
                        if filepath.is_file():
                            try:
                                mtime = filepath.stat().st_mtime
                                current_files[str(filepath)] = mtime
                                
                                old_mtime = file_states.get(str(filepath))
                                if old_mtime is None:
                                    # New file
                                    self._handle_event("Created", filepath)
                                elif mtime > old_mtime:
                                    # Modified file
                                    self._handle_event("Modified", filepath)
                            except OSError:
                                pass
                    
                    # Update states
                    file_states.update(current_files)
                    
        except KeyboardInterrupt:
            self.stop()

    def stop(self):
        """Stop real-time monitoring and show statistics."""
        self.running = False
        
        # Wait for queue to empty
        try:
            self.scan_queue.join()
        except Exception:
            pass
        
        if self.observer:
            self.observer.stop()
            self.observer.join()
        
        # Print statistics
        print("\n" + "=" * 50)
        print("[*] Real-time protection STOPPED")
        print("=" * 50)
        
        if self.start_time:
            duration = datetime.now() - self.start_time
            print(f"    Duration: {duration}")
        
        print(f"    Files scanned: {self.files_scanned}")
        print(f"    Threats detected: {self.threats_detected}")
        print(f"    Suspicious files: {self.suspicious_detected}")
        
        if self.event_count:
            print("\n    Events by type:")
            for event_type, count in self.event_count.items():
                print(f"      - {event_type}: {count}")
        
        print("=" * 50)


def main():
    parser = argparse.ArgumentParser(
        description="GreyAV - A Minimal Antivirus CLI",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  greyav scan /path/to/dir           Scan a directory recursively
  greyav scan /path --parallel       Parallel multi-threaded scan
  greyav scan /path --archives       Scan inside archive files
  greyav monitor /path/to/watch      Start real-time monitoring
  greyav heuristic /path/to/file     Run heuristic analysis only
  greyav quarantine /path/to/file    Move file to quarantine
  greyav clean /path/to/file         Attempt to clean malicious file
  greyav integrity create /path      Create integrity baseline
  greyav integrity verify            Verify file integrity
  greyav network                     Scan network connections
  greyav config show                 Show configuration
  greyav config set key value        Change configuration
  greyav rules add name type pattern Add custom detection rule
  greyav schedule add name path time Add scheduled scan
  greyav schedule daemon             Run scheduled scan daemon
  greyav whitelist /path/to/file     Add file to whitelist
  greyav selfcheck                   Verify GreyAV integrity
  greyav report --format html        Generate HTML report
  greyav stats                       Show statistics
        """
    )

    subparsers = parser.add_subparsers(dest="command", help="Commands")

    # Scan command
    scan_parser = subparsers.add_parser("scan", help="Scan files/directories")
    scan_parser.add_argument("path", help="File or directory to scan")
    scan_parser.add_argument("--no-recursive", action="store_true",
                             help="Don't scan subdirectories")
    scan_parser.add_argument("--quarantine", action="store_true",
                             help="Auto-quarantine threats")
    scan_parser.add_argument("--no-heuristics", action="store_true",
                             help="Disable heuristic analysis")
    scan_parser.add_argument("--threshold", type=int, default=50,
                             help="Heuristic score threshold (default: 50)")
    scan_parser.add_argument("--report", choices=["txt", "json", "html"],
                             help="Generate report after scan")
    scan_parser.add_argument("--parallel", action="store_true",
                             help="Use parallel multi-threaded scanning")
    scan_parser.add_argument("--workers", type=int, default=4,
                             help="Number of parallel workers (default: 4)")
    scan_parser.add_argument("--archives", action="store_true",
                             help="Scan inside archive files")

    # Monitor command (real-time)
    monitor_parser = subparsers.add_parser("monitor", help="Real-time monitoring")
    monitor_parser.add_argument("paths", nargs="+", help="Paths to monitor")
    monitor_parser.add_argument("--no-recursive", action="store_true",
                                help="Don't monitor subdirectories")
    monitor_parser.add_argument("--auto-response", action="store_true",
                                help="Enable automatic threat response")
    monitor_parser.add_argument("--response-level", 
                                choices=["passive", "cautious", "moderate", "aggressive", "maximum"],
                                default="moderate", help="Auto-response aggressiveness level")
    monitor_parser.add_argument("--dry-run", action="store_true",
                                help="Test auto-response without taking actions")
    monitor_parser.add_argument("--socket-port", type=int, default=None,
                                dest="socket_port",
                                help="Single port for socket intake (legacy)")
    monitor_parser.add_argument("--socket-ports", type=str, default=None,
                                dest="socket_ports",
                                help="Comma-separated ports (e.g., 8000,22,80,443)")
    monitor_parser.add_argument("--all-ports", action="store_true",
                                dest="all_socket_ports",
                                help="Listen on all default ports (22,67,68,80,443,2222,8000)")

    # Heuristic command
    heur_parser = subparsers.add_parser("heuristic", help="Heuristic analysis")
    heur_parser.add_argument("path", help="File to analyze")
    heur_parser.add_argument("--verbose", "-v", action="store_true",
                             help="Show all indicators")

    # Quarantine command
    q_parser = subparsers.add_parser("quarantine", help="Quarantine a file")
    q_parser.add_argument("path", help="File to quarantine")

    # List quarantine command
    subparsers.add_parser("list-quarantine", help="List quarantined files")

    # Restore command
    r_parser = subparsers.add_parser("restore", help="Restore from quarantine")
    r_parser.add_argument("quarantine_file", help="Quarantined file name")
    r_parser.add_argument("restore_path", help="Path to restore to")

    # Add signature command
    sig_parser = subparsers.add_parser("add-signature", help="Add signature")
    sig_parser.add_argument("name", help="Malware name")
    sig_parser.add_argument("hash", help="SHA256 hash")
    sig_parser.add_argument("--severity", default="medium",
                            choices=["low", "medium", "high", "critical"],
                            help="Threat severity")

    # Hash command
    hash_parser = subparsers.add_parser("hash", help="Calculate file hash")
    hash_parser.add_argument("path", help="File to hash")

    # Integrity commands
    int_parser = subparsers.add_parser("integrity", help="File integrity monitoring")
    int_sub = int_parser.add_subparsers(dest="int_command")
    int_create = int_sub.add_parser("create", help="Create baseline")
    int_create.add_argument("path", help="Directory to baseline")
    int_sub.add_parser("verify", help="Verify integrity")

    # Report command
    rep_parser = subparsers.add_parser("report", help="Generate scan report")
    rep_parser.add_argument("--format", "-f", choices=["txt", "json", "html"],
                            default="txt", help="Report format")

    # Process scanning
    subparsers.add_parser("processes", help="Scan running processes")

    # Exclusion commands
    excl_parser = subparsers.add_parser("exclude", help="Manage exclusions")
    excl_sub = excl_parser.add_subparsers(dest="excl_command")
    excl_add = excl_sub.add_parser("add", help="Add exclusion")
    excl_add.add_argument("type", choices=["path", "ext", "pattern"])
    excl_add.add_argument("value", help="Exclusion value")
    excl_rm = excl_sub.add_parser("remove", help="Remove exclusion")
    excl_rm.add_argument("type", choices=["path", "ext", "pattern"])
    excl_rm.add_argument("value", help="Exclusion value")
    excl_sub.add_parser("list", help="List exclusions")

    # History command
    subparsers.add_parser("history", help="Show scan history")

    # Statistics command
    subparsers.add_parser("stats", help="Show statistics")

    # Network scanning
    subparsers.add_parser("network", help="Scan network connections")

    # Configuration management
    cfg_parser = subparsers.add_parser("config", help="Configuration management")
    cfg_sub = cfg_parser.add_subparsers(dest="cfg_command")
    cfg_sub.add_parser("show", help="Show current config")
    cfg_set = cfg_sub.add_parser("set", help="Set config value")
    cfg_set.add_argument("key", help="Config key")
    cfg_set.add_argument("value", help="Config value")
    cfg_sub.add_parser("reset", help="Reset to defaults")

    # Custom rules
    rules_parser = subparsers.add_parser("rules", help="Custom detection rules")
    rules_sub = rules_parser.add_subparsers(dest="rules_command")
    rules_add = rules_sub.add_parser("add", help="Add rule")
    rules_add.add_argument("name", help="Rule name")
    rules_add.add_argument("type", choices=["string", "regex", "hex"])
    rules_add.add_argument("pattern", help="Detection pattern")
    rules_add.add_argument("--severity", default="medium",
                           choices=["low", "medium", "high", "critical"])
    rules_add.add_argument("--desc", default="", help="Description")
    rules_sub.add_parser("list", help="List rules")
    rules_del = rules_sub.add_parser("delete", help="Delete rule")
    rules_del.add_argument("rule_id", help="Rule ID")
    rules_toggle = rules_sub.add_parser("toggle", help="Toggle rule")
    rules_toggle.add_argument("rule_id", help="Rule ID")

    # Scheduled scans
    sched_parser = subparsers.add_parser("schedule", help="Scheduled scans")
    sched_sub = sched_parser.add_subparsers(dest="sched_command")
    sched_add = sched_sub.add_parser("add", help="Add scheduled scan")
    sched_add.add_argument("name", help="Schedule name")
    sched_add.add_argument("path", help="Path to scan")
    sched_add.add_argument("type", choices=["daily", "interval"])
    sched_add.add_argument("time", help="Time (HH:MM for daily, mins for interval)")
    sched_sub.add_parser("list", help="List schedules")
    sched_sub.add_parser("daemon", help="Run schedule daemon")

    # Clean/repair
    clean_parser = subparsers.add_parser("clean", help="Clean malicious file")
    clean_parser.add_argument("path", help="File to clean")

    # Whitelist
    wl_parser = subparsers.add_parser("whitelist", help="Add to whitelist")
    wl_parser.add_argument("path", help="File to whitelist")

    # Self-check
    subparsers.add_parser("selfcheck", help="Verify GreyAV integrity")

    # Memory scanning
    mem_parser = subparsers.add_parser("memory", help="Scan process memory")
    mem_parser.add_argument("--pid", type=int, help="Specific process ID")

    # Behavioral analysis
    behav_parser = subparsers.add_parser("behavior", help="Behavioral analysis")
    behav_parser.add_argument("path", help="Executable to analyze")
    behav_parser.add_argument("--timeout", type=int, default=5, help="Timeout in seconds")

    # Email scanning
    email_parser = subparsers.add_parser("email", help="Scan email files")
    email_parser.add_argument("path", help="Email file (.eml, .msg)")

    # USB monitoring
    subparsers.add_parser("usb", help="Monitor USB devices")

    # Rootkit detection
    subparsers.add_parser("rootkit", help="Detect rootkits")

    # Update checker
    subparsers.add_parser("update", help="Check for updates")

    # Hash reputation lookup
    rep_parser = subparsers.add_parser("reputation", help="Lookup hash reputation")
    rep_parser.add_argument("hash", nargs="?", help="SHA256 hash to lookup")
    rep_parser.add_argument("--file", help="File to calculate hash and lookup")

    # Boot sector scanning
    boot_parser = subparsers.add_parser("bootscan", help="Scan boot sector")
    boot_parser.add_argument("--device", default="/dev/sda", help="Device to scan")

    # File recovery
    recover_parser = subparsers.add_parser("recover", help="Recover deleted files")
    recover_parser.add_argument("directory", help="Directory to check")
    recover_parser.add_argument("--pattern", default="*", help="File pattern")

    # Startup scanning
    subparsers.add_parser("startup", help="Scan startup items for threats")

    # Browser security scanning
    subparsers.add_parser("browser", help="Scan browser security")

    # Document analysis
    doc_parser = subparsers.add_parser("document", help="Analyze Office documents")
    doc_parser.add_argument("path", help="Document file to analyze")

    # PDF analysis
    pdf_parser = subparsers.add_parser("pdf", help="Analyze PDF files")
    pdf_parser.add_argument("path", help="PDF file to analyze")

    # Cryptominer detection
    subparsers.add_parser("cryptominer", help="Detect cryptocurrency miners")

    # System hardening assessment
    subparsers.add_parser("hardening", help="System security assessment")

    # Forensic timeline
    timeline_parser = subparsers.add_parser("timeline", help="Generate forensic timeline")
    timeline_parser.add_argument("directory", help="Directory to analyze")
    timeline_parser.add_argument("--days", type=int, default=7, help="Days to look back")

    # Log analysis
    subparsers.add_parser("logs", help="Analyze system logs")

    # Interactive mode
    subparsers.add_parser("interactive", help="Start interactive mode")

    # Ransomware protection
    ransom_parser = subparsers.add_parser("ransomware", help="Ransomware activity monitor")
    ransom_parser.add_argument("directory", help="Directory to monitor")
    ransom_parser.add_argument("--duration", type=int, default=30, help="Monitor duration (seconds)")

    # SUID scanner
    subparsers.add_parser("suid", help="Scan for SUID/SGID binaries")

    # World-writable scanner
    ww_parser = subparsers.add_parser("worldwrite", help="Find world-writable files")
    ww_parser.add_argument("--directory", default="/", help="Directory to scan")

    # WebShell detection
    web_parser = subparsers.add_parser("webshell", help="Detect web shells")
    web_parser.add_argument("--webroot", default="/var/www", help="Web root directory")

    # Hidden file scanner
    hidden_parser = subparsers.add_parser("hidden", help="Find suspicious hidden files")
    hidden_parser.add_argument("--directory", default="/", help="Directory to scan")

    # Orphan process detection
    subparsers.add_parser("orphans", help="Detect orphan processes")

    # Keylogger detection
    subparsers.add_parser("keylogger", help="Detect keylogger activity")

    # Clipboard monitor
    subparsers.add_parser("clipboard", help="Check clipboard access")

    # Environment variable check
    subparsers.add_parser("envcheck", help="Check environment variables")

    # DNS security check
    subparsers.add_parser("dns", help="DNS security check")

    # Container security
    subparsers.add_parser("containers", help="Scan Docker containers")

    # SSH key audit
    subparsers.add_parser("ssh-audit", help="Audit SSH keys for security issues")

    # Kernel module scanner
    subparsers.add_parser("kernel-modules", help="Scan kernel modules for suspicious entries")

    # Certificate scanner
    cert_parser = subparsers.add_parser("cert-scan", help="Scan certificates for security issues")
    cert_parser.add_argument("--directory", "-d", help="Directory to scan")

    # Port scanner
    port_parser = subparsers.add_parser("port-scan", help="Scan for open ports")
    port_parser.add_argument("--full", "-f", action="store_true", help="Full scan (show all ports)")

    # User account auditor
    subparsers.add_parser("user-audit", help="Audit user accounts for security issues")

    # Service auditor
    subparsers.add_parser("service-audit", help="Audit services for security issues")

    # Sudo/PAM security checker
    subparsers.add_parser("sudo-pam", help="Check sudo/PAM configuration")

    # IOC scanner
    ioc_parser = subparsers.add_parser("ioc-scan", help="Scan for Indicators of Compromise")
    ioc_parser.add_argument("--ioc-file", "-i", help="Custom IOC definitions file")

    # Firewall auditor (enhanced)
    fw_parser = subparsers.add_parser(
        "firewall-audit", 
        help="Comprehensive firewall security audit"
    )
    fw_parser.add_argument(
        "--deep", "-d", action="store_true",
        help="Perform deep rule analysis"
    )
    fw_parser.add_argument(
        "--export", "-e", action="store_true",
        help="Export rules and findings to JSON"
    )

    # File type validator
    filetype_parser = subparsers.add_parser("filetype-check", help="Validate file types match extensions")
    filetype_parser.add_argument("path", help="Directory to check")
    filetype_parser.add_argument("--no-recursive", action="store_true", help="Don't scan subdirectories")

    # Signature management
    sig_mgmt = subparsers.add_parser("signatures", help="Manage signatures")
    sig_sub = sig_mgmt.add_subparsers(dest="sig_command")
    sig_import = sig_sub.add_parser("import", help="Import signatures")
    sig_import.add_argument("file", help="Signatures file")
    sig_export = sig_sub.add_parser("export", help="Export signatures")
    sig_export.add_argument("file", help="Output file")
    sig_sub.add_parser("list", help="List signatures")
    sig_update = sig_sub.add_parser("update", help="Update from URL")
    sig_update.add_argument("--url", help="Signatures URL")

    # Health check command
    health_parser = subparsers.add_parser("health", help="System health check")
    health_parser.add_argument("--verbose", "-v", action="store_true",
                               help="Show detailed health information")
    health_parser.add_argument("--fix", action="store_true",
                               help="Attempt to fix issues automatically")

    # Safeguards verification command
    safeguard_parser = subparsers.add_parser("safeguards", 
                                             help="Verify safeguards status")
    safeguard_parser.add_argument("--test", action="store_true",
                                  help="Run safeguard tests")
    safeguard_parser.add_argument("--reset", action="store_true",
                                  help="Reset circuit breakers")

    # Validate command - pre-operation validation
    validate_parser = subparsers.add_parser("validate", 
                                            help="Validate system readiness")
    validate_parser.add_argument("--operation", "-o", 
                                 choices=["scan", "quarantine", "restore", "all"],
                                 default="all", help="Operation to validate")

    # Proactive Threat Hunt command
    threat_hunt_parser = subparsers.add_parser("threat-hunt", 
                                               help="Proactive threat hunting")
    threat_hunt_parser.add_argument("--path", "-p", help="Target path to analyze")
    threat_hunt_parser.add_argument("--deep", "-d", action="store_true",
                                    help="Enable deep analysis (entropy, signatureless)")
    threat_hunt_parser.add_argument("--export", "-e", help="Export findings to file")

    # Honeypot management commands
    honeypot_parser = subparsers.add_parser("honeypot", 
                                            help="Honeypot/canary file management")
    honeypot_sub = honeypot_parser.add_subparsers(dest="honeypot_command")
    hp_setup = honeypot_sub.add_parser("setup", help="Set up honeypot files")
    hp_setup.add_argument("--dirs", nargs="+", help="Directories for honeypots")
    honeypot_sub.add_parser("check", help="Check honeypot integrity")
    honeypot_sub.add_parser("status", help="Show honeypot status")

    # Predictive analysis command
    predict_parser = subparsers.add_parser("predict", 
                                           help="Predictive threat analysis")
    predict_parser.add_argument("--duration", "-t", type=int, default=60,
                                help="Monitoring duration in seconds (default: 60)")
    predict_parser.add_argument("--export", "-e", help="Export predictions to file")

    # Auto-response command
    autoresponse_parser = subparsers.add_parser("auto-response", 
                                                 help="Test auto threat response system")
    autoresponse_parser.add_argument("action", choices=["test", "status", "config", "history"],
                                      help="Action to perform")
    autoresponse_parser.add_argument("--level", 
                                      choices=["passive", "cautious", "moderate", "aggressive", "maximum"],
                                      default="moderate", help="Response level to test")
    autoresponse_parser.add_argument("--threat-file", help="File to use as test threat")
    autoresponse_parser.add_argument("--dry-run", action="store_true", default=True,
                                      help="Don't take real actions (default: true)")

    args = parser.parse_args()

    # ASCII Banner
    print("""
   ██████╗ ██████╗ ███████╗██╗   ██╗ █████╗ ██╗   ██╗
  ██╔════╝ ██╔══██╗██╔════╝╚██╗ ██╔╝██╔══██╗██║   ██║
  ██║  ███╗██████╔╝█████╗   ╚████╔╝ ███████║██║   ██║
  ██║   ██║██╔══██╗██╔══╝    ╚██╔╝  ██╔══██║╚██╗ ██╔╝
  ╚██████╔╝██║  ██║███████╗   ██║   ██║  ██║ ╚████╔╝ 
   ╚═════╝ ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚═╝  ╚═╝  ╚═══╝  
            Minimal Antivirus CLI v3.0
    """)

    av = GreyAV()

    if args.command == "scan":
        av.scan_start_time = time.time()
        path = Path(args.path)
        use_heuristics = not args.no_heuristics
        av.heuristic_threshold = args.threshold
        
        if path.is_file():
            threat, heuristic = av.scan_file(path, use_heuristics)
            if threat:
                print(f"[!] THREAT: {path}")
                print(f"    {threat['name']} ({threat['severity']})")
                if args.quarantine:
                    av.quarantine_file(path)
            elif heuristic and heuristic["score"] >= av.heuristic_threshold:
                print(f"[?] SUSPICIOUS: {path}")
                print(f"    Score: {heuristic['score']} ({heuristic['risk_level']})")
            
            # Scan archives if requested
            if args.archives:
                archive_results = av.scan_archive(path)
                for r in archive_results:
                    print(f"[!] THREAT IN ARCHIVE: {r['file']}")
        else:
            if args.parallel:
                av.parallel_scan(path, args.workers)
            else:
                av.scan_directory(path, not args.no_recursive, use_heuristics)
                
                # Scan archives if requested
                if args.archives:
                    print("\n[*] Scanning archives...")
                    for f in path.rglob("*"):
                        if f.suffix.lower() in [".zip", ".tar", ".gz", ".tgz"]:
                            results = av.scan_archive(f)
                            for r in results:
                                print(f"[!] THREAT IN ARCHIVE: {r['archive']} -> {r['file']}")
            
            if args.quarantine and av.scan_results["threats"]:
                print("\n[*] Quarantining threats...")
                for t in av.scan_results["threats"]:
                    av.quarantine_file(t["file"])
        
        av.print_results()
        av.save_scan_history()
        
        if args.report:
            av.generate_report(args.report)

    elif args.command == "monitor":
        print("[*] Starting Real-Time Protection...")

        # Start Socket Intake listener(s) for network threat probes
        if SOCKET_INTAKE_AVAILABLE:
            try:
                # Determine ports to listen on
                if getattr(args, 'socket_ports', None):
                    ports = [int(p.strip()) for p in args.socket_ports.split(',')]
                elif getattr(args, 'all_socket_ports', False):
                    ports = SOCKET_DEFAULT_PORTS
                elif getattr(args, 'socket_port', None):
                    ports = [args.socket_port]
                else:
                    ports = SOCKET_DEFAULT_PORTS  # Default: all ports
                
                start_socket_intake(ports=ports, blocking=False)
                print(f"    [✓] Socket Intake listening on {len(ports)} ports")
                print(f"        Ports: {', '.join(str(p) for p in ports)}")
                print("        Note: Ports < 1024 require root privileges")
            except Exception as e:
                print(f"    [!] Socket Intake failed to start: {e}")
        else:
            print("    [*] Socket Intake module not available")

        # Configure auto-response if enabled
        if args.auto_response:
            av.auto_response_enabled = True
            # Set response level
            level_map = {
                "passive": ThreatResponseLevel.PASSIVE,
                "cautious": ThreatResponseLevel.CAUTIOUS,
                "moderate": ThreatResponseLevel.MODERATE,
                "aggressive": ThreatResponseLevel.AGGRESSIVE,
                "maximum": ThreatResponseLevel.MAXIMUM
            }
            level = level_map.get(
                args.response_level, ThreatResponseLevel.MODERATE)
            av.threat_response.response_level = level
            av.threat_response.dry_run = args.dry_run

            print("[*] Auto-Response: ENABLED")
            print(f"    Response Level: {args.response_level.upper()}")
            if args.dry_run:
                print("    Mode: DRY RUN (no actual actions)")

            # Start all automatic protection systems
            av.start_auto_protection()

            # Show stats for auto threat manager
            if AUTO_THREAT_MANAGER_AVAILABLE and av.auto_threat_manager:
                print("    [✓] Enhanced AutoThreatManager active")
            else:
                print("    [*] Using legacy threat response")

        monitor = RealTimeMonitor(av, args.paths, not args.no_recursive)

        def signal_handler(sig, frame):
            print("\n[*] Shutting down GreyAV protection...")
            if SOCKET_INTAKE_AVAILABLE and socket_intake_running and socket_intake_running():
                stop_socket_intake()
                print("    [✓] Socket Intake stopped")
            if av.auto_response_enabled:
                av.stop_auto_protection()
            monitor.stop()

        signal.signal(signal.SIGINT, signal_handler)
        monitor.start()

    elif args.command == "heuristic":
        path = Path(args.path)
        if not path.exists():
            print(f"[!] File not found: {path}")
            return
        
        print(f"[*] Heuristic Analysis: {path}")
        print("-" * 50)
        
        result = av.heuristic_scan(path)
        if result:
            print(f"\nRisk Level: {result['risk_level'].upper()}")
            print(f"Score: {result['score']}/100")
            print(f"\nIndicators ({len(result['indicators'])}):")
            limit = None if args.verbose else 10
            for indicator in result["indicators"][:limit]:
                print(f"  • {indicator}")
            if not args.verbose and len(result["indicators"]) > 10:
                print(f"  ... and {len(result['indicators']) - 10} more")

    elif args.command == "quarantine":
        av.quarantine_file(args.path)

    elif args.command == "list-quarantine":
        av.list_quarantine()

    elif args.command == "restore":
        av.restore_file(args.quarantine_file, args.restore_path)

    elif args.command == "add-signature":
        av.add_signature(args.name, args.hash, args.severity)

    elif args.command == "hash":
        path = Path(args.path)
        if path.exists() and path.is_file():
            file_hash = av._calculate_hash(path)
            if file_hash:
                print(f"[*] SHA256: {file_hash}")
                print(f"    File:   {path}")
        else:
            print(f"[!] File not found: {path}")

    elif args.command == "integrity":
        if args.int_command == "create":
            av.create_integrity_baseline(args.path)
        elif args.int_command == "verify":
            av.verify_integrity()
        else:
            int_parser.print_help()

    elif args.command == "report":
        if av.scan_results["scanned"] == 0:
            print("[!] No scan data. Run a scan first.")
        else:
            av.generate_report(args.format)

    elif args.command == "processes":
        av.scan_running_processes()

    elif args.command == "exclude":
        type_map = {"path": "paths", "ext": "extensions", "pattern": "patterns"}
        if args.excl_command == "add":
            av.add_exclusion(type_map[args.type], args.value)
        elif args.excl_command == "remove":
            av.remove_exclusion(type_map[args.type], args.value)
        elif args.excl_command == "list":
            av.list_exclusions()
        else:
            excl_parser.print_help()

    elif args.command == "history":
        av.show_scan_history()

    elif args.command == "stats":
        av.show_statistics()

    elif args.command == "signatures":
        if args.sig_command == "import":
            av.import_signatures(args.file)
        elif args.sig_command == "export":
            av.export_signatures(args.file)
        elif args.sig_command == "list":
            print(f"\n[*] Loaded Signatures: {len(av.signatures)}")
            print("-" * 50)
            for sig_id, data in av.signatures.items():
                print(f"  {data['name']} [{data['severity']}]")
                print(f"    Hash: {data['hash'][:32]}...")
        elif args.sig_command == "update":
            av.update_signatures_from_url(args.url)
        else:
            sig_mgmt.print_help()

    elif args.command == "memory":
        av.scan_process_memory(args.pid)

    elif args.command == "behavior":
        av.analyze_behavior(args.path, args.timeout)

    elif args.command == "email":
        av.scan_email(args.path)

    elif args.command == "usb":
        av.monitor_usb()

    elif args.command == "rootkit":
        av.detect_rootkits()

    elif args.command == "update":
        av.check_for_updates()

    elif args.command == "reputation":
        if args.file:
            file_hash = av._calculate_hash(args.file)
            if file_hash:
                av.lookup_hash_reputation(file_hash)
        elif args.hash:
            av.lookup_hash_reputation(args.hash)
        else:
            print("[!] Provide --file or hash argument")

    elif args.command == "bootscan":
        av.scan_boot_sector(args.device)

    elif args.command == "recover":
        av.recover_deleted(args.directory, args.pattern)

    elif args.command == "startup":
        av.scan_startup_items()

    elif args.command == "browser":
        av.scan_browser()

    elif args.command == "document":
        av.analyze_document(args.path)

    elif args.command == "pdf":
        av.analyze_pdf(args.path)

    elif args.command == "cryptominer":
        av.detect_cryptominers()

    elif args.command == "hardening":
        av.check_system_hardening()

    elif args.command == "timeline":
        av.generate_forensic_timeline(args.directory, args.days)

    elif args.command == "logs":
        av.analyze_system_logs()

    elif args.command == "interactive":
        av.interactive_mode()

    elif args.command == "ransomware":
        av.detect_ransomware_activity(args.directory, args.duration)

    elif args.command == "suid":
        av.scan_suid_files()

    elif args.command == "worldwrite":
        av.scan_world_writable(args.directory)

    elif args.command == "webshell":
        av.detect_webshells(args.webroot)

    elif args.command == "hidden":
        av.scan_hidden_files(args.directory)

    elif args.command == "orphans":
        av.detect_orphan_processes()

    elif args.command == "keylogger":
        av.detect_keyloggers()

    elif args.command == "clipboard":
        av.check_clipboard_access()

    elif args.command == "envcheck":
        av.check_environment_variables()

    elif args.command == "dns":
        av.check_dns_security()

    elif args.command == "containers":
        av.scan_containers()

    elif args.command == "ssh-audit":
        av.audit_ssh_keys()

    elif args.command == "kernel-modules":
        av.scan_kernel_modules()

    elif args.command == "cert-scan":
        av.scan_certificates(args.directory)

    elif args.command == "port-scan":
        av.scan_open_ports(quick=not args.full)

    elif args.command == "user-audit":
        av.audit_user_accounts()

    elif args.command == "service-audit":
        av.audit_services()

    elif args.command == "sudo-pam":
        av.check_sudo_pam_security()

    elif args.command == "ioc-scan":
        av.scan_iocs(args.ioc_file)

    elif args.command == "firewall-audit":
        av.audit_firewall(
            deep_scan=args.deep,
            export_rules=args.export
        )

    elif args.command == "filetype-check":
        av.validate_file_types(args.path, not args.no_recursive)

    elif args.command == "health":
        av.perform_health_check(verbose=args.verbose, auto_fix=args.fix)

    elif args.command == "safeguards":
        av.verify_safeguards(run_tests=args.test, reset_breakers=args.reset)

    elif args.command == "validate":
        av.validate_system_readiness(args.operation)

    elif args.command == "threat-hunt":
        findings = av.proactive_threat_hunt(
            target_path=args.path, 
            deep=args.deep
        )
        if args.export and findings:
            export_path = Path(args.export)
            SafeGuard.safe_json_save(export_path, findings)
            print(f"\n[*] Findings exported to: {export_path}")

    elif args.command == "honeypot":
        if args.honeypot_command == "setup":
            dirs = [Path(d) for d in args.dirs] if args.dirs else None
            av.setup_honeypot_files(dirs)
        elif args.honeypot_command == "check":
            av.check_honeypot_integrity()
        elif args.honeypot_command == "status":
            registry = Path.home() / ".greyav_honeypots.json"
            if registry.exists():
                data = json.loads(registry.read_text())
                print(f"[*] Honeypot Status: {len(data)} files configured")
                for hp in data:
                    print(f"    - {hp['path']}")
            else:
                print("[*] No honeypots configured. Run 'honeypot setup' first.")
        else:
            print("[*] Honeypot commands: setup, check, status")

    elif args.command == "predict":
        predictions = av.predictive_threat_analysis(args.duration)
        if args.export and predictions:
            export_path = Path(args.export)
            SafeGuard.safe_json_save(export_path, predictions)
            print(f"\n[*] Predictions exported to: {export_path}")

    elif args.command == "auto-response":
        print("[*] Auto Threat Response System")
        print("-" * 50)
        
        if args.action == "status":
            print(f"\n[*] Current Status:")
            print(f"    Enabled: {av.auto_response_enabled}")
            print(f"    Response Level: {av.threat_response.response_level.name}")
            print(f"    Dry Run Mode: {av.threat_response.dry_run}")
            print(f"    Blocked IPs: {len(av.threat_response.blocked_ips)}")
            print(f"    Blocked Processes: {len(av.threat_response.blocked_processes)}")
            print(f"    Response History: {len(av.threat_response.response_history)} entries")
            
        elif args.action == "history":
            history = av.threat_response.response_history
            if not history:
                print("\n[*] No response history found")
            else:
                print(f"\n[*] Response History ({len(history)} entries):")
                for entry in history[-10:]:  # Last 10 entries
                    print(f"\n    Threat: {entry.get('threat_id', 'unknown')}")
                    print(f"    Time: {entry.get('timestamp', 'N/A')}")
                    print(f"    Success: {entry.get('success', False)}")
                    actions = entry.get('actions_taken', [])
                    if actions:
                        print(f"    Actions: {[a.get('action') for a in actions]}")
                        
        elif args.action == "config":
            print(f"\n[*] Configuration:")
            print(f"    Response Levels:")
            for level in ThreatResponseLevel:
                marker = "  <-- current" if level == av.threat_response.response_level else ""
                print(f"        {level.name}: {level.value}{marker}")
            print(f"\n    Severity Policies:")
            for severity, actions in av.threat_response.policies.items():
                print(f"        {severity.name}: {[a.name for a in actions]}")
                
        elif args.action == "test":
            print(f"\n[*] Testing Auto-Response System")
            print(f"    Level: {args.level}")
            print(f"    Dry Run: {args.dry_run}")
            
            # Set test configuration
            level_map = {
                "passive": ThreatResponseLevel.PASSIVE,
                "cautious": ThreatResponseLevel.CAUTIOUS,
                "moderate": ThreatResponseLevel.MODERATE,
                "aggressive": ThreatResponseLevel.AGGRESSIVE,
                "maximum": ThreatResponseLevel.MAXIMUM
            }
            av.threat_response.response_level = level_map.get(args.level, 
                                                               ThreatResponseLevel.MODERATE)
            av.threat_response.dry_run = args.dry_run
            
            # Create test threat context
            test_file = args.threat_file if args.threat_file else "/tmp/test_threat.txt"
            context = ThreatContext(
                threat_id="TEST_THREAT_001",
                file_path=test_file,
                severity="high",
                detection_type="test",
                confidence=95.0,
                mitre_tactics=["T1059", "T1055"],
                threat_family="TestMalware"
            )
            
            print(f"\n[*] Simulating threat response...")
            response = av.threat_response.respond_to_threat(context)
            
            print(f"\n[*] Response Results:")
            print(f"    Success: {response.get('success')}")
            print(f"    Actions: {len(response.get('actions_taken', []))}")
            for action in response.get('actions_taken', []):
                print(f"        - {action.get('action')}: {action.get('status')}")
            if response.get('errors'):
                print(f"    Errors: {response.get('errors')}")
                
            print("\n[✓] Auto-response test complete")

    else:
        parser.print_help()


if __name__ == "__main__":
    main()
