"""
Policy Engine

Evaluates metrics against configured policies and generates
enforcement decisions. The engine considers baseline metrics,
current state, and configured targets to recommend actions.

Safety: Decisions are advisory - the enforcement manager applies
additional safety checks before executing any action.
"""

import logging
import re
from dataclasses import dataclass
from datetime import datetime
from typing import Dict, Any, List, Optional
from enum import Enum

from ..config import Config

logger = logging.getLogger(__name__)


class ActionType(str, Enum):
    """Types of enforcement actions."""
    CPU_CGROUP_CREATE = "cpu_cgroup_create"
    CPU_CGROUP_LIMIT = "cpu_cgroup_limit"
    CPU_SCHED_IDLE = "cpu_sched_idle"
    CPU_WEIGHT_ADJUST = "cpu_weight_adjust"
    RAM_CGROUP_CREATE = "ram_cgroup_create"
    RAM_CGROUP_LIMIT = "ram_cgroup_limit"
    RAM_KSM_ENABLE = "ram_ksm_enable"
    RAM_DROP_CACHES = "ram_drop_caches"
    RAM_RECLAIM = "ram_reclaim"  # memory.reclaim (Linux 5.17+)
    RAM_DIAGNOSE = "ram_diagnose"  # Memory diagnostics
    DISK_COMPRESS_LOGS = "disk_compress_logs"
    DISK_DEDUPLICATE = "disk_deduplicate"
    DISK_IONICE = "disk_ionice"
    DISK_SPARSE_CONVERT = "disk_sparse_convert"


@dataclass
class PolicyDecision:
    """
    A policy decision recommending an enforcement action.
    
    The enforcement manager may modify or reject this decision
    based on safety constraints.
    """
    subsystem: str  # "cpu", "ram", or "disk"
    action_type: ActionType
    action_required: bool
    priority: int  # 1 = highest, 10 = lowest
    parameters: Dict[str, Any]
    reason: str
    expected_reduction_percent: float
    timestamp: datetime = None
    
    def __post_init__(self):
        if self.timestamp is None:
            self.timestamp = datetime.utcnow()


class PolicyEngine:
    """
    Evaluates metrics and generates enforcement decisions.
    
    Principle: "Only use the bits needed, drop the bloat."
    
    The engine compares current resource usage against baseline
    and configured targets to recommend optimization actions.
    """
    
    def __init__(self, config: Config):
        """
        Initialize the policy engine.
        
        Args:
            config: Application configuration
        """
        self.config = config
        self._action_history: List[PolicyDecision] = []
        
        logger.info("Policy engine initialized")
    
    async def evaluate(
        self, 
        baseline: Dict[str, Any], 
        current: Dict[str, Any]
    ) -> List[PolicyDecision]:
        """
        Evaluate current metrics against baseline and policies.
        
        This is the main entry point for policy evaluation. It
        examines each subsystem and generates appropriate decisions.
        
        Args:
            baseline: Baseline metrics captured during warmup
            current: Current metrics snapshot
            
        Returns:
            List of policy decisions to be evaluated by enforcement
        """
        decisions = []
        
        try:
            # Evaluate CPU policies
            if self.config.cpu.enabled:
                cpu_decisions = await self._evaluate_cpu(baseline, current)
                decisions.extend(cpu_decisions)
            
            # Evaluate RAM policies
            if self.config.ram.enabled:
                ram_decisions = await self._evaluate_ram(baseline, current)
                decisions.extend(ram_decisions)
            
            # Evaluate Disk policies
            if self.config.disk.enabled:
                disk_decisions = await self._evaluate_disk(baseline, current)
                decisions.extend(disk_decisions)
            
            # Sort by priority
            decisions.sort(key=lambda d: d.priority)
            
            # Store for history
            self._action_history.extend(decisions)
            
            # Log summary
            action_required = [d for d in decisions if d.action_required]
            if action_required:
                logger.debug(f"Policy engine: {len(action_required)} actions recommended")
            
        except Exception as e:
            logger.error(f"Policy evaluation error: {e}")
        
        return decisions
    
    async def _evaluate_cpu(
        self, 
        baseline: Dict[str, Any], 
        current: Dict[str, Any]
    ) -> List[PolicyDecision]:
        """
        Evaluate CPU usage and recommend optimization actions.
        
        Strategy:
        1. If CPU usage is high, recommend cgroup quotas
        2. If specific processes are consuming excess CPU, target them
        3. Apply SCHED_IDLE to non-critical background tasks
        """
        decisions = []
        
        try:
            baseline_cpu = baseline.get("cpu", {})
            current_cpu = current.get("cpu", {})
            
            baseline_usage = baseline_cpu.get("usage_percent", 0)
            current_usage = current_cpu.get("usage_percent", 0)
            
            # Calculate target based on configured reduction
            target_usage = baseline_usage * (self.config.cpu.target_quota_percent / 100)
            
            # If current usage exceeds target, recommend cgroup limits
            if current_usage > target_usage and current_usage > 20:
                # Calculate required quota to achieve target
                required_quota = int((target_usage / 100) * self.config.cpu.cpu_period_us)
                
                decisions.append(PolicyDecision(
                    subsystem="cpu",
                    action_type=ActionType.CPU_CGROUP_LIMIT,
                    action_required=True,
                    priority=2,
                    parameters={
                        "quota_us": required_quota,
                        "period_us": self.config.cpu.cpu_period_us,
                        "target_patterns": self.config.cpu.target_patterns
                    },
                    reason=f"CPU usage {current_usage:.1f}% exceeds target {target_usage:.1f}%",
                    expected_reduction_percent=((current_usage - target_usage) / current_usage) * 100
                ))
            
            # If configured, apply SCHED_IDLE to non-critical processes
            if self.config.cpu.use_sched_idle and current_usage > 50:
                decisions.append(PolicyDecision(
                    subsystem="cpu",
                    action_type=ActionType.CPU_SCHED_IDLE,
                    action_required=True,
                    priority=3,
                    parameters={
                        "target_patterns": self.config.cpu.target_patterns,
                        "protected_patterns": self.config.cpu.protected_patterns
                    },
                    reason="Apply SCHED_IDLE to reduce CPU contention",
                    expected_reduction_percent=20
                ))
                
        except Exception as e:
            logger.error(f"CPU policy evaluation error: {e}")
        
        return decisions
    
    async def _evaluate_ram(
        self, 
        baseline: Dict[str, Any], 
        current: Dict[str, Any]
    ) -> List[PolicyDecision]:
        """
        Evaluate RAM usage and recommend optimization actions.
        
        Strategy:
        1. If memory usage is high, recommend cgroup limits
        2. Enable KSM if similar pages detected
        3. Drop caches if safe and beneficial
        """
        decisions = []
        
        try:
            baseline_ram = baseline.get("ram", {})
            current_ram = current.get("ram", {})
            
            baseline_used = baseline_ram.get("used_mb", 0)
            current_used = current_ram.get("used_mb", 0)
            total_mb = current_ram.get("total_mb", 1)
            current_cached = current_ram.get("cached_mb", 0)
            available_mb = current_ram.get("available_mb", 0)
            
            # Calculate usage percentage
            usage_percent = (current_used / total_mb) * 100 if total_mb > 0 else 0
            
            # If memory usage is high, recommend cgroup limits
            target_mb = baseline_used * 0.1  # 90% reduction target
            
            if current_used > self.config.ram.default_limit_mb and usage_percent > 60:
                decisions.append(PolicyDecision(
                    subsystem="ram",
                    action_type=ActionType.RAM_CGROUP_LIMIT,
                    action_required=True,
                    priority=2,
                    parameters={
                        "limit_mb": self.config.ram.default_limit_mb,
                        "min_limit_mb": self.config.ram.min_limit_mb,
                        "target_patterns": self.config.ram.target_patterns
                    },
                    reason=f"RAM usage {current_used:.0f}MB exceeds threshold",
                    expected_reduction_percent=min(90, ((current_used - self.config.ram.default_limit_mb) / current_used) * 100)
                ))
            
            # Enable KSM if not already enabled and beneficial
            if self.config.ram.enable_ksm:
                ksm = current_ram.get("ksm", {})
                if not ksm or not ksm.get("enabled", False):
                    decisions.append(PolicyDecision(
                        subsystem="ram",
                        action_type=ActionType.RAM_KSM_ENABLE,
                        action_required=True,
                        priority=4,
                        parameters={
                            "sleep_ms": self.config.ram.ksm_sleep_ms,
                            "pages_to_scan": self.config.ram.ksm_pages_to_scan
                        },
                        reason="Enable KSM for memory deduplication",
                        expected_reduction_percent=10
                    ))
            
            # Drop caches if configured and safe
            if self.config.ram.enable_cache_drops:
                if (current_cached > self.config.ram.cache_drop_threshold_mb * 2 and
                    available_mb < self.config.ram.cache_drop_threshold_mb):
                    decisions.append(PolicyDecision(
                        subsystem="ram",
                        action_type=ActionType.RAM_DROP_CACHES,
                        action_required=True,
                        priority=5,
                        parameters={
                            "level": 3,  # Drop pagecache, dentries, inodes
                            "threshold_mb": self.config.ram.cache_drop_threshold_mb
                        },
                        reason=f"Available memory low ({available_mb:.0f}MB), cache high ({current_cached:.0f}MB)",
                        expected_reduction_percent=30
                    ))
                    
        except Exception as e:
            logger.error(f"RAM policy evaluation error: {e}")
        
        return decisions
    
    async def _evaluate_disk(
        self, 
        baseline: Dict[str, Any], 
        current: Dict[str, Any]
    ) -> List[PolicyDecision]:
        """
        Evaluate disk usage and recommend optimization actions.
        
        Strategy:
        1. Compress old log files
        2. Deduplicate identical files (if enabled and consented)
        3. Throttle heavy writers with ionice
        """
        decisions = []
        
        try:
            baseline_disk = baseline.get("disk", {})
            current_disk = current.get("disk", {})
            
            write_mbs = current_disk.get("write_mbs", 0)
            
            # Recommend log compression
            if self.config.disk.compress_logs:
                decisions.append(PolicyDecision(
                    subsystem="disk",
                    action_type=ActionType.DISK_COMPRESS_LOGS,
                    action_required=True,
                    priority=5,
                    parameters={
                        "log_dirs": self.config.disk.log_dirs,
                        "min_age_days": self.config.disk.compress_min_age_days
                    },
                    reason="Compress rotated log files to save disk space",
                    expected_reduction_percent=70  # Typical gzip ratio
                ))
            
            # Recommend deduplication if enabled
            if self.config.disk.enable_dedup and self.config.disk.dedup_dirs:
                decisions.append(PolicyDecision(
                    subsystem="disk",
                    action_type=ActionType.DISK_DEDUPLICATE,
                    action_required=True,
                    priority=6,
                    parameters={
                        "dedup_dirs": self.config.disk.dedup_dirs,
                        "min_size": self.config.disk.dedup_min_size
                    },
                    reason="Deduplicate files with identical content",
                    expected_reduction_percent=20
                ))
            
            # Throttle heavy writers
            if write_mbs > self.config.disk.heavy_writer_threshold_mbs:
                decisions.append(PolicyDecision(
                    subsystem="disk",
                    action_type=ActionType.DISK_IONICE,
                    action_required=True,
                    priority=3,
                    parameters={
                        "level": self.config.disk.ionice_level,
                        "threshold_mbs": self.config.disk.heavy_writer_threshold_mbs
                    },
                    reason=f"Heavy disk writes detected ({write_mbs:.1f} MB/s)",
                    expected_reduction_percent=40
                ))
                
        except Exception as e:
            logger.error(f"Disk policy evaluation error: {e}")
        
        return decisions
    
    def matches_pattern(self, name: str, patterns: List[str]) -> bool:
        """
        Check if a process name matches any of the patterns.
        
        Patterns support basic glob-style matching with *.
        
        Args:
            name: Process name to check
            patterns: List of patterns to match against
            
        Returns:
            True if name matches any pattern
        """
        for pattern in patterns:
            # Convert glob to regex
            regex = pattern.replace(".", r"\.").replace("*", ".*")
            if re.match(f"^{regex}$", name, re.IGNORECASE):
                return True
        return False
    
    def get_decision_history(self, limit: int = 100) -> List[Dict[str, Any]]:
        """
        Get recent policy decisions for audit purposes.
        
        Args:
            limit: Maximum number of decisions to return
            
        Returns:
            List of decision dictionaries
        """
        decisions = self._action_history[-limit:]
        
        return [
            {
                "timestamp": d.timestamp.isoformat(),
                "subsystem": d.subsystem,
                "action_type": d.action_type.value,
                "action_required": d.action_required,
                "priority": d.priority,
                "reason": d.reason,
                "expected_reduction": d.expected_reduction_percent
            }
            for d in decisions
        ]
