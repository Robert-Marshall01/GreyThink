#!/usr/bin/env python3
"""
debug_gpu_optimizer.py - Comprehensive GPU Optimizer Debugging Module

This module provides diagnostic utilities to identify root causes of suboptimal
GPU optimization. It focuses on:
- Spec completeness and correctness verification
- Planner decision transparency and tracing
- Enforcement activity analysis
- Daemon health monitoring
- Performance validation workflows

All functions are read-only and never modify system state.

Usage:
    from debug_gpu_optimizer import verify_spec_integrity, trace_plan_decisions
    
    result = verify_spec_integrity(spec)
    if result['confidence_score'] < 0.8:
        print("Spec incomplete:", result['missing_fields'])
        print("Fixes:", result['suggested_fixes'])
"""

from __future__ import annotations

import json
import logging
import os
import re
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

# Optional YAML support
try:
    import yaml
    HAS_YAML = True
except ImportError:
    yaml = None  # type: ignore
    HAS_YAML = False

logger = logging.getLogger("grey_gpu_optimizer.debug")

# =============================================================================
# Required Spec Fields - These are critical for accurate optimization
# =============================================================================

REQUIRED_SPEC_FIELDS = {
    "vendor": {
        "description": "GPU vendor (nvidia, amd, intel)",
        "importance": "critical",
        "default_used": "unknown",
        "fix": "Ensure nvidia-smi, rocm-smi, or lspci can identify vendor",
    },
    "model": {
        "description": "GPU model name",
        "importance": "high",
        "default_used": "unknown",
        "fix": "Parse model from nvidia-smi --query-gpu=name or lspci -v",
    },
    "vram_total_mb": {
        "description": "Total VRAM in megabytes",
        "importance": "critical",
        "default_used": 0,
        "fix": "nvidia-smi --query-gpu=memory.total --format=csv,noheader,nounits",
    },
    "vram_free_mb": {
        "description": "Available VRAM in megabytes",
        "importance": "critical",
        "default_used": 0,
        "fix": "nvidia-smi --query-gpu=memory.free --format=csv,noheader,nounits",
    },
    "compute_capability": {
        "description": "CUDA compute capability (e.g., 8.6)",
        "importance": "high",
        "default_used": "",
        "fix": "Use torch.cuda.get_device_properties(0).major/minor or nvidia-smi",
    },
    "cuda_version": {
        "description": "CUDA version string",
        "importance": "medium",
        "default_used": "",
        "fix": "Parse from nvidia-smi output: 'CUDA Version: X.Y'",
    },
    "num_sm": {
        "description": "Number of streaming multiprocessors",
        "importance": "high",
        "default_used": 0,
        "fix": "torch.cuda.get_device_properties(0).multi_processor_count",
    },
    "pci_bandwidth": {
        "description": "PCI Express bandwidth (e.g., '16 GT/s')",
        "importance": "medium",
        "default_used": "",
        "fix": "lspci -vv | grep -i 'lnksta' for link speed",
    },
    "clock_mhz": {
        "description": "GPU clock speed in MHz",
        "importance": "medium",
        "default_used": 0,
        "fix": "nvidia-smi --query-gpu=clocks.gr --format=csv,noheader,nounits",
    },
    "thermal_limits": {
        "description": "Thermal throttle and shutdown limits dict",
        "importance": "high",
        "default_used": {"throttle_c": 83, "shutdown_c": 100},
        "fix": "nvidia-smi --query-gpu=temperature.gpu,temperature.shutdown",
    },
}

# Planner heuristic thresholds - these affect optimization aggressiveness
PLANNER_THRESHOLDS = {
    "vram_small_threshold_mb": 4096,
    "vram_medium_threshold_mb": 8192,
    "vram_large_threshold_mb": 16384,
    "safe_vram_utilization_pct": 0.70,
    "aggressive_vram_utilization_pct": 0.90,
    "intel_batch_reduction_factor": 0.5,
    "amd_batch_reduction_factor": 0.9,
    "high_sm_count_threshold": 64,
    "sm_scaling_factor_cap": 2.0,
}


# =============================================================================
# Data Classes for Diagnostic Results
# =============================================================================

@dataclass
class SpecIntegrityResult:
    """Result of spec integrity verification."""
    missing_fields: list[str] = field(default_factory=list)
    defaulted_fields: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    confidence_score: float = 0.0
    suggested_fixes: dict[str, str] = field(default_factory=dict)
    field_details: dict[str, dict] = field(default_factory=dict)
    
    def to_dict(self) -> dict[str, Any]:
        return asdict(self)


@dataclass
class PlanTraceEntry:
    """Single entry in plan decision trace."""
    recommendation: str
    value: Any
    spec_fields_used: list[str]
    defaults_used: bool
    confidence: float
    rationale: str
    potential_issue: str = ""
    suggested_adjustment: str = ""


@dataclass
class EnforcementAnalysis:
    """Analysis of enforcement log activity."""
    actions_count: int = 0
    dry_run_only: bool = True
    last_action_timestamp: str = ""
    anomalies: list[str] = field(default_factory=list)
    proposed_actions: list[str] = field(default_factory=list)
    effective_actions: list[str] = field(default_factory=list)
    gaps: list[str] = field(default_factory=list)


@dataclass 
class SimulationResult:
    """Result of enforcement simulation."""
    proposed_actions: list[dict] = field(default_factory=list)
    estimated_impact: dict[str, Any] = field(default_factory=dict)
    safety_checks: dict[str, bool] = field(default_factory=dict)
    warnings: list[str] = field(default_factory=list)
    expected_improvements: dict[str, str] = field(default_factory=dict)


# =============================================================================
# Spec Integrity Verification
# =============================================================================

def verify_spec_integrity(spec: dict[str, Any]) -> dict[str, Any]:
    """
    Verify GPU spec completeness and correctness.
    
    This function checks that all required spec fields exist and have
    meaningful values. It identifies fields that are defaulted or guessed,
    and provides specific fixes for each missing/incomplete field.
    
    Args:
        spec: GPU specification dictionary (from detect_gpus())
    
    Returns:
        Dictionary containing:
        - missing_fields: List of fields that are completely missing
        - defaulted_fields: List of fields using default/fallback values
        - warnings: List of potential issues detected
        - confidence_score: 0.0-1.0 score for spec quality
        - suggested_fixes: Dict mapping field name to exact fix command/code
        - field_details: Dict with detailed info about each field
    
    Example:
        >>> spec = {"vendor": "nvidia", "vram_total_mb": 8192}
        >>> result = verify_spec_integrity(spec)
        >>> if result["confidence_score"] < 0.8:
        ...     print("Spec needs improvement:", result["suggested_fixes"])
    
    ROOT CAUSE: If spec is incomplete, planner uses conservative defaults,
    resulting in suboptimal batch sizes and VRAM caps. Fixing spec completeness
    is the #1 way to achieve macro performance improvements.
    """
    result = SpecIntegrityResult()
    
    total_weight = 0.0
    achieved_weight = 0.0
    
    for field_name, field_info in REQUIRED_SPEC_FIELDS.items():
        # Assign weights based on importance
        weight = {"critical": 1.0, "high": 0.7, "medium": 0.4, "low": 0.2}.get(
            field_info["importance"], 0.3
        )
        total_weight += weight
        
        field_detail = {
            "present": False,
            "value": None,
            "is_default": False,
            "importance": field_info["importance"],
            "description": field_info["description"],
        }
        
        if field_name not in spec:
            # Field completely missing
            result.missing_fields.append(field_name)
            result.suggested_fixes[field_name] = field_info["fix"]
            result.warnings.append(
                f"CRITICAL: '{field_name}' missing - planner will use default "
                f"'{field_info['default_used']}' which severely limits optimization"
            )
        else:
            value = spec[field_name]
            field_detail["present"] = True
            field_detail["value"] = value
            
            # Check if value appears to be a default/placeholder
            is_default = _is_default_value(field_name, value, field_info)
            field_detail["is_default"] = is_default
            
            if is_default:
                result.defaulted_fields.append(field_name)
                result.suggested_fixes[field_name] = field_info["fix"]
                result.warnings.append(
                    f"WARNING: '{field_name}' = {value} appears to be a default value"
                )
                achieved_weight += weight * 0.3  # Partial credit for defaulted
            else:
                achieved_weight += weight  # Full credit for real values
                
                # Additional validation for specific fields
                validation_warning = _validate_field_value(field_name, value)
                if validation_warning:
                    result.warnings.append(validation_warning)
                    achieved_weight -= weight * 0.1  # Small penalty for warnings
        
        result.field_details[field_name] = field_detail
    
    # Calculate confidence score
    result.confidence_score = round(achieved_weight / total_weight, 3) if total_weight > 0 else 0.0
    
    # Add meta-warnings about overall spec quality
    if result.confidence_score < 0.5:
        result.warnings.insert(0, 
            "SEVERE: Spec confidence below 0.5 - planner will operate in "
            "ultra-conservative mode. Fix missing fields immediately."
        )
    elif result.confidence_score < 0.8:
        result.warnings.insert(0,
            "WARNING: Spec confidence below 0.8 - some optimizations may be "
            "underutilized. Consider fixing defaulted fields."
        )
    
    # Check for consistency issues
    _check_spec_consistency(spec, result)
    
    return result.to_dict()


def _is_default_value(field_name: str, value: Any, field_info: dict) -> bool:
    """Check if a value appears to be a default/placeholder."""
    default = field_info["default_used"]
    
    # Direct match with known default
    if value == default:
        return True
    
    # Numeric fields: 0 is usually a sign of failed detection
    if field_name in ["vram_total_mb", "vram_free_mb", "num_sm", "clock_mhz"]:
        if value == 0 or value is None:
            return True
    
    # String fields: empty or "unknown" indicates failure
    if field_name in ["vendor", "model", "compute_capability", "cuda_version", "pci_bandwidth"]:
        if not value or value == "unknown" or value == "":
            return True
    
    return False


def _validate_field_value(field_name: str, value: Any) -> str | None:
    """Validate specific field values for sanity."""
    if field_name == "vram_total_mb":
        if value < 1024:
            return f"vram_total_mb={value}MB seems too low - verify detection"
        if value > 100000:
            return f"vram_total_mb={value}MB seems too high - possible parsing error"
    
    if field_name == "vram_free_mb":
        if value < 0:
            return f"vram_free_mb={value}MB is negative - parsing error"
    
    if field_name == "num_sm":
        if value > 200:
            return f"num_sm={value} is unusually high - verify detection"
    
    if field_name == "clock_mhz":
        if value > 3000:
            return f"clock_mhz={value} is unusually high - verify units"
    
    if field_name == "compute_capability":
        if value and not re.match(r"^\d+\.\d+$", str(value)):
            return f"compute_capability='{value}' has unexpected format (expected X.Y)"
    
    return None


def _check_spec_consistency(spec: dict, result: SpecIntegrityResult) -> None:
    """Check for logical consistency between spec fields."""
    vram_total = spec.get("vram_total_mb", 0)
    vram_free = spec.get("vram_free_mb", 0)
    
    if vram_total > 0 and vram_free > 0:
        if vram_free > vram_total:
            result.warnings.append(
                f"INCONSISTENCY: vram_free_mb ({vram_free}) > vram_total_mb ({vram_total})"
            )
    
    vendor = spec.get("vendor", "")
    cuda_version = spec.get("cuda_version", "")
    compute_cap = spec.get("compute_capability", "")
    
    if vendor == "nvidia" and not cuda_version and not compute_cap:
        result.warnings.append(
            "INCONSISTENCY: NVIDIA vendor but no CUDA info - nvidia-smi may have failed"
        )
    
    if vendor in ["amd", "intel"] and (cuda_version or compute_cap):
        result.warnings.append(
            f"INCONSISTENCY: {vendor} vendor has CUDA info - possible detection error"
        )


# =============================================================================
# Plan Decision Tracing
# =============================================================================

def trace_plan_decisions(
    plan: dict[str, Any],
    spec: dict[str, Any]
) -> list[str]:
    """
    Trace each planner decision to its source spec fields.
    
    This function analyzes an optimization plan and shows exactly which
    spec fields influenced each decision. It flags low-confidence decisions
    that rely on defaults and identifies heuristics that may be too conservative.
    
    Args:
        plan: Optimization plan dictionary
        spec: GPU specification dictionary
    
    Returns:
        List of trace lines, one per recommendation, showing:
        - The recommendation and its value
        - Which spec fields were used
        - Whether defaults were used
        - Confidence level
        - Potential issues and suggested adjustments
    
    Example:
        >>> traces = trace_plan_decisions(plan, spec)
        >>> for line in traces:
        ...     print(line)
    
    ROOT CAUSE: Conservative heuristics in planner (e.g., always halving batch
    size for Intel) may be preventing macro improvements. This trace helps
    identify which heuristics are limiting performance.
    """
    traces: list[str] = []
    entries: list[PlanTraceEntry] = []
    
    # Extract spec values with defaults
    vram_total = spec.get("vram_total_mb", 0)
    vram_free = spec.get("vram_free_mb", 0)
    vendor = spec.get("vendor", "unknown")
    compute_cap = spec.get("compute_capability", "")
    num_sm = spec.get("num_sm", 0)
    cuda_version = spec.get("cuda_version", "")
    
    # Trace VRAM cap decision
    vram_cap = plan.get("per_process_vram_cap_mb", 0)
    vram_entry = _trace_vram_cap_decision(vram_cap, vram_total, vram_free, vendor, plan.get("mode", "safe"))
    entries.append(vram_entry)
    
    # Trace batch size decision
    batch_size = plan.get("recommended_batch_size", 1)
    batch_entry = _trace_batch_size_decision(batch_size, vram_total, vendor, compute_cap, num_sm, cuda_version)
    entries.append(batch_entry)
    
    # Trace thermal thresholds
    cooldown = plan.get("cooldown_threshold_c", 80)
    resume = plan.get("resume_threshold_c", 65)
    thermal_entry = _trace_thermal_decision(cooldown, resume, vendor, spec.get("thermal_limits", {}))
    entries.append(thermal_entry)
    
    # Trace scheduling policy
    scheduling = plan.get("fair_scheduling_policy", "round_robin")
    schedule_entry = _trace_scheduling_decision(scheduling, plan.get("mode", "safe"))
    entries.append(schedule_entry)
    
    # Trace dedup decision
    dedup_enabled = plan.get("vram_deduplication_enabled", False)
    reclaimed = plan.get("estimated_reclaimed_mb", 0)
    dedup_entry = _trace_dedup_decision(dedup_enabled, reclaimed, vram_total, plan.get("mode", "safe"))
    entries.append(dedup_entry)
    
    # Format traces as strings
    for entry in entries:
        trace_line = (
            f"[{entry.recommendation}] value={entry.value}\n"
            f"  ├─ spec_fields_used: {entry.spec_fields_used}\n"
            f"  ├─ defaults_used: {entry.defaults_used}\n"
            f"  ├─ confidence: {entry.confidence:.2f}\n"
            f"  ├─ rationale: {entry.rationale}"
        )
        if entry.potential_issue:
            trace_line += f"\n  ├─ ⚠️ ISSUE: {entry.potential_issue}"
        if entry.suggested_adjustment:
            trace_line += f"\n  └─ 💡 ADJUSTMENT: {entry.suggested_adjustment}"
        else:
            trace_line += "\n  └─ ✓ OK"
        traces.append(trace_line)
    
    # Add summary
    avg_confidence = sum(e.confidence for e in entries) / len(entries) if entries else 0
    issues_count = sum(1 for e in entries if e.potential_issue)
    defaults_count = sum(1 for e in entries if e.defaults_used)
    
    summary = (
        f"\n{'='*60}\n"
        f"PLAN TRACE SUMMARY\n"
        f"{'='*60}\n"
        f"  Total recommendations traced: {len(entries)}\n"
        f"  Average confidence: {avg_confidence:.2f}\n"
        f"  Decisions using defaults: {defaults_count}\n"
        f"  Potential issues flagged: {issues_count}\n"
    )
    
    if avg_confidence < 0.7:
        summary += (
            f"\n  ⚠️ LOW CONFIDENCE: Average confidence below 0.7\n"
            f"     Root cause: Spec missing critical fields\n"
            f"     Fix: Run verify_spec_integrity() and fix missing fields\n"
        )
    
    if issues_count > 0:
        summary += (
            f"\n  ⚠️ ISSUES DETECTED: {issues_count} recommendations may be suboptimal\n"
            f"     Review suggested adjustments above\n"
        )
    
    traces.append(summary)
    
    return traces


def _trace_vram_cap_decision(
    vram_cap: int,
    vram_total: int,
    vram_free: int,
    vendor: str,
    mode: str
) -> PlanTraceEntry:
    """Trace VRAM cap decision."""
    spec_fields = ["vram_total_mb"]
    defaults_used = vram_total == 0
    
    if vram_total == 0:
        confidence = 0.3
        rationale = "Using default VRAM cap because vram_total_mb not detected"
        issue = "Cannot optimize VRAM usage without total VRAM info"
        adjustment = "Fix nvidia-smi/rocm-smi parsing to get real VRAM"
    else:
        utilization = vram_cap / vram_total if vram_total > 0 else 0
        
        if mode == "aggressive":
            expected_util = PLANNER_THRESHOLDS["aggressive_vram_utilization_pct"]
        else:
            expected_util = PLANNER_THRESHOLDS["safe_vram_utilization_pct"]
        
        if utilization < expected_util - 0.1:
            confidence = 0.6
            rationale = f"VRAM cap at {utilization:.0%} utilization, expected {expected_util:.0%} for {mode} mode"
            issue = f"VRAM cap may be too conservative ({utilization:.0%} vs {expected_util:.0%})"
            adjustment = f"Increase per_process_vram_cap_mb to {int(vram_total * expected_util)}"
        else:
            confidence = 0.85
            rationale = f"VRAM cap at {utilization:.0%} utilization, matches {mode} mode target"
            issue = ""
            adjustment = ""
    
    return PlanTraceEntry(
        recommendation="per_process_vram_cap_mb",
        value=vram_cap,
        spec_fields_used=spec_fields,
        defaults_used=defaults_used,
        confidence=confidence,
        rationale=rationale,
        potential_issue=issue,
        suggested_adjustment=adjustment,
    )


def _trace_batch_size_decision(
    batch_size: int,
    vram_total: int,
    vendor: str,
    compute_cap: str,
    num_sm: int,
    cuda_version: str
) -> PlanTraceEntry:
    """Trace batch size decision."""
    spec_fields = ["vram_total_mb", "vendor"]
    defaults_used = vram_total == 0
    issues = []
    adjustments = []
    
    if compute_cap:
        spec_fields.append("compute_capability")
    if num_sm:
        spec_fields.append("num_sm")
    if cuda_version:
        spec_fields.append("cuda_version")
    
    # Check for overly conservative Intel reduction
    if vendor == "intel":
        expected_batch = batch_size * 2  # Undo the halving
        if vram_total >= 6144:  # 6GB+ Intel should handle larger batches
            issues.append("Intel batch reduction may be too aggressive for 6GB+ iGPU")
            adjustments.append(f"Consider batch_size={expected_batch} if VRAM pressure is low")
    
    # Check if SM scaling was applied
    if num_sm > 0 and num_sm >= PLANNER_THRESHOLDS["high_sm_count_threshold"]:
        spec_fields.append("num_sm")
        # SM scaling should increase batch size
    elif num_sm == 0:
        defaults_used = True
        issues.append("num_sm not detected - batch size not scaled for GPU cores")
        adjustments.append("Add torch.cuda probe or nvidia-smi query for SM count")
    
    # Calculate confidence
    if defaults_used:
        confidence = 0.4
        rationale = "Batch size using defaults due to missing spec fields"
    elif issues:
        confidence = 0.6
        rationale = f"Batch size based on {vendor} vendor and {vram_total}MB VRAM"
    else:
        confidence = 0.8
        rationale = f"Batch size optimized for {vendor} with {vram_total}MB VRAM"
    
    return PlanTraceEntry(
        recommendation="recommended_batch_size",
        value=batch_size,
        spec_fields_used=spec_fields,
        defaults_used=defaults_used,
        confidence=confidence,
        rationale=rationale,
        potential_issue="; ".join(issues) if issues else "",
        suggested_adjustment="; ".join(adjustments) if adjustments else "",
    )


def _trace_thermal_decision(
    cooldown: int,
    resume: int,
    vendor: str,
    thermal_limits: dict
) -> PlanTraceEntry:
    """Trace thermal threshold decision."""
    spec_fields = ["vendor"]
    defaults_used = not thermal_limits
    
    if thermal_limits:
        spec_fields.append("thermal_limits")
        throttle_temp = thermal_limits.get("throttle_c", 83)
        
        if cooldown >= throttle_temp:
            issue = f"Cooldown ({cooldown}°C) >= hardware throttle ({throttle_temp}°C)"
            adjustment = f"Lower cooldown_threshold_c to {throttle_temp - 5}°C"
            confidence = 0.5
        else:
            issue = ""
            adjustment = ""
            confidence = 0.85
        
        rationale = f"Thermal thresholds based on {vendor} limits (throttle at {throttle_temp}°C)"
    else:
        rationale = "Using default thermal thresholds - hardware limits not detected"
        issue = "Cannot optimize thermal management without hardware limits"
        adjustment = "Parse nvidia-smi --query-gpu=temperature.shutdown for limits"
        confidence = 0.4
    
    return PlanTraceEntry(
        recommendation="thermal_thresholds",
        value=f"{cooldown}°C/{resume}°C",
        spec_fields_used=spec_fields,
        defaults_used=defaults_used,
        confidence=confidence,
        rationale=rationale,
        potential_issue=issue,
        suggested_adjustment=adjustment,
    )


def _trace_scheduling_decision(scheduling: str, mode: str) -> PlanTraceEntry:
    """Trace scheduling policy decision."""
    if mode == "aggressive" and scheduling == "round_robin":
        issue = "Round-robin in aggressive mode may limit throughput"
        adjustment = "Consider priority_weighted for aggressive workloads"
        confidence = 0.6
    elif mode == "safe" and scheduling == "priority_weighted":
        issue = "Priority-weighted in safe mode may cause starvation"
        adjustment = "Use round_robin for fair safe mode"
        confidence = 0.6
    else:
        issue = ""
        adjustment = ""
        confidence = 0.85
    
    rationale = f"{scheduling} scheduling for {mode} mode"
    
    return PlanTraceEntry(
        recommendation="fair_scheduling_policy",
        value=scheduling,
        spec_fields_used=["mode"],
        defaults_used=False,
        confidence=confidence,
        rationale=rationale,
        potential_issue=issue,
        suggested_adjustment=adjustment,
    )


def _trace_dedup_decision(
    enabled: bool,
    reclaimed_mb: int,
    vram_total: int,
    mode: str
) -> PlanTraceEntry:
    """Trace VRAM deduplication decision."""
    spec_fields = ["vram_total_mb", "mode"]
    
    if vram_total >= 8192 and mode == "aggressive" and not enabled:
        issue = "Dedup disabled despite sufficient VRAM and aggressive mode"
        adjustment = "Enable vram_deduplication for potential 10-15% VRAM savings"
        confidence = 0.6
    elif enabled and reclaimed_mb == 0:
        issue = "Dedup enabled but estimated reclaim is 0"
        adjustment = "Review dedup estimation logic"
        confidence = 0.5
    elif not enabled and vram_total < 8192:
        issue = ""
        adjustment = ""
        confidence = 0.8
        rationale = f"Dedup disabled - VRAM ({vram_total}MB) below 8GB threshold"
    else:
        issue = ""
        adjustment = ""
        confidence = 0.85
    
    rationale = f"Dedup {'enabled' if enabled else 'disabled'}, estimated reclaim: {reclaimed_mb}MB"
    
    return PlanTraceEntry(
        recommendation="vram_deduplication",
        value=f"enabled={enabled}, reclaim={reclaimed_mb}MB",
        spec_fields_used=spec_fields,
        defaults_used=vram_total == 0,
        confidence=confidence,
        rationale=rationale,
        potential_issue=issue,
        suggested_adjustment=adjustment,
    )


# =============================================================================
# Enforcement Activity Analysis
# =============================================================================

def check_enforcement_activity(log_path: str) -> dict[str, Any]:
    """
    Parse daemon logs and analyze enforcement activity.
    
    This function reads structured JSON logs from the daemon and identifies:
    - How many enforcement actions were taken
    - Whether actions were dry-run only or actual enforcement
    - When the last action occurred
    - Any anomalies or gaps in enforcement
    
    Args:
        log_path: Path to daemon log file (JSONL format)
    
    Returns:
        Dictionary containing:
        - actions_count: Total number of enforcement actions
        - dry_run_only: True if all actions were dry-run
        - last_action_timestamp: ISO timestamp of last action
        - anomalies: List of detected anomalies
        - proposed_actions: Actions that were proposed
        - effective_actions: Actions that were actually applied
        - gaps: Enforcement gaps (e.g., thresholds breached but no action)
    
    ROOT CAUSE: If actions_count == 0, the daemon is either not running,
    not detecting threshold breaches, or stuck in dry-run mode. This is
    a primary cause of zero performance improvement.
    """
    result = EnforcementAnalysis()
    
    log_file = Path(log_path)
    if not log_file.exists():
        result.anomalies.append(f"Log file not found: {log_path}")
        result.gaps.append("Cannot analyze enforcement - no log file")
        return asdict(result)
    
    try:
        entries = _parse_jsonl_log(log_file)
    except Exception as e:
        result.anomalies.append(f"Failed to parse log: {e}")
        return asdict(result)
    
    if not entries:
        result.anomalies.append("Log file is empty")
        result.gaps.append("No daemon activity recorded")
        return asdict(result)
    
    # Analyze entries
    action_entries = []
    sample_entries = []
    threshold_breaches = []
    
    for entry in entries:
        event_type = entry.get("event_type", entry.get("event", ""))
        
        if event_type == "plan_applied":
            action_entries.append(entry)
            if not entry.get("dry_run", True):
                result.effective_actions.append(entry.get("details", str(entry)))
        
        if event_type == "sample":
            sample_entries.append(entry)
            # Check for threshold breaches
            temp = entry.get("temp_c", entry.get("details", {}).get("temp_c", 0))
            vram_used = entry.get("vram_used_mb", entry.get("details", {}).get("vram_used_mb", 0))
            
            if temp > 80:
                threshold_breaches.append(f"Thermal breach: {temp}°C at {entry.get('timestamp', 'unknown')}")
            if vram_used > 0:
                # Would need cap to determine breach
                pass
    
    result.actions_count = len(action_entries)
    result.dry_run_only = all(e.get("dry_run", True) for e in action_entries)
    
    if action_entries:
        last = max(action_entries, key=lambda e: e.get("timestamp", ""))
        result.last_action_timestamp = last.get("timestamp", "")
        
        for entry in action_entries:
            action_desc = entry.get("details", entry.get("actions_count", "action"))
            if entry.get("dry_run", True):
                result.proposed_actions.append(f"[DRY-RUN] {action_desc}")
            else:
                result.proposed_actions.append(f"[LIVE] {action_desc}")
    
    # Check for gaps
    if threshold_breaches and result.actions_count == 0:
        result.gaps.append(
            f"Threshold breaches detected but no enforcement actions: {threshold_breaches[:3]}"
        )
    
    if result.dry_run_only and result.actions_count > 0:
        result.gaps.append(
            "All actions are dry-run only - no actual enforcement occurring"
        )
    
    # Check for anomalies
    if len(sample_entries) > 0:
        timestamps = [e.get("timestamp", "") for e in sample_entries if e.get("timestamp")]
        if timestamps:
            # Check for gaps in sampling
            gaps = _detect_sampling_gaps(timestamps)
            if gaps:
                result.anomalies.extend(gaps)
    
    return asdict(result)


def _parse_jsonl_log(log_file: Path) -> list[dict]:
    """Parse JSONL log file."""
    entries = []
    with open(log_file) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                # Handle both JSON and nested JSON in message field
                entry = json.loads(line)
                
                # If the entry has a 'message' field that's JSON, parse it
                if "message" in entry:
                    try:
                        inner = json.loads(entry["message"])
                        entry.update(inner)
                    except (json.JSONDecodeError, TypeError):
                        pass
                
                entries.append(entry)
            except json.JSONDecodeError:
                continue
    return entries


def _detect_sampling_gaps(timestamps: list[str]) -> list[str]:
    """Detect gaps in sampling timestamps."""
    gaps = []
    
    try:
        parsed = []
        for ts in timestamps:
            if ts:
                try:
                    dt = datetime.fromisoformat(ts.replace("Z", "+00:00"))
                    parsed.append(dt)
                except ValueError:
                    continue
        
        if len(parsed) < 2:
            return gaps
        
        parsed.sort()
        
        for i in range(1, len(parsed)):
            delta = (parsed[i] - parsed[i-1]).total_seconds()
            if delta > 120:  # More than 2 minutes gap
                gaps.append(
                    f"Sampling gap of {delta:.0f}s between {parsed[i-1].isoformat()} "
                    f"and {parsed[i].isoformat()}"
                )
    except Exception:
        pass
    
    return gaps


# =============================================================================
# Enforcement Simulation
# =============================================================================

def simulate_enforcement(
    plan: dict[str, Any],
    spec: dict[str, Any],
    dry_run: bool = True
) -> dict[str, Any]:
    """
    Simulate enforcement pass and estimate impact.
    
    This function simulates what would happen if the plan were applied,
    without making any actual changes. It estimates the impact of each
    action and performs safety checks.
    
    Args:
        plan: Optimization plan to simulate
        spec: GPU specification
        dry_run: Must be True (safety check)
    
    Returns:
        Dictionary containing:
        - proposed_actions: List of actions that would be taken
        - estimated_impact: Estimated effects (reclaimed MB, etc.)
        - safety_checks: Dict of safety validations
        - warnings: Any concerns about the actions
        - expected_improvements: What improvements to expect
    
    ROOT CAUSE: If simulation shows zero expected impact, the plan is
    too conservative or the enforcement actions are ineffective.
    """
    if not dry_run:
        return {
            "error": "simulate_enforcement requires dry_run=True for safety",
            "proposed_actions": [],
            "estimated_impact": {},
            "safety_checks": {"dry_run_enforced": False},
            "warnings": ["Cannot simulate with dry_run=False"],
            "expected_improvements": {},
        }
    
    result = SimulationResult()
    
    # Safety checks
    result.safety_checks = {
        "dry_run_enforced": dry_run,
        "consent_not_set": True,  # We don't set consent in simulation
        "confirm_not_set": True,
        "no_destructive_actions": True,
    }
    
    vram_total = spec.get("vram_total_mb", 0)
    vram_free = spec.get("vram_free_mb", 0)
    vendor = spec.get("vendor", "unknown")
    
    # Simulate VRAM cap enforcement
    vram_cap = plan.get("per_process_vram_cap_mb", 0)
    if vram_cap > 0:
        action = {
            "type": "set_vram_cap",
            "value": vram_cap,
            "current_free": vram_free,
            "effect": "Limit per-process VRAM allocation",
        }
        result.proposed_actions.append(action)
        
        # Estimate impact
        if vram_free > 0 and vram_total > 0:
            vram_used = vram_total - vram_free
            if vram_used > vram_cap:
                potential_reclaim = vram_used - vram_cap
                result.estimated_impact["vram_reclaim_mb"] = potential_reclaim
                result.expected_improvements["memory"] = (
                    f"Could reclaim up to {potential_reclaim}MB from over-allocating processes"
                )
    
    # Simulate thermal management
    cooldown = plan.get("cooldown_threshold_c", 80)
    resume = plan.get("resume_threshold_c", 65)
    
    action = {
        "type": "set_thermal_thresholds",
        "cooldown_c": cooldown,
        "resume_c": resume,
        "effect": "Throttle when above cooldown, resume when below resume",
    }
    result.proposed_actions.append(action)
    result.expected_improvements["thermal"] = (
        f"Automatic throttling at {cooldown}°C prevents thermal damage"
    )
    
    # Simulate batch size hint
    batch_size = plan.get("recommended_batch_size", 1)
    action = {
        "type": "set_batch_hint",
        "value": batch_size,
        "effect": "Hint file for ML frameworks to use optimal batch size",
    }
    result.proposed_actions.append(action)
    
    if batch_size > 1:
        result.expected_improvements["throughput"] = (
            f"Batch size {batch_size} should improve throughput over batch=1"
        )
    
    # Simulate dedup
    if plan.get("vram_deduplication_enabled", False):
        reclaim_est = plan.get("estimated_reclaimed_mb", 0)
        action = {
            "type": "enable_dedup",
            "estimated_reclaim_mb": reclaim_est,
            "effect": "Identify and consolidate duplicate VRAM buffers",
        }
        result.proposed_actions.append(action)
        
        if reclaim_est > 0:
            result.estimated_impact["dedup_reclaim_mb"] = reclaim_est
            result.expected_improvements["dedup"] = (
                f"Deduplication could reclaim ~{reclaim_est}MB"
            )
    
    # Simulate preemptive throttle
    throttle_config = plan.get("preemptive_throttle", {})
    if throttle_config.get("enabled", False):
        action = {
            "type": "enable_preemptive_throttle",
            "throttle_pct": throttle_config.get("throttle_pct", 50),
            "grace_period_s": throttle_config.get("grace_period_s", 5),
            "effect": "Preemptively throttle before thermal limits",
        }
        result.proposed_actions.append(action)
    
    # Calculate total estimated impact
    total_reclaim = sum(
        v for k, v in result.estimated_impact.items()
        if "reclaim" in k and isinstance(v, (int, float))
    )
    result.estimated_impact["total_estimated_reclaim_mb"] = total_reclaim
    
    # Warnings
    if total_reclaim == 0:
        result.warnings.append(
            "No VRAM reclamation expected - spec may be incomplete or plan too conservative"
        )
    
    if not result.expected_improvements:
        result.warnings.append(
            "No improvements expected - verify spec and plan configuration"
        )
    
    if vram_total == 0:
        result.warnings.append(
            "vram_total_mb not detected - cannot accurately estimate impact"
        )
    
    return asdict(result)


# =============================================================================
# Diagnostic Report Generation
# =============================================================================

def generate_diagnostic_report(
    spec_path: str,
    plan_path: str,
    log_path: str,
    out_path: str
) -> dict[str, Any]:
    """
    Generate comprehensive diagnostic report.
    
    This function aggregates all diagnostic checks and produces a single
    report file that can be used for debugging and CI gating.
    
    Args:
        spec_path: Path to GPU spec JSON file
        plan_path: Path to optimization plan (YAML or JSON)
        log_path: Path to daemon log file
        out_path: Path to write output report
    
    Returns:
        The report dictionary (also written to out_path)
    
    Report includes:
        - spec_analysis: From verify_spec_integrity()
        - plan_trace: From trace_plan_decisions()
        - enforcement_analysis: From check_enforcement_activity()
        - simulation: From simulate_enforcement()
        - overall_health: Summary health score and recommendations
    """
    report = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "inputs": {
            "spec_path": spec_path,
            "plan_path": plan_path,
            "log_path": log_path,
        },
        "spec_analysis": {},
        "plan_trace": [],
        "enforcement_analysis": {},
        "simulation": {},
        "overall_health": {},
    }
    
    # Load spec
    spec = {}
    try:
        with open(spec_path) as f:
            spec_data = json.load(f)
            # Handle list format from detect_gpus
            if isinstance(spec_data, list) and spec_data:
                spec = spec_data[0]
            else:
                spec = spec_data
        report["spec_analysis"] = verify_spec_integrity(spec)
    except Exception as e:
        report["spec_analysis"] = {"error": str(e)}
    
    # Load plan
    plan = {}
    try:
        plan_file = Path(plan_path)
        if plan_file.suffix in [".yaml", ".yml"] and HAS_YAML:
            with open(plan_file) as f:
                plan = yaml.safe_load(f) or {}
        else:
            with open(plan_file) as f:
                plan = json.load(f)
        
        if spec:
            report["plan_trace"] = trace_plan_decisions(plan, spec)
    except Exception as e:
        report["plan_trace"] = [f"Error: {e}"]
    
    # Analyze enforcement logs
    try:
        report["enforcement_analysis"] = check_enforcement_activity(log_path)
    except Exception as e:
        report["enforcement_analysis"] = {"error": str(e)}
    
    # Run simulation
    if spec and plan:
        try:
            report["simulation"] = simulate_enforcement(plan, spec, dry_run=True)
        except Exception as e:
            report["simulation"] = {"error": str(e)}
    
    # Calculate overall health
    report["overall_health"] = _calculate_health_score(report)
    
    # Write report
    try:
        out_file = Path(out_path)
        out_file.parent.mkdir(parents=True, exist_ok=True)
        with open(out_file, "w") as f:
            json.dump(report, f, indent=2, default=str)
    except Exception as e:
        report["write_error"] = str(e)
    
    return report


def _calculate_health_score(report: dict) -> dict[str, Any]:
    """Calculate overall system health score."""
    health = {
        "score": 0.0,
        "max_score": 100.0,
        "grade": "F",
        "critical_issues": [],
        "recommendations": [],
    }
    
    points = 0.0
    
    # Spec health (40 points max)
    spec_analysis = report.get("spec_analysis", {})
    if "error" not in spec_analysis:
        spec_confidence = spec_analysis.get("confidence_score", 0)
        points += spec_confidence * 40
        
        if spec_confidence < 0.5:
            health["critical_issues"].append("Spec confidence below 50%")
            health["recommendations"].append("Fix GPU detection - see suggested_fixes")
        elif spec_confidence < 0.8:
            health["recommendations"].append("Improve spec completeness for better optimization")
    
    # Plan health (20 points max)
    plan_trace = report.get("plan_trace", [])
    if plan_trace and not any("Error" in str(t) for t in plan_trace):
        # Check for issues in trace
        issue_count = sum(1 for t in plan_trace if "ISSUE" in str(t))
        if issue_count == 0:
            points += 20
        else:
            points += max(0, 20 - issue_count * 5)
            health["recommendations"].append(f"Address {issue_count} planner issues")
    
    # Enforcement health (20 points max)
    enforcement = report.get("enforcement_analysis", {})
    if "error" not in enforcement:
        if enforcement.get("actions_count", 0) > 0:
            points += 10
            if not enforcement.get("dry_run_only", True):
                points += 10
            else:
                health["recommendations"].append("Enable live enforcement (with consent)")
        else:
            health["critical_issues"].append("No enforcement actions detected")
            health["recommendations"].append("Start daemon and verify it's detecting threshold breaches")
    
    # Simulation health (20 points max)
    simulation = report.get("simulation", {})
    if "error" not in simulation:
        reclaim = simulation.get("estimated_impact", {}).get("total_estimated_reclaim_mb", 0)
        if reclaim > 0:
            points += 10
        
        improvements = simulation.get("expected_improvements", {})
        if improvements:
            points += min(10, len(improvements) * 3)
        else:
            health["recommendations"].append("Review plan for optimization opportunities")
    
    health["score"] = round(points, 1)
    
    # Assign grade
    if points >= 90:
        health["grade"] = "A"
    elif points >= 80:
        health["grade"] = "B"
    elif points >= 70:
        health["grade"] = "C"
    elif points >= 60:
        health["grade"] = "D"
    else:
        health["grade"] = "F"
    
    # Detection success flag for CI
    health["detection_success"] = spec_analysis.get("confidence_score", 0) >= 0.5
    health["plan_confidence"] = spec_analysis.get("confidence_score", 0)
    
    return health


# =============================================================================
# CLI Integration (to be used by gpu-debug command)
# =============================================================================

def main():
    """CLI entry point for debug module."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description="GPU Optimizer Debugging Tools"
    )
    subparsers = parser.add_subparsers(dest="command")
    
    # verify-spec
    verify_parser = subparsers.add_parser("verify-spec", help="Verify spec integrity")
    verify_parser.add_argument("--spec", required=True, help="Path to spec JSON")
    
    # trace-plan
    trace_parser = subparsers.add_parser("trace-plan", help="Trace plan decisions")
    trace_parser.add_argument("--plan", required=True, help="Path to plan file")
    trace_parser.add_argument("--spec", required=True, help="Path to spec JSON")
    
    # check-enforcement
    check_parser = subparsers.add_parser("check-enforcement", help="Check enforcement logs")
    check_parser.add_argument("--log", required=True, help="Path to daemon log")
    
    # simulate
    sim_parser = subparsers.add_parser("simulate", help="Simulate enforcement")
    sim_parser.add_argument("--plan", required=True, help="Path to plan file")
    sim_parser.add_argument("--spec", required=True, help="Path to spec JSON")
    sim_parser.add_argument("--dry-run", action="store_true", default=True)
    
    # report
    report_parser = subparsers.add_parser("report", help="Generate diagnostic report")
    report_parser.add_argument("--spec", required=True)
    report_parser.add_argument("--plan", required=True)
    report_parser.add_argument("--log", required=True)
    report_parser.add_argument("--out", required=True)
    
    args = parser.parse_args()
    
    if args.command == "verify-spec":
        with open(args.spec) as f:
            spec_data = json.load(f)
            spec = spec_data[0] if isinstance(spec_data, list) else spec_data
        result = verify_spec_integrity(spec)
        print(json.dumps(result, indent=2))
    
    elif args.command == "trace-plan":
        with open(args.spec) as f:
            spec_data = json.load(f)
            spec = spec_data[0] if isinstance(spec_data, list) else spec_data
        
        plan_path = Path(args.plan)
        if plan_path.suffix in [".yaml", ".yml"] and HAS_YAML:
            with open(plan_path) as f:
                plan = yaml.safe_load(f)
        else:
            with open(plan_path) as f:
                plan = json.load(f)
        
        traces = trace_plan_decisions(plan, spec)
        for trace in traces:
            print(trace)
    
    elif args.command == "check-enforcement":
        result = check_enforcement_activity(args.log)
        print(json.dumps(result, indent=2))
    
    elif args.command == "simulate":
        with open(args.spec) as f:
            spec_data = json.load(f)
            spec = spec_data[0] if isinstance(spec_data, list) else spec_data
        
        plan_path = Path(args.plan)
        if plan_path.suffix in [".yaml", ".yml"] and HAS_YAML:
            with open(plan_path) as f:
                plan = yaml.safe_load(f)
        else:
            with open(plan_path) as f:
                plan = json.load(f)
        
        result = simulate_enforcement(plan, spec, dry_run=True)
        print(json.dumps(result, indent=2))
    
    elif args.command == "report":
        result = generate_diagnostic_report(
            args.spec, args.plan, args.log, args.out
        )
        print(f"Report written to: {args.out}")
        print(f"Health Score: {result['overall_health']['score']}/100 ({result['overall_health']['grade']})")
        if result['overall_health']['critical_issues']:
            print("Critical Issues:")
            for issue in result['overall_health']['critical_issues']:
                print(f"  ❌ {issue}")
    
    else:
        parser.print_help()


if __name__ == "__main__":
    main()
