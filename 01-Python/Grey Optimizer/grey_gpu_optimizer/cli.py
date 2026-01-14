#!/usr/bin/env python3
"""
grey_gpu_optimizer/cli.py - Command Line Interface

This module provides the `grey-gpu-opt` CLI entrypoint with subcommands
for GPU detection, planning, enforcement, and daemon control.

Subcommands:
    detect   - Detect GPU hardware and print specifications
    plan     - Generate optimization plan for detected GPU
    apply    - Apply optimization plan (dry-run by default)
    status   - Show current daemon and GPU status
    start-daemon - Start the background optimization daemon

Usage:
    grey-gpu-opt detect
    grey-gpu-opt plan --mode safe
    grey-gpu-opt apply --dry-run
    grey-gpu-opt status
    grey-gpu-opt start-daemon --interval 30

Safety:
    - detect and plan are read-only operations
    - apply defaults to dry-run mode
    - Destructive operations require explicit --force and --confirm flags
"""

from __future__ import annotations

import argparse
import json
import sys
from datetime import datetime, timezone
from typing import Any

from grey_gpu_optimizer.optimizer import (
    GPUSpec,
    OptimizationPlan,
    ApplyResult,
    detect_gpus,
    plan_optimizations,
    apply_plan,
    load_spec,
    load_plan,
    CONFIG_DIR,
)
from grey_gpu_optimizer.daemon import (
    GPUOptimizerDaemon,
    daemon_status,
    sample_gpu_metrics,
)
from grey_gpu_optimizer.logging_config import setup_logging

# Optional YAML support
try:
    import yaml
    HAS_YAML = True
except ImportError:
    yaml = None  # type: ignore
    HAS_YAML = False


# =============================================================================
# Output Formatting
# =============================================================================

def format_json(data: dict[str, Any], indent: int = 2) -> str:
    """Format data as JSON."""
    return json.dumps(data, indent=indent, default=str)


def format_yaml(data: dict[str, Any]) -> str:
    """Format data as YAML if available, else JSON."""
    if HAS_YAML:
        return yaml.dump(data, default_flow_style=False, sort_keys=False)
    return format_json(data)


def format_table(data: dict[str, Any], title: str = "") -> str:
    """Format data as a simple table."""
    lines = []
    if title:
        lines.append(f"\n{'=' * 60}")
        lines.append(f"  {title}")
        lines.append(f"{'=' * 60}")
    
    max_key_len = max(len(str(k)) for k in data.keys()) if data else 10
    
    for key, value in data.items():
        if isinstance(value, dict):
            lines.append(f"  {key}:")
            for sub_key, sub_value in value.items():
                lines.append(f"    {sub_key}: {sub_value}")
        elif isinstance(value, list):
            lines.append(f"  {key}:")
            for item in value:
                lines.append(f"    - {item}")
        else:
            lines.append(f"  {key:<{max_key_len}} : {value}")
    
    lines.append("")
    return "\n".join(lines)


def print_output(
    data: dict[str, Any],
    output_format: str = "table",
    title: str = ""
) -> None:
    """Print data in the specified format."""
    if output_format == "json":
        print(format_json(data))
    elif output_format == "yaml":
        print(format_yaml(data))
    else:  # table
        print(format_table(data, title))


# =============================================================================
# Subcommand Handlers
# =============================================================================

def cmd_detect(args: argparse.Namespace) -> int:
    """Handle the 'detect' subcommand."""
    print("Detecting GPU hardware...\n")
    
    specs = detect_gpus(
        use_vendor_cli=not args.no_vendor_cli,
        use_system_tools=not args.no_system_tools,
        use_python_libs=not args.no_python_libs,
        verbose=args.verbose,
    )
    
    if not specs:
        print("No GPU detected.")
        return 1
    
    for i, spec in enumerate(specs):
        title = f"GPU {i}" if len(specs) > 1 else "GPU Specification"
        print_output(spec.to_dict(), args.output, title)
    
    if args.save:
        print(f"Specification saved to: {CONFIG_DIR / 'gpu_spec.json'}")
    
    return 0


def cmd_plan(args: argparse.Namespace) -> int:
    """Handle the 'plan' subcommand."""
    # Check for cached spec or run detection
    specs = load_spec()
    
    if not specs:
        print("No cached GPU specification found. Running detection...")
        specs = detect_gpus()
        if not specs:
            print("ERROR: Could not detect GPU. Run 'grey-gpu-opt detect' first.")
            return 1
    
    spec = specs[0]
    
    print(f"Generating {args.mode} optimization plan...\n")
    
    plan = plan_optimizations(spec, mode=args.mode)
    
    print_output(plan.to_dict(), args.output, f"Optimization Plan ({args.mode})")
    
    # Print confidence interpretation
    if plan.confidence >= 0.8:
        confidence_msg = "HIGH - recommendations are well-supported by detected specs"
    elif plan.confidence >= 0.5:
        confidence_msg = "MEDIUM - some assumptions made due to incomplete detection"
    else:
        confidence_msg = "LOW - limited detection data, use with caution"
    
    print(f"Confidence: {plan.confidence:.2f} ({confidence_msg})")
    
    if plan.rationale:
        print("\nRationale:")
        for key, value in plan.rationale.items():
            print(f"  {key}: {value}")
    
    return 0


def cmd_apply(args: argparse.Namespace) -> int:
    """Handle the 'apply' subcommand."""
    # Load plan
    plan = load_plan()
    
    if not plan:
        print("No cached plan found. Generating safe plan...")
        specs = load_spec()
        if not specs:
            specs = detect_gpus()
            if not specs:
                print("ERROR: Could not detect GPU.")
                return 1
        plan = plan_optimizations(specs[0], mode="safe")
    
    dry_run = not args.force
    consent = args.force and args.confirm
    confirm = args.confirm
    
    if not dry_run and not (consent and confirm):
        print("ERROR: Destructive application requires both --force and --confirm flags.")
        print("Use --dry-run (default) to simulate actions safely.")
        return 1
    
    mode_str = "DRY-RUN" if dry_run else "LIVE"
    print(f"Applying optimization plan ({mode_str})...\n")
    
    result = apply_plan(
        plan,
        dry_run=dry_run,
        consent=consent,
        confirm=confirm,
        verbose=args.verbose,
    )
    
    print_output(result.to_dict(), args.output, f"Apply Result ({mode_str})")
    
    if result.artifacts:
        print("\nGenerated Artifacts:")
        for name, path in result.artifacts.items():
            print(f"  {name}: {path}")
    
    return 0 if result.success else 1


def cmd_status(args: argparse.Namespace) -> int:
    """Handle the 'status' subcommand."""
    status = daemon_status()
    
    # Get current metrics
    specs = load_spec()
    spec = specs[0] if specs else None
    sample = sample_gpu_metrics(spec)
    
    status_data = {
        "daemon_running": status.running,
        "daemon_start_time": status.start_time or "N/A",
        "last_check": status.last_check or "N/A",
        "check_count": status.check_count,
        "gpu_vendor": status.gpu_vendor or (spec.vendor if spec else "unknown"),
        "gpu_model": status.gpu_model or (spec.model if spec else "unknown"),
        "vram_total_mb": status.vram_total_mb or (spec.vram_total_mb if spec else 0),
        "vram_free_mb": sample.vram_free_mb,
        "current_temp_c": sample.temp_c,
        "gpu_util_pct": sample.gpu_util_pct,
        "plan_mode": status.plan_mode or "N/A",
        "plan_confidence": status.plan_confidence,
        "recent_errors": status.errors[-3:] if status.errors else [],
    }
    
    print_output(status_data, args.output, "GPU Optimizer Status")
    
    # Show last actions if any
    if status.last_actions:
        print("Recent Actions:")
        for action in status.last_actions[-5:]:
            print(f"  - {action}")
    
    return 0


def cmd_start_daemon(args: argparse.Namespace) -> int:
    """Handle the 'start-daemon' subcommand."""
    print(f"Starting GPU Optimizer Daemon (interval={args.interval}s)...")
    print("Press Ctrl+C to stop.\n")
    
    # Ensure detection runs first
    specs = load_spec()
    if not specs:
        print("Running initial GPU detection...")
        specs = detect_gpus()
        if not specs:
            print("ERROR: Could not detect GPU.")
            return 1
        print(f"Detected: {specs[0].vendor} {specs[0].model}")
    
    daemon = GPUOptimizerDaemon(
        interval_s=args.interval,
        dry_run=not args.force,
        verbose=args.verbose,
        consent=args.force and args.confirm,
        confirm=args.confirm,
    )
    
    try:
        daemon.run()  # Blocking
    except KeyboardInterrupt:
        print("\nDaemon stopped by user.")
    
    return 0


# =============================================================================
# Main Entry Point
# =============================================================================

def create_parser() -> argparse.ArgumentParser:
    """Create the argument parser."""
    parser = argparse.ArgumentParser(
        prog="grey-gpu-opt",
        description="Grey GPU Optimizer - Immune-system governance for GPU resources",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    grey-gpu-opt detect                    # Detect GPU hardware
    grey-gpu-opt plan --mode aggressive    # Generate aggressive plan
    grey-gpu-opt apply --dry-run           # Simulate plan application
    grey-gpu-opt status                    # Show current status
    grey-gpu-opt start-daemon --interval 60  # Start monitoring daemon
        """
    )
    
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose output"
    )
    parser.add_argument(
        "-o", "--output",
        choices=["table", "json", "yaml"],
        default="table",
        help="Output format (default: table)"
    )
    parser.add_argument(
        "--version",
        action="version",
        version="%(prog)s 1.0.0"
    )
    
    subparsers = parser.add_subparsers(dest="command", help="Subcommands")
    
    # detect subcommand
    detect_parser = subparsers.add_parser(
        "detect",
        help="Detect GPU hardware and print specifications"
    )
    detect_parser.add_argument(
        "--no-vendor-cli",
        action="store_true",
        help="Disable vendor CLI detection (nvidia-smi, rocm-smi)"
    )
    detect_parser.add_argument(
        "--no-system-tools",
        action="store_true",
        help="Disable system tool detection (lspci, lshw)"
    )
    detect_parser.add_argument(
        "--no-python-libs",
        action="store_true",
        help="Disable Python library probing (torch, pyopencl)"
    )
    detect_parser.add_argument(
        "--save",
        action="store_true",
        default=True,
        help="Save specification to config (default: True)"
    )
    detect_parser.set_defaults(func=cmd_detect)
    
    # plan subcommand
    plan_parser = subparsers.add_parser(
        "plan",
        help="Generate optimization plan for detected GPU"
    )
    plan_parser.add_argument(
        "-m", "--mode",
        choices=["safe", "aggressive"],
        default="safe",
        help="Optimization mode (default: safe)"
    )
    plan_parser.set_defaults(func=cmd_plan)
    
    # apply subcommand
    apply_parser = subparsers.add_parser(
        "apply",
        help="Apply optimization plan (dry-run by default)"
    )
    apply_parser.add_argument(
        "--dry-run",
        action="store_true",
        default=True,
        help="Simulate actions without making changes (default)"
    )
    apply_parser.add_argument(
        "--force",
        action="store_true",
        help="Actually apply changes (requires --confirm)"
    )
    apply_parser.add_argument(
        "--confirm",
        action="store_true",
        help="Confirm destructive actions (required with --force)"
    )
    apply_parser.set_defaults(func=cmd_apply)
    
    # status subcommand
    status_parser = subparsers.add_parser(
        "status",
        help="Show current daemon and GPU status"
    )
    status_parser.set_defaults(func=cmd_status)
    
    # start-daemon subcommand
    daemon_parser = subparsers.add_parser(
        "start-daemon",
        help="Start the background optimization daemon"
    )
    daemon_parser.add_argument(
        "-i", "--interval",
        type=int,
        default=30,
        help="Monitoring interval in seconds (default: 30)"
    )
    daemon_parser.add_argument(
        "--force",
        action="store_true",
        help="Allow live enforcement (requires --confirm)"
    )
    daemon_parser.add_argument(
        "--confirm",
        action="store_true",
        help="Confirm live enforcement"
    )
    daemon_parser.set_defaults(func=cmd_start_daemon)
    
    return parser


def main(argv: list[str] | None = None) -> int:
    """Main CLI entry point."""
    parser = create_parser()
    args = parser.parse_args(argv)
    
    # Set up logging
    setup_logging(verbose=args.verbose)
    
    if not args.command:
        parser.print_help()
        return 0
    
    if hasattr(args, "func"):
        return args.func(args)
    
    parser.print_help()
    return 0


if __name__ == "__main__":
    sys.exit(main())
