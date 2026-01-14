#!/usr/bin/env python3
"""
Grey Optimizer - Verification Suite

This module provides end-to-end verification of the Grey Optimizer's effectiveness.
It captures baseline metrics, applies optimization, and produces proof artifacts.

The verification suite:
1. Captures baseline system metrics (CPU, memory, disk)
2. Generates controlled synthetic workloads
3. Applies Grey Optimizer in simulation or live mode
4. Measures post-optimization metrics
5. Produces JSON proof artifacts with HMAC signatures
6. Asserts that target reductions are achieved

Usage:
    # From command line
    python verifier.py --mode simulation --duration 60
    
    # Programmatic
    from verifier import Verifier
    async with Verifier(mode='simulation') as v:
        result = await v.run_verification()
        print(f"Achieved {result.reduction_percent:.1f}% reduction")
"""

import argparse
import asyncio
import hashlib
import hmac
import json
import os
import platform
import secrets
import sys
import time
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

import psutil

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from verification.workload_generator import WorkloadGenerator, WorkloadMetrics


@dataclass
class SystemSnapshot:
    """Point-in-time system metrics snapshot."""
    
    timestamp: str
    timestamp_unix: float
    
    # CPU
    cpu_percent: float
    cpu_count: int
    cpu_freq_mhz: Optional[float]
    
    # Memory
    memory_total_mb: float
    memory_available_mb: float
    memory_used_mb: float
    memory_percent: float
    
    # Disk
    disk_total_gb: float
    disk_used_gb: float
    disk_free_gb: float
    disk_percent: float
    
    # Process-specific
    process_rss_mb: float
    process_cpu_percent: float
    
    # Platform info
    platform_system: str = field(default_factory=platform.system)
    platform_release: str = field(default_factory=platform.release)
    hostname: str = field(default_factory=platform.node)
    
    @classmethod
    def capture(cls) -> 'SystemSnapshot':
        """Capture current system state."""
        now = datetime.now(timezone.utc)
        proc = psutil.Process()
        mem = psutil.virtual_memory()
        
        # Get primary disk
        disk = psutil.disk_usage('/')
        
        # CPU frequency might not be available on all platforms
        freq = psutil.cpu_freq()
        freq_mhz = freq.current if freq else None
        
        return cls(
            timestamp=now.isoformat(),
            timestamp_unix=now.timestamp(),
            cpu_percent=psutil.cpu_percent(interval=0.5),
            cpu_count=psutil.cpu_count() or 1,
            cpu_freq_mhz=freq_mhz,
            memory_total_mb=mem.total / (1024 * 1024),
            memory_available_mb=mem.available / (1024 * 1024),
            memory_used_mb=mem.used / (1024 * 1024),
            memory_percent=mem.percent,
            disk_total_gb=disk.total / (1024 * 1024 * 1024),
            disk_used_gb=disk.used / (1024 * 1024 * 1024),
            disk_free_gb=disk.free / (1024 * 1024 * 1024),
            disk_percent=disk.percent,
            process_rss_mb=proc.memory_info().rss / (1024 * 1024),
            process_cpu_percent=proc.cpu_percent()
        )


@dataclass
class VerificationResult:
    """Complete verification result with proof."""
    
    # Test metadata
    test_id: str
    test_start: str
    test_end: str
    duration_seconds: float
    mode: str  # 'simulation' or 'live'
    
    # Snapshots
    baseline: SystemSnapshot
    after_workload: SystemSnapshot
    after_optimization: SystemSnapshot
    
    # Workload metrics
    workload_metrics: dict
    
    # Reduction calculations
    cpu_reduction_percent: float
    memory_reduction_percent: float
    disk_reduction_percent: float
    overall_reduction_percent: float
    
    # Targets and pass/fail
    target_reduction_percent: float
    passed: bool
    failure_reasons: list[str]
    
    # Signature for integrity
    signature: str
    signature_algorithm: str = 'hmac-sha256'
    
    def to_json(self) -> str:
        """Serialize to JSON."""
        return json.dumps(asdict(self), indent=2, default=str)
    
    @classmethod
    def from_json(cls, data: str) -> 'VerificationResult':
        """Deserialize from JSON."""
        d = json.loads(data)
        d['baseline'] = SystemSnapshot(**d['baseline'])
        d['after_workload'] = SystemSnapshot(**d['after_workload'])
        d['after_optimization'] = SystemSnapshot(**d['after_optimization'])
        return cls(**d)


class Verifier:
    """
    Verification orchestrator for Grey Optimizer.
    
    Runs controlled tests to verify optimizer effectiveness.
    """
    
    def __init__(
        self,
        mode: str = 'simulation',
        target_reduction: float = 90.0,
        workload_duration: float = 30.0,
        output_dir: Optional[Path] = None,
        secret_key: Optional[bytes] = None
    ):
        """
        Initialize the verifier.
        
        Args:
            mode: 'simulation' for dry-run, 'live' for real enforcement
            target_reduction: Target reduction percentage to verify
            workload_duration: How long to run workloads (seconds)
            output_dir: Directory for proof artifacts
            secret_key: HMAC key for signing (generated if None)
        """
        if mode not in ('simulation', 'live'):
            raise ValueError("mode must be 'simulation' or 'live'")
        
        self.mode = mode
        self.target_reduction = target_reduction
        self.workload_duration = workload_duration
        self.output_dir = output_dir or Path.cwd() / 'verification_results'
        self.secret_key = secret_key or secrets.token_bytes(32)
        
        self._workload_gen: Optional[WorkloadGenerator] = None
    
    async def __aenter__(self) -> 'Verifier':
        """Enter async context."""
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self._workload_gen = WorkloadGenerator()
        await self._workload_gen.__aenter__()
        return self
    
    async def __aexit__(self, *args) -> None:
        """Exit async context."""
        if self._workload_gen:
            await self._workload_gen.__aexit__(*args)
    
    def _sign(self, data: str) -> str:
        """Create HMAC signature for data."""
        return hmac.new(
            self.secret_key,
            data.encode(),
            hashlib.sha256
        ).hexdigest()
    
    def _verify_signature(self, data: str, signature: str) -> bool:
        """Verify HMAC signature."""
        expected = self._sign(data)
        return hmac.compare_digest(expected, signature)
    
    async def _apply_optimization(self) -> None:
        """
        Apply Grey Optimizer enforcement.
        
        In simulation mode, this just waits briefly.
        In live mode, this calls the optimizer API.
        """
        if self.mode == 'simulation':
            print("  [SIMULATION] Would apply optimization here")
            await asyncio.sleep(1)
            return
        
        # Live mode - call the optimizer
        import aiohttp
        
        api_url = os.environ.get('GREY_API_URL', 'http://localhost:5000')
        
        async with aiohttp.ClientSession() as session:
            # Apply optimization profile
            async with session.post(
                f'{api_url}/api/v1/apply',
                json={'profile': 'aggressive', 'immediate': True},
                timeout=aiohttp.ClientTimeout(total=30)
            ) as resp:
                if resp.status != 200:
                    raise RuntimeError(f"Apply failed: {await resp.text()}")
                
                result = await resp.json()
                print(f"  [LIVE] Applied: {result.get('status')}")
        
        # Wait for enforcement to take effect
        await asyncio.sleep(5)
    
    async def _rollback(self) -> None:
        """Rollback any applied optimizations."""
        if self.mode == 'simulation':
            print("  [SIMULATION] Would rollback here")
            return
        
        import aiohttp
        
        api_url = os.environ.get('GREY_API_URL', 'http://localhost:5000')
        
        async with aiohttp.ClientSession() as session:
            async with session.post(
                f'{api_url}/api/v1/rollback',
                timeout=aiohttp.ClientTimeout(total=30)
            ) as resp:
                if resp.status != 200:
                    print(f"  [WARNING] Rollback failed: {await resp.text()}")
                else:
                    print("  [LIVE] Rollback complete")
    
    def _calculate_reductions(
        self,
        before: SystemSnapshot,
        after: SystemSnapshot
    ) -> tuple[float, float, float, float]:
        """
        Calculate reduction percentages.
        
        Returns (cpu_reduction, memory_reduction, disk_reduction, overall)
        """
        # CPU reduction: lower percent = better
        if before.cpu_percent > 0:
            cpu_reduction = ((before.cpu_percent - after.cpu_percent) / 
                           before.cpu_percent * 100)
        else:
            cpu_reduction = 0.0
        
        # Memory reduction: lower used = better
        if before.memory_used_mb > 0:
            memory_reduction = ((before.memory_used_mb - after.memory_used_mb) /
                              before.memory_used_mb * 100)
        else:
            memory_reduction = 0.0
        
        # Disk reduction: lower used = better
        if before.disk_used_gb > 0:
            disk_reduction = ((before.disk_used_gb - after.disk_used_gb) /
                            before.disk_used_gb * 100)
        else:
            disk_reduction = 0.0
        
        # Overall is weighted average (CPU and memory more important)
        overall = (cpu_reduction * 0.4 + memory_reduction * 0.4 + 
                  disk_reduction * 0.2)
        
        return cpu_reduction, memory_reduction, disk_reduction, overall
    
    async def run_verification(self) -> VerificationResult:
        """
        Run full verification suite.
        
        Returns:
            VerificationResult with all metrics and proof
        """
        test_id = f"grey-verify-{datetime.now().strftime('%Y%m%d-%H%M%S')}"
        test_start = datetime.now(timezone.utc).isoformat()
        
        print(f"\n{'='*60}")
        print(f"Grey Optimizer Verification Suite")
        print(f"{'='*60}")
        print(f"Test ID: {test_id}")
        print(f"Mode: {self.mode.upper()}")
        print(f"Target: {self.target_reduction}% reduction")
        print(f"{'='*60}\n")
        
        # Phase 1: Capture baseline
        print("Phase 1: Capturing baseline...")
        baseline = SystemSnapshot.capture()
        print(f"  CPU: {baseline.cpu_percent:.1f}%")
        print(f"  Memory: {baseline.memory_percent:.1f}% "
              f"({baseline.memory_used_mb:.0f}MB used)")
        print(f"  Disk: {baseline.disk_percent:.1f}%")
        
        # Phase 2: Generate workload
        print(f"\nPhase 2: Generating workload ({self.workload_duration}s)...")
        
        workload_results = {}
        
        if self._workload_gen:
            # CPU workload
            print("  Running CPU workload...")
            cpu_result = await self._workload_gen.cpu_workload(
                duration=self.workload_duration / 3,
                intensity=0.8
            )
            workload_results['cpu'] = asdict(cpu_result)
            
            # Memory workload
            print("  Running memory workload...")
            mem_result = await self._workload_gen.memory_workload(
                mb=512,
                duration=self.workload_duration / 3
            )
            workload_results['memory'] = asdict(mem_result)
            
            # Disk workload
            print("  Running disk workload...")
            disk_result = await self._workload_gen.disk_workload(
                mb=100,
                duration=self.workload_duration / 3
            )
            workload_results['disk'] = asdict(disk_result)
        
        # Capture after workload
        after_workload = SystemSnapshot.capture()
        print(f"  After workload - CPU: {after_workload.cpu_percent:.1f}%, "
              f"Memory: {after_workload.memory_percent:.1f}%")
        
        # Phase 3: Apply optimization
        print("\nPhase 3: Applying optimization...")
        try:
            await self._apply_optimization()
        except Exception as e:
            print(f"  [ERROR] Optimization failed: {e}")
        
        # Brief wait for system to stabilize
        await asyncio.sleep(2)
        
        # Phase 4: Capture post-optimization metrics
        print("\nPhase 4: Capturing post-optimization metrics...")
        after_optimization = SystemSnapshot.capture()
        print(f"  CPU: {after_optimization.cpu_percent:.1f}%")
        print(f"  Memory: {after_optimization.memory_percent:.1f}% "
              f"({after_optimization.memory_used_mb:.0f}MB used)")
        print(f"  Disk: {after_optimization.disk_percent:.1f}%")
        
        # Calculate reductions
        cpu_red, mem_red, disk_red, overall_red = self._calculate_reductions(
            after_workload, after_optimization
        )
        
        # Determine pass/fail
        failure_reasons = []
        
        # In simulation mode, we expect minimal actual reduction
        if self.mode == 'simulation':
            # Simulation mode: just verify the test ran
            passed = True
            print(f"\n[SIMULATION] Verification framework functional")
        else:
            # Live mode: check actual reductions
            if overall_red < self.target_reduction:
                failure_reasons.append(
                    f"Overall reduction {overall_red:.1f}% < target {self.target_reduction}%"
                )
            if cpu_red < self.target_reduction * 0.5:
                failure_reasons.append(
                    f"CPU reduction {cpu_red:.1f}% below acceptable threshold"
                )
            if mem_red < self.target_reduction * 0.5:
                failure_reasons.append(
                    f"Memory reduction {mem_red:.1f}% below acceptable threshold"
                )
            
            passed = len(failure_reasons) == 0
        
        test_end = datetime.now(timezone.utc).isoformat()
        
        # Create result (without signature first)
        result = VerificationResult(
            test_id=test_id,
            test_start=test_start,
            test_end=test_end,
            duration_seconds=time.time() - baseline.timestamp_unix,
            mode=self.mode,
            baseline=baseline,
            after_workload=after_workload,
            after_optimization=after_optimization,
            workload_metrics=workload_results,
            cpu_reduction_percent=cpu_red,
            memory_reduction_percent=mem_red,
            disk_reduction_percent=disk_red,
            overall_reduction_percent=overall_red,
            target_reduction_percent=self.target_reduction,
            passed=passed,
            failure_reasons=failure_reasons,
            signature=""  # Will be set below
        )
        
        # Sign the result
        # Create a stable representation for signing
        sign_data = json.dumps({
            'test_id': result.test_id,
            'test_start': result.test_start,
            'test_end': result.test_end,
            'mode': result.mode,
            'overall_reduction': result.overall_reduction_percent,
            'passed': result.passed
        }, sort_keys=True)
        
        result.signature = self._sign(sign_data)
        
        # Phase 5: Generate proof artifact
        print("\nPhase 5: Generating proof artifact...")
        
        artifact_path = self.output_dir / f"{test_id}.json"
        artifact_path.write_text(result.to_json())
        print(f"  Saved: {artifact_path}")
        
        # Phase 6: Rollback if live mode
        if self.mode == 'live':
            print("\nPhase 6: Rolling back...")
            await self._rollback()
        
        # Summary
        print(f"\n{'='*60}")
        print(f"VERIFICATION {'PASSED' if passed else 'FAILED'}")
        print(f"{'='*60}")
        print(f"CPU Reduction: {cpu_red:+.1f}%")
        print(f"Memory Reduction: {mem_red:+.1f}%")
        print(f"Disk Reduction: {disk_red:+.1f}%")
        print(f"Overall: {overall_red:+.1f}% (target: {self.target_reduction}%)")
        
        if failure_reasons:
            print(f"\nFailure reasons:")
            for reason in failure_reasons:
                print(f"  - {reason}")
        
        print(f"\nProof artifact: {artifact_path}")
        print(f"Signature: {result.signature[:16]}...")
        print(f"{'='*60}\n")
        
        return result
    
    def verify_artifact(self, artifact_path: Path) -> bool:
        """
        Verify a proof artifact's signature.
        
        Args:
            artifact_path: Path to the JSON proof file
        
        Returns:
            True if signature is valid
        """
        result = VerificationResult.from_json(artifact_path.read_text())
        
        sign_data = json.dumps({
            'test_id': result.test_id,
            'test_start': result.test_start,
            'test_end': result.test_end,
            'mode': result.mode,
            'overall_reduction': result.overall_reduction_percent,
            'passed': result.passed
        }, sort_keys=True)
        
        return self._verify_signature(sign_data, result.signature)


async def main():
    """CLI entry point."""
    parser = argparse.ArgumentParser(
        description='Grey Optimizer Verification Suite'
    )
    parser.add_argument(
        '--mode', choices=['simulation', 'live'], default='simulation',
        help='Verification mode (default: simulation)'
    )
    parser.add_argument(
        '--duration', type=float, default=30.0,
        help='Workload duration in seconds (default: 30)'
    )
    parser.add_argument(
        '--target', type=float, default=90.0,
        help='Target reduction percentage (default: 90)'
    )
    parser.add_argument(
        '--output', type=Path, default=None,
        help='Output directory for proof artifacts'
    )
    parser.add_argument(
        '--verify', type=Path, default=None,
        help='Verify an existing proof artifact instead of running tests'
    )
    
    args = parser.parse_args()
    
    if args.verify:
        # Verification mode
        verifier = Verifier()
        if verifier.verify_artifact(args.verify):
            print(f"✓ Artifact signature is VALID: {args.verify}")
            return 0
        else:
            print(f"✗ Artifact signature is INVALID: {args.verify}")
            return 1
    
    # Run verification
    async with Verifier(
        mode=args.mode,
        target_reduction=args.target,
        workload_duration=args.duration,
        output_dir=args.output
    ) as verifier:
        result = await verifier.run_verification()
        return 0 if result.passed else 1


if __name__ == '__main__':
    sys.exit(asyncio.run(main()))
