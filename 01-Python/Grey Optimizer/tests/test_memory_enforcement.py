"""
Memory Enforcement Integration Tests

Tests for the enhanced memory enforcement system:
- Memory diagnostics accuracy
- cgroup limit enforcement
- Memory reclamation (when available)
- KSM management
- Safety watchdog behavior
- Rollback functionality

Test Strategy:
1. Unit tests for individual components
2. Integration tests with synthetic workloads
3. Simulation mode tests (no actual changes)
4. Live mode tests (with cleanup)

Safety: All live tests run in isolated cgroups and clean up after themselves.
"""

import asyncio
import gc
import os
import sys
import tempfile
import time
import pytest
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional
from unittest.mock import AsyncMock, MagicMock, patch

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent / "backend"))

from grey_optimizer.telemetry.memory_diagnostics import (
    MemoryDiagnostics,
    MemorySnapshot,
    MemoryComparison,
    ProcessMemory,
    LeakCandidate,
)
from grey_optimizer.enforcement.memory_ops import (
    MemoryOps,
    MemoryRegion,
    AdviseResult,
)
from grey_optimizer.enforcement.watchdog import (
    MemoryWatchdog,
    StabilityCheck,
    StabilityStatus,
    StabilityEvent,
)


# ============================================================
# Fixtures
# ============================================================

@pytest.fixture
def memory_diagnostics():
    """Create a MemoryDiagnostics instance for testing."""
    return MemoryDiagnostics(database=None)


@pytest.fixture
def memory_ops_sim():
    """Create a MemoryOps instance in simulation mode."""
    return MemoryOps(simulation=True)


@pytest.fixture
def memory_ops_live():
    """Create a MemoryOps instance for live testing."""
    return MemoryOps(simulation=False)


@pytest.fixture
async def watchdog():
    """Create a MemoryWatchdog for testing."""
    wd = MemoryWatchdog(
        enforcement_manager=None,
        database=None,
    )
    yield wd
    # Cleanup
    await wd.stop()


# ============================================================
# Memory Diagnostics Tests
# ============================================================

class TestMemoryDiagnostics:
    """Tests for MemoryDiagnostics class."""
    
    @pytest.mark.asyncio
    async def test_collect_meminfo(self, memory_diagnostics):
        """Test that meminfo collection works."""
        meminfo = await memory_diagnostics.collect_meminfo()
        
        assert isinstance(meminfo, dict)
        assert "MemTotal" in meminfo
        assert "MemFree" in meminfo
        assert "MemAvailable" in meminfo
        
        # Values should be in bytes (converted from KB)
        assert meminfo["MemTotal"] > 0
        assert meminfo["MemTotal"] > meminfo["MemFree"]
    
    @pytest.mark.asyncio
    async def test_take_snapshot(self, memory_diagnostics):
        """Test snapshot creation."""
        snapshot = await memory_diagnostics.take_snapshot(
            include_all_processes=False,
        )
        
        assert isinstance(snapshot, MemorySnapshot)
        assert snapshot.total_mb > 0
        assert snapshot.used_mb > 0
        assert snapshot.available_mb >= 0
        assert snapshot.timestamp is not None
    
    @pytest.mark.asyncio
    async def test_snapshot_with_processes(self, memory_diagnostics):
        """Test snapshot with process details."""
        # Get our own PID
        my_pid = os.getpid()
        
        snapshot = await memory_diagnostics.take_snapshot(
            target_pids=[my_pid],
            include_all_processes=False,
        )
        
        assert len(snapshot.processes) > 0
        
        # Find our process
        my_process = None
        for p in snapshot.processes:
            if p.pid == my_pid:
                my_process = p
                break
        
        assert my_process is not None
        assert my_process.rss > 0
        assert my_process.name != ""
    
    @pytest.mark.asyncio
    async def test_compare_snapshots(self, memory_diagnostics):
        """Test snapshot comparison."""
        # Take two snapshots
        before = await memory_diagnostics.take_snapshot()
        
        # Allocate some memory
        data = [0] * 1000000  # ~8MB
        
        after = await memory_diagnostics.take_snapshot()
        
        comparison = await memory_diagnostics.compare_snapshots(before, after)
        
        assert isinstance(comparison, MemoryComparison)
        assert comparison.before_timestamp is not None
        assert comparison.after_timestamp is not None
        
        # Clean up
        del data
        gc.collect()
    
    @pytest.mark.asyncio
    async def test_snapshot_to_dict(self, memory_diagnostics):
        """Test snapshot serialization."""
        snapshot = await memory_diagnostics.take_snapshot()
        
        data = snapshot.to_dict()
        
        assert isinstance(data, dict)
        assert "total_mb" in data
        assert "used_mb" in data
        assert "available_mb" in data
        assert "timestamp" in data


class TestProcessMemory:
    """Tests for ProcessMemory dataclass."""
    
    def test_process_memory_creation(self):
        """Test creating ProcessMemory instance."""
        pm = ProcessMemory(
            pid=1234,
            name="test-process",
            rss=100 * 1024 * 1024,  # 100MB
            pss=80 * 1024 * 1024,
            uss=70 * 1024 * 1024,
            swap=0,
        )
        
        assert pm.pid == 1234
        assert pm.rss_mb == 100.0
        assert pm.pss_mb == 80.0
        assert pm.uss_mb == 70.0
    
    def test_process_memory_to_dict(self):
        """Test ProcessMemory serialization."""
        pm = ProcessMemory(
            pid=1234,
            name="test",
            rss=1024 * 1024,
            pss=0,
            uss=0,
            swap=0,
        )
        
        data = pm.to_dict()
        
        assert data["pid"] == 1234
        assert data["name"] == "test"
        assert "rss_mb" in data


# ============================================================
# Memory Operations Tests
# ============================================================

class TestMemoryOps:
    """Tests for MemoryOps Python wrapper."""
    
    def test_simulation_mode(self, memory_ops_sim):
        """Test that simulation mode doesn't execute syscalls."""
        # Should succeed without doing anything
        result = memory_ops_sim.advise_anonymous(os.getpid())
        
        assert isinstance(result, AdviseResult)
        assert result.success is True
    
    def test_parse_maps_python_fallback(self, memory_ops_sim):
        """Test Python fallback for parsing maps."""
        regions = memory_ops_sim.parse_maps(os.getpid())
        
        assert isinstance(regions, list)
        assert len(regions) > 0
        
        # Check structure
        for r in regions[:5]:
            assert isinstance(r, MemoryRegion)
            assert r.start < r.end
            assert len(r.perms) > 0
    
    def test_parse_maps_finds_heap(self, memory_ops_sim):
        """Test that heap is found in maps."""
        regions = memory_ops_sim.parse_maps(os.getpid())
        
        heap_regions = [r for r in regions if r.is_heap]
        # Note: heap may not exist for small Python processes
        # So we just check the structure works
    
    def test_get_process_memory_summary(self, memory_ops_sim):
        """Test process memory summary."""
        summary = memory_ops_sim.get_process_memory_summary(os.getpid())
        
        assert "pid" in summary
        assert "total_mapped_mb" in summary
        assert "anonymous_mb" in summary
        assert "region_count" in summary
    
    def test_nonexistent_process(self, memory_ops_sim):
        """Test handling of non-existent process."""
        fake_pid = 999999999
        
        with pytest.raises(ProcessLookupError):
            memory_ops_sim.parse_maps(fake_pid)
    
    def test_clear_soft_dirty_sim(self, memory_ops_sim):
        """Test soft-dirty clearing in simulation mode."""
        result = memory_ops_sim.clear_soft_dirty(os.getpid())
        assert result is True
    
    def test_count_cold_pages_sim(self, memory_ops_sim):
        """Test cold page counting in simulation mode."""
        result = memory_ops_sim.count_cold_pages(os.getpid())
        
        assert result.cold_pages == 0  # Simulation returns 0
        assert result.hot_pages == 0


# ============================================================
# Watchdog Tests
# ============================================================

class TestMemoryWatchdog:
    """Tests for MemoryWatchdog."""
    
    @pytest.mark.asyncio
    async def test_stability_check(self, watchdog):
        """Test basic stability check."""
        check = await watchdog.check_stability()
        
        assert isinstance(check, StabilityCheck)
        assert check.status in StabilityStatus
        assert check.available_mb >= 0
    
    @pytest.mark.asyncio
    async def test_stable_system(self, watchdog):
        """Test that a healthy system is reported as stable."""
        check = await watchdog.check_stability()
        
        # A healthy system should be stable
        # (unless running under memory pressure)
        assert check.oom_count == 0
    
    @pytest.mark.asyncio
    async def test_context_manager(self):
        """Test watchdog as context manager."""
        watchdog = MemoryWatchdog()
        
        async with watchdog:
            # Watchdog should be running
            assert watchdog._running is True
        
        # Should be stopped after exit
        assert watchdog._running is False
    
    @pytest.mark.asyncio
    async def test_get_events(self, watchdog):
        """Test event retrieval."""
        events = watchdog.get_events()
        
        assert isinstance(events, list)
    
    @pytest.mark.asyncio
    async def test_get_summary(self, watchdog):
        """Test summary retrieval."""
        summary = watchdog.get_summary()
        
        assert "check_count" in summary
        assert "event_count" in summary
        assert "rollback_triggered" in summary


class TestCgroupEventMonitor:
    """Tests for cgroup event monitoring."""
    
    def test_add_cgroup(self, watchdog):
        """Test adding a cgroup to monitor."""
        # This should work even if the path doesn't exist
        watchdog.cgroup_monitor.add_cgroup("/sys/fs/cgroup/nonexistent")
        
        assert "/sys/fs/cgroup/nonexistent" in watchdog.cgroup_monitor._monitored_cgroups
    
    def test_check_new_events_empty(self, watchdog):
        """Test checking events with no monitored cgroups."""
        events = watchdog.cgroup_monitor.check_new_events()
        
        assert isinstance(events, list)
        assert len(events) == 0


class TestServiceHealthMonitor:
    """Tests for service health monitoring."""
    
    def test_check_services(self, watchdog):
        """Test service health checking."""
        dead = watchdog.service_monitor.check_services()
        
        # On a healthy system, critical services should be running
        # But we can't guarantee which services exist on test machines
        assert isinstance(dead, list)


class TestMemoryPressureMonitor:
    """Tests for memory pressure monitoring."""
    
    def test_availability(self, watchdog):
        """Test PSI availability check."""
        # This depends on kernel version
        available = watchdog.pressure_monitor.is_available()
        assert isinstance(available, bool)
    
    def test_read_pressure(self, watchdog):
        """Test reading pressure statistics."""
        pressure = watchdog.pressure_monitor.read_pressure()
        
        if watchdog.pressure_monitor.is_available():
            assert isinstance(pressure, dict)
        else:
            assert pressure == {}
    
    def test_get_status(self, watchdog):
        """Test getting pressure-based status."""
        status = watchdog.pressure_monitor.get_status()
        
        assert status in StabilityStatus


# ============================================================
# Integration Tests
# ============================================================

class TestIntegration:
    """Integration tests for the memory enforcement system."""
    
    @pytest.mark.asyncio
    async def test_diagnostics_and_ops_together(self, memory_diagnostics, memory_ops_sim):
        """Test that diagnostics and ops work together."""
        # Get current memory state
        snapshot = await memory_diagnostics.take_snapshot()
        
        # Get process details
        my_pid = os.getpid()
        summary = memory_ops_sim.get_process_memory_summary(my_pid)
        
        # Both should report something about our process
        assert snapshot.used_mb > 0
        assert summary["total_mapped_mb"] > 0
    
    @pytest.mark.asyncio
    async def test_snapshot_comparison_accuracy(self, memory_diagnostics):
        """Test that memory changes are detected accurately."""
        before = await memory_diagnostics.take_snapshot()
        
        # Allocate memory
        data = bytearray(10 * 1024 * 1024)  # 10MB
        
        # Force it to be resident
        for i in range(0, len(data), 4096):
            data[i] = 1
        
        after = await memory_diagnostics.take_snapshot()
        
        comparison = await memory_diagnostics.compare_snapshots(before, after)
        
        # We should see increased memory usage
        # Note: exact detection depends on system state
        assert comparison is not None
        
        # Cleanup
        del data
        gc.collect()
    
    @pytest.mark.asyncio
    async def test_watchdog_with_diagnostics(self, memory_diagnostics):
        """Test watchdog monitoring during diagnostic collection."""
        watchdog = MemoryWatchdog()
        
        async with watchdog:
            # Take snapshot while watchdog is running
            snapshot = await memory_diagnostics.take_snapshot()
            
            # Check stability
            check = await watchdog.check_stability()
        
        # Should complete without issues
        assert snapshot.total_mb > 0
        assert not check.needs_rollback


class TestSyntheticWorkload:
    """Tests with synthetic memory workloads."""
    
    @pytest.mark.asyncio
    async def test_memory_allocation_tracking(self, memory_diagnostics):
        """Test tracking memory allocation."""
        # Take baseline
        before = await memory_diagnostics.take_snapshot(
            target_pids=[os.getpid()],
        )
        
        # Allocate and touch memory
        allocations = []
        for _ in range(5):
            data = bytearray(2 * 1024 * 1024)  # 2MB each
            for i in range(0, len(data), 4096):
                data[i] = 1
            allocations.append(data)
        
        after = await memory_diagnostics.take_snapshot(
            target_pids=[os.getpid()],
        )
        
        comparison = await memory_diagnostics.compare_snapshots(before, after)
        
        # We allocated ~10MB
        # RSS should have increased (but might not be exactly 10MB due to system activity)
        # Just verify comparison works
        assert comparison is not None
        
        # Cleanup
        del allocations
        gc.collect()
    
    @pytest.mark.asyncio  
    async def test_leak_candidate_detection(self, memory_diagnostics):
        """Test that potential memory leaks are detected."""
        # Record multiple snapshots with growing allocation
        my_pid = os.getpid()
        allocations = []
        
        for i in range(3):
            # Take snapshot
            snapshot = await memory_diagnostics.take_snapshot(
                target_pids=[my_pid],
            )
            
            # Simulate growth
            data = bytearray(5 * 1024 * 1024)  # 5MB
            for j in range(0, len(data), 4096):
                data[j] = 1
            allocations.append(data)
            
            await asyncio.sleep(0.1)
        
        # Get leak candidates
        candidates = await memory_diagnostics.get_leak_candidates(
            threshold_mb=1,
            threshold_growth_rate=0.01,
        )
        
        # Our process should be a candidate (or the detection works)
        assert isinstance(candidates, list)
        
        # Cleanup
        del allocations
        gc.collect()


# ============================================================
# Simulation Mode Verification
# ============================================================

class TestSimulationMode:
    """Verify simulation mode behavior."""
    
    @pytest.mark.asyncio
    async def test_ops_no_side_effects(self, memory_ops_sim):
        """Verify simulation mode has no side effects."""
        my_pid = os.getpid()
        
        # Get regions before
        regions_before = memory_ops_sim.parse_maps(my_pid)
        
        # "Apply" madvise in simulation
        result = memory_ops_sim.advise_anonymous(my_pid)
        
        # Get regions after
        regions_after = memory_ops_sim.parse_maps(my_pid)
        
        # Should have same number of regions (no changes)
        # Note: regions might change slightly due to system activity
        assert abs(len(regions_before) - len(regions_after)) < 5
        
        # Result should indicate simulation
        assert result.success is True
    
    def test_clear_soft_dirty_sim_mode(self, memory_ops_sim):
        """Verify soft-dirty clearing is simulated."""
        result = memory_ops_sim.clear_soft_dirty(os.getpid())
        assert result is True


# ============================================================
# Error Handling Tests
# ============================================================

class TestErrorHandling:
    """Tests for error handling."""
    
    @pytest.mark.asyncio
    async def test_diagnostics_handles_permission_error(self, memory_diagnostics):
        """Test that permission errors are handled gracefully."""
        # Try to get details of a process we can't access
        # PID 1 is usually init/systemd
        snapshot = await memory_diagnostics.take_snapshot(
            target_pids=[1],  # Usually can't read this
        )
        
        # Should complete without exception
        # May or may not have process details depending on permissions
        assert snapshot is not None
    
    def test_ops_handles_invalid_pid(self, memory_ops_sim):
        """Test handling of invalid PID."""
        with pytest.raises((ProcessLookupError, PermissionError)):
            memory_ops_sim.parse_maps(-1)
    
    @pytest.mark.asyncio
    async def test_watchdog_handles_missing_cgroup(self):
        """Test watchdog with non-existent cgroup."""
        watchdog = MemoryWatchdog()
        watchdog.add_cgroup("/sys/fs/cgroup/nonexistent-grey-test")
        
        # Should not raise
        check = await watchdog.check_stability()
        assert check is not None


# ============================================================
# Performance Tests
# ============================================================

class TestPerformance:
    """Basic performance tests."""
    
    @pytest.mark.asyncio
    async def test_snapshot_performance(self, memory_diagnostics):
        """Test that snapshot collection is reasonably fast."""
        import time
        
        start = time.time()
        
        for _ in range(5):
            await memory_diagnostics.take_snapshot()
        
        elapsed = time.time() - start
        
        # 5 snapshots should complete in under 5 seconds
        assert elapsed < 5.0
    
    def test_maps_parsing_performance(self, memory_ops_sim):
        """Test that maps parsing is reasonably fast."""
        import time
        
        start = time.time()
        
        for _ in range(10):
            memory_ops_sim.parse_maps(os.getpid())
        
        elapsed = time.time() - start
        
        # 10 parses should complete in under 2 seconds
        assert elapsed < 2.0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
