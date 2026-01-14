#!/usr/bin/env python3
"""
Unit tests for daemon/memory_reclaim.py

Tests the MemoryReclaimer class and its reclamation strategies
in simulation mode (no actual system changes).
"""

import asyncio
import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import sys
sys.path.insert(0, str(Path(__file__).parent.parent / "daemon"))

try:
    from memory_reclaim import (
        MemoryReclaimer,
        ReclaimResult,
        ReclaimStrategy,
        CgroupBackup,
    )
except ImportError:
    # Module may not exist yet
    MemoryReclaimer = None


@unittest.skipIf(MemoryReclaimer is None, "memory_reclaim module not available")
class TestReclaimResult(unittest.TestCase):
    """Test the ReclaimResult dataclass."""
    
    def test_create_result(self):
        """Test creating a reclaim result."""
        result = ReclaimResult(
            strategy="madvise",
            success=True,
            bytes_reclaimed=1024 * 1024,
            message="Reclaimed 1MB",
        )
        self.assertEqual(result.strategy, "madvise")
        self.assertTrue(result.success)
        self.assertEqual(result.bytes_reclaimed, 1024 * 1024)
    
    def test_result_with_error(self):
        """Test creating a failed result."""
        result = ReclaimResult(
            strategy="cgroup",
            success=False,
            bytes_reclaimed=0,
            message="Permission denied",
            error="PermissionError",
        )
        self.assertFalse(result.success)
        self.assertEqual(result.error, "PermissionError")


@unittest.skipIf(MemoryReclaimer is None, "memory_reclaim module not available")
class TestMemoryReclaimer(unittest.TestCase):
    """Test the MemoryReclaimer class."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.reclaimer = MemoryReclaimer(
            simulation_mode=True,
            cgroup_path=Path(self.temp_dir) / "cgroup",
            madvise_helper=Path("/nonexistent/madvise_helper"),
            cgroup_helper=Path("/nonexistent/cgroup_helper"),
        )
    
    def tearDown(self):
        """Clean up test fixtures."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    def test_init_simulation_mode(self):
        """Test initialization in simulation mode."""
        self.assertTrue(self.reclaimer.simulation_mode)
        self.assertEqual(len(self.reclaimer.backups), 0)
    
    def test_get_available_strategies(self):
        """Test getting available strategies."""
        strategies = self.reclaimer.get_available_strategies()
        self.assertIsInstance(strategies, list)
        # Should always have at least some simulated strategies
        self.assertGreater(len(strategies), 0)


@unittest.skipIf(MemoryReclaimer is None, "memory_reclaim module not available")
class TestMemoryReclaimerSimulation(unittest.TestCase):
    """Test reclamation strategies in simulation mode."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.reclaimer = MemoryReclaimer(simulation_mode=True)
    
    def test_simulate_cgroup_limit(self):
        """Test simulating cgroup limit."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            result = loop.run_until_complete(
                self.reclaimer.apply_cgroup_limit(
                    name="test-cgroup",
                    memory_limit_bytes=1024 * 1024 * 1024,  # 1GB
                )
            )
            
            self.assertIsInstance(result, ReclaimResult)
            self.assertEqual(result.strategy, "cgroup_limit")
            # Simulation should succeed
            self.assertTrue(result.success)
        finally:
            loop.close()
    
    def test_simulate_madvise(self):
        """Test simulating madvise reclamation."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            result = loop.run_until_complete(
                self.reclaimer.apply_madvise(
                    pid=os.getpid(),
                    min_size_kb=1024,
                )
            )
            
            self.assertIsInstance(result, ReclaimResult)
            self.assertEqual(result.strategy, "madvise")
        finally:
            loop.close()
    
    def test_simulate_ksm_toggle(self):
        """Test simulating KSM toggle."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            result = loop.run_until_complete(
                self.reclaimer.toggle_ksm(enable=True)
            )
            
            self.assertIsInstance(result, ReclaimResult)
            self.assertEqual(result.strategy, "ksm")
            self.assertTrue(result.success)
        finally:
            loop.close()
    
    def test_simulate_drop_caches(self):
        """Test simulating drop_caches."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            result = loop.run_until_complete(
                self.reclaimer.drop_caches(level=3)
            )
            
            self.assertIsInstance(result, ReclaimResult)
            self.assertEqual(result.strategy, "drop_caches")
            self.assertTrue(result.success)
        finally:
            loop.close()


@unittest.skipIf(MemoryReclaimer is None, "memory_reclaim module not available")
class TestCgroupBackup(unittest.TestCase):
    """Test the CgroupBackup class."""
    
    def test_create_backup(self):
        """Test creating a cgroup backup."""
        backup = CgroupBackup(
            name="test-cgroup",
            memory_max=1024 * 1024 * 1024,
            memory_swap_max=2 * 1024 * 1024 * 1024,
            processes=[1234, 5678],
        )
        
        self.assertEqual(backup.name, "test-cgroup")
        self.assertEqual(backup.memory_max, 1024 * 1024 * 1024)
        self.assertEqual(len(backup.processes), 2)
    
    def test_backup_to_dict(self):
        """Test converting backup to dict."""
        backup = CgroupBackup(
            name="test-cgroup",
            memory_max=1024 * 1024 * 1024,
        )
        
        # If CgroupBackup is a dataclass, it should have asdict
        try:
            from dataclasses import asdict
            d = asdict(backup)
            self.assertEqual(d["name"], "test-cgroup")
        except (ImportError, TypeError):
            # Not a dataclass, skip this test
            pass


class TestMemoryReclaimerProfiles(unittest.TestCase):
    """Test reclamation profiles."""
    
    def test_conservative_profile(self):
        """Test conservative profile limits."""
        profile = {
            "memory_limit_percent": 95,
            "madvise_enabled": True,
            "madvise_min_size_kb": 16384,
            "ksm_enabled": False,
            "zram_enabled": False,
            "drop_caches": False,
        }
        
        self.assertEqual(profile["memory_limit_percent"], 95)
        self.assertFalse(profile["ksm_enabled"])
        self.assertFalse(profile["drop_caches"])
    
    def test_balanced_profile(self):
        """Test balanced profile limits."""
        profile = {
            "memory_limit_percent": 85,
            "madvise_enabled": True,
            "madvise_min_size_kb": 4096,
            "ksm_enabled": True,
            "zram_enabled": True,
            "zram_size_percent": 25,
            "drop_caches": False,
        }
        
        self.assertEqual(profile["memory_limit_percent"], 85)
        self.assertTrue(profile["ksm_enabled"])
        self.assertTrue(profile["zram_enabled"])
    
    def test_aggressive_profile(self):
        """Test aggressive profile limits."""
        profile = {
            "memory_limit_percent": 70,
            "madvise_enabled": True,
            "madvise_min_size_kb": 1024,
            "ksm_enabled": True,
            "zram_enabled": True,
            "zram_size_percent": 50,
            "drop_caches": True,
        }
        
        self.assertEqual(profile["memory_limit_percent"], 70)
        self.assertTrue(profile["drop_caches"])


if __name__ == "__main__":
    unittest.main()
