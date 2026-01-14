#!/usr/bin/env python3
"""
Unit tests for daemon/telemetry.py

Tests the TelemetrySnapshotter and related classes for
capturing memory snapshots and comparing baseline vs post.
"""

import asyncio
import json
import os
import sqlite3
import tempfile
import unittest
from datetime import datetime
from pathlib import Path
from unittest.mock import MagicMock, patch

import sys
sys.path.insert(0, str(Path(__file__).parent.parent / "daemon"))

try:
    from telemetry import (
        ProcessMemory,
        MemorySnapshot,
        SnapshotComparison,
        TelemetrySnapshotter,
        SnapshotStore,
    )
except ImportError:
    TelemetrySnapshotter = None


@unittest.skipIf(TelemetrySnapshotter is None, "telemetry module not available")
class TestProcessMemory(unittest.TestCase):
    """Test the ProcessMemory dataclass."""
    
    def test_create_process_memory(self):
        """Test creating process memory info."""
        pm = ProcessMemory(
            pid=1234,
            comm="python3",
            rss=100 * 1024 * 1024,
            pss=80 * 1024 * 1024,
            uss=60 * 1024 * 1024,
            swap=0,
        )
        
        self.assertEqual(pm.pid, 1234)
        self.assertEqual(pm.comm, "python3")
        self.assertEqual(pm.rss, 100 * 1024 * 1024)
        self.assertEqual(pm.pss, 80 * 1024 * 1024)
    
    def test_process_memory_to_dict(self):
        """Test converting to dictionary."""
        pm = ProcessMemory(
            pid=1234,
            comm="test",
            rss=1024,
            pss=512,
            uss=256,
        )
        
        try:
            from dataclasses import asdict
            d = asdict(pm)
            self.assertEqual(d["pid"], 1234)
            self.assertIn("rss", d)
        except (ImportError, TypeError):
            pass


@unittest.skipIf(TelemetrySnapshotter is None, "telemetry module not available")
class TestMemorySnapshot(unittest.TestCase):
    """Test the MemorySnapshot dataclass."""
    
    def test_create_snapshot(self):
        """Test creating a memory snapshot."""
        snapshot = MemorySnapshot(
            timestamp=datetime.now().isoformat(),
            system_total=16 * 1024 * 1024 * 1024,
            system_available=8 * 1024 * 1024 * 1024,
            system_used=8 * 1024 * 1024 * 1024,
            system_used_percent=50.0,
            system_cached=2 * 1024 * 1024 * 1024,
            system_buffers=256 * 1024 * 1024,
            swap_total=8 * 1024 * 1024 * 1024,
            swap_used=0,
            processes=[],
        )
        
        self.assertIsNotNone(snapshot.timestamp)
        self.assertEqual(snapshot.system_used_percent, 50.0)
        self.assertEqual(snapshot.swap_used, 0)


@unittest.skipIf(TelemetrySnapshotter is None, "telemetry module not available")
class TestSnapshotComparison(unittest.TestCase):
    """Test comparing baseline and post snapshots."""
    
    def setUp(self):
        """Create test snapshots."""
        self.baseline = MemorySnapshot(
            timestamp=datetime.now().isoformat(),
            system_total=16 * 1024 * 1024 * 1024,
            system_available=4 * 1024 * 1024 * 1024,
            system_used=12 * 1024 * 1024 * 1024,
            system_used_percent=75.0,
            system_cached=2 * 1024 * 1024 * 1024,
            system_buffers=256 * 1024 * 1024,
            swap_total=8 * 1024 * 1024 * 1024,
            swap_used=1 * 1024 * 1024 * 1024,
            processes=[
                ProcessMemory(pid=1, comm="init", rss=100*1024*1024, pss=90*1024*1024, uss=80*1024*1024),
                ProcessMemory(pid=2, comm="test", rss=500*1024*1024, pss=450*1024*1024, uss=400*1024*1024),
            ],
        )
        
        self.post = MemorySnapshot(
            timestamp=datetime.now().isoformat(),
            system_total=16 * 1024 * 1024 * 1024,
            system_available=6 * 1024 * 1024 * 1024,
            system_used=10 * 1024 * 1024 * 1024,
            system_used_percent=62.5,
            system_cached=1 * 1024 * 1024 * 1024,
            system_buffers=128 * 1024 * 1024,
            swap_total=8 * 1024 * 1024 * 1024,
            swap_used=512 * 1024 * 1024,
            processes=[
                ProcessMemory(pid=1, comm="init", rss=80*1024*1024, pss=70*1024*1024, uss=60*1024*1024),
                ProcessMemory(pid=2, comm="test", rss=400*1024*1024, pss=350*1024*1024, uss=300*1024*1024),
            ],
        )
    
    def test_create_comparison(self):
        """Test creating a snapshot comparison."""
        comparison = SnapshotComparison(
            baseline=self.baseline,
            post=self.post,
        )
        
        self.assertEqual(comparison.baseline, self.baseline)
        self.assertEqual(comparison.post, self.post)
    
    def test_calculate_reduction(self):
        """Test calculating memory reduction."""
        comparison = SnapshotComparison(
            baseline=self.baseline,
            post=self.post,
        )
        
        # System reduction: 12GB - 10GB = 2GB
        expected_reduction = 2 * 1024 * 1024 * 1024
        self.assertEqual(
            comparison.system_used_delta(),
            -expected_reduction  # Negative means reduction
        )
    
    def test_reduction_percentage(self):
        """Test calculating reduction percentage."""
        comparison = SnapshotComparison(
            baseline=self.baseline,
            post=self.post,
        )
        
        # 2GB / 12GB = 16.67%
        pct = comparison.system_reduction_percent()
        self.assertAlmostEqual(pct, 16.67, places=1)


@unittest.skipIf(TelemetrySnapshotter is None, "telemetry module not available")
class TestTelemetrySnapshotter(unittest.TestCase):
    """Test the TelemetrySnapshotter class."""
    
    def test_init(self):
        """Test initialization."""
        snapshotter = TelemetrySnapshotter()
        self.assertIsNotNone(snapshotter)
    
    def test_capture_system_memory(self):
        """Test capturing system memory info."""
        snapshotter = TelemetrySnapshotter()
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            snapshot = loop.run_until_complete(
                snapshotter.capture_snapshot()
            )
            
            self.assertIsInstance(snapshot, MemorySnapshot)
            self.assertGreater(snapshot.system_total, 0)
            self.assertGreaterEqual(snapshot.system_used_percent, 0)
            self.assertLessEqual(snapshot.system_used_percent, 100)
        finally:
            loop.close()
    
    def test_capture_with_processes(self):
        """Test capturing with process info."""
        snapshotter = TelemetrySnapshotter()
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            snapshot = loop.run_until_complete(
                snapshotter.capture_snapshot(include_processes=True, top_n=10)
            )
            
            self.assertIsInstance(snapshot.processes, list)
            # Should capture at least some processes
            if len(snapshot.processes) > 0:
                self.assertIsInstance(snapshot.processes[0], ProcessMemory)
        finally:
            loop.close()


@unittest.skipIf(TelemetrySnapshotter is None, "telemetry module not available")
class TestSnapshotStore(unittest.TestCase):
    """Test the SnapshotStore class."""
    
    def setUp(self):
        """Set up test database."""
        self.temp_dir = tempfile.mkdtemp()
        self.db_path = Path(self.temp_dir) / "snapshots.db"
        self.store = SnapshotStore(db_path=self.db_path)
    
    def tearDown(self):
        """Clean up test database."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    def test_init_creates_db(self):
        """Test that initialization creates the database."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            loop.run_until_complete(self.store.initialize())
            self.assertTrue(self.db_path.exists())
        finally:
            loop.close()
    
    def test_save_and_load_snapshot(self):
        """Test saving and loading a snapshot."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            loop.run_until_complete(self.store.initialize())
            
            # Create a test snapshot
            snapshot = MemorySnapshot(
                timestamp=datetime.now().isoformat(),
                system_total=16 * 1024 * 1024 * 1024,
                system_available=8 * 1024 * 1024 * 1024,
                system_used=8 * 1024 * 1024 * 1024,
                system_used_percent=50.0,
                system_cached=2 * 1024 * 1024 * 1024,
                system_buffers=256 * 1024 * 1024,
                swap_total=0,
                swap_used=0,
                processes=[],
            )
            
            # Save it
            snapshot_id = loop.run_until_complete(
                self.store.save_snapshot(snapshot, snapshot_type="baseline")
            )
            
            self.assertIsNotNone(snapshot_id)
            
            # Load it back
            loaded = loop.run_until_complete(
                self.store.get_latest_snapshot(snapshot_type="baseline")
            )
            
            self.assertIsNotNone(loaded)
            self.assertEqual(loaded.system_used_percent, 50.0)
        finally:
            loop.close()


if __name__ == "__main__":
    unittest.main()
