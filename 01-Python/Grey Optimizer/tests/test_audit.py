#!/usr/bin/env python3
"""
Unit tests for daemon/audit.py

Tests the AuditDatabase class with HMAC-signed checkpoints
and tamper-evident audit logging.
"""

import asyncio
import hashlib
import hmac
import json
import os
import sqlite3
import tempfile
import unittest
from datetime import datetime
from pathlib import Path

import sys
sys.path.insert(0, str(Path(__file__).parent.parent / "daemon"))

try:
    from audit import (
        AuditEntry,
        Checkpoint,
        HMACKeyManager,
        AuditDatabase,
    )
except ImportError:
    AuditDatabase = None


@unittest.skipIf(AuditDatabase is None, "audit module not available")
class TestHMACKeyManager(unittest.TestCase):
    """Test the HMACKeyManager class."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.key_file = Path(self.temp_dir) / "hmac.key"
    
    def tearDown(self):
        """Clean up test fixtures."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    def test_generate_key(self):
        """Test generating HMAC key."""
        manager = HMACKeyManager(key_file=self.key_file)
        key = manager.get_or_create_key()
        
        self.assertIsInstance(key, bytes)
        self.assertEqual(len(key), 32)  # 256-bit key
    
    def test_key_persistence(self):
        """Test that key persists across instances."""
        manager1 = HMACKeyManager(key_file=self.key_file)
        key1 = manager1.get_or_create_key()
        
        manager2 = HMACKeyManager(key_file=self.key_file)
        key2 = manager2.get_or_create_key()
        
        self.assertEqual(key1, key2)
    
    def test_sign_data(self):
        """Test signing data with HMAC."""
        manager = HMACKeyManager(key_file=self.key_file)
        data = "test data to sign"
        signature = manager.sign(data)
        
        self.assertIsInstance(signature, str)
        self.assertEqual(len(signature), 64)  # SHA256 hex
    
    def test_verify_signature(self):
        """Test verifying HMAC signature."""
        manager = HMACKeyManager(key_file=self.key_file)
        data = "test data to sign"
        signature = manager.sign(data)
        
        self.assertTrue(manager.verify(data, signature))
        self.assertFalse(manager.verify("tampered data", signature))


@unittest.skipIf(AuditDatabase is None, "audit module not available")
class TestAuditEntry(unittest.TestCase):
    """Test the AuditEntry dataclass."""
    
    def test_create_entry(self):
        """Test creating an audit entry."""
        entry = AuditEntry(
            action="apply_cgroup",
            category="enforcement",
            details="Set memory limit to 8GB",
            mode="live",
            user="root",
            hostname="server01",
            success=True,
        )
        
        self.assertEqual(entry.action, "apply_cgroup")
        self.assertEqual(entry.category, "enforcement")
        self.assertTrue(entry.success)
    
    def test_entry_timestamp(self):
        """Test that entry gets a timestamp."""
        entry = AuditEntry(
            action="test",
            category="test",
        )
        
        # Timestamp should be set
        self.assertIsNotNone(entry.timestamp)


@unittest.skipIf(AuditDatabase is None, "audit module not available")
class TestCheckpoint(unittest.TestCase):
    """Test the Checkpoint dataclass."""
    
    def test_create_checkpoint(self):
        """Test creating a checkpoint."""
        checkpoint = Checkpoint(
            id=1,
            timestamp=datetime.now().isoformat(),
            entry_count=100,
            last_entry_id=100,
            hash_chain="abc123...",
            signature="def456...",
        )
        
        self.assertEqual(checkpoint.id, 1)
        self.assertEqual(checkpoint.entry_count, 100)


@unittest.skipIf(AuditDatabase is None, "audit module not available")
class TestAuditDatabase(unittest.TestCase):
    """Test the AuditDatabase class."""
    
    def setUp(self):
        """Set up test database."""
        self.temp_dir = tempfile.mkdtemp()
        self.db_path = Path(self.temp_dir) / "audit.db"
        self.key_file = Path(self.temp_dir) / "hmac.key"
        self.db = AuditDatabase(
            db_path=self.db_path,
            key_file=self.key_file,
        )
    
    def tearDown(self):
        """Clean up test database."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    def test_init_creates_db(self):
        """Test that initialization creates the database."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            loop.run_until_complete(self.db.initialize())
            self.assertTrue(self.db_path.exists())
        finally:
            loop.close()
    
    def test_log_entry(self):
        """Test logging an audit entry."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            loop.run_until_complete(self.db.initialize())
            
            entry_id = loop.run_until_complete(
                self.db.log(
                    action="test_action",
                    category="test",
                    details="Test details",
                    mode="simulation",
                    success=True,
                )
            )
            
            self.assertIsNotNone(entry_id)
            self.assertGreater(entry_id, 0)
        finally:
            loop.close()
    
    def test_log_multiple_entries(self):
        """Test logging multiple entries."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            loop.run_until_complete(self.db.initialize())
            
            ids = []
            for i in range(10):
                entry_id = loop.run_until_complete(
                    self.db.log(
                        action=f"action_{i}",
                        category="test",
                        success=True,
                    )
                )
                ids.append(entry_id)
            
            # IDs should be sequential
            self.assertEqual(len(ids), 10)
            for i in range(1, 10):
                self.assertEqual(ids[i], ids[i-1] + 1)
        finally:
            loop.close()
    
    def test_create_checkpoint(self):
        """Test creating a checkpoint."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            loop.run_until_complete(self.db.initialize())
            
            # Log some entries first
            for i in range(5):
                loop.run_until_complete(
                    self.db.log(action=f"action_{i}", category="test")
                )
            
            # Create checkpoint
            checkpoint = loop.run_until_complete(
                self.db.create_checkpoint()
            )
            
            self.assertIsNotNone(checkpoint)
            self.assertEqual(checkpoint.entry_count, 5)
            self.assertIsNotNone(checkpoint.signature)
        finally:
            loop.close()
    
    def test_verify_checkpoint(self):
        """Test verifying a checkpoint."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            loop.run_until_complete(self.db.initialize())
            
            # Log some entries
            for i in range(5):
                loop.run_until_complete(
                    self.db.log(action=f"action_{i}", category="test")
                )
            
            # Create checkpoint
            checkpoint = loop.run_until_complete(
                self.db.create_checkpoint()
            )
            
            # Verify checkpoint
            is_valid = loop.run_until_complete(
                self.db.verify_checkpoint(checkpoint.id)
            )
            
            self.assertTrue(is_valid)
        finally:
            loop.close()
    
    def test_get_entries(self):
        """Test getting audit entries."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            loop.run_until_complete(self.db.initialize())
            
            # Log some entries
            for i in range(10):
                loop.run_until_complete(
                    self.db.log(
                        action=f"action_{i}",
                        category="enforcement" if i % 2 == 0 else "health",
                    )
                )
            
            # Get all entries
            entries = loop.run_until_complete(
                self.db.get_entries(limit=100)
            )
            
            self.assertEqual(len(entries), 10)
            
            # Get entries by category
            enforcement_entries = loop.run_until_complete(
                self.db.get_entries(category="enforcement")
            )
            
            self.assertEqual(len(enforcement_entries), 5)
        finally:
            loop.close()
    
    def test_export_json(self):
        """Test exporting audit log as JSON."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            loop.run_until_complete(self.db.initialize())
            
            # Log some entries
            loop.run_until_complete(
                self.db.log(action="test", category="test", details="details")
            )
            
            # Export to JSON
            json_str = loop.run_until_complete(
                self.db.export_json()
            )
            
            self.assertIsInstance(json_str, str)
            data = json.loads(json_str)
            self.assertIn("entries", data)
            self.assertEqual(len(data["entries"]), 1)
        finally:
            loop.close()


@unittest.skipIf(AuditDatabase is None, "audit module not available")
class TestAuditTamperDetection(unittest.TestCase):
    """Test tamper detection in audit log."""
    
    def setUp(self):
        """Set up test database."""
        self.temp_dir = tempfile.mkdtemp()
        self.db_path = Path(self.temp_dir) / "audit.db"
        self.key_file = Path(self.temp_dir) / "hmac.key"
        self.db = AuditDatabase(
            db_path=self.db_path,
            key_file=self.key_file,
        )
    
    def tearDown(self):
        """Clean up test database."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)
    
    def test_detect_modified_entry(self):
        """Test that modifying an entry is detected."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        try:
            loop.run_until_complete(self.db.initialize())
            
            # Log entries and create checkpoint
            for i in range(5):
                loop.run_until_complete(
                    self.db.log(action=f"action_{i}", category="test")
                )
            
            checkpoint = loop.run_until_complete(
                self.db.create_checkpoint()
            )
            
            # Tamper with an entry directly in the database
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            cursor.execute("UPDATE audit_log SET action = 'tampered' WHERE id = 3")
            conn.commit()
            conn.close()
            
            # Verification should fail
            is_valid = loop.run_until_complete(
                self.db.verify_checkpoint(checkpoint.id)
            )
            
            self.assertFalse(is_valid)
        finally:
            loop.close()


if __name__ == "__main__":
    unittest.main()
