"""
Database Module

SQLite persistence for audit logs and proof artifacts.
Uses an append-only log for audit entries to ensure integrity.

Safety: The audit log is designed to be tamper-evident.
Each entry includes a hash of the previous entry.
"""

import asyncio
import aiosqlite
import json
import hashlib
import logging
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, List, Optional

logger = logging.getLogger(__name__)


class Database:
    """
    SQLite database for Grey Optimizer persistence.
    
    Stores:
    - Audit log of all enforcement actions
    - Proof artifacts with signatures
    - Configuration snapshots
    - Metric baselines
    """
    
    def __init__(self, db_path: str):
        """
        Initialize the database.
        
        Args:
            db_path: Path to SQLite database file
        """
        self.db_path = db_path
        self._connection: Optional[aiosqlite.Connection] = None
        self._lock = asyncio.Lock()
        
        # Ensure directory exists
        Path(db_path).parent.mkdir(parents=True, exist_ok=True)
        
        logger.debug(f"Database initialized: {db_path}")
    
    async def initialize(self) -> None:
        """
        Initialize database connection and schema.
        
        Creates tables if they don't exist.
        """
        self._connection = await aiosqlite.connect(self.db_path)
        
        # Enable foreign keys
        await self._connection.execute("PRAGMA foreign_keys = ON")
        
        # Create schema
        await self._connection.executescript("""
            -- Append-only audit log
            -- Each entry hashes the previous for tamper-evidence
            CREATE TABLE IF NOT EXISTS audit_log (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp TEXT NOT NULL,
                action TEXT NOT NULL,
                subsystem TEXT NOT NULL,
                parameters TEXT NOT NULL,
                outcome TEXT NOT NULL,
                details TEXT,
                prev_hash TEXT,
                entry_hash TEXT NOT NULL
            );
            
            -- Index for efficient timestamp queries
            CREATE INDEX IF NOT EXISTS idx_audit_timestamp 
                ON audit_log(timestamp);
            
            -- Index for action type queries
            CREATE INDEX IF NOT EXISTS idx_audit_action 
                ON audit_log(action);
            
            -- Proof artifacts
            CREATE TABLE IF NOT EXISTS proofs (
                id TEXT PRIMARY KEY,
                timestamp TEXT NOT NULL,
                subsystem TEXT NOT NULL,
                baseline TEXT NOT NULL,
                post_enforcement TEXT NOT NULL,
                reduction_percent REAL NOT NULL,
                signature TEXT NOT NULL,
                exported INTEGER DEFAULT 0
            );
            
            -- Index for proof queries
            CREATE INDEX IF NOT EXISTS idx_proofs_timestamp 
                ON proofs(timestamp);
            
            -- Configuration snapshots
            CREATE TABLE IF NOT EXISTS config_snapshots (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp TEXT NOT NULL,
                config TEXT NOT NULL,
                reason TEXT
            );
            
            -- Metric baselines
            CREATE TABLE IF NOT EXISTS baselines (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp TEXT NOT NULL,
                metrics TEXT NOT NULL
            );
        """)
        
        await self._connection.commit()
        logger.info("Database schema initialized")
    
    async def close(self) -> None:
        """Close database connection."""
        if self._connection:
            await self._connection.close()
            self._connection = None
            logger.debug("Database connection closed")
    
    async def log_action(
        self,
        action: str,
        subsystem: str,
        parameters: Dict[str, Any],
        outcome: str,
        details: Optional[Dict[str, Any]] = None
    ) -> int:
        """
        Log an enforcement action to the audit log.
        
        The audit log is append-only and each entry includes
        a hash of the previous entry for tamper-evidence.
        
        Args:
            action: Type of action (e.g., "cpu_cgroup_limit")
            subsystem: Subsystem ("cpu", "ram", "disk", "core")
            parameters: Action parameters
            outcome: "success", "failure", or "skipped"
            details: Additional details
            
        Returns:
            ID of the created log entry
        """
        async with self._lock:
            timestamp = datetime.utcnow().isoformat()
            params_json = json.dumps(parameters)
            details_json = json.dumps(details) if details else None
            
            # Get previous entry hash for chain
            cursor = await self._connection.execute(
                "SELECT entry_hash FROM audit_log ORDER BY id DESC LIMIT 1"
            )
            row = await cursor.fetchone()
            prev_hash = row[0] if row else "genesis"
            
            # Calculate entry hash
            entry_data = f"{timestamp}|{action}|{subsystem}|{params_json}|{outcome}|{prev_hash}"
            entry_hash = hashlib.sha256(entry_data.encode()).hexdigest()
            
            # Insert entry
            cursor = await self._connection.execute(
                """
                INSERT INTO audit_log 
                    (timestamp, action, subsystem, parameters, outcome, details, prev_hash, entry_hash)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (timestamp, action, subsystem, params_json, outcome, details_json, prev_hash, entry_hash)
            )
            
            await self._connection.commit()
            
            entry_id = cursor.lastrowid
            logger.debug(f"Logged action: {action} ({outcome}) -> #{entry_id}")
            
            return entry_id
    
    async def get_audit_log(self, limit: int = 100) -> List[Dict[str, Any]]:
        """
        Get recent audit log entries.
        
        Args:
            limit: Maximum entries to return
            
        Returns:
            List of audit log entries (most recent first)
        """
        cursor = await self._connection.execute(
            """
            SELECT id, timestamp, action, subsystem, parameters, outcome, details, entry_hash
            FROM audit_log
            ORDER BY id DESC
            LIMIT ?
            """,
            (limit,)
        )
        
        rows = await cursor.fetchall()
        
        return [
            {
                "id": row[0],
                "timestamp": row[1],
                "action": row[2],
                "subsystem": row[3],
                "parameters": json.loads(row[4]),
                "outcome": row[5],
                "details": json.loads(row[6]) if row[6] else None,
                "hash": row[7]
            }
            for row in rows
        ]
    
    async def verify_audit_chain(self) -> Dict[str, Any]:
        """
        Verify the integrity of the audit log chain.
        
        Returns:
            Verification result with any broken links
        """
        cursor = await self._connection.execute(
            """
            SELECT id, timestamp, action, subsystem, parameters, outcome, prev_hash, entry_hash
            FROM audit_log
            ORDER BY id ASC
            """
        )
        
        rows = await cursor.fetchall()
        
        if not rows:
            return {"valid": True, "entries": 0, "broken_links": []}
        
        broken_links = []
        expected_prev = "genesis"
        
        for row in rows:
            entry_id, timestamp, action, subsystem, parameters, outcome, prev_hash, entry_hash = row
            
            # Check prev_hash chain
            if prev_hash != expected_prev:
                broken_links.append({
                    "id": entry_id,
                    "expected_prev": expected_prev,
                    "actual_prev": prev_hash
                })
            
            # Verify entry hash
            entry_data = f"{timestamp}|{action}|{subsystem}|{parameters}|{outcome}|{prev_hash}"
            calculated_hash = hashlib.sha256(entry_data.encode()).hexdigest()
            
            if calculated_hash != entry_hash:
                broken_links.append({
                    "id": entry_id,
                    "error": "hash_mismatch",
                    "expected": calculated_hash,
                    "actual": entry_hash
                })
            
            expected_prev = entry_hash
        
        return {
            "valid": len(broken_links) == 0,
            "entries": len(rows),
            "broken_links": broken_links
        }
    
    async def save_proof(
        self,
        proof_id: str,
        subsystem: str,
        baseline: Dict[str, Any],
        post_enforcement: Dict[str, Any],
        reduction_percent: float,
        signature: str
    ) -> None:
        """
        Save a proof artifact to the database.
        
        Args:
            proof_id: Unique proof identifier
            subsystem: Subsystem the proof is for
            baseline: Baseline metrics
            post_enforcement: Post-enforcement metrics
            reduction_percent: Calculated reduction
            signature: HMAC signature
        """
        async with self._lock:
            timestamp = datetime.utcnow().isoformat()
            
            await self._connection.execute(
                """
                INSERT OR REPLACE INTO proofs 
                    (id, timestamp, subsystem, baseline, post_enforcement, reduction_percent, signature)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    proof_id,
                    timestamp,
                    subsystem,
                    json.dumps(baseline),
                    json.dumps(post_enforcement),
                    reduction_percent,
                    signature
                )
            )
            
            await self._connection.commit()
            logger.debug(f"Saved proof: {proof_id}")
    
    async def get_proofs(self, limit: int = 100) -> List[Dict[str, Any]]:
        """
        Get proof artifacts.
        
        Args:
            limit: Maximum proofs to return
            
        Returns:
            List of proof summaries
        """
        cursor = await self._connection.execute(
            """
            SELECT id, timestamp, subsystem, reduction_percent, exported
            FROM proofs
            ORDER BY timestamp DESC
            LIMIT ?
            """,
            (limit,)
        )
        
        rows = await cursor.fetchall()
        
        return [
            {
                "id": row[0],
                "timestamp": row[1],
                "subsystem": row[2],
                "reduction_percent": row[3],
                "exported": bool(row[4])
            }
            for row in rows
        ]
    
    async def get_proof(self, proof_id: str) -> Optional[Dict[str, Any]]:
        """
        Get a specific proof artifact.
        
        Args:
            proof_id: Proof identifier
            
        Returns:
            Full proof data or None if not found
        """
        cursor = await self._connection.execute(
            """
            SELECT id, timestamp, subsystem, baseline, post_enforcement, 
                   reduction_percent, signature, exported
            FROM proofs
            WHERE id = ?
            """,
            (proof_id,)
        )
        
        row = await cursor.fetchone()
        
        if not row:
            return None
        
        return {
            "id": row[0],
            "timestamp": row[1],
            "subsystem": row[2],
            "baseline": json.loads(row[3]),
            "post_enforcement": json.loads(row[4]),
            "reduction_percent": row[5],
            "signature": row[6],
            "exported": bool(row[7])
        }
    
    async def save_baseline(self, metrics: Dict[str, Any]) -> int:
        """
        Save a baseline metrics snapshot.
        
        Args:
            metrics: Baseline metrics
            
        Returns:
            ID of the saved baseline
        """
        async with self._lock:
            timestamp = datetime.utcnow().isoformat()
            
            cursor = await self._connection.execute(
                """
                INSERT INTO baselines (timestamp, metrics)
                VALUES (?, ?)
                """,
                (timestamp, json.dumps(metrics))
            )
            
            await self._connection.commit()
            return cursor.lastrowid
    
    async def get_latest_baseline(self) -> Optional[Dict[str, Any]]:
        """
        Get the most recent baseline.
        
        Returns:
            Baseline metrics or None if none exist
        """
        cursor = await self._connection.execute(
            """
            SELECT timestamp, metrics
            FROM baselines
            ORDER BY id DESC
            LIMIT 1
            """
        )
        
        row = await cursor.fetchone()
        
        if not row:
            return None
        
        return {
            "timestamp": row[0],
            "metrics": json.loads(row[1])
        }
