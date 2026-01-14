"""
Grey Optimizer - Audit Module

Append-only SQLite audit database with HMAC-signed checkpoints.

Security Properties:
- Append-only: Entries cannot be modified after creation
- HMAC-signed: Checkpoints are cryptographically signed
- Tamper-evident: Any modification invalidates signatures
- Exportable: Full audit trail can be exported as JSON

This module provides the trust anchor for proof artifacts.
"""

import asyncio
import hashlib
import hmac
import json
import logging
import os
import secrets
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

logger = logging.getLogger(__name__)

# Artifacts directory
ARTIFACTS_DIR = Path(os.environ.get(
    "GREY_ARTIFACTS_DIR",
    "/var/lib/grey-optimizer/artifacts"
))

# Default HMAC key path
HMAC_KEY_PATH = Path(os.environ.get(
    "GREY_HMAC_KEY_PATH",
    "/var/lib/grey-optimizer/.hmac_key"
))


@dataclass
class AuditEntry:
    """A single audit log entry."""
    id: Optional[int] = None
    timestamp: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    action: str = ""
    subsystem: str = ""
    parameters: Dict[str, Any] = field(default_factory=dict)
    outcome: str = ""  # "success", "failure", "simulated"
    details: Dict[str, Any] = field(default_factory=dict)
    user: str = ""
    session_id: str = ""
    simulated: bool = False  # True if this was a simulation run
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "timestamp": self.timestamp.isoformat(),
            "action": self.action,
            "subsystem": self.subsystem,
            "parameters": self.parameters,
            "outcome": self.outcome,
            "details": self.details,
            "user": self.user,
            "session_id": self.session_id,
            "simulated": self.simulated,
        }


@dataclass
class ArtifactRecord:
    """
    Record of a signed proof artifact.
    
    Live artifacts are HMAC-signed for tamper evidence.
    Simulation artifacts are explicitly marked as unsigned.
    """
    id: Optional[int] = None
    timestamp: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    action_id: str = ""  # Links to action that created this artifact
    artifact_path: str = ""  # Path to the artifact file
    artifact_type: str = ""  # "before_snapshot", "after_snapshot", "comparison", etc.
    content_hash: str = ""  # SHA-256 of file contents
    hmac_signature: str = ""  # HMAC-SHA256 signature (empty if simulated)
    simulated: bool = False  # True if simulation artifact (unsigned)
    file_size: int = 0
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "timestamp": self.timestamp.isoformat(),
            "action_id": self.action_id,
            "artifact_path": self.artifact_path,
            "artifact_type": self.artifact_type,
            "content_hash": self.content_hash,
            "hmac_signature": self.hmac_signature,
            "simulated": self.simulated,
            "file_size": self.file_size,
            "signed": bool(self.hmac_signature) and not self.simulated,
            "metadata": self.metadata,
        }


@dataclass
class Checkpoint:
    """
    HMAC-signed checkpoint for audit verification.
    
    A checkpoint captures the state of the audit log at a point in time
    and signs it to detect tampering.
    """
    id: Optional[int] = None
    timestamp: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    entry_count: int = 0
    last_entry_id: int = 0
    entries_hash: str = ""  # SHA-256 of all entries up to this point
    hmac_signature: str = ""  # HMAC-SHA256 of the checkpoint data
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "timestamp": self.timestamp.isoformat(),
            "entry_count": self.entry_count,
            "last_entry_id": self.last_entry_id,
            "entries_hash": self.entries_hash,
            "hmac_signature": self.hmac_signature,
            "metadata": self.metadata,
        }
    
    def compute_signature_data(self) -> str:
        """Get the data to be signed."""
        return json.dumps({
            "timestamp": self.timestamp.isoformat(),
            "entry_count": self.entry_count,
            "last_entry_id": self.last_entry_id,
            "entries_hash": self.entries_hash,
        }, sort_keys=True)


class HMACKeyManager:
    """Manages the HMAC key for signing checkpoints."""
    
    def __init__(self, key_path: Path = HMAC_KEY_PATH):
        self.key_path = key_path
        self._key: Optional[bytes] = None
    
    def get_key(self) -> bytes:
        """
        Get the HMAC key, generating if needed.
        
        The key is stored in a file with restricted permissions.
        """
        if self._key is not None:
            return self._key
        
        if self.key_path.exists():
            self._key = self.key_path.read_bytes()
            return self._key
        
        # Generate new key
        self._key = secrets.token_bytes(32)
        
        # Ensure directory exists
        self.key_path.parent.mkdir(parents=True, exist_ok=True)
        
        # Write with restricted permissions
        self.key_path.write_bytes(self._key)
        os.chmod(self.key_path, 0o600)
        
        logger.info(f"Generated new HMAC key at {self.key_path}")
        return self._key
    
    def sign(self, data: str) -> str:
        """Sign data with HMAC-SHA256."""
        key = self.get_key()
        signature = hmac.new(key, data.encode(), hashlib.sha256).hexdigest()
        return signature
    
    def verify(self, data: str, signature: str) -> bool:
        """Verify an HMAC signature."""
        expected = self.sign(data)
        return hmac.compare_digest(expected, signature)


class AuditDatabase:
    """
    Append-only audit database with HMAC-signed checkpoints.
    
    This database is designed for tamper-evident logging of all
    Grey Optimizer actions.
    """
    
    def __init__(
        self,
        db_path: Path,
        hmac_key_manager: Optional[HMACKeyManager] = None,
    ):
        """
        Initialize the audit database.
        
        Args:
            db_path: Path to SQLite database file
            hmac_key_manager: Optional custom key manager
        """
        self.db_path = db_path
        self.hmac = hmac_key_manager or HMACKeyManager()
        self._session_id = secrets.token_hex(8)
        self._initialized = False
    
    async def initialize(self) -> None:
        """Initialize the database schema."""
        import aiosqlite
        
        # Ensure directory exists
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        
        async with aiosqlite.connect(self.db_path) as db:
            # Audit entries table
            await db.execute("""
                CREATE TABLE IF NOT EXISTS audit_entries (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    timestamp TEXT NOT NULL,
                    action TEXT NOT NULL,
                    subsystem TEXT NOT NULL,
                    parameters TEXT NOT NULL,
                    outcome TEXT NOT NULL,
                    details TEXT NOT NULL,
                    user TEXT,
                    session_id TEXT,
                    simulated INTEGER DEFAULT 0,
                    created_at TEXT DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            # Checkpoints table
            await db.execute("""
                CREATE TABLE IF NOT EXISTS checkpoints (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    timestamp TEXT NOT NULL,
                    entry_count INTEGER NOT NULL,
                    last_entry_id INTEGER NOT NULL,
                    entries_hash TEXT NOT NULL,
                    hmac_signature TEXT NOT NULL,
                    metadata TEXT,
                    created_at TEXT DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            # Artifacts table for proof artifact tracking
            await db.execute("""
                CREATE TABLE IF NOT EXISTS artifacts (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    timestamp TEXT NOT NULL,
                    action_id TEXT NOT NULL,
                    artifact_path TEXT NOT NULL,
                    artifact_type TEXT NOT NULL,
                    content_hash TEXT NOT NULL,
                    hmac_signature TEXT,
                    simulated INTEGER DEFAULT 0,
                    file_size INTEGER,
                    metadata TEXT,
                    created_at TEXT DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            # Indexes
            await db.execute("""
                CREATE INDEX IF NOT EXISTS idx_audit_timestamp 
                ON audit_entries(timestamp)
            """)
            await db.execute("""
                CREATE INDEX IF NOT EXISTS idx_audit_action 
                ON audit_entries(action)
            """)
            await db.execute("""
                CREATE INDEX IF NOT EXISTS idx_audit_subsystem 
                ON audit_entries(subsystem)
            """)
            await db.execute("""
                CREATE INDEX IF NOT EXISTS idx_audit_session 
                ON audit_entries(session_id)
            """)
            
            # Artifact indexes
            await db.execute("""
                CREATE INDEX IF NOT EXISTS idx_artifact_action 
                ON artifacts(action_id)
            """)
            await db.execute("""
                CREATE INDEX IF NOT EXISTS idx_artifact_type 
                ON artifacts(artifact_type)
            """)
            
            # Schema migrations for existing databases
            # Add 'simulated' column to audit_entries if it doesn't exist
            try:
                await db.execute("""
                    ALTER TABLE audit_entries ADD COLUMN simulated INTEGER DEFAULT 0
                """)
                logger.info("Migrated audit_entries: added 'simulated' column")
            except Exception:
                pass  # Column already exists
            
            # Add 'simulated' column to artifacts if it doesn't exist
            try:
                await db.execute("""
                    ALTER TABLE artifacts ADD COLUMN simulated INTEGER DEFAULT 0
                """)
                logger.info("Migrated artifacts: added 'simulated' column")
            except Exception:
                pass  # Column already exists
            
            await db.commit()
        
        self._initialized = True
        logger.info(f"Audit database initialized: {self.db_path}")
    
    async def log(
        self,
        action: str,
        subsystem: str,
        parameters: Optional[Dict[str, Any]] = None,
        outcome: str = "success",
        details: Optional[Dict[str, Any]] = None,
        user: Optional[str] = None,
        simulated: bool = False,
    ) -> int:
        """
        Log an audit entry.
        
        Args:
            action: The action being logged (e.g., "apply_cgroup_limit")
            subsystem: The subsystem (e.g., "memory_reclaim")
            parameters: Action parameters
            outcome: "success", "failure", or "simulated"
            details: Additional details
            user: Optional user identifier
            simulated: True if this was a simulation run (not live)
            
        Returns:
            The entry ID
        """
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        entry = AuditEntry(
            timestamp=datetime.now(timezone.utc),
            action=action,
            subsystem=subsystem,
            parameters=parameters or {},
            outcome=outcome,
            details=details or {},
            user=user or os.environ.get("USER", "unknown"),
            session_id=self._session_id,
            simulated=simulated,
        )
        
        async with aiosqlite.connect(self.db_path) as db:
            cursor = await db.execute(
                """INSERT INTO audit_entries 
                   (timestamp, action, subsystem, parameters, outcome, details, user, session_id, simulated)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)""",
                (
                    entry.timestamp.isoformat(),
                    entry.action,
                    entry.subsystem,
                    json.dumps(entry.parameters),
                    entry.outcome,
                    json.dumps(entry.details),
                    entry.user,
                    entry.session_id,
                    1 if entry.simulated else 0,
                )
            )
            await db.commit()
            entry_id = cursor.lastrowid
        
        logger.debug(f"Audit log: {action} ({subsystem}) -> {outcome} [simulated={simulated}]")
        return entry_id
    
    async def create_checkpoint(
        self,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> Checkpoint:
        """
        Create an HMAC-signed checkpoint.
        
        A checkpoint cryptographically commits to the current state
        of the audit log, making tampering evident.
        
        Args:
            metadata: Optional metadata to include in checkpoint
            
        Returns:
            The created Checkpoint
        """
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        async with aiosqlite.connect(self.db_path) as db:
            # Get entry count and last ID
            cursor = await db.execute(
                "SELECT COUNT(*), MAX(id) FROM audit_entries"
            )
            row = await cursor.fetchone()
            entry_count = row[0] or 0
            last_entry_id = row[1] or 0
            
            # Compute hash of all entries
            cursor = await db.execute(
                "SELECT id, timestamp, action, subsystem, parameters, outcome "
                "FROM audit_entries ORDER BY id"
            )
            rows = await cursor.fetchall()
            
            hasher = hashlib.sha256()
            for row in rows:
                hasher.update(json.dumps(list(row)).encode())
            entries_hash = hasher.hexdigest()
            
            # Create checkpoint
            checkpoint = Checkpoint(
                timestamp=datetime.now(timezone.utc),
                entry_count=entry_count,
                last_entry_id=last_entry_id,
                entries_hash=entries_hash,
                metadata=metadata or {},
            )
            
            # Sign checkpoint
            signature_data = checkpoint.compute_signature_data()
            checkpoint.hmac_signature = self.hmac.sign(signature_data)
            
            # Save checkpoint
            cursor = await db.execute(
                """INSERT INTO checkpoints 
                   (timestamp, entry_count, last_entry_id, entries_hash, hmac_signature, metadata)
                   VALUES (?, ?, ?, ?, ?, ?)""",
                (
                    checkpoint.timestamp.isoformat(),
                    checkpoint.entry_count,
                    checkpoint.last_entry_id,
                    checkpoint.entries_hash,
                    checkpoint.hmac_signature,
                    json.dumps(checkpoint.metadata),
                )
            )
            await db.commit()
            checkpoint.id = cursor.lastrowid
        
        logger.info(f"Created checkpoint {checkpoint.id} with {entry_count} entries")
        return checkpoint
    
    async def verify_checkpoint(self, checkpoint_id: int) -> Dict[str, Any]:
        """
        Verify a checkpoint's integrity.
        
        Args:
            checkpoint_id: ID of checkpoint to verify
            
        Returns:
            Verification result with status and details
        """
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        async with aiosqlite.connect(self.db_path) as db:
            # Get checkpoint
            db.row_factory = aiosqlite.Row
            cursor = await db.execute(
                "SELECT * FROM checkpoints WHERE id = ?",
                (checkpoint_id,)
            )
            row = await cursor.fetchone()
            
            if not row:
                return {
                    "valid": False,
                    "error": f"Checkpoint {checkpoint_id} not found",
                }
            
            checkpoint = Checkpoint(
                id=row["id"],
                timestamp=datetime.fromisoformat(row["timestamp"]),
                entry_count=row["entry_count"],
                last_entry_id=row["last_entry_id"],
                entries_hash=row["entries_hash"],
                hmac_signature=row["hmac_signature"],
                metadata=json.loads(row["metadata"]) if row["metadata"] else {},
            )
            
            # Verify HMAC signature
            signature_data = checkpoint.compute_signature_data()
            signature_valid = self.hmac.verify(signature_data, checkpoint.hmac_signature)
            
            if not signature_valid:
                return {
                    "valid": False,
                    "checkpoint_id": checkpoint_id,
                    "error": "HMAC signature verification failed",
                }
            
            # Recompute entries hash
            cursor = await db.execute(
                "SELECT id, timestamp, action, subsystem, parameters, outcome "
                "FROM audit_entries WHERE id <= ? ORDER BY id",
                (checkpoint.last_entry_id,)
            )
            rows = await cursor.fetchall()
            
            hasher = hashlib.sha256()
            for row in rows:
                hasher.update(json.dumps([
                    row["id"], row["timestamp"], row["action"],
                    row["subsystem"], row["parameters"], row["outcome"]
                ]).encode())
            computed_hash = hasher.hexdigest()
            
            hash_valid = computed_hash == checkpoint.entries_hash
            
            if not hash_valid:
                return {
                    "valid": False,
                    "checkpoint_id": checkpoint_id,
                    "error": "Entries hash mismatch - audit log may have been tampered",
                    "expected_hash": checkpoint.entries_hash,
                    "computed_hash": computed_hash,
                }
            
            return {
                "valid": True,
                "checkpoint_id": checkpoint_id,
                "timestamp": checkpoint.timestamp.isoformat(),
                "entry_count": checkpoint.entry_count,
                "entries_hash": checkpoint.entries_hash,
            }
    
    async def verify_all_checkpoints(self) -> Dict[str, Any]:
        """Verify all checkpoints in the database."""
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        results = {
            "all_valid": True,
            "checkpoints": [],
            "errors": [],
        }
        
        async with aiosqlite.connect(self.db_path) as db:
            cursor = await db.execute(
                "SELECT id FROM checkpoints ORDER BY id"
            )
            rows = await cursor.fetchall()
            
            for row in rows:
                checkpoint_id = row[0]
                result = await self.verify_checkpoint(checkpoint_id)
                results["checkpoints"].append(result)
                
                if not result["valid"]:
                    results["all_valid"] = False
                    results["errors"].append(result.get("error"))
        
        return results
    
    async def export_json(
        self,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None,
        include_checkpoints: bool = True,
    ) -> Dict[str, Any]:
        """
        Export audit log as JSON.
        
        Args:
            start_date: Optional start date filter
            end_date: Optional end date filter
            include_checkpoints: Whether to include checkpoints
            
        Returns:
            Complete audit export with entries and checkpoints
        """
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            
            # Build query
            query = "SELECT * FROM audit_entries WHERE 1=1"
            params = []
            
            if start_date:
                query += " AND timestamp >= ?"
                params.append(start_date.isoformat())
            if end_date:
                query += " AND timestamp <= ?"
                params.append(end_date.isoformat())
            
            query += " ORDER BY id"
            
            cursor = await db.execute(query, params)
            rows = await cursor.fetchall()
            
            entries = []
            for row in rows:
                entries.append({
                    "id": row["id"],
                    "timestamp": row["timestamp"],
                    "action": row["action"],
                    "subsystem": row["subsystem"],
                    "parameters": json.loads(row["parameters"]),
                    "outcome": row["outcome"],
                    "details": json.loads(row["details"]),
                    "user": row["user"],
                    "session_id": row["session_id"],
                })
            
            export = {
                "export_timestamp": datetime.now(timezone.utc).isoformat(),
                "entry_count": len(entries),
                "entries": entries,
            }
            
            if include_checkpoints:
                cursor = await db.execute(
                    "SELECT * FROM checkpoints ORDER BY id"
                )
                rows = await cursor.fetchall()
                
                checkpoints = []
                for row in rows:
                    checkpoints.append({
                        "id": row["id"],
                        "timestamp": row["timestamp"],
                        "entry_count": row["entry_count"],
                        "last_entry_id": row["last_entry_id"],
                        "entries_hash": row["entries_hash"],
                        "hmac_signature": row["hmac_signature"],
                        "metadata": json.loads(row["metadata"]) if row["metadata"] else {},
                    })
                
                export["checkpoints"] = checkpoints
        
        return export
    
    async def get_recent_entries(
        self,
        limit: int = 50,
        action_filter: Optional[str] = None,
        subsystem_filter: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        """Get recent audit entries."""
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            
            query = "SELECT * FROM audit_entries WHERE 1=1"
            params = []
            
            if action_filter:
                query += " AND action = ?"
                params.append(action_filter)
            if subsystem_filter:
                query += " AND subsystem = ?"
                params.append(subsystem_filter)
            
            query += " ORDER BY id DESC LIMIT ?"
            params.append(limit)
            
            cursor = await db.execute(query, params)
            rows = await cursor.fetchall()
            
            return [
                {
                    "id": row["id"],
                    "timestamp": row["timestamp"],
                    "action": row["action"],
                    "subsystem": row["subsystem"],
                    "outcome": row["outcome"],
                    "user": row["user"],
                }
                for row in rows
            ]
    
    async def get_statistics(self) -> Dict[str, Any]:
        """Get audit database statistics."""
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        async with aiosqlite.connect(self.db_path) as db:
            # Total entries
            cursor = await db.execute("SELECT COUNT(*) FROM audit_entries")
            total_entries = (await cursor.fetchone())[0]
            
            # Total checkpoints
            cursor = await db.execute("SELECT COUNT(*) FROM checkpoints")
            total_checkpoints = (await cursor.fetchone())[0]
            
            # Entries by outcome
            cursor = await db.execute(
                "SELECT outcome, COUNT(*) FROM audit_entries GROUP BY outcome"
            )
            outcome_counts = dict(await cursor.fetchall())
            
            # Entries by subsystem
            cursor = await db.execute(
                "SELECT subsystem, COUNT(*) FROM audit_entries GROUP BY subsystem"
            )
            subsystem_counts = dict(await cursor.fetchall())
            
            # Latest entry
            cursor = await db.execute(
                "SELECT timestamp FROM audit_entries ORDER BY id DESC LIMIT 1"
            )
            row = await cursor.fetchone()
            latest_entry = row[0] if row else None
            
            # Latest checkpoint
            cursor = await db.execute(
                "SELECT timestamp FROM checkpoints ORDER BY id DESC LIMIT 1"
            )
            row = await cursor.fetchone()
            latest_checkpoint = row[0] if row else None
            
            return {
                "total_entries": total_entries,
                "total_checkpoints": total_checkpoints,
                "entries_by_outcome": outcome_counts,
                "entries_by_subsystem": subsystem_counts,
                "latest_entry": latest_entry,
                "latest_checkpoint": latest_checkpoint,
            }

    async def sign_artifact(
        self,
        artifact_path: Union[str, Path],
        action_id: str,
        artifact_type: str,
        simulated: bool = False,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> ArtifactRecord:
        """
        Sign and register a proof artifact.
        
        Live artifacts receive an HMAC signature for tamper evidence.
        Simulation artifacts are explicitly marked as unsigned.
        
        Args:
            artifact_path: Path to the artifact file
            action_id: ID of the action that created this artifact
            artifact_type: Type of artifact (e.g., "before_snapshot", "after_snapshot")
            simulated: True if this is a simulation artifact (will not be signed)
            metadata: Optional additional metadata
            
        Returns:
            ArtifactRecord with content hash and optional HMAC signature
        """
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        artifact_path = Path(artifact_path)
        
        if not artifact_path.exists():
            raise FileNotFoundError(f"Artifact not found: {artifact_path}")
        
        # Read file and compute content hash
        content = artifact_path.read_bytes()
        content_hash = hashlib.sha256(content).hexdigest()
        file_size = len(content)
        
        # Compute HMAC signature for live artifacts only
        hmac_signature = ""
        if not simulated:
            # Sign the content hash + action_id + artifact_type
            signature_data = json.dumps({
                "action_id": action_id,
                "artifact_type": artifact_type,
                "content_hash": content_hash,
                "file_size": file_size,
                "artifact_path": str(artifact_path),
            }, sort_keys=True)
            hmac_signature = self.hmac.sign(signature_data)
        
        # Create artifact record
        artifact = ArtifactRecord(
            timestamp=datetime.now(timezone.utc),
            action_id=action_id,
            artifact_path=str(artifact_path),
            artifact_type=artifact_type,
            content_hash=content_hash,
            hmac_signature=hmac_signature,
            simulated=simulated,
            file_size=file_size,
            metadata=metadata or {},
        )
        
        # Save to database
        async with aiosqlite.connect(self.db_path) as db:
            cursor = await db.execute(
                """INSERT INTO artifacts 
                   (timestamp, action_id, artifact_path, artifact_type, content_hash, 
                    hmac_signature, simulated, file_size, metadata)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)""",
                (
                    artifact.timestamp.isoformat(),
                    artifact.action_id,
                    artifact.artifact_path,
                    artifact.artifact_type,
                    artifact.content_hash,
                    artifact.hmac_signature,
                    1 if artifact.simulated else 0,
                    artifact.file_size,
                    json.dumps(artifact.metadata),
                )
            )
            await db.commit()
            artifact.id = cursor.lastrowid
        
        sign_status = "unsigned (simulation)" if simulated else "HMAC-signed"
        logger.info(f"Registered artifact {artifact.id}: {artifact_type} [{sign_status}]")
        return artifact

    async def verify_artifact(
        self,
        artifact_path: Union[str, Path],
    ) -> Dict[str, Any]:
        """
        Verify an artifact's integrity.
        
        Args:
            artifact_path: Path to the artifact file to verify
            
        Returns:
            Verification result with status and details
        """
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        artifact_path = Path(artifact_path)
        
        # Look up artifact record
        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            cursor = await db.execute(
                "SELECT * FROM artifacts WHERE artifact_path = ?",
                (str(artifact_path),)
            )
            row = await cursor.fetchone()
            
            if not row:
                return {
                    "valid": False,
                    "error": f"Artifact not registered: {artifact_path}",
                    "file_exists": artifact_path.exists(),
                }
            
            # Check if file still exists
            if not artifact_path.exists():
                return {
                    "valid": False,
                    "error": "Artifact file missing",
                    "artifact_path": str(artifact_path),
                    "registered_at": row["timestamp"],
                }
            
            # Recompute content hash
            content = artifact_path.read_bytes()
            computed_hash = hashlib.sha256(content).hexdigest()
            
            if computed_hash != row["content_hash"]:
                return {
                    "valid": False,
                    "error": "Content hash mismatch - artifact may have been tampered",
                    "expected_hash": row["content_hash"],
                    "computed_hash": computed_hash,
                }
            
            # For signed artifacts, verify HMAC
            simulated = bool(row["simulated"])
            if not simulated and row["hmac_signature"]:
                signature_data = json.dumps({
                    "action_id": row["action_id"],
                    "artifact_type": row["artifact_type"],
                    "content_hash": row["content_hash"],
                    "file_size": row["file_size"],
                    "artifact_path": row["artifact_path"],
                }, sort_keys=True)
                
                if not self.hmac.verify(signature_data, row["hmac_signature"]):
                    return {
                        "valid": False,
                        "error": "HMAC signature verification failed",
                    }
            
            return {
                "valid": True,
                "artifact_path": str(artifact_path),
                "action_id": row["action_id"],
                "artifact_type": row["artifact_type"],
                "content_hash": computed_hash,
                "file_size": len(content),
                "simulated": simulated,
                "signed": not simulated and bool(row["hmac_signature"]),
                "registered_at": row["timestamp"],
            }

    async def get_artifacts_for_action(
        self,
        action_id: str,
    ) -> List[Dict[str, Any]]:
        """Get all artifacts associated with an action."""
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            cursor = await db.execute(
                "SELECT * FROM artifacts WHERE action_id = ? ORDER BY id",
                (action_id,)
            )
            rows = await cursor.fetchall()
            
            return [
                {
                    "id": row["id"],
                    "timestamp": row["timestamp"],
                    "action_id": row["action_id"],
                    "artifact_path": row["artifact_path"],
                    "artifact_type": row["artifact_type"],
                    "content_hash": row["content_hash"],
                    "hmac_signature": row["hmac_signature"],
                    "simulated": bool(row["simulated"]),
                    "file_size": row["file_size"],
                    "signed": not row["simulated"] and bool(row["hmac_signature"]),
                }
                for row in rows
            ]

    async def verify_all_artifacts(self) -> Dict[str, Any]:
        """Verify all registered artifacts."""
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        results = {
            "all_valid": True,
            "total_artifacts": 0,
            "verified": 0,
            "failed": 0,
            "missing": 0,
            "simulated": 0,
            "errors": [],
        }
        
        async with aiosqlite.connect(self.db_path) as db:
            cursor = await db.execute("SELECT artifact_path FROM artifacts")
            rows = await cursor.fetchall()
            
            results["total_artifacts"] = len(rows)
            
            for row in rows:
                artifact_path = row[0]
                result = await self.verify_artifact(artifact_path)
                
                if result.get("valid"):
                    if result.get("simulated"):
                        results["simulated"] += 1
                    else:
                        results["verified"] += 1
                else:
                    results["all_valid"] = False
                    if "missing" in result.get("error", "").lower():
                        results["missing"] += 1
                    else:
                        results["failed"] += 1
                    results["errors"].append({
                        "artifact_path": artifact_path,
                        "error": result.get("error"),
                    })
        
        return results
