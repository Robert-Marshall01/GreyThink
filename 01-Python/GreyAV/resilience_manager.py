"""
Resilience Engine for GreyAV Antivirus/EDR System.

This module provides mechanisms to maintain baselines of critical system artifacts,
detect malicious alterations, and automatically roll back changes to restore safe states.
"""

import hashlib
import shutil
import subprocess
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional


@dataclass
class ResilienceEvent:
    """
    Represents a resilience action taken by the system.
    
    Attributes:
        event_id: Unique identifier for this resilience event.
        artifact_type: Type of artifact affected ("file", "registry", "process", "config").
        artifact_name: Name or path of the affected artifact.
        action_taken: Action performed ("rollback", "restore", "respawn", "quarantine").
        timestamp: When the resilience action was performed.
        description: Human-readable description of what occurred.
    """
    event_id: str
    artifact_type: str  # "file", "registry", "process", "config"
    artifact_name: str
    action_taken: str  # "rollback", "restore", "respawn", "quarantine"
    timestamp: datetime
    description: str
    
    @staticmethod
    def generate_id() -> str:
        """Generate a unique event ID."""
        return str(uuid.uuid4())


@dataclass
class ArtifactBaseline:
    """
    Stores baseline information for a monitored artifact.
    
    Attributes:
        artifact_type: Type of artifact.
        artifact_name: Name or identifier of the artifact.
        metadata: Dictionary containing baseline data (hash, content, state, etc.).
        recorded_at: When the baseline was recorded.
        backup_path: Optional path to backup copy (for files).
    """
    artifact_type: str
    artifact_name: str
    metadata: Dict[str, Any]
    recorded_at: datetime = field(default_factory=datetime.now)
    backup_path: Optional[str] = None


class BaselineStore:
    """
    Manages storage and retrieval of artifact baselines.
    
    This class maintains an in-memory store of baselines for critical system
    artifacts including files, registry keys, processes, and configurations.
    """
    
    def __init__(self, backup_directory: Optional[str] = None):
        """
        Initialize the BaselineStore.
        
        Args:
            backup_directory: Optional directory path for storing file backups.
        """
        self._baselines: Dict[str, ArtifactBaseline] = {}
        self._backup_dir = Path(backup_directory) if backup_directory else Path("./baseline_backups")
        self._backup_dir.mkdir(parents=True, exist_ok=True)
    
    def _generate_key(self, artifact_type: str, artifact_name: str) -> str:
        """Generate a unique key for artifact lookup."""
        return f"{artifact_type}:{artifact_name}"
    
    def record_baseline(
        self,
        artifact_type: str,
        artifact_name: str,
        metadata: Dict[str, Any],
        create_backup: bool = True
    ) -> ArtifactBaseline:
        """
        Record a baseline for an artifact.
        
        Args:
            artifact_type: Type of artifact ("file", "registry", "process", "config").
            artifact_name: Name or path of the artifact.
            metadata: Dictionary containing baseline data.
            create_backup: Whether to create a backup copy (for files).
        
        Returns:
            The created ArtifactBaseline object.
        """
        key = self._generate_key(artifact_type, artifact_name)
        
        backup_path = None
        if artifact_type == "file" and create_backup:
            backup_path = self._create_file_backup(artifact_name)
        
        baseline = ArtifactBaseline(
            artifact_type=artifact_type,
            artifact_name=artifact_name,
            metadata=metadata,
            recorded_at=datetime.now(),
            backup_path=backup_path
        )
        
        self._baselines[key] = baseline
        return baseline
    
    def get_baseline(self, artifact_type: str, artifact_name: str) -> Optional[Dict[str, Any]]:
        """
        Retrieve baseline metadata for an artifact.
        
        Args:
            artifact_type: Type of artifact.
            artifact_name: Name or path of the artifact.
        
        Returns:
            Baseline metadata dictionary, or None if not found.
        """
        key = self._generate_key(artifact_type, artifact_name)
        baseline = self._baselines.get(key)
        return baseline.metadata if baseline else None
    
    def get_full_baseline(self, artifact_type: str, artifact_name: str) -> Optional[ArtifactBaseline]:
        """
        Retrieve the full baseline object for an artifact.
        
        Args:
            artifact_type: Type of artifact.
            artifact_name: Name or path of the artifact.
        
        Returns:
            ArtifactBaseline object, or None if not found.
        """
        key = self._generate_key(artifact_type, artifact_name)
        return self._baselines.get(key)
    
    def remove_baseline(self, artifact_type: str, artifact_name: str) -> bool:
        """
        Remove a baseline from the store.
        
        Args:
            artifact_type: Type of artifact.
            artifact_name: Name or path of the artifact.
        
        Returns:
            True if baseline was removed, False if not found.
        """
        key = self._generate_key(artifact_type, artifact_name)
        if key in self._baselines:
            del self._baselines[key]
            return True
        return False
    
    def list_baselines(self, artifact_type: Optional[str] = None) -> List[ArtifactBaseline]:
        """
        List all stored baselines, optionally filtered by type.
        
        Args:
            artifact_type: Optional filter for artifact type.
        
        Returns:
            List of ArtifactBaseline objects.
        """
        if artifact_type:
            return [b for b in self._baselines.values() if b.artifact_type == artifact_type]
        return list(self._baselines.values())
    
    def _create_file_backup(self, file_path: str) -> Optional[str]:
        """
        Create a backup copy of a file.
        
        Args:
            file_path: Path to the file to backup.
        
        Returns:
            Path to the backup file, or None if backup failed.
        """
        try:
            source = Path(file_path)
            if not source.exists():
                return None
            
            # Create unique backup filename
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            backup_name = f"{source.name}.{timestamp}.baseline"
            backup_path = self._backup_dir / backup_name
            
            shutil.copy2(source, backup_path)
            return str(backup_path)
        except Exception:
            return None
    
    def _compute_file_hash(self, file_path: str) -> Optional[str]:
        """
        Compute SHA-256 hash of a file.
        
        Args:
            file_path: Path to the file.
        
        Returns:
            Hex digest of the file hash, or None if file cannot be read.
        """
        try:
            hasher = hashlib.sha256()
            with open(file_path, "rb") as f:
                for chunk in iter(lambda: f.read(8192), b""):
                    hasher.update(chunk)
            return hasher.hexdigest()
        except Exception:
            return None


class ResilienceManager:
    """
    Manages system resilience by monitoring artifacts and performing recovery actions.
    
    This class subscribes to the event pipeline, compares events against stored
    baselines, and triggers appropriate rollback/restore actions when deviations
    are detected.
    """
    
    def __init__(self, baseline_store: BaselineStore):
        """
        Initialize the ResilienceManager.
        
        Args:
            baseline_store: BaselineStore instance for accessing artifact baselines.
        """
        self._baseline_store = baseline_store
        self._consumers: List[Callable[[ResilienceEvent], None]] = []
        self._monitored_processes: Dict[str, Dict[str, Any]] = {}
        self._config_store: Dict[str, str] = {}
    
    def register_consumer(self, consumer: Callable[[ResilienceEvent], None]) -> None:
        """
        Register a consumer to receive ResilienceEvent notifications.
        
        Args:
            consumer: Callable that accepts a ResilienceEvent.
        """
        self._consumers.append(consumer)
    
    def unregister_consumer(self, consumer: Callable[[ResilienceEvent], None]) -> bool:
        """
        Unregister a consumer from receiving notifications.
        
        Args:
            consumer: The consumer to unregister.
        
        Returns:
            True if consumer was removed, False if not found.
        """
        try:
            self._consumers.remove(consumer)
            return True
        except ValueError:
            return False
    
    def _emit_event(self, event: ResilienceEvent) -> None:
        """Emit a ResilienceEvent to all registered consumers."""
        for consumer in self._consumers:
            try:
                consumer(event)
            except Exception:
                # Log but don't fail on consumer errors
                pass
    
    def monitor_event(self, event: Any) -> Optional[ResilienceEvent]:
        """
        Monitor an incoming event and trigger resilience actions if needed.
        
        This method compares the event against stored baselines and triggers
        appropriate rollback/restore actions when deviations are detected.
        
        Args:
            event: An event object with artifact information to check.
                   Expected to have 'artifact_type', 'artifact_name', and
                   optionally 'current_state' attributes.
        
        Returns:
            ResilienceEvent if a recovery action was performed, None otherwise.
        """
        # Extract event information
        artifact_type = getattr(event, "artifact_type", None)
        artifact_name = getattr(event, "artifact_name", None)
        
        if not artifact_type or not artifact_name:
            return None
        
        # Get baseline for comparison
        baseline = self._baseline_store.get_baseline(artifact_type, artifact_name)
        if not baseline:
            return None
        
        # Check for deviation based on artifact type
        deviation_detected = self._check_deviation(event, baseline)
        
        if deviation_detected:
            return self._perform_recovery(artifact_type, artifact_name, event)
        
        return None
    
    def _check_deviation(self, event: Any, baseline: Dict[str, Any]) -> bool:
        """
        Check if an event indicates a deviation from baseline.
        
        Args:
            event: The incoming event.
            baseline: The stored baseline metadata.
        
        Returns:
            True if deviation detected, False otherwise.
        """
        current_state = getattr(event, "current_state", None)
        if current_state is None:
            return False
        
        # Compare relevant baseline fields
        if "hash" in baseline and "hash" in current_state:
            if baseline["hash"] != current_state["hash"]:
                return True
        
        if "content" in baseline and "content" in current_state:
            if baseline["content"] != current_state["content"]:
                return True
        
        if "state" in baseline and "state" in current_state:
            if baseline["state"] != current_state["state"]:
                return True
        
        return False
    
    def _perform_recovery(
        self,
        artifact_type: str,
        artifact_name: str,
        event: Any
    ) -> ResilienceEvent:
        """
        Perform the appropriate recovery action based on artifact type.
        
        Args:
            artifact_type: Type of artifact to recover.
            artifact_name: Name/path of the artifact.
            event: The triggering event.
        
        Returns:
            ResilienceEvent describing the action taken.
        """
        if artifact_type == "file":
            return self.rollback_file(artifact_name)
        elif artifact_type == "registry":
            return self.rollback_registry(artifact_name)
        elif artifact_type == "process":
            return self.respawn_process(artifact_name)
        elif artifact_type == "config":
            return self.restore_config(artifact_name)
        else:
            # Unknown artifact type - create generic event
            resilience_event = ResilienceEvent(
                event_id=ResilienceEvent.generate_id(),
                artifact_type=artifact_type,
                artifact_name=artifact_name,
                action_taken="quarantine",
                timestamp=datetime.now(),
                description=f"Unknown artifact type '{artifact_type}' - quarantined for review"
            )
            self._emit_event(resilience_event)
            return resilience_event
    
    def rollback_file(self, path: str) -> ResilienceEvent:
        """
        Roll back a file to its baseline state.
        
        Args:
            path: Path to the file to roll back.
        
        Returns:
            ResilienceEvent describing the rollback action.
        """
        baseline = self._baseline_store.get_full_baseline("file", path)
        success = False
        description = ""
        
        if baseline and baseline.backup_path:
            try:
                backup = Path(baseline.backup_path)
                target = Path(path)
                
                if backup.exists():
                    # Create directory if needed
                    target.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copy2(backup, target)
                    success = True
                    description = f"File '{path}' rolled back from backup '{baseline.backup_path}'"
                else:
                    description = f"Backup file not found for '{path}'"
            except Exception as e:
                description = f"Failed to rollback file '{path}': {str(e)}"
        else:
            description = f"No baseline backup available for file '{path}'"
        
        action = "rollback" if success else "quarantine"
        
        resilience_event = ResilienceEvent(
            event_id=ResilienceEvent.generate_id(),
            artifact_type="file",
            artifact_name=path,
            action_taken=action,
            timestamp=datetime.now(),
            description=description
        )
        
        self._emit_event(resilience_event)
        return resilience_event
    
    def rollback_registry(self, key: str) -> ResilienceEvent:
        """
        Roll back a registry key to its baseline state.
        
        Note: This is a mock implementation. Actual registry operations
        require platform-specific code (Windows-only).
        
        Args:
            key: Registry key path to roll back.
        
        Returns:
            ResilienceEvent describing the rollback action.
        """
        baseline = self._baseline_store.get_baseline("registry", key)
        success = False
        description = ""
        
        if baseline:
            try:
                # Mock registry rollback
                # In a real implementation, this would use winreg or similar
                stored_value = baseline.get("value")
                stored_type = baseline.get("type", "REG_SZ")
                
                # Simulate registry write
                success = self._mock_registry_write(key, stored_value, stored_type)
                
                if success:
                    description = f"Registry key '{key}' rolled back to baseline value"
                else:
                    description = f"Failed to write registry key '{key}'"
            except Exception as e:
                description = f"Failed to rollback registry key '{key}': {str(e)}"
        else:
            description = f"No baseline available for registry key '{key}'"
        
        action = "rollback" if success else "quarantine"
        
        resilience_event = ResilienceEvent(
            event_id=ResilienceEvent.generate_id(),
            artifact_type="registry",
            artifact_name=key,
            action_taken=action,
            timestamp=datetime.now(),
            description=description
        )
        
        self._emit_event(resilience_event)
        return resilience_event
    
    def _mock_registry_write(self, key: str, value: Any, reg_type: str) -> bool:
        """
        Mock implementation of registry write operation.
        
        Args:
            key: Registry key path.
            value: Value to write.
            reg_type: Registry value type.
        
        Returns:
            True (simulated success).
        """
        # This is a mock - real implementation would use winreg on Windows
        return True
    
    def respawn_process(self, name: str) -> ResilienceEvent:
        """
        Respawn a critical process that was terminated.
        
        Args:
            name: Name or path of the process to respawn.
        
        Returns:
            ResilienceEvent describing the respawn action.
        """
        baseline = self._baseline_store.get_baseline("process", name)
        success = False
        description = ""
        
        if baseline:
            try:
                command = baseline.get("command", name)
                args = baseline.get("args", [])
                working_dir = baseline.get("working_dir")
                
                # Attempt to respawn the process
                success = self._spawn_process(command, args, working_dir)
                
                if success:
                    description = f"Process '{name}' successfully respawned"
                else:
                    description = f"Failed to respawn process '{name}'"
            except Exception as e:
                description = f"Failed to respawn process '{name}': {str(e)}"
        else:
            # Try to spawn with just the name
            try:
                success = self._spawn_process(name, [], None)
                if success:
                    description = f"Process '{name}' spawned (no baseline)"
                else:
                    description = f"Failed to spawn process '{name}' (no baseline)"
            except Exception as e:
                description = f"Failed to spawn process '{name}': {str(e)}"
        
        action = "respawn" if success else "quarantine"
        
        resilience_event = ResilienceEvent(
            event_id=ResilienceEvent.generate_id(),
            artifact_type="process",
            artifact_name=name,
            action_taken=action,
            timestamp=datetime.now(),
            description=description
        )
        
        self._emit_event(resilience_event)
        return resilience_event
    
    def _spawn_process(
        self,
        command: str,
        args: List[str],
        working_dir: Optional[str]
    ) -> bool:
        """
        Spawn a process with given parameters.
        
        Args:
            command: Command or executable path.
            args: Command line arguments.
            working_dir: Working directory for the process.
        
        Returns:
            True if process was spawned successfully, False otherwise.
        """
        try:
            cmd = [command] + args
            subprocess.Popen(
                cmd,
                cwd=working_dir,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                start_new_session=True
            )
            return True
        except Exception:
            return False
    
    def restore_config(self, name: str) -> ResilienceEvent:
        """
        Restore a configuration to its baseline state.
        
        Args:
            name: Name/identifier of the configuration to restore.
        
        Returns:
            ResilienceEvent describing the restore action.
        """
        baseline = self._baseline_store.get_baseline("config", name)
        success = False
        description = ""
        
        if baseline:
            try:
                config_content = baseline.get("content")
                config_path = baseline.get("path")
                
                if config_path and config_content:
                    # Write configuration back to file
                    with open(config_path, "w") as f:
                        f.write(config_content)
                    success = True
                    description = f"Configuration '{name}' restored from baseline"
                elif config_content:
                    # Store in memory config store
                    self._config_store[name] = config_content
                    success = True
                    description = f"Configuration '{name}' restored to memory store"
                else:
                    description = f"No content available for configuration '{name}'"
            except Exception as e:
                description = f"Failed to restore configuration '{name}': {str(e)}"
        else:
            description = f"No baseline available for configuration '{name}'"
        
        action = "restore" if success else "quarantine"
        
        resilience_event = ResilienceEvent(
            event_id=ResilienceEvent.generate_id(),
            artifact_type="config",
            artifact_name=name,
            action_taken=action,
            timestamp=datetime.now(),
            description=description
        )
        
        self._emit_event(resilience_event)
        return resilience_event
    
    def record_file_baseline(self, path: str) -> Optional[ArtifactBaseline]:
        """
        Convenience method to record a file baseline with automatic hash computation.
        
        Args:
            path: Path to the file.
        
        Returns:
            ArtifactBaseline object, or None if file cannot be processed.
        """
        file_path = Path(path)
        if not file_path.exists():
            return None
        
        file_hash = self._baseline_store._compute_file_hash(path)
        if not file_hash:
            return None
        
        metadata = {
            "hash": file_hash,
            "size": file_path.stat().st_size,
            "mtime": file_path.stat().st_mtime,
            "permissions": oct(file_path.stat().st_mode)[-3:]
        }
        
        return self._baseline_store.record_baseline("file", path, metadata)
    
    def record_process_baseline(
        self,
        name: str,
        command: str,
        args: Optional[List[str]] = None,
        working_dir: Optional[str] = None
    ) -> ArtifactBaseline:
        """
        Record a baseline for a critical process.
        
        Args:
            name: Process name/identifier.
            command: Command to execute the process.
            args: Command line arguments.
            working_dir: Working directory for the process.
        
        Returns:
            ArtifactBaseline object.
        """
        metadata = {
            "command": command,
            "args": args or [],
            "working_dir": working_dir,
            "state": "running"
        }
        
        return self._baseline_store.record_baseline("process", name, metadata, create_backup=False)
    
    def record_config_baseline(
        self,
        name: str,
        content: str,
        path: Optional[str] = None
    ) -> ArtifactBaseline:
        """
        Record a baseline for a configuration.
        
        Args:
            name: Configuration name/identifier.
            content: Configuration content.
            path: Optional file path for the configuration.
        
        Returns:
            ArtifactBaseline object.
        """
        metadata = {
            "content": content,
            "path": path,
            "hash": hashlib.sha256(content.encode()).hexdigest()
        }
        
        return self._baseline_store.record_baseline("config", name, metadata, create_backup=False)
    
    def record_registry_baseline(
        self,
        key: str,
        value: Any,
        reg_type: str = "REG_SZ"
    ) -> ArtifactBaseline:
        """
        Record a baseline for a registry key.
        
        Args:
            key: Registry key path.
            value: Registry value.
            reg_type: Registry value type.
        
        Returns:
            ArtifactBaseline object.
        """
        metadata = {
            "value": value,
            "type": reg_type
        }
        
        return self._baseline_store.record_baseline("registry", key, metadata, create_backup=False)


# Convenience factory function
def create_resilience_manager(backup_directory: Optional[str] = None) -> ResilienceManager:
    """
    Factory function to create a fully configured ResilienceManager.
    
    Args:
        backup_directory: Optional directory for file backups.
    
    Returns:
        Configured ResilienceManager instance.
    """
    baseline_store = BaselineStore(backup_directory=backup_directory)
    return ResilienceManager(baseline_store)


# Example usage and integration helpers
if __name__ == "__main__":
    # Demo usage
    manager = create_resilience_manager("./baseline_backups")
    
    # Register a simple consumer
    def log_resilience_event(event: ResilienceEvent):
        print(f"[RESILIENCE] {event.timestamp}: {event.action_taken} on {event.artifact_type} "
              f"'{event.artifact_name}' - {event.description}")
    
    manager.register_consumer(log_resilience_event)
    
    # Record some baselines
    manager.record_config_baseline(
        name="greyav_config",
        content='{"scan_enabled": true, "realtime_protection": true}',
        path="./config.json"
    )
    
    manager.record_process_baseline(
        name="greyav_monitor",
        command="python",
        args=["greyav.py", "--monitor"],
        working_dir="."
    )
    
    print("ResilienceManager initialized with example baselines.")
    print(f"Stored baselines: {len(manager._baseline_store.list_baselines())}")
