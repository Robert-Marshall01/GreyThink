"""
Proof Generator

Creates signed proof artifacts that demonstrate measurable
resource reductions. Proofs include HMAC signatures for
authenticity verification.

These artifacts can be exported and used to verify that
Grey Optimizer achieved its stated goals.
"""

import hashlib
import hmac
import json
import logging
import os
import secrets
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, Optional

logger = logging.getLogger(__name__)


class ProofGenerator:
    """
    Generates and manages proof artifacts.
    
    Each proof contains:
    - Timestamp of generation
    - Subsystem (cpu, ram, disk)
    - Baseline metrics before optimization
    - Post-enforcement metrics after optimization
    - Calculated reduction percentage
    - HMAC signature for authenticity
    """
    
    def __init__(self, proofs_dir: str, hmac_key: Optional[str] = None):
        """
        Initialize the proof generator.
        
        Args:
            proofs_dir: Directory to store proof artifacts
            hmac_key: Key for signing proofs (generated if not provided)
        """
        self.proofs_dir = Path(proofs_dir)
        self.proofs_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize or load HMAC key
        self.hmac_key = self._initialize_key(hmac_key)
        
        logger.info(f"Proof generator initialized (dir={proofs_dir})")
    
    def _initialize_key(self, key: Optional[str]) -> bytes:
        """
        Initialize HMAC key.
        
        If no key is provided, generate and save one.
        
        Args:
            key: Provided key or None
            
        Returns:
            Key as bytes
        """
        key_file = self.proofs_dir / ".hmac_key"
        
        if key:
            return key.encode()
        
        if key_file.exists():
            with open(key_file, "rb") as f:
                return f.read()
        
        # Generate new key
        new_key = secrets.token_bytes(32)
        
        with open(key_file, "wb") as f:
            f.write(new_key)
        
        # Set restrictive permissions
        os.chmod(key_file, 0o600)
        
        logger.info("Generated new HMAC key for proof signing")
        return new_key
    
    async def generate(
        self,
        subsystem: str,
        baseline: Dict[str, Any],
        current: Dict[str, Any],
        reduction: float
    ) -> Dict[str, Any]:
        """
        Generate a proof artifact.
        
        Creates a signed proof demonstrating the resource reduction
        achieved by Grey Optimizer.
        
        Args:
            subsystem: The subsystem ("cpu", "ram", "disk")
            baseline: Baseline metrics before optimization
            current: Current metrics after optimization
            reduction: Calculated reduction percentage
            
        Returns:
            The generated proof artifact
        """
        timestamp = datetime.utcnow().isoformat()
        
        # Generate unique ID
        proof_id = f"{subsystem}-{timestamp.replace(':', '-').replace('.', '-')}"
        
        # Extract relevant metrics for the subsystem
        if subsystem == "cpu":
            baseline_value = baseline.get("cpu", {}).get("usage_percent", 0)
            current_value = current.get("cpu", {}).get("usage_percent", 0)
            unit = "percent"
        elif subsystem == "ram":
            baseline_value = baseline.get("ram", {}).get("used_mb", 0)
            current_value = current.get("ram", {}).get("used_mb", 0)
            unit = "MB"
        elif subsystem == "disk":
            baseline_value = baseline.get("disk", {}).get("write_mbs", 0)
            current_value = current.get("disk", {}).get("write_mbs", 0)
            unit = "MB/s"
        else:
            baseline_value = 0
            current_value = 0
            unit = "unknown"
        
        # Calculate actual reduction
        if baseline_value > 0:
            actual_reduction = ((baseline_value - current_value) / baseline_value) * 100
        else:
            actual_reduction = 0
        
        # Build proof data
        proof_data = {
            "id": proof_id,
            "timestamp": timestamp,
            "subsystem": subsystem,
            "baseline": {
                "timestamp": baseline.get("timestamp", timestamp),
                "value": baseline_value,
                "unit": unit,
                "raw": baseline.get(subsystem, {})
            },
            "post_enforcement": {
                "timestamp": current.get("timestamp", timestamp),
                "value": current_value,
                "unit": unit,
                "raw": current.get(subsystem, {})
            },
            "reduction_percent": round(actual_reduction, 2),
            "target_reduction_percent": 90.0,
            "target_met": actual_reduction >= 90.0
        }
        
        # Sign the proof
        signature = self._sign_proof(proof_data)
        proof_data["signature"] = signature
        
        # Save to file
        proof_file = self.proofs_dir / f"{proof_id}.json"
        with open(proof_file, "w") as f:
            json.dump(proof_data, f, indent=2)
        
        logger.debug(f"Generated proof: {proof_id} (reduction={actual_reduction:.1f}%)")
        
        return proof_data
    
    def _sign_proof(self, proof_data: Dict[str, Any]) -> str:
        """
        Create HMAC signature for a proof.
        
        Args:
            proof_data: The proof data to sign
            
        Returns:
            Hex-encoded HMAC signature
        """
        # Create canonical representation for signing
        canonical = json.dumps({
            "id": proof_data["id"],
            "timestamp": proof_data["timestamp"],
            "subsystem": proof_data["subsystem"],
            "baseline_value": proof_data["baseline"]["value"],
            "post_value": proof_data["post_enforcement"]["value"],
            "reduction": proof_data["reduction_percent"]
        }, sort_keys=True)
        
        # Calculate HMAC-SHA256
        signature = hmac.new(
            self.hmac_key,
            canonical.encode(),
            hashlib.sha256
        ).hexdigest()
        
        return f"hmac-sha256:{signature}"
    
    def verify_signature(self, proof_data: Dict[str, Any]) -> bool:
        """
        Verify the signature of a proof artifact.
        
        Args:
            proof_data: The proof to verify
            
        Returns:
            True if signature is valid
        """
        if "signature" not in proof_data:
            return False
        
        stored_signature = proof_data["signature"]
        
        # Remove signature before recalculating
        proof_copy = {k: v for k, v in proof_data.items() if k != "signature"}
        expected_signature = self._sign_proof(proof_copy)
        
        return hmac.compare_digest(stored_signature, expected_signature)
    
    async def export_csv(self, output_path: str) -> int:
        """
        Export all proofs to CSV format.
        
        Args:
            output_path: Path for the CSV file
            
        Returns:
            Number of proofs exported
        """
        import csv
        
        proofs = []
        
        for proof_file in self.proofs_dir.glob("*.json"):
            try:
                with open(proof_file, "r") as f:
                    proof = json.load(f)
                proofs.append(proof)
            except Exception as e:
                logger.warning(f"Could not load proof {proof_file}: {e}")
        
        if not proofs:
            return 0
        
        # Sort by timestamp
        proofs.sort(key=lambda p: p["timestamp"])
        
        # Write CSV
        with open(output_path, "w", newline="") as f:
            writer = csv.writer(f)
            
            # Header
            writer.writerow([
                "ID", "Timestamp", "Subsystem",
                "Baseline Value", "Post Value", "Unit",
                "Reduction %", "Target Met", "Signature Valid"
            ])
            
            # Data rows
            for proof in proofs:
                writer.writerow([
                    proof["id"],
                    proof["timestamp"],
                    proof["subsystem"],
                    proof["baseline"]["value"],
                    proof["post_enforcement"]["value"],
                    proof["baseline"]["unit"],
                    proof["reduction_percent"],
                    proof.get("target_met", False),
                    self.verify_signature(proof)
                ])
        
        logger.info(f"Exported {len(proofs)} proofs to {output_path}")
        return len(proofs)
    
    async def get_summary(self) -> Dict[str, Any]:
        """
        Get summary of all proof artifacts.
        
        Returns:
            Summary statistics
        """
        proofs = []
        
        for proof_file in self.proofs_dir.glob("*.json"):
            try:
                with open(proof_file, "r") as f:
                    proofs.append(json.load(f))
            except Exception:
                continue
        
        if not proofs:
            return {
                "total_proofs": 0,
                "by_subsystem": {},
                "targets_met": 0,
                "average_reduction": 0
            }
        
        by_subsystem = {}
        for proof in proofs:
            subsystem = proof["subsystem"]
            if subsystem not in by_subsystem:
                by_subsystem[subsystem] = {
                    "count": 0,
                    "total_reduction": 0,
                    "targets_met": 0
                }
            
            by_subsystem[subsystem]["count"] += 1
            by_subsystem[subsystem]["total_reduction"] += proof["reduction_percent"]
            if proof.get("target_met", False):
                by_subsystem[subsystem]["targets_met"] += 1
        
        # Calculate averages
        for subsystem in by_subsystem:
            count = by_subsystem[subsystem]["count"]
            by_subsystem[subsystem]["average_reduction"] = round(
                by_subsystem[subsystem]["total_reduction"] / count, 2
            )
        
        return {
            "total_proofs": len(proofs),
            "by_subsystem": by_subsystem,
            "targets_met": sum(1 for p in proofs if p.get("target_met", False)),
            "average_reduction": round(
                sum(p["reduction_percent"] for p in proofs) / len(proofs), 2
            )
        }
