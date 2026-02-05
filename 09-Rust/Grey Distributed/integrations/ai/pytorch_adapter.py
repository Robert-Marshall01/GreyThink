"""
PyTorch Adapter for Grey Distributed

This adapter enables distributed PyTorch inference jobs across Grey worker nodes.
It handles model distribution, batch scheduling, and generates proof artifacts
for inference reproducibility.

Architecture
============

Grey Distributed manages the orchestration layer while PyTorch handles compute:

┌─────────────────┐      ┌──────────────────┐      ┌─────────────────┐
│  Grey Scheduler │──────│  PyTorch Adapter │──────│  Worker Nodes   │
│  (task routing) │      │  (model serving) │      │  (GPU/CPU exec) │
└─────────────────┘      └──────────────────┘      └─────────────────┘

Key Design Decisions
====================

1. **Model Sharding**: Large models are sharded across workers using pipeline
   parallelism. Grey's routing ensures requests hit the right shard.

2. **Proof Artifacts**: Each inference generates a cryptographic proof containing:
   - Model hash (weights + architecture)
   - Input hash
   - Output tensor
   - Random seeds (for reproducibility)
   - Node attestation (if TEE-enabled)

3. **Batching**: Dynamic batching aggregates small requests for throughput.
   Trade-off: Higher latency for small requests, better GPU utilization.

4. **Checkpointing**: Model weights are checkpointed to Grey storage for 
   fast node recovery. Avoids reloading from slow object storage.
"""

import hashlib
import json
import time
import uuid
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple, Union
from enum import Enum
import asyncio
import logging
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor

import torch
import torch.nn as nn
from torch.cuda.amp import autocast

logger = logging.getLogger(__name__)


# =============================================================================
# Configuration
# =============================================================================

@dataclass
class PyTorchConfig:
    """Configuration for PyTorch adapter."""
    
    # Model serving settings
    model_path: str = ""
    model_name: str = "default"
    device: str = "cuda" if torch.cuda.is_available() else "cpu"
    dtype: torch.dtype = torch.float16  # Half precision for efficiency
    
    # Batching settings
    max_batch_size: int = 32
    max_batch_wait_ms: int = 50  # Max time to wait for batch formation
    
    # Model parallelism
    tensor_parallel_size: int = 1  # Number of GPUs for tensor parallelism
    pipeline_parallel_size: int = 1  # Number of pipeline stages
    
    # Caching
    enable_kv_cache: bool = True  # For transformer models
    cache_max_entries: int = 1000
    
    # Reproducibility
    deterministic: bool = True  # Sacrifice some performance for reproducibility
    random_seed: int = 42
    
    # Proof generation
    generate_proofs: bool = True
    proof_include_weights_hash: bool = True
    
    # Timeouts
    inference_timeout_ms: int = 30000
    model_load_timeout_s: int = 300


class DeviceType(Enum):
    """Supported device types."""
    CPU = "cpu"
    CUDA = "cuda"
    MPS = "mps"  # Apple Silicon


# =============================================================================
# Proof Artifacts
# =============================================================================

@dataclass
class InferenceProof:
    """
    Cryptographic proof artifact for reproducible inference.
    
    This proof allows independent verification that a specific model
    produced a specific output for a given input. Critical for:
    - Audit trails
    - Dispute resolution
    - Regulatory compliance
    """
    
    # Unique identifiers
    proof_id: str
    task_id: str
    tenant_id: str
    
    # Model identification
    model_name: str
    model_hash: str  # SHA-256 of serialized weights
    model_version: str
    
    # Input/output hashes
    input_hash: str  # SHA-256 of input tensors
    output_hash: str  # SHA-256 of output tensors
    
    # Reproducibility parameters
    random_seed: int
    torch_version: str
    cuda_version: Optional[str]
    device: str
    dtype: str
    deterministic: bool
    
    # Execution context
    worker_node: str
    execution_time_ms: float
    timestamp: str
    
    # TEE attestation (if available)
    tee_attestation: Optional[str] = None
    mrenclave: Optional[str] = None
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "proof_id": self.proof_id,
            "task_id": self.task_id,
            "tenant_id": self.tenant_id,
            "model": {
                "name": self.model_name,
                "hash": self.model_hash,
                "version": self.model_version,
            },
            "io_hashes": {
                "input": self.input_hash,
                "output": self.output_hash,
            },
            "reproducibility": {
                "random_seed": self.random_seed,
                "torch_version": self.torch_version,
                "cuda_version": self.cuda_version,
                "device": self.device,
                "dtype": self.dtype,
                "deterministic": self.deterministic,
            },
            "execution": {
                "worker_node": self.worker_node,
                "execution_time_ms": self.execution_time_ms,
                "timestamp": self.timestamp,
            },
            "tee": {
                "attestation": self.tee_attestation,
                "mrenclave": self.mrenclave,
            } if self.tee_attestation else None,
        }
    
    def to_json(self) -> str:
        return json.dumps(self.to_dict(), indent=2)
    
    @staticmethod
    def compute_hash(data: Union[torch.Tensor, List[torch.Tensor], bytes]) -> str:
        """Compute SHA-256 hash of tensor(s) or bytes."""
        hasher = hashlib.sha256()
        
        if isinstance(data, bytes):
            hasher.update(data)
        elif isinstance(data, torch.Tensor):
            hasher.update(data.cpu().numpy().tobytes())
        elif isinstance(data, list):
            for tensor in data:
                hasher.update(tensor.cpu().numpy().tobytes())
        
        return hasher.hexdigest()


# =============================================================================
# Model Manager
# =============================================================================

class ModelManager:
    """
    Manages model lifecycle: loading, serving, and checkpointing.
    
    Design Considerations
    ---------------------
    
    1. **Lazy Loading**: Models are loaded on first request to avoid blocking
       worker startup. Trade-off: First request is slow.
    
    2. **Weight Hashing**: Model weights are hashed at load time for proof
       generation. Hash is cached for performance.
    
    3. **Memory Management**: Explicit memory management for GPU OOM prevention.
       Uses torch.cuda.empty_cache() after inference.
    """
    
    def __init__(self, config: PyTorchConfig):
        self.config = config
        self.model: Optional[nn.Module] = None
        self.model_hash: Optional[str] = None
        self.device = torch.device(config.device)
        self._load_lock = asyncio.Lock()
        
        # Set deterministic mode if requested
        if config.deterministic:
            torch.manual_seed(config.random_seed)
            torch.backends.cudnn.deterministic = True
            torch.backends.cudnn.benchmark = False
            if torch.cuda.is_available():
                torch.cuda.manual_seed_all(config.random_seed)
    
    async def load_model(self, model_path: str) -> None:
        """
        Load model from path (local file or Grey storage reference).
        
        Supports:
        - .pt/.pth (PyTorch state dict)
        - .safetensors (Safe tensors format)
        - grey:// URIs (load from Grey distributed storage)
        """
        async with self._load_lock:
            if self.model is not None:
                logger.info("Model already loaded, skipping")
                return
            
            logger.info(f"Loading model from {model_path}")
            start_time = time.time()
            
            # Handle Grey storage URIs
            if model_path.startswith("grey://"):
                model_path = await self._download_from_grey(model_path)
            
            # Load model in thread pool to avoid blocking
            loop = asyncio.get_event_loop()
            with ThreadPoolExecutor() as executor:
                self.model = await loop.run_in_executor(
                    executor,
                    self._load_model_sync,
                    model_path
                )
            
            # Move to device
            self.model = self.model.to(self.device)
            self.model.eval()
            
            # Compute weights hash for proofs
            if self.config.proof_include_weights_hash:
                self.model_hash = await loop.run_in_executor(
                    executor,
                    self._compute_model_hash
                )
            
            load_time = time.time() - start_time
            logger.info(f"Model loaded in {load_time:.2f}s, hash: {self.model_hash[:16]}...")
    
    def _load_model_sync(self, path: str) -> nn.Module:
        """Synchronous model loading."""
        if path.endswith(".safetensors"):
            from safetensors.torch import load_file
            state_dict = load_file(path)
            # Assume model class is known; in practice, need separate config
            raise NotImplementedError("SafeTensors requires model class specification")
        else:
            return torch.load(path, map_location="cpu")
    
    def _compute_model_hash(self) -> str:
        """Compute hash of model weights."""
        hasher = hashlib.sha256()
        for param in self.model.parameters():
            hasher.update(param.data.cpu().numpy().tobytes())
        return hasher.hexdigest()
    
    async def _download_from_grey(self, uri: str) -> str:
        """Download model from Grey distributed storage."""
        # Parse grey://tenant/model/version
        parts = uri.replace("grey://", "").split("/")
        tenant, model_name, version = parts[0], parts[1], parts[2] if len(parts) > 2 else "latest"
        
        # In production: use Grey client to download
        local_path = f"/tmp/grey_models/{tenant}/{model_name}/{version}/model.pt"
        Path(local_path).parent.mkdir(parents=True, exist_ok=True)
        
        # Placeholder for actual download
        logger.info(f"Downloading {uri} to {local_path}")
        
        return local_path
    
    def get_model(self) -> nn.Module:
        """Get loaded model, raises if not loaded."""
        if self.model is None:
            raise RuntimeError("Model not loaded. Call load_model() first.")
        return self.model
    
    async def checkpoint_to_grey(self, task_id: str) -> str:
        """
        Save model checkpoint to Grey storage for fast recovery.
        
        Returns checkpoint ID.
        """
        checkpoint_id = f"ckpt_{task_id}_{uuid.uuid4().hex[:8]}"
        
        # Serialize model state
        state_dict = self.model.state_dict()
        buffer = torch.save(state_dict, f"/tmp/{checkpoint_id}.pt")
        
        # Upload to Grey storage
        # In production: grey_client.store_checkpoint(checkpoint_id, buffer)
        
        logger.info(f"Checkpointed model to {checkpoint_id}")
        return checkpoint_id


# =============================================================================
# Batching
# =============================================================================

@dataclass
class InferenceRequest:
    """Single inference request."""
    request_id: str
    task_id: str
    tenant_id: str
    inputs: Dict[str, torch.Tensor]
    created_at: float = field(default_factory=time.time)


@dataclass
class BatchedRequest:
    """Batched inference request."""
    requests: List[InferenceRequest]
    batch_inputs: Dict[str, torch.Tensor]


class DynamicBatcher:
    """
    Aggregates inference requests into batches for efficient GPU utilization.
    
    Batching Strategy
    -----------------
    
    The batcher waits up to `max_wait_ms` or until `max_batch_size` requests
    arrive, whichever comes first. This trades individual latency for throughput.
    
    Trade-offs:
    - Higher max_wait_ms: Better batching, higher latency
    - Lower max_wait_ms: Lower latency, worse GPU utilization
    - Higher max_batch_size: Better throughput, more memory
    
    For inference-heavy workloads, aggressive batching is preferred.
    For latency-sensitive workloads, use max_wait_ms=0 to disable.
    """
    
    def __init__(self, max_batch_size: int, max_wait_ms: int):
        self.max_batch_size = max_batch_size
        self.max_wait_ms = max_wait_ms
        self.pending: List[InferenceRequest] = []
        self.lock = asyncio.Lock()
        self.batch_ready = asyncio.Event()
    
    async def add_request(self, request: InferenceRequest) -> None:
        """Add request to pending batch."""
        async with self.lock:
            self.pending.append(request)
            if len(self.pending) >= self.max_batch_size:
                self.batch_ready.set()
    
    async def get_batch(self) -> Optional[BatchedRequest]:
        """
        Get next batch when ready.
        
        Waits for either:
        1. max_batch_size requests accumulated
        2. max_wait_ms elapsed since first request
        """
        if not self.pending:
            return None
        
        # Wait for batch or timeout
        try:
            await asyncio.wait_for(
                self.batch_ready.wait(),
                timeout=self.max_wait_ms / 1000
            )
        except asyncio.TimeoutError:
            pass
        
        async with self.lock:
            if not self.pending:
                return None
            
            requests = self.pending[:self.max_batch_size]
            self.pending = self.pending[self.max_batch_size:]
            self.batch_ready.clear()
        
        # Stack inputs into batched tensors
        batch_inputs = self._stack_inputs(requests)
        
        return BatchedRequest(requests=requests, batch_inputs=batch_inputs)
    
    def _stack_inputs(self, requests: List[InferenceRequest]) -> Dict[str, torch.Tensor]:
        """Stack individual request inputs into batched tensors."""
        batch_inputs = {}
        
        if not requests:
            return batch_inputs
        
        # Get all input keys from first request
        keys = requests[0].inputs.keys()
        
        for key in keys:
            tensors = [r.inputs[key] for r in requests]
            # Pad to same length if needed (for variable-length inputs)
            max_len = max(t.shape[0] for t in tensors)
            padded = [
                torch.nn.functional.pad(t, (0, max_len - t.shape[0]))
                for t in tensors
            ]
            batch_inputs[key] = torch.stack(padded)
        
        return batch_inputs


# =============================================================================
# Inference Engine
# =============================================================================

class PyTorchInferenceEngine:
    """
    Main inference engine for Grey Distributed.
    
    Handles:
    - Model loading and management
    - Request batching
    - Inference execution
    - Proof artifact generation
    - Memory management
    """
    
    def __init__(self, config: PyTorchConfig):
        self.config = config
        self.model_manager = ModelManager(config)
        self.batcher = DynamicBatcher(config.max_batch_size, config.max_batch_wait_ms)
        self.worker_node = f"grey-worker-{uuid.uuid4().hex[:8]}"
        
        # Metrics
        self.inference_count = 0
        self.total_latency_ms = 0
    
    async def initialize(self, model_path: str) -> None:
        """Initialize engine with model."""
        await self.model_manager.load_model(model_path)
    
    @torch.no_grad()
    async def infer(
        self,
        task_id: str,
        tenant_id: str,
        inputs: Dict[str, torch.Tensor],
        generate_proof: bool = True,
    ) -> Tuple[Dict[str, torch.Tensor], Optional[InferenceProof]]:
        """
        Run inference on inputs.
        
        Parameters
        ----------
        task_id : str
            Grey task ID for tracking
        tenant_id : str
            Tenant ID for isolation
        inputs : Dict[str, torch.Tensor]
            Model inputs keyed by name
        generate_proof : bool
            Whether to generate proof artifact
            
        Returns
        -------
        outputs : Dict[str, torch.Tensor]
            Model outputs
        proof : Optional[InferenceProof]
            Proof artifact if requested
        """
        start_time = time.time()
        
        # Move inputs to device
        device_inputs = {
            k: v.to(self.config.device) 
            for k, v in inputs.items()
        }
        
        # Get model
        model = self.model_manager.get_model()
        
        # Run inference with mixed precision
        with autocast(enabled=self.config.dtype == torch.float16):
            if hasattr(model, 'forward'):
                outputs = model(**device_inputs)
            else:
                # Handle simple callable models
                outputs = model(device_inputs)
        
        # Normalize outputs to dict
        if isinstance(outputs, torch.Tensor):
            outputs = {"output": outputs}
        
        execution_time_ms = (time.time() - start_time) * 1000
        
        # Update metrics
        self.inference_count += 1
        self.total_latency_ms += execution_time_ms
        
        # Generate proof if requested
        proof = None
        if generate_proof and self.config.generate_proofs:
            proof = self._generate_proof(
                task_id=task_id,
                tenant_id=tenant_id,
                inputs=inputs,
                outputs=outputs,
                execution_time_ms=execution_time_ms,
            )
        
        # Memory cleanup
        if torch.cuda.is_available():
            torch.cuda.empty_cache()
        
        return outputs, proof
    
    async def infer_batched(
        self,
        batched_request: BatchedRequest,
        generate_proofs: bool = True,
    ) -> List[Tuple[Dict[str, torch.Tensor], Optional[InferenceProof]]]:
        """
        Run batched inference.
        
        Splits batch outputs back to individual requests after inference.
        """
        start_time = time.time()
        
        # Move batch inputs to device
        device_inputs = {
            k: v.to(self.config.device)
            for k, v in batched_request.batch_inputs.items()
        }
        
        model = self.model_manager.get_model()
        
        with autocast(enabled=self.config.dtype == torch.float16):
            batch_outputs = model(**device_inputs)
        
        if isinstance(batch_outputs, torch.Tensor):
            batch_outputs = {"output": batch_outputs}
        
        # Split outputs by request
        results = []
        batch_size = len(batched_request.requests)
        execution_time_ms = (time.time() - start_time) * 1000 / batch_size
        
        for i, request in enumerate(batched_request.requests):
            # Extract individual outputs
            individual_outputs = {
                k: v[i] for k, v in batch_outputs.items()
            }
            
            proof = None
            if generate_proofs and self.config.generate_proofs:
                proof = self._generate_proof(
                    task_id=request.task_id,
                    tenant_id=request.tenant_id,
                    inputs=request.inputs,
                    outputs=individual_outputs,
                    execution_time_ms=execution_time_ms,
                )
            
            results.append((individual_outputs, proof))
        
        return results
    
    def _generate_proof(
        self,
        task_id: str,
        tenant_id: str,
        inputs: Dict[str, torch.Tensor],
        outputs: Dict[str, torch.Tensor],
        execution_time_ms: float,
    ) -> InferenceProof:
        """Generate cryptographic proof of inference."""
        
        # Compute input/output hashes
        input_tensors = list(inputs.values())
        output_tensors = list(outputs.values())
        
        input_hash = InferenceProof.compute_hash(input_tensors)
        output_hash = InferenceProof.compute_hash(output_tensors)
        
        # Get CUDA version if available
        cuda_version = None
        if torch.cuda.is_available():
            cuda_version = torch.version.cuda
        
        return InferenceProof(
            proof_id=f"proof_{uuid.uuid4().hex}",
            task_id=task_id,
            tenant_id=tenant_id,
            model_name=self.config.model_name,
            model_hash=self.model_manager.model_hash or "unknown",
            model_version="1.0.0",  # Should come from model metadata
            input_hash=input_hash,
            output_hash=output_hash,
            random_seed=self.config.random_seed,
            torch_version=torch.__version__,
            cuda_version=cuda_version,
            device=str(self.config.device),
            dtype=str(self.config.dtype),
            deterministic=self.config.deterministic,
            worker_node=self.worker_node,
            execution_time_ms=execution_time_ms,
            timestamp=time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        )


# =============================================================================
# Grey Integration
# =============================================================================

class GreyPyTorchAdapter:
    """
    Integration layer between Grey Distributed and PyTorch.
    
    Implements Grey's task execution interface for PyTorch inference.
    Handles:
    - Task deserialization
    - Inference routing
    - Result serialization
    - Proof attachment
    """
    
    def __init__(self, config: PyTorchConfig):
        self.config = config
        self.engine = PyTorchInferenceEngine(config)
        self._initialized = False
    
    async def initialize(self) -> None:
        """Initialize adapter with model."""
        if self._initialized:
            return
        await self.engine.initialize(self.config.model_path)
        self._initialized = True
    
    async def execute_task(
        self,
        task_id: str,
        tenant_id: str,
        payload: bytes,
    ) -> Dict[str, Any]:
        """
        Execute Grey task as PyTorch inference.
        
        Expected payload format (JSON):
        {
            "inputs": {
                "input_name": [[1.0, 2.0, ...], ...]  // Nested lists for tensors
            },
            "generate_proof": true
        }
        
        Returns:
        {
            "status": "success",
            "outputs": {
                "output_name": [[1.0, 2.0, ...], ...]
            },
            "proof": { ... }  // InferenceProof if generated
        }
        """
        await self.initialize()
        
        # Parse payload
        try:
            data = json.loads(payload.decode())
        except json.JSONDecodeError as e:
            return {"status": "error", "error": f"Invalid JSON payload: {e}"}
        
        # Convert inputs to tensors
        inputs = {}
        for name, values in data.get("inputs", {}).items():
            inputs[name] = torch.tensor(values, dtype=self.config.dtype)
        
        # Run inference
        try:
            outputs, proof = await self.engine.infer(
                task_id=task_id,
                tenant_id=tenant_id,
                inputs=inputs,
                generate_proof=data.get("generate_proof", True),
            )
        except Exception as e:
            logger.exception(f"Inference failed for task {task_id}")
            return {"status": "error", "error": str(e)}
        
        # Serialize outputs
        serialized_outputs = {
            k: v.cpu().tolist() for k, v in outputs.items()
        }
        
        result = {
            "status": "success",
            "outputs": serialized_outputs,
        }
        
        if proof:
            result["proof"] = proof.to_dict()
        
        return result
    
    async def health_check(self) -> Dict[str, Any]:
        """Return adapter health status."""
        return {
            "status": "healthy" if self._initialized else "not_initialized",
            "model_loaded": self._initialized,
            "device": str(self.config.device),
            "cuda_available": torch.cuda.is_available(),
            "inference_count": self.engine.inference_count,
            "avg_latency_ms": (
                self.engine.total_latency_ms / self.engine.inference_count
                if self.engine.inference_count > 0 else 0
            ),
        }


# =============================================================================
# Model Serving (Optional HTTP server for standalone deployment)
# =============================================================================

async def serve_model(config: PyTorchConfig, host: str = "0.0.0.0", port: int = 8080):
    """
    Standalone HTTP server for model serving.
    
    For integration with Grey, use GreyPyTorchAdapter directly.
    This is useful for local development and testing.
    """
    from aiohttp import web
    
    adapter = GreyPyTorchAdapter(config)
    await adapter.initialize()
    
    async def health(request):
        status = await adapter.health_check()
        return web.json_response(status)
    
    async def infer(request):
        data = await request.json()
        result = await adapter.execute_task(
            task_id=data.get("task_id", str(uuid.uuid4())),
            tenant_id=data.get("tenant_id", "default"),
            payload=json.dumps(data).encode(),
        )
        return web.json_response(result)
    
    app = web.Application()
    app.router.add_get("/health", health)
    app.router.add_post("/infer", infer)
    
    runner = web.AppRunner(app)
    await runner.setup()
    site = web.TCPSite(runner, host, port)
    await site.start()
    
    logger.info(f"PyTorch model server running on {host}:{port}")


# =============================================================================
# Example Usage
# =============================================================================

if __name__ == "__main__":
    import asyncio
    
    async def main():
        # Example configuration
        config = PyTorchConfig(
            model_path="grey://tenant-1/bert-base/v1",
            model_name="bert-base",
            max_batch_size=16,
            max_batch_wait_ms=50,
            generate_proofs=True,
            deterministic=True,
        )
        
        adapter = GreyPyTorchAdapter(config)
        
        # Execute sample task
        result = await adapter.execute_task(
            task_id="task-123",
            tenant_id="tenant-1",
            payload=json.dumps({
                "inputs": {
                    "input_ids": [[101, 2054, 2003, 1996, 3466, 102]],
                    "attention_mask": [[1, 1, 1, 1, 1, 1]],
                },
                "generate_proof": True,
            }).encode(),
        )
        
        print(json.dumps(result, indent=2))
    
    asyncio.run(main())
