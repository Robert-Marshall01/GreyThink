"""
TensorFlow Adapter for Grey Distributed

This adapter enables distributed TensorFlow/Keras inference jobs across Grey
worker nodes. It provides SavedModel serving, batch processing, and generates
proof artifacts for inference reproducibility.

Architecture
============

TensorFlow Serving integration with Grey's task distribution:

┌─────────────────┐      ┌──────────────────┐      ┌─────────────────┐
│  Grey Scheduler │──────│ TensorFlow Adapt │──────│   TF Serving    │
│  (task routing) │      │  (model router)  │      │  (GPU workers)  │
└─────────────────┘      └──────────────────┘      └─────────────────┘

Key Design Decisions
====================

1. **SavedModel Format**: Uses TF SavedModel for portability and versioning.
   Includes model graph, weights, and signatures in one artifact.

2. **Multi-Model Serving**: Single adapter can serve multiple models.
   Grey routes requests to appropriate model based on task metadata.

3. **Proof Generation**: Each inference generates a proof containing:
   - Model signature hash
   - Input/output tensor hashes  
   - Execution environment fingerprint

4. **Batch Scheduling**: TensorFlow's built-in batching combined with
   Grey's scheduling for optimal resource utilization.

TensorFlow vs PyTorch Trade-offs
================================

| Aspect          | TensorFlow                 | PyTorch                |
|-----------------|----------------------------|------------------------|
| Graph Mode      | Static graph (XLA)         | Dynamic graph          |
| Serving         | TF Serving (gRPC)          | TorchServe             |
| Mobile          | TF Lite (optimized)        | PyTorch Mobile         |
| Quantization    | Post-training + QAT        | Dynamic + static       |
| Debugging       | Harder (graph mode)        | Easier (eager mode)    |

Choose TensorFlow for:
- Production deployments with TF Serving infrastructure
- Mobile/edge deployment with TF Lite
- XLA optimization requirements
"""

import hashlib
import json
import os
import time
import uuid
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple, Union
from enum import Enum
import asyncio
import logging
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor

import numpy as np

# Suppress TF warnings
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'

import tensorflow as tf
from tensorflow import keras

logger = logging.getLogger(__name__)


# =============================================================================
# Configuration
# =============================================================================

@dataclass
class TensorFlowConfig:
    """Configuration for TensorFlow adapter."""
    
    # Model settings
    model_path: str = ""
    model_name: str = "default"
    model_version: int = 1
    
    # Execution settings
    device: str = "/GPU:0" if tf.config.list_physical_devices('GPU') else "/CPU:0"
    use_xla: bool = True  # XLA compilation for performance
    mixed_precision: bool = True  # FP16 compute, FP32 accumulate
    
    # Batching settings
    max_batch_size: int = 32
    batch_timeout_ms: int = 50
    
    # Model parallelism
    num_gpus: int = 1
    enable_mirrored_strategy: bool = False
    
    # Caching
    enable_signature_cache: bool = True
    cache_max_models: int = 10
    
    # Reproducibility
    deterministic: bool = True
    random_seed: int = 42
    
    # Proof generation
    generate_proofs: bool = True
    include_signature_hash: bool = True
    
    # Timeouts
    inference_timeout_ms: int = 30000
    model_load_timeout_s: int = 300
    
    # TF Serving integration (optional)
    tf_serving_host: Optional[str] = None
    tf_serving_port: int = 8500
    tf_serving_grpc: bool = True


# =============================================================================
# Proof Artifacts
# =============================================================================

@dataclass
class TFInferenceProof:
    """
    Cryptographic proof artifact for TensorFlow inference.
    
    Similar to PyTorch proof but includes TensorFlow-specific metadata
    like SavedModel signatures and XLA compilation status.
    """
    
    # Identifiers
    proof_id: str
    task_id: str
    tenant_id: str
    
    # Model identification
    model_name: str
    model_version: int
    signature_name: str
    signature_hash: str  # Hash of signature inputs/outputs definition
    
    # Input/output hashes
    input_hash: str
    output_hash: str
    
    # Reproducibility
    random_seed: int
    tf_version: str
    cuda_version: Optional[str]
    xla_enabled: bool
    mixed_precision: bool
    deterministic: bool
    
    # Execution context
    worker_node: str
    execution_time_ms: float
    timestamp: str
    
    # TEE attestation
    tee_attestation: Optional[str] = None
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "proof_id": self.proof_id,
            "task_id": self.task_id,
            "tenant_id": self.tenant_id,
            "model": {
                "name": self.model_name,
                "version": self.model_version,
                "signature_name": self.signature_name,
                "signature_hash": self.signature_hash,
            },
            "io_hashes": {
                "input": self.input_hash,
                "output": self.output_hash,
            },
            "reproducibility": {
                "random_seed": self.random_seed,
                "tf_version": self.tf_version,
                "cuda_version": self.cuda_version,
                "xla_enabled": self.xla_enabled,
                "mixed_precision": self.mixed_precision,
                "deterministic": self.deterministic,
            },
            "execution": {
                "worker_node": self.worker_node,
                "execution_time_ms": self.execution_time_ms,
                "timestamp": self.timestamp,
            },
            "tee": {
                "attestation": self.tee_attestation,
            } if self.tee_attestation else None,
        }
    
    def to_json(self) -> str:
        return json.dumps(self.to_dict(), indent=2)
    
    @staticmethod
    def compute_hash(data: Union[tf.Tensor, np.ndarray, List, bytes]) -> str:
        """Compute SHA-256 hash of tensor(s)."""
        hasher = hashlib.sha256()
        
        if isinstance(data, bytes):
            hasher.update(data)
        elif isinstance(data, tf.Tensor):
            hasher.update(data.numpy().tobytes())
        elif isinstance(data, np.ndarray):
            hasher.update(data.tobytes())
        elif isinstance(data, dict):
            for key in sorted(data.keys()):
                val = data[key]
                if isinstance(val, tf.Tensor):
                    hasher.update(val.numpy().tobytes())
                elif isinstance(val, np.ndarray):
                    hasher.update(val.tobytes())
        elif isinstance(data, list):
            for item in data:
                if isinstance(item, tf.Tensor):
                    hasher.update(item.numpy().tobytes())
                elif isinstance(item, np.ndarray):
                    hasher.update(item.tobytes())
        
        return hasher.hexdigest()


# =============================================================================
# Model Manager
# =============================================================================

@dataclass
class LoadedModel:
    """Container for loaded TensorFlow model."""
    model: Any  # tf.saved_model or keras.Model
    signature: Any  # Callable signature
    signature_hash: str
    input_specs: Dict[str, tf.TensorSpec]
    output_specs: Dict[str, tf.TensorSpec]
    version: int


class TFModelManager:
    """
    Manages TensorFlow model lifecycle.
    
    Design Notes
    ------------
    
    1. **SavedModel Loading**: Prefers tf.saved_model.load() over keras.load_model()
       for production deployments. SavedModel includes concrete functions.
    
    2. **Signature Selection**: SavedModels can have multiple signatures.
       Default is 'serving_default', but custom signatures can be specified.
    
    3. **XLA Compilation**: Models can be JIT-compiled with XLA for performance.
       Trade-off: Compilation adds startup latency but improves inference speed.
    
    4. **Memory Management**: TensorFlow manages GPU memory automatically.
       Use tf.config.experimental.set_memory_growth() to avoid OOM.
    """
    
    def __init__(self, config: TensorFlowConfig):
        self.config = config
        self.models: Dict[str, LoadedModel] = {}
        self._load_lock = asyncio.Lock()
        
        # Configure TF for reproducibility
        if config.deterministic:
            tf.random.set_seed(config.random_seed)
            os.environ['TF_DETERMINISTIC_OPS'] = '1'
        
        # Configure mixed precision
        if config.mixed_precision:
            policy = keras.mixed_precision.Policy('mixed_float16')
            keras.mixed_precision.set_global_policy(policy)
        
        # Configure GPU memory growth
        for gpu in tf.config.list_physical_devices('GPU'):
            tf.config.experimental.set_memory_growth(gpu, True)
    
    async def load_model(
        self,
        model_path: str,
        model_name: Optional[str] = None,
        signature_name: str = "serving_default",
    ) -> LoadedModel:
        """
        Load SavedModel from path.
        
        Parameters
        ----------
        model_path : str
            Path to SavedModel directory or grey:// URI
        model_name : str
            Name for model registry
        signature_name : str
            Which signature to use for inference
            
        Returns
        -------
        LoadedModel with signature ready for inference
        """
        model_name = model_name or self.config.model_name
        
        async with self._load_lock:
            if model_name in self.models:
                logger.info(f"Model {model_name} already loaded")
                return self.models[model_name]
            
            logger.info(f"Loading model {model_name} from {model_path}")
            start_time = time.time()
            
            # Handle Grey storage URIs
            if model_path.startswith("grey://"):
                model_path = await self._download_from_grey(model_path)
            
            # Load in thread pool
            loop = asyncio.get_event_loop()
            with ThreadPoolExecutor() as executor:
                model, signature, input_specs, output_specs = await loop.run_in_executor(
                    executor,
                    self._load_model_sync,
                    model_path,
                    signature_name,
                )
            
            # Compute signature hash for proofs
            signature_hash = self._compute_signature_hash(input_specs, output_specs)
            
            loaded = LoadedModel(
                model=model,
                signature=signature,
                signature_hash=signature_hash,
                input_specs=input_specs,
                output_specs=output_specs,
                version=self.config.model_version,
            )
            
            self.models[model_name] = loaded
            
            load_time = time.time() - start_time
            logger.info(f"Model {model_name} loaded in {load_time:.2f}s")
            
            return loaded
    
    def _load_model_sync(
        self,
        path: str,
        signature_name: str,
    ) -> Tuple[Any, Any, Dict, Dict]:
        """Synchronous model loading."""
        
        # Load SavedModel
        model = tf.saved_model.load(path)
        
        # Get signature
        if hasattr(model, 'signatures'):
            signature = model.signatures[signature_name]
        else:
            # Fallback for Keras models
            signature = model
        
        # Extract input/output specs
        input_specs = {}
        output_specs = {}
        
        if hasattr(signature, 'structured_input_signature'):
            for name, spec in signature.structured_input_signature[1].items():
                input_specs[name] = spec
        
        if hasattr(signature, 'structured_outputs'):
            for name, spec in signature.structured_outputs.items():
                if hasattr(spec, 'dtype'):
                    output_specs[name] = tf.TensorSpec(
                        shape=spec.shape,
                        dtype=spec.dtype,
                        name=name,
                    )
        
        # Optional XLA compilation
        if self.config.use_xla:
            signature = tf.function(signature, jit_compile=True)
        
        return model, signature, input_specs, output_specs
    
    def _compute_signature_hash(
        self,
        input_specs: Dict[str, tf.TensorSpec],
        output_specs: Dict[str, tf.TensorSpec],
    ) -> str:
        """Compute hash of model signature for proofs."""
        hasher = hashlib.sha256()
        
        for name in sorted(input_specs.keys()):
            spec = input_specs[name]
            hasher.update(f"in:{name}:{spec.dtype}:{spec.shape}".encode())
        
        for name in sorted(output_specs.keys()):
            spec = output_specs[name]
            hasher.update(f"out:{name}:{spec.dtype}:{spec.shape}".encode())
        
        return hasher.hexdigest()
    
    async def _download_from_grey(self, uri: str) -> str:
        """Download model from Grey storage."""
        parts = uri.replace("grey://", "").split("/")
        tenant, model_name = parts[0], parts[1]
        version = parts[2] if len(parts) > 2 else "latest"
        
        local_path = f"/tmp/grey_models/{tenant}/{model_name}/{version}"
        Path(local_path).mkdir(parents=True, exist_ok=True)
        
        logger.info(f"Downloading {uri} to {local_path}")
        # In production: use Grey client
        
        return local_path
    
    def get_model(self, model_name: Optional[str] = None) -> LoadedModel:
        """Get loaded model by name."""
        model_name = model_name or self.config.model_name
        if model_name not in self.models:
            raise RuntimeError(f"Model {model_name} not loaded")
        return self.models[model_name]


# =============================================================================
# Batching
# =============================================================================

@dataclass 
class TFInferenceRequest:
    """Single inference request."""
    request_id: str
    task_id: str
    tenant_id: str
    model_name: str
    inputs: Dict[str, np.ndarray]
    created_at: float = field(default_factory=time.time)


class TFDynamicBatcher:
    """
    Dynamic batching for TensorFlow inference.
    
    Uses NumPy for batching before converting to TensorFlow tensors.
    This avoids multiple GPU memory allocations during batch formation.
    """
    
    def __init__(self, max_batch_size: int, timeout_ms: int):
        self.max_batch_size = max_batch_size
        self.timeout_ms = timeout_ms
        self.pending: Dict[str, List[TFInferenceRequest]] = {}  # by model
        self.lock = asyncio.Lock()
    
    async def add_request(self, request: TFInferenceRequest) -> None:
        """Add request to pending batch for its model."""
        async with self.lock:
            if request.model_name not in self.pending:
                self.pending[request.model_name] = []
            self.pending[request.model_name].append(request)
    
    async def get_batch(self, model_name: str) -> Optional[List[TFInferenceRequest]]:
        """Get batch for a specific model."""
        await asyncio.sleep(self.timeout_ms / 1000)  # Wait for accumulation
        
        async with self.lock:
            if model_name not in self.pending or not self.pending[model_name]:
                return None
            
            batch = self.pending[model_name][:self.max_batch_size]
            self.pending[model_name] = self.pending[model_name][self.max_batch_size:]
            
            return batch
    
    def batch_inputs(
        self,
        requests: List[TFInferenceRequest],
    ) -> Dict[str, tf.Tensor]:
        """Combine request inputs into batched tensors."""
        if not requests:
            return {}
        
        batched = {}
        input_keys = requests[0].inputs.keys()
        
        for key in input_keys:
            arrays = [r.inputs[key] for r in requests]
            # Pad if needed
            max_len = max(a.shape[0] if len(a.shape) > 0 else 1 for a in arrays)
            padded = []
            for a in arrays:
                if len(a.shape) > 0 and a.shape[0] < max_len:
                    pad_width = [(0, max_len - a.shape[0])] + [(0, 0)] * (len(a.shape) - 1)
                    a = np.pad(a, pad_width, mode='constant')
                padded.append(a)
            batched[key] = tf.constant(np.stack(padded))
        
        return batched


# =============================================================================
# Inference Engine
# =============================================================================

class TensorFlowInferenceEngine:
    """
    Main TensorFlow inference engine for Grey.
    
    Supports:
    - Single and batched inference
    - Multiple model serving
    - Proof generation
    - TF Serving integration (optional)
    """
    
    def __init__(self, config: TensorFlowConfig):
        self.config = config
        self.model_manager = TFModelManager(config)
        self.batcher = TFDynamicBatcher(config.max_batch_size, config.batch_timeout_ms)
        self.worker_node = f"grey-tf-{uuid.uuid4().hex[:8]}"
        
        # Metrics
        self.inference_count = 0
        self.total_latency_ms = 0.0
    
    async def initialize(self, model_path: Optional[str] = None) -> None:
        """Initialize with default model."""
        path = model_path or self.config.model_path
        if path:
            await self.model_manager.load_model(path)
    
    async def infer(
        self,
        task_id: str,
        tenant_id: str,
        inputs: Dict[str, np.ndarray],
        model_name: Optional[str] = None,
        generate_proof: bool = True,
    ) -> Tuple[Dict[str, np.ndarray], Optional[TFInferenceProof]]:
        """
        Run single inference.
        
        Parameters
        ----------
        task_id : str
            Grey task ID
        tenant_id : str
            Tenant ID
        inputs : Dict[str, np.ndarray]
            Input tensors as numpy arrays
        model_name : str
            Which model to use (if multi-model)
        generate_proof : bool
            Whether to generate proof artifact
            
        Returns
        -------
        outputs : Dict[str, np.ndarray]
            Output tensors as numpy arrays
        proof : Optional[TFInferenceProof]
            Proof artifact if requested
        """
        start_time = time.time()
        
        model_name = model_name or self.config.model_name
        loaded = self.model_manager.get_model(model_name)
        
        # Convert to TensorFlow tensors
        tf_inputs = {k: tf.constant(v) for k, v in inputs.items()}
        
        # Run inference
        with tf.device(self.config.device):
            outputs = loaded.signature(**tf_inputs)
        
        # Convert outputs to numpy
        np_outputs = {}
        if isinstance(outputs, dict):
            np_outputs = {k: v.numpy() for k, v in outputs.items()}
        elif isinstance(outputs, tf.Tensor):
            np_outputs = {"output": outputs.numpy()}
        
        execution_time_ms = (time.time() - start_time) * 1000
        self.inference_count += 1
        self.total_latency_ms += execution_time_ms
        
        # Generate proof
        proof = None
        if generate_proof and self.config.generate_proofs:
            proof = self._generate_proof(
                task_id=task_id,
                tenant_id=tenant_id,
                model_name=model_name,
                loaded=loaded,
                inputs=inputs,
                outputs=np_outputs,
                execution_time_ms=execution_time_ms,
            )
        
        return np_outputs, proof
    
    async def infer_batch(
        self,
        requests: List[TFInferenceRequest],
        generate_proofs: bool = True,
    ) -> List[Tuple[Dict[str, np.ndarray], Optional[TFInferenceProof]]]:
        """Run batched inference."""
        if not requests:
            return []
        
        start_time = time.time()
        model_name = requests[0].model_name
        loaded = self.model_manager.get_model(model_name)
        
        # Batch inputs
        batched_inputs = self.batcher.batch_inputs(requests)
        
        # Run inference
        with tf.device(self.config.device):
            batch_outputs = loaded.signature(**batched_inputs)
        
        # Convert and split outputs
        if isinstance(batch_outputs, dict):
            batch_np = {k: v.numpy() for k, v in batch_outputs.items()}
        else:
            batch_np = {"output": batch_outputs.numpy()}
        
        execution_time_ms = (time.time() - start_time) * 1000 / len(requests)
        
        results = []
        for i, req in enumerate(requests):
            individual_outputs = {k: v[i] for k, v in batch_np.items()}
            
            proof = None
            if generate_proofs and self.config.generate_proofs:
                proof = self._generate_proof(
                    task_id=req.task_id,
                    tenant_id=req.tenant_id,
                    model_name=model_name,
                    loaded=loaded,
                    inputs=req.inputs,
                    outputs=individual_outputs,
                    execution_time_ms=execution_time_ms,
                )
            
            results.append((individual_outputs, proof))
        
        return results
    
    def _generate_proof(
        self,
        task_id: str,
        tenant_id: str,
        model_name: str,
        loaded: LoadedModel,
        inputs: Dict[str, np.ndarray],
        outputs: Dict[str, np.ndarray],
        execution_time_ms: float,
    ) -> TFInferenceProof:
        """Generate inference proof."""
        
        input_hash = TFInferenceProof.compute_hash(inputs)
        output_hash = TFInferenceProof.compute_hash(outputs)
        
        cuda_version = None
        if tf.config.list_physical_devices('GPU'):
            # Get CUDA version from TF build info
            cuda_version = tf.sysconfig.get_build_info().get('cuda_version', 'unknown')
        
        return TFInferenceProof(
            proof_id=f"proof_{uuid.uuid4().hex}",
            task_id=task_id,
            tenant_id=tenant_id,
            model_name=model_name,
            model_version=loaded.version,
            signature_name="serving_default",
            signature_hash=loaded.signature_hash,
            input_hash=input_hash,
            output_hash=output_hash,
            random_seed=self.config.random_seed,
            tf_version=tf.__version__,
            cuda_version=cuda_version,
            xla_enabled=self.config.use_xla,
            mixed_precision=self.config.mixed_precision,
            deterministic=self.config.deterministic,
            worker_node=self.worker_node,
            execution_time_ms=execution_time_ms,
            timestamp=time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        )


# =============================================================================
# TF Serving Integration
# =============================================================================

class TFServingClient:
    """
    Client for TensorFlow Serving when using external serving infrastructure.
    
    Use Cases
    ---------
    
    1. **Existing TF Serving Deployment**: Connect to pre-existing infrastructure
    2. **Kubernetes**: TF Serving pods with Grey routing
    3. **Multi-Model**: Serve many models from single TF Serving instance
    
    Trade-offs vs Embedded
    ----------------------
    
    TF Serving (external):
    + Optimized for production serving
    + Supports model versioning and A/B testing
    + Better resource isolation
    - Additional infrastructure complexity
    - Network latency overhead
    
    Embedded (in-process):
    + Simpler deployment
    + Lower latency (no network hop)
    - Less flexibility for model updates
    - Memory shared with application
    """
    
    def __init__(self, host: str, port: int, use_grpc: bool = True):
        self.host = host
        self.port = port
        self.use_grpc = use_grpc
        self._channel = None
        self._stub = None
    
    async def connect(self):
        """Establish connection to TF Serving."""
        if self.use_grpc:
            import grpc
            from tensorflow_serving.apis import prediction_service_pb2_grpc
            
            self._channel = grpc.aio.insecure_channel(f"{self.host}:{self.port}")
            self._stub = prediction_service_pb2_grpc.PredictionServiceStub(self._channel)
        
        logger.info(f"Connected to TF Serving at {self.host}:{self.port}")
    
    async def predict(
        self,
        model_name: str,
        inputs: Dict[str, np.ndarray],
        model_version: Optional[int] = None,
        signature_name: str = "serving_default",
    ) -> Dict[str, np.ndarray]:
        """Send prediction request to TF Serving."""
        if self.use_grpc:
            return await self._predict_grpc(model_name, inputs, model_version, signature_name)
        else:
            return await self._predict_rest(model_name, inputs, model_version, signature_name)
    
    async def _predict_grpc(
        self,
        model_name: str,
        inputs: Dict[str, np.ndarray],
        model_version: Optional[int],
        signature_name: str,
    ) -> Dict[str, np.ndarray]:
        """gRPC prediction request."""
        from tensorflow_serving.apis import predict_pb2
        from tensorflow_serving.apis.model_pb2 import ModelSpec
        
        request = predict_pb2.PredictRequest()
        request.model_spec.name = model_name
        request.model_spec.signature_name = signature_name
        
        if model_version:
            request.model_spec.version.value = model_version
        
        for name, array in inputs.items():
            request.inputs[name].CopyFrom(tf.make_tensor_proto(array))
        
        response = await self._stub.Predict(request)
        
        outputs = {}
        for name, tensor_proto in response.outputs.items():
            outputs[name] = tf.make_ndarray(tensor_proto)
        
        return outputs
    
    async def _predict_rest(
        self,
        model_name: str,
        inputs: Dict[str, np.ndarray],
        model_version: Optional[int],
        signature_name: str,
    ) -> Dict[str, np.ndarray]:
        """REST prediction request."""
        import aiohttp
        
        version_path = f"/v{model_version}" if model_version else ""
        url = f"http://{self.host}:{self.port}/v1/models/{model_name}{version_path}:predict"
        
        payload = {
            "signature_name": signature_name,
            "inputs": {k: v.tolist() for k, v in inputs.items()},
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.post(url, json=payload) as resp:
                result = await resp.json()
        
        return {k: np.array(v) for k, v in result.get("outputs", {}).items()}
    
    async def close(self):
        """Close connection."""
        if self._channel:
            await self._channel.close()


# =============================================================================
# Grey Integration
# =============================================================================

class GreyTensorFlowAdapter:
    """
    Integration layer between Grey Distributed and TensorFlow.
    
    Implements Grey's task execution interface for TensorFlow inference.
    """
    
    def __init__(self, config: TensorFlowConfig):
        self.config = config
        
        # Use TF Serving if configured, otherwise embedded
        if config.tf_serving_host:
            self.serving_client = TFServingClient(
                config.tf_serving_host,
                config.tf_serving_port,
                config.tf_serving_grpc,
            )
            self.engine = None
        else:
            self.serving_client = None
            self.engine = TensorFlowInferenceEngine(config)
        
        self._initialized = False
    
    async def initialize(self) -> None:
        """Initialize adapter."""
        if self._initialized:
            return
        
        if self.serving_client:
            await self.serving_client.connect()
        elif self.engine:
            await self.engine.initialize()
        
        self._initialized = True
    
    async def execute_task(
        self,
        task_id: str,
        tenant_id: str,
        payload: bytes,
    ) -> Dict[str, Any]:
        """
        Execute Grey task as TensorFlow inference.
        
        Expected payload format (JSON):
        {
            "model_name": "bert-base",  // optional, uses default
            "inputs": {
                "input_ids": [[101, 2054, 2003, ...]],
                "attention_mask": [[1, 1, 1, ...]]
            },
            "generate_proof": true
        }
        
        Returns:
        {
            "status": "success",
            "outputs": { ... },
            "proof": { ... }
        }
        """
        await self.initialize()
        
        try:
            data = json.loads(payload.decode())
        except json.JSONDecodeError as e:
            return {"status": "error", "error": f"Invalid JSON: {e}"}
        
        model_name = data.get("model_name", self.config.model_name)
        inputs = {k: np.array(v) for k, v in data.get("inputs", {}).items()}
        generate_proof = data.get("generate_proof", True)
        
        try:
            if self.serving_client:
                outputs_np = await self.serving_client.predict(model_name, inputs)
                proof = None  # External serving doesn't generate proofs
            else:
                outputs_np, proof = await self.engine.infer(
                    task_id=task_id,
                    tenant_id=tenant_id,
                    inputs=inputs,
                    model_name=model_name,
                    generate_proof=generate_proof,
                )
        except Exception as e:
            logger.exception(f"Inference failed for task {task_id}")
            return {"status": "error", "error": str(e)}
        
        result = {
            "status": "success",
            "outputs": {k: v.tolist() for k, v in outputs_np.items()},
        }
        
        if proof:
            result["proof"] = proof.to_dict()
        
        return result
    
    async def health_check(self) -> Dict[str, Any]:
        """Return adapter health."""
        return {
            "status": "healthy" if self._initialized else "not_initialized",
            "mode": "tf_serving" if self.serving_client else "embedded",
            "tf_version": tf.__version__,
            "gpu_available": bool(tf.config.list_physical_devices('GPU')),
            "xla_enabled": self.config.use_xla,
            "inference_count": self.engine.inference_count if self.engine else 0,
        }
    
    async def shutdown(self):
        """Clean shutdown."""
        if self.serving_client:
            await self.serving_client.close()


# =============================================================================
# Example Usage
# =============================================================================

if __name__ == "__main__":
    async def main():
        config = TensorFlowConfig(
            model_path="grey://tenant-1/bert/v1",
            model_name="bert",
            use_xla=True,
            mixed_precision=True,
            generate_proofs=True,
        )
        
        adapter = GreyTensorFlowAdapter(config)
        
        result = await adapter.execute_task(
            task_id="task-456",
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
