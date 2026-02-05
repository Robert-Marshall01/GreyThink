# Grey Distributed — AI Pipeline Integration

Integration patterns for PyTorch and TensorFlow inference workloads.

---

## Overview

Grey Distributed provides:

- **Distributed model serving** — Inference across worker nodes
- **TEE execution** — Confidential inference with proof artifacts
- **Reproducibility** — Deterministic results with attestation
- **Dynamic batching** — Efficient GPU utilization

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Grey Task Scheduler                           │
│                   (routes to AI-capable nodes)                   │
└────────────────────────┬────────────────────────────────────────┘
                         │
         ┌───────────────┼───────────────┐
         ▼               ▼               ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│  Worker 1   │  │  Worker 2   │  │  Worker 3   │
│  (GPU/TEE)  │  │  (GPU/TEE)  │  │  (CPU)      │
├─────────────┤  ├─────────────┤  ├─────────────┤
│ PyTorch RT  │  │ TensorFlow  │  │ ONNX RT     │
│ + TorchServe│  │ + TF Serving│  │             │
└─────────────┘  └─────────────┘  └─────────────┘
         │               │               │
         └───────────────┼───────────────┘
                         ▼
              ┌─────────────────────┐
              │   Model Registry    │
              │   (S3/GCS/Azure)    │
              └─────────────────────┘
```

---

## PyTorch Adapter

**File:** [integrations/ai/pytorch_adapter.py](../../integrations/ai/pytorch_adapter.py)

### Features

- Model loading from registry (S3, GCS, Azure Blob)
- Automatic batching with configurable window
- TorchScript optimization
- TEE proof generation
- GPU memory management

### Configuration

```yaml
pytorch:
  model_registry: s3://grey-models
  device: cuda  # or cpu, tee
  
  batching:
    enabled: true
    max_batch_size: 32
    max_wait_ms: 100
  
  optimization:
    torchscript: true
    quantization: dynamic  # none, dynamic, static
    amp: true  # Automatic Mixed Precision
  
  memory:
    max_gpu_memory_gb: 8
    cache_models: true
    max_cached_models: 5
  
  tee:
    enabled: true
    platform: sgx
    generate_proofs: true
```

### Usage

```python
from integrations.ai.pytorch_adapter import PyTorchAdapter

# Initialize adapter
adapter = PyTorchAdapter(config)

# Register model
await adapter.register_model(
    model_id="text-classifier-v1",
    model_path="s3://grey-models/text-classifier.pt",
    model_type="torchscript"
)

# Submit inference task
result = await adapter.infer(
    tenant_id="tenant-123",
    model_id="text-classifier-v1",
    inputs={"text": ["Hello world"]},
    require_proof=True  # Generate TEE attestation
)

# Result includes proof artifact
print(result.outputs)  # {"class": "greeting", "confidence": 0.95}
print(result.proof)    # {"platform": "SGX", "measurement": "..."}
```

### Proof Artifact Generation

```python
@dataclass
class InferenceProof:
    """TEE attestation for inference reproducibility."""
    
    task_id: str
    model_id: str
    model_hash: str           # SHA-256 of model weights
    input_hash: str           # SHA-256 of inputs
    output_hash: str          # SHA-256 of outputs
    platform: str             # SGX, TDX, SEV-SNP
    measurement: str          # MRENCLAVE/MRTD/etc
    report_data: bytes        # Platform-specific report
    signature: bytes          # Signed by TEE
    timestamp: int            # Unix timestamp
```

### Batch Processing

```python
# Configure batching
adapter.configure_batching(
    max_batch_size=32,
    max_wait_ms=100,
    pad_to_multiple=8  # For tensor cores
)

# Submit many requests (automatically batched)
tasks = [
    adapter.infer(tenant_id, model_id, {"text": text})
    for text in documents
]
results = await asyncio.gather(*tasks)
```

**Tradeoffs:**
- Higher batch size → Better throughput, higher latency
- Lower max_wait → Lower latency, smaller batches
- Grey defaults: 32 batch size, 100ms wait

---

## TensorFlow Adapter

**File:** [integrations/ai/tensorflow_adapter.py](../../integrations/ai/tensorflow_adapter.py)

### Features

- SavedModel and TFLite support
- TF Serving integration (optional)
- XLA compilation
- Distributed inference with tf.distribute
- TEE proof generation

### Configuration

```yaml
tensorflow:
  model_registry: s3://grey-models
  device: gpu  # cpu, gpu, tpu
  
  serving:
    use_tf_serving: false  # Use embedded or TF Serving
    tf_serving_url: http://tf-serving:8501
  
  optimization:
    xla: true
    mixed_precision: true
    parallelize: true
  
  batching:
    enabled: true
    max_batch_size: 64
    batch_timeout_micros: 100000
  
  tee:
    enabled: true
    platform: sev-snp
    generate_proofs: true
```

### Usage

```python
from integrations.ai.tensorflow_adapter import TensorFlowAdapter

# Initialize adapter
adapter = TensorFlowAdapter(config)

# Load SavedModel
await adapter.load_model(
    model_id="image-classifier-v2",
    model_path="s3://grey-models/image-classifier/saved_model"
)

# Run inference
result = await adapter.infer(
    tenant_id="tenant-123",
    model_id="image-classifier-v2",
    inputs={"image": image_tensor},
    require_proof=True
)
```

### TF Serving Integration

```python
# Configure TF Serving mode
adapter = TensorFlowAdapter(
    config,
    tf_serving_url="http://tf-serving:8501"
)

# Requests are proxied to TF Serving
# But proofs are still generated by Grey
result = await adapter.infer(
    tenant_id="tenant-123",
    model_id="image-classifier-v2",
    inputs={"instances": [{"image": base64_image}]},
    require_proof=True
)
```

---

## Model Registry

### Supported Backends

| Backend | URI Format | Features |
|---------|------------|----------|
| S3 | `s3://bucket/path` | Versioning, lifecycle |
| GCS | `gs://bucket/path` | Versioning, IAM |
| Azure Blob | `azure://container/path` | Geo-redundancy |
| HTTP | `https://host/path` | CDN, caching |

### Model Versioning

```python
# Register versioned model
await adapter.register_model(
    model_id="classifier",
    model_path="s3://models/classifier/v2",
    version="v2",
    metadata={
        "accuracy": 0.95,
        "training_date": "2024-01-15"
    }
)

# Load specific version
await adapter.load_model("classifier", version="v2")

# Load latest
await adapter.load_model("classifier", version="latest")

# List versions
versions = await adapter.list_model_versions("classifier")
```

### Model Caching

```python
# Configure cache
adapter.configure_cache(
    max_models=5,
    max_memory_gb=16,
    eviction_policy="lru"  # or "lfu"
)

# Warm cache on startup
await adapter.warm_cache([
    ("classifier-v1", "v1"),
    ("embeddings", "latest")
])
```

---

## TEE Integration

### SGX Inference

```python
# Configure SGX enclave
adapter = PyTorchAdapter(
    config,
    tee_config={
        "platform": "sgx",
        "enclave_size_mb": 256,
        "heap_size_mb": 128,
        "enable_remote_attestation": True
    }
)

# All inference runs inside enclave
result = await adapter.infer(
    tenant_id="tenant-123",
    model_id="private-model",
    inputs=encrypted_inputs,
    require_proof=True
)

# Verify attestation
verified = await adapter.verify_attestation(result.proof)
```

### Proof Verification

```python
from integrations.ai.proof_verifier import verify_inference_proof

# Verify proof independently
proof = result.proof

# Check platform attestation
assert verify_inference_proof(
    proof,
    expected_mrenclave="abc123...",
    expected_model_hash="def456..."
)

# Reproducibility check
# Re-run with same inputs should produce same output_hash
```

---

## Distributed Inference

### Sharded Models

For large models that don't fit on single GPU:

```python
# Configure model parallelism
adapter = PyTorchAdapter(
    config,
    model_parallel={
        "enabled": True,
        "num_gpus": 4,
        "strategy": "pipeline"  # or "tensor"
    }
)

# Load sharded model
await adapter.load_sharded_model(
    model_id="large-llm",
    shard_paths=[
        "s3://models/llm/shard-0",
        "s3://models/llm/shard-1",
        "s3://models/llm/shard-2",
        "s3://models/llm/shard-3"
    ]
)
```

### Multi-Node Inference

```python
# Grey scheduler handles node selection
result = await grey_client.submit_task(
    task_type="ai_inference",
    payload={
        "model_id": "large-llm",
        "inputs": {"prompt": "Once upon a time..."},
        "generate_length": 100
    },
    constraints={
        "gpu_count": 4,
        "gpu_type": "A100",
        "memory_gb": 80
    }
)
```

---

## Performance Optimization

### Quantization

```python
# Dynamic quantization (inference-time)
adapter.quantize_model("classifier", strategy="dynamic")

# Static quantization (requires calibration)
adapter.calibrate_model(
    "classifier",
    calibration_data=sample_inputs,
    strategy="static",
    dtype="int8"
)
```

### Mixed Precision

```python
# Enable AMP (Automatic Mixed Precision)
adapter.enable_amp()

# Result: ~2x throughput on modern GPUs
# Tradeoff: Slight accuracy loss for some models
```

### Request Coalescing

```python
# Group similar requests for efficiency
adapter.configure_coalescing(
    group_by=["model_id", "input_shape"],
    max_group_size=16,
    max_wait_ms=50
)
```

---

## Monitoring

### Metrics

```prometheus
# Inference latency
grey_ai_inference_duration_seconds{model="classifier",quantile="0.99"}

# Throughput
grey_ai_inferences_total{model="classifier",tenant="tenant-123"}

# Batch size distribution
grey_ai_batch_size{model="classifier",quantile="0.5"}

# GPU utilization
grey_ai_gpu_utilization{node="worker-1",device="0"}

# Model cache
grey_ai_model_cache_hits_total
grey_ai_model_cache_misses_total
grey_ai_model_cache_memory_bytes
```

### Alerts

```yaml
groups:
  - name: grey-ai
    rules:
      - alert: HighInferenceLatency
        expr: histogram_quantile(0.99, grey_ai_inference_duration_seconds) > 1
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "P99 inference latency > 1s"
          
      - alert: GPUMemoryExhaustion
        expr: grey_ai_gpu_memory_used / grey_ai_gpu_memory_total > 0.95
        for: 1m
        labels:
          severity: critical
```

---

## Best Practices

### 1. Model Preparation

```python
# Always trace/compile models before deployment
import torch
model = torch.jit.trace(model, example_input)
torch.jit.save(model, "model.pt")

# Validate on target hardware
adapter.validate_model("classifier", test_inputs)
```

### 2. Input Validation

```python
# Validate shapes and types
async def infer(self, inputs):
    self._validate_inputs(inputs)  # Fail fast
    return await self._run_inference(inputs)
```

### 3. Graceful Degradation

```python
# Fallback to CPU if GPU unavailable
async def infer(self, inputs):
    try:
        return await self._gpu_inference(inputs)
    except GPUUnavailable:
        return await self._cpu_inference(inputs)
```

### 4. Resource Quotas

```yaml
# Per-tenant limits
quotas:
  tenant-123:
    max_concurrent_inferences: 10
    max_gpu_seconds_per_hour: 3600
    allowed_models:
      - classifier-v1
      - embeddings-v2
```

---

*AI adapters are in [integrations/ai/](../../integrations/ai/)*
