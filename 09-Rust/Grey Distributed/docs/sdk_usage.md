# Grey Distributed — SDK Usage Guide

This guide covers using the Python and JavaScript SDKs to interact with Grey Distributed clusters.

---

## Table of Contents

1. [Installation](#installation)
2. [Authentication](#authentication)
3. [Python SDK](#python-sdk)
   - [Basic Usage](#python-basic-usage)
   - [Task Submission](#python-task-submission)
   - [Task Monitoring](#python-task-monitoring)
   - [Pipelines](#python-pipelines)
   - [Error Handling](#python-error-handling)
   - [Advanced Patterns](#python-advanced-patterns)
4. [JavaScript SDK](#javascript-sdk)
   - [Basic Usage](#js-basic-usage)
   - [Task Submission](#js-task-submission)
   - [Task Monitoring](#js-task-monitoring)
   - [Browser Integration](#browser-integration)
   - [Error Handling](#js-error-handling)
5. [Common Patterns](#common-patterns)
6. [Best Practices](#best-practices)
7. [Troubleshooting](#troubleshooting)

---

## Installation

### Python SDK

```bash
# From PyPI
pip install grey-distributed

# From source
pip install git+https://github.com/grey-io/grey-python.git

# With optional dependencies
pip install grey-distributed[async,grpc]
```

**Requirements:**
- Python 3.9+
- `requests` (sync client)
- `aiohttp` (async client, optional)
- `grpcio` (gRPC support, optional)

### JavaScript SDK

```bash
# npm
npm install @grey-io/grey-client

# yarn
yarn add @grey-io/grey-client

# pnpm
pnpm add @grey-io/grey-client
```

**Requirements:**
- Node.js 18+ or modern browser
- Fetch API (native or polyfill)

---

## Authentication

Grey Distributed uses API keys scoped to tenants. Each API key provides:
- Tenant isolation (your tasks are invisible to other tenants)
- Rate limiting and quota enforcement
- Audit trail for compliance

### Obtaining API Keys

1. Log into the Grey Console: `https://console.grey.io`
2. Navigate to Settings → API Keys
3. Create a new key with appropriate permissions
4. Store securely (keys are shown only once)

### Environment Variables

Both SDKs respect these environment variables:

```bash
export GREY_API_KEY="your-api-key"
export GREY_API_ENDPOINT="https://api.grey.io"  # or your cluster URL
export GREY_TENANT_ID="your-tenant-id"          # optional, embedded in key
```

---

## Python SDK

### Python Basic Usage

```python
from grey_distributed import GreyClient

# Initialize with environment variables
client = GreyClient()

# Or with explicit configuration
client = GreyClient(
    endpoint="https://api.grey.io",
    api_key="your-api-key",
    tenant_id="your-tenant",
    timeout=30.0
)

# Check cluster health
health = client.cluster_health()
print(f"Cluster healthy: {health.healthy}")
print(f"Active nodes: {health.nodes['active']}")
```

### Python Task Submission

#### Stateless Tasks

Stateless tasks are pure functions with no side effects:

```python
# Simple task
result = client.submit_task(
    payload={"numbers": [1, 2, 3, 4, 5]},
    task_type="stateless"
)
print(f"Task ID: {result.task_id}")

# With all options
result = client.submit_task(
    payload={
        "function": "process_data",
        "data": {"key": "value"}
    },
    task_type="stateless",
    priority="high",              # low, normal, high, critical
    timeout=300,                  # 5 minute timeout
    idempotency_key="process-123", # Prevents duplicate submission
    labels={"env": "production", "team": "ml"},
    tee_required=True             # Require TEE execution
)
```

#### Stateful Tasks

Stateful tasks maintain state across executions:

```python
# Create stateful task with initial state
result = client.submit_task(
    payload={"action": "initialize"},
    task_type="stateful",
    state_key="session-12345",    # Unique key for state
    initial_state={"counter": 0}
)

# Update state in subsequent call
result = client.submit_task(
    payload={"action": "increment"},
    task_type="stateful",
    state_key="session-12345"     # Same key to access state
)
```

#### Batch Tasks

Submit multiple tasks efficiently:

```python
# Prepare batch
tasks = [
    {"payload": {"id": i, "data": f"item-{i}"}}
    for i in range(100)
]

# Submit batch
result = client.submit_batch(
    tasks=tasks,
    task_type="stateless",
    priority="normal"
)

print(f"Submitted {len(result.task_ids)} tasks")
print(f"Batch ID: {result.batch_id}")
```

### Python Task Monitoring

#### Polling Status

```python
# Get status once
status = client.get_task_status("task-id")
print(f"Status: {status.status}")
print(f"Progress: {status.progress}%")

# Wait for completion
status = client.wait_for_task(
    "task-id",
    timeout=300,           # Max wait time
    poll_interval=2.0      # Seconds between checks
)

if status.status == "completed":
    print(f"Result: {status.result}")
else:
    print(f"Failed: {status.error}")
```

#### Streaming Updates

```python
# Stream status updates
for update in client.stream_task("task-id"):
    print(f"[{update.timestamp}] {update.status}: {update.message}")
    
    if update.status in ("completed", "failed", "cancelled"):
        break
```

#### Batch Monitoring

```python
# Monitor multiple tasks
task_ids = ["task-1", "task-2", "task-3"]
statuses = client.get_batch_status(task_ids)

for task_id, status in statuses.items():
    print(f"{task_id}: {status.status}")

# Wait for all to complete
results = client.wait_for_batch(
    task_ids,
    timeout=300,
    fail_fast=False  # Continue even if some fail
)
```

### Python Pipelines

Pipelines chain tasks with data dependencies:

```python
# Define pipeline
pipeline = client.create_pipeline("data-processing")

# Add stages
stage1 = pipeline.add_stage(
    name="extract",
    payload={"source": "s3://bucket/data.csv"}
)

stage2 = pipeline.add_stage(
    name="transform",
    payload={"operations": ["normalize", "filter"]},
    depends_on=[stage1]  # Wait for extraction
)

stage3 = pipeline.add_stage(
    name="load",
    payload={"destination": "postgres://..."},
    depends_on=[stage2]  # Wait for transformation
)

# Execute pipeline
result = pipeline.execute(
    priority="high",
    timeout=3600  # 1 hour for entire pipeline
)

# Monitor pipeline
for status in pipeline.monitor():
    print(f"Stage {status.stage}: {status.status}")
    if status.is_terminal:
        break
```

### Python Error Handling

```python
from grey_distributed import (
    GreyClient,
    GreyAPIError,
    RateLimitError,
    QuotaExceededError,
    TaskTimeoutError,
    AuthenticationError,
    TaskNotFoundError
)

client = GreyClient()

try:
    result = client.submit_task(
        payload={"data": "..."},
        task_type="stateless"
    )
    
except AuthenticationError as e:
    print(f"Check your API key: {e}")
    
except RateLimitError as e:
    # SDK handles retries automatically, but may still raise
    print(f"Rate limited, retry after: {e.retry_after}s")
    
except QuotaExceededError as e:
    print(f"Quota exceeded: {e.quota_type}")
    print(f"Current: {e.current}, Limit: {e.limit}")
    
except TaskTimeoutError as e:
    print(f"Task {e.task_id} timed out after {e.timeout}s")
    
except GreyAPIError as e:
    print(f"API error {e.status_code}: {e.message}")
```

### Python Advanced Patterns

#### Async Client

```python
import asyncio
from grey_distributed import AsyncGreyClient

async def main():
    client = AsyncGreyClient()
    
    # Submit concurrently
    tasks = await asyncio.gather(
        client.submit_task(payload={"id": 1}),
        client.submit_task(payload={"id": 2}),
        client.submit_task(payload={"id": 3}),
    )
    
    # Wait for all
    results = await asyncio.gather(*[
        client.wait_for_task(t.task_id)
        for t in tasks
    ])
    
    await client.close()

asyncio.run(main())
```

#### Context Manager

```python
from grey_distributed import GreyClient

with GreyClient() as client:
    result = client.submit_task(payload={"data": "..."})
    status = client.wait_for_task(result.task_id)
    print(status.result)
# Connection automatically closed
```

#### Custom Retry Policy

```python
from grey_distributed import GreyClient, RetryConfig

client = GreyClient(
    retry_config=RetryConfig(
        max_retries=5,
        initial_delay=0.5,
        max_delay=30.0,
        exponential_base=2,
        jitter=0.1,
        retryable_errors=[500, 502, 503, 504]
    )
)
```

#### TEE Proof Verification

```python
# Get proof for TEE-executed task
proof = client.get_task_proof("task-id")

print(f"Platform: {proof.platform}")  # sgx, sev, etc.
print(f"MRENCLAVE: {proof.mrenclave}")
print(f"Valid: {proof.verified}")

# Verify locally
verification = proof.verify_locally(
    expected_mrenclave="a1b2c3...",
    trusted_signing_key="..."
)

if verification.valid:
    print("Proof verified!")
else:
    print(f"Verification failed: {verification.error}")
```

---

## JavaScript SDK

### JS Basic Usage

```javascript
import { GreyClient } from '@grey-io/grey-client';

// Initialize client
const client = new GreyClient({
  endpoint: 'https://api.grey.io',
  apiKey: process.env.GREY_API_KEY,
  // tenantId is usually embedded in the API key
});

// Check cluster health
const health = await client.clusterHealth();
console.log(`Cluster healthy: ${health.healthy}`);
console.log(`Active nodes: ${health.nodes.active}`);
```

### JS Task Submission

#### Basic Submission

```javascript
// Submit task
const result = await client.submitTask({
  payload: { numbers: [1, 2, 3, 4, 5] },
  type: 'stateless',
});

console.log(`Task ID: ${result.taskId}`);

// With options
const result = await client.submitTask({
  payload: {
    function: 'processData',
    data: { key: 'value' }
  },
  type: 'stateless',
  priority: 'high',
  timeout: 300,
  idempotencyKey: 'process-123',
  labels: { env: 'production' },
  teeRequired: true
});
```

#### TypeScript Types

```typescript
import { 
  GreyClient, 
  SubmitTaskOptions, 
  TaskStatus,
  TaskResult 
} from '@grey-io/grey-client';

const client = new GreyClient({
  endpoint: 'https://api.grey.io',
  apiKey: process.env.GREY_API_KEY!
});

const options: SubmitTaskOptions = {
  payload: { data: 'test' },
  type: 'stateless',
  priority: 'high'
};

const result: TaskResult = await client.submitTask(options);
const status: TaskStatus = await client.getTaskStatus(result.taskId);
```

### JS Task Monitoring

#### Polling

```javascript
// Get status
const status = await client.getTaskStatus(taskId);
console.log(`Status: ${status.status}`);

// Wait for completion
const finalStatus = await client.waitForTask(taskId, {
  timeout: 300000,    // 5 minutes in ms
  pollInterval: 2000  // 2 seconds
});

if (finalStatus.status === 'completed') {
  console.log('Result:', finalStatus.result);
} else {
  console.error('Failed:', finalStatus.error);
}
```

#### Event Streaming

```javascript
// Stream updates
const stream = client.streamTask(taskId);

stream.on('update', (update) => {
  console.log(`[${update.timestamp}] ${update.status}`);
});

stream.on('completed', (result) => {
  console.log('Completed:', result);
});

stream.on('error', (error) => {
  console.error('Error:', error);
});

// Later: stop streaming
stream.close();
```

#### Promises with AbortController

```javascript
const controller = new AbortController();

// Set timeout
setTimeout(() => controller.abort(), 60000);

try {
  const status = await client.waitForTask(taskId, {
    signal: controller.signal
  });
} catch (err) {
  if (err.name === 'AbortError') {
    console.log('Aborted waiting for task');
  }
}
```

### Browser Integration

#### React Hook

```jsx
import { useGreyTask, GreyProvider } from '@grey-io/grey-client/react';

// Wrap app with provider
function App() {
  return (
    <GreyProvider endpoint="https://api.grey.io" apiKey={apiKey}>
      <TaskRunner />
    </GreyProvider>
  );
}

function TaskRunner() {
  const { submit, status, result, error, isLoading } = useGreyTask();
  
  const handleSubmit = async () => {
    await submit({
      payload: { data: 'test' },
      type: 'stateless'
    });
  };
  
  return (
    <div>
      <button onClick={handleSubmit} disabled={isLoading}>
        {isLoading ? 'Running...' : 'Run Task'}
      </button>
      
      {status && <p>Status: {status}</p>}
      {result && <pre>{JSON.stringify(result, null, 2)}</pre>}
      {error && <p className="error">{error.message}</p>}
    </div>
  );
}
```

#### Vue Composable

```vue
<script setup>
import { useGreyTask } from '@grey-io/grey-client/vue';

const { submit, status, result, error, isLoading } = useGreyTask();

const runTask = async () => {
  await submit({
    payload: { data: 'test' },
    type: 'stateless'
  });
};
</script>

<template>
  <button @click="runTask" :disabled="isLoading">
    {{ isLoading ? 'Running...' : 'Run Task' }}
  </button>
  <p v-if="status">Status: {{ status }}</p>
  <pre v-if="result">{{ JSON.stringify(result, null, 2) }}</pre>
</template>
```

### JS Error Handling

```javascript
import {
  GreyClient,
  GreyAPIError,
  RateLimitError,
  QuotaExceededError,
  AuthenticationError
} from '@grey-io/grey-client';

try {
  const result = await client.submitTask({
    payload: { data: '...' },
    type: 'stateless'
  });
  
} catch (error) {
  if (error instanceof AuthenticationError) {
    console.error('Invalid API key');
    
  } else if (error instanceof RateLimitError) {
    console.log(`Rate limited, retry after: ${error.retryAfter}s`);
    
  } else if (error instanceof QuotaExceededError) {
    console.log(`Quota exceeded: ${error.quotaType}`);
    console.log(`Current: ${error.current}, Limit: ${error.limit}`);
    
  } else if (error instanceof GreyAPIError) {
    console.error(`API error ${error.statusCode}: ${error.message}`);
    
  } else {
    throw error;
  }
}
```

---

## Common Patterns

### Idempotent Task Submission

Prevent duplicate tasks when retrying:

```python
# Python
result = client.submit_task(
    payload={"order_id": "12345"},
    idempotency_key="order-12345-process"
)
# Same idempotency_key returns same task_id without re-execution
```

```javascript
// JavaScript
const result = await client.submitTask({
  payload: { orderId: '12345' },
  idempotencyKey: 'order-12345-process'
});
```

### Handling Backpressure

SDKs automatically apply backpressure when queue is full:

```python
# Python - automatic with retries
result = client.submit_task(payload={"data": "..."})

# Or check queue depth first
health = client.cluster_health()
if health.queue.depth > 10000:
    print("Queue is deep, consider delaying submission")
```

```javascript
// JavaScript - check before submitting
const health = await client.clusterHealth();
if (health.queue.depth > 10000) {
  await sleep(5000);  // Wait for queue to drain
}
const result = await client.submitTask({ payload: {...} });
```

### Fan-out/Fan-in Pattern

```python
# Python - Fan-out
task_ids = []
for item in items:
    result = client.submit_task(payload={"item": item})
    task_ids.append(result.task_id)

# Fan-in - wait for all
results = client.wait_for_batch(task_ids)
combined = aggregate(results)
```

### Circuit Breaker

```python
from grey_distributed import GreyClient, CircuitBreakerConfig

client = GreyClient(
    circuit_breaker=CircuitBreakerConfig(
        failure_threshold=5,    # Open after 5 failures
        success_threshold=3,    # Close after 3 successes
        timeout=30.0,           # Half-open after 30s
        excluded_exceptions=[QuotaExceededError]
    )
)
```

---

## Best Practices

### 1. Use Idempotency Keys

Always provide idempotency keys for important tasks:

```python
# Good
result = client.submit_task(
    payload=payment_data,
    idempotency_key=f"payment-{payment_id}"
)

# Risk of duplicates on retry
result = client.submit_task(payload=payment_data)
```

### 2. Set Appropriate Timeouts

```python
# Short timeout for quick tasks
result = client.submit_task(
    payload={"quick": True},
    timeout=10  # 10 seconds
)

# Longer timeout for batch processing
result = client.submit_task(
    payload={"batch": large_data},
    timeout=3600  # 1 hour
)
```

### 3. Use Labels for Filtering

```python
result = client.submit_task(
    payload={...},
    labels={
        "env": "production",
        "team": "data-engineering",
        "pipeline": "etl-daily"
    }
)

# Later: find tasks
tasks = client.list_tasks(labels={"pipeline": "etl-daily"})
```

### 4. Handle Errors Gracefully

```python
try:
    result = client.submit_task(payload={...})
except RateLimitError:
    # Already retried by SDK, now truly limited
    queue_for_later(payload)
except QuotaExceededError:
    alert_ops_team()
    raise
```

### 5. Monitor Queue Depth

```python
while True:
    health = client.cluster_health()
    if health.queue.depth < 1000:
        submit_next_batch()
    else:
        time.sleep(10)
```

### 6. Use Connection Pooling

```python
# Create client once, reuse for all requests
client = GreyClient(max_connections=20)

# Good: reuse client
for item in items:
    client.submit_task(payload={"item": item})

# Bad: create client per request
for item in items:
    GreyClient().submit_task(payload={"item": item})
```

---

## Troubleshooting

### Connection Issues

```python
# Enable debug logging
import logging
logging.getLogger('grey_distributed').setLevel(logging.DEBUG)

# Test connectivity
try:
    client.cluster_health()
except Exception as e:
    print(f"Connection failed: {e}")
```

### Rate Limiting

```python
# Check current rate limit status
status = client.get_rate_limit_status()
print(f"Remaining: {status.remaining}/{status.limit}")
print(f"Resets at: {status.reset_at}")
```

### Quota Issues

```python
# Check quota usage
usage = client.get_quota_usage()
for quota_type, info in usage.items():
    percent = info["used"] / info["limit"] * 100
    print(f"{quota_type}: {percent:.1f}% used")
```

### Task Debugging

```python
# Get task details
task = client.get_task("task-id")
print(f"Created: {task.created_at}")
print(f"Started: {task.started_at}")
print(f"Node: {task.executed_on}")
print(f"Duration: {task.duration_ms}ms")

# Get execution logs
logs = client.get_task_logs("task-id")
for entry in logs:
    print(f"[{entry.level}] {entry.message}")
```

---

## Version Compatibility

| SDK Version | API Version | Python | Node.js |
|-------------|-------------|--------|---------|
| 1.x         | v1          | 3.9+   | 18+     |
| 0.x         | v0 (legacy) | 3.8+   | 16+     |

---

*Last Updated: 2024*  
*SDK Maintainers: Grey Platform Team*
