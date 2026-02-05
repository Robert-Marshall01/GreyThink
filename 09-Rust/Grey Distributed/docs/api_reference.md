# Grey Distributed — API Reference

Complete API reference for Grey Distributed REST and gRPC APIs.

---

## Table of Contents

1. [Overview](#overview)
2. [Authentication](#authentication)
3. [REST API](#rest-api)
   - [Tasks](#tasks-api)
   - [Cluster](#cluster-api)
   - [Security](#security-api)
   - [Governance](#governance-api)
4. [gRPC API](#grpc-api)
5. [Error Handling](#error-handling)
6. [Rate Limiting](#rate-limiting)
7. [Webhooks](#webhooks)

---

## Overview

### Base URLs

| Environment | REST API | gRPC |
|-------------|----------|------|
| Production | `https://api.grey.io/v1` | `grpc.grey.io:443` |
| Staging | `https://staging-api.grey.io/v1` | `staging-grpc.grey.io:443` |
| Self-hosted | `https://<your-cluster>/api/v1` | `<your-cluster>:4100` |

### API Versioning

- Current version: `v1`
- Version is included in URL path: `/api/v1/...`
- Breaking changes require new major version
- Deprecated features include `Sunset` header

### Content Types

- Request bodies: `application/json`
- Responses: `application/json`
- Proof artifacts: `application/octet-stream`

### Request IDs

All responses include `X-Request-ID` header for debugging:

```
X-Request-ID: req_7b2d8e4a-1f3c-4e5d-9a8b-2c4d6e8f0a1b
```

---

## Authentication

### API Key Authentication

Include API key in `Authorization` header:

```http
Authorization: Bearer grey_api_key_abc123...
```

Or use `X-API-Key` header:

```http
X-API-Key: grey_api_key_abc123...
```

### API Key Format

```
grey_api_key_{tenant_id}_{random_suffix}
```

- `grey_api_key_` — Fixed prefix
- `{tenant_id}` — Your tenant identifier
- `{random_suffix}` — Cryptographic random string

### Scopes

API keys can have scopes:

| Scope | Description |
|-------|-------------|
| `tasks:read` | Read task status and results |
| `tasks:write` | Submit and cancel tasks |
| `cluster:read` | Read cluster health |
| `cluster:admin` | Manage cluster (quarantine, etc.) |
| `security:read` | Read attestation status |
| `security:admin` | Manage security settings |

### Example

```bash
curl -X POST "https://api.grey.io/v1/tasks" \
  -H "Authorization: Bearer grey_api_key_tenant123_abc..." \
  -H "Content-Type: application/json" \
  -d '{"type": "stateless", "payload": {"data": "test"}}'
```

---

## REST API

### Tasks API

#### Submit Task

Create a new task for execution.

```http
POST /v1/tasks
```

**Request Body:**

```json
{
  "type": "stateless",
  "payload": {
    "function": "process",
    "data": {"key": "value"}
  },
  "priority": "normal",
  "timeout_seconds": 300,
  "idempotency_key": "unique-key-123",
  "labels": {
    "env": "production",
    "team": "data"
  },
  "tee_required": false,
  "state_key": null
}
```

**Parameters:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `type` | string | Yes | Task type: `stateless`, `stateful`, `pipeline`, `batch` |
| `payload` | object | Yes | Task input data |
| `priority` | string | No | Priority level: `low`, `normal`, `high`, `critical`. Default: `normal` |
| `timeout_seconds` | integer | No | Task timeout. Default: 300 |
| `idempotency_key` | string | No | Unique key for deduplication |
| `labels` | object | No | Key-value labels for filtering |
| `tee_required` | boolean | No | Require TEE execution. Default: false |
| `state_key` | string | No | Key for stateful tasks |
| `dependencies` | array | No | Task IDs this task depends on |

**Response:** `201 Created`

```json
{
  "task_id": "task_550e8400-e29b-41d4-a716-446655440000",
  "status": "pending",
  "created_at": "2024-01-15T10:30:00Z",
  "estimated_start": "2024-01-15T10:30:05Z"
}
```

**Error Responses:**

| Status | Code | Description |
|--------|------|-------------|
| 400 | `invalid_payload` | Malformed or invalid payload |
| 401 | `unauthorized` | Missing or invalid API key |
| 409 | `duplicate_task` | Idempotency key already used |
| 429 | `rate_limited` | Rate limit exceeded |
| 503 | `queue_full` | Queue at capacity |

---

#### Get Task Status

Retrieve the current status of a task.

```http
GET /v1/tasks/{task_id}
```

**Path Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `task_id` | string | Task identifier |

**Response:** `200 OK`

```json
{
  "task_id": "task_550e8400-e29b-41d4-a716-446655440000",
  "type": "stateless",
  "status": "completed",
  "priority": "normal",
  "progress": 100,
  "created_at": "2024-01-15T10:30:00Z",
  "started_at": "2024-01-15T10:30:02Z",
  "completed_at": "2024-01-15T10:30:15Z",
  "duration_ms": 13000,
  "executed_on": "grey-worker-7",
  "result": {
    "output": "processed",
    "count": 42
  },
  "labels": {
    "env": "production"
  },
  "tee_executed": true,
  "proof_available": true
}
```

**Task Status Values:**

| Status | Description |
|--------|-------------|
| `pending` | Queued, waiting for execution |
| `running` | Currently executing |
| `completed` | Finished successfully |
| `failed` | Finished with error |
| `cancelled` | Cancelled by user |
| `timeout` | Exceeded timeout |

**Error Responses:**

| Status | Code | Description |
|--------|------|-------------|
| 404 | `task_not_found` | Task does not exist |

---

#### List Tasks

List tasks with optional filtering.

```http
GET /v1/tasks
```

**Query Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `status` | string | Filter by status (comma-separated) |
| `type` | string | Filter by task type |
| `label` | string | Filter by label (`key=value`, repeatable) |
| `since` | string | Tasks created after (RFC3339) |
| `until` | string | Tasks created before (RFC3339) |
| `limit` | integer | Max results (default: 100, max: 1000) |
| `cursor` | string | Pagination cursor |

**Example:**

```http
GET /v1/tasks?status=running,pending&label=env=production&limit=50
```

**Response:** `200 OK`

```json
{
  "tasks": [
    {
      "task_id": "task_abc123",
      "type": "stateless",
      "status": "running",
      "created_at": "2024-01-15T10:30:00Z"
    }
  ],
  "total": 150,
  "has_more": true,
  "next_cursor": "cursor_xyz789"
}
```

---

#### Cancel Task

Cancel a pending or running task.

```http
DELETE /v1/tasks/{task_id}
```

**Request Body (optional):**

```json
{
  "reason": "No longer needed"
}
```

**Response:** `200 OK`

```json
{
  "task_id": "task_abc123",
  "status": "cancelled",
  "cancelled_at": "2024-01-15T10:35:00Z"
}
```

**Error Responses:**

| Status | Code | Description |
|--------|------|-------------|
| 404 | `task_not_found` | Task does not exist |
| 409 | `task_not_cancellable` | Task already completed |

---

#### Get Task Result

Get the result of a completed task.

```http
GET /v1/tasks/{task_id}/result
```

**Response:** `200 OK`

```json
{
  "task_id": "task_abc123",
  "result": {
    "output": "processed data",
    "metrics": {"count": 1000}
  },
  "result_size_bytes": 1024
}
```

---

#### Get Task Proof

Get TEE proof artifact for a task.

```http
GET /v1/tasks/{task_id}/proof
```

**Response:** `200 OK`

```json
{
  "task_id": "task_abc123",
  "platform": "sgx",
  "mrenclave": "a1b2c3d4e5f6...",
  "mrsigner": "f6e5d4c3b2a1...",
  "quote": "base64-encoded-quote",
  "verified": true,
  "verified_at": "2024-01-15T10:30:20Z"
}
```

**Download raw proof:**

```http
GET /v1/tasks/{task_id}/proof?format=binary
```

Returns: `application/octet-stream`

---

#### Stream Task Updates

Server-Sent Events for task status updates.

```http
GET /v1/tasks/{task_id}/stream
Accept: text/event-stream
```

**Events:**

```
event: status
data: {"status": "running", "progress": 50, "message": "Processing..."}

event: status
data: {"status": "completed", "progress": 100}

event: done
data: {}
```

---

### Cluster API

#### Get Cluster Health

```http
GET /v1/cluster/health
```

**Response:** `200 OK`

```json
{
  "healthy": true,
  "version": "1.5.0",
  "nodes": {
    "total": 15,
    "active": 14,
    "draining": 1,
    "quarantined": 0
  },
  "consensus": {
    "leader": "grey-coordinator-0",
    "term": 42,
    "quorum_satisfied": true,
    "members": 3
  },
  "queue": {
    "depth": 1234,
    "oldest_task_seconds": 2.5
  },
  "utilization": {
    "cpu_percent": 72,
    "memory_percent": 65
  },
  "throughput": {
    "tasks_per_second": 5000,
    "tasks_last_hour": 18000000
  }
}
```

---

#### Get Resource Usage

Get current tenant resource usage and quotas.

```http
GET /v1/cluster/usage
```

**Response:** `200 OK`

```json
{
  "tenant_id": "tenant-abc",
  "quotas": {
    "cpu_cores": {
      "limit": 100,
      "used": 72,
      "available": 28
    },
    "memory_gb": {
      "limit": 256,
      "used": 168,
      "available": 88
    },
    "max_concurrent_tasks": {
      "limit": 10000,
      "used": 3402,
      "available": 6598
    }
  },
  "rate_limits": {
    "submit_rate": {
      "limit": 1000,
      "current": 450,
      "reset_at": "2024-01-15T10:31:00Z"
    }
  },
  "throttled": false
}
```

---

#### List Nodes

```http
GET /v1/cluster/nodes
```

**Query Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `role` | string | Filter: `coordinator`, `worker`, `gateway` |
| `status` | string | Filter: `active`, `draining`, `quarantined` |

**Response:** `200 OK`

```json
{
  "nodes": [
    {
      "node_id": "grey-coordinator-0",
      "role": "coordinator",
      "status": "active",
      "address": "10.0.1.10:4100",
      "started_at": "2024-01-10T00:00:00Z",
      "resources": {
        "cpu_cores": 8,
        "memory_gb": 32
      },
      "tasks_running": 0,
      "is_leader": true
    },
    {
      "node_id": "grey-worker-1",
      "role": "worker",
      "status": "active",
      "address": "10.0.1.20:4100",
      "started_at": "2024-01-10T00:00:00Z",
      "resources": {
        "cpu_cores": 16,
        "memory_gb": 64
      },
      "tasks_running": 42,
      "tee_enabled": true
    }
  ]
}
```

---

### Security API

#### Get Attestation Status

Get cluster-wide TEE attestation status.

```http
GET /v1/security/attestation
```

**Response:** `200 OK`

```json
{
  "nodes_attested": 10,
  "nodes_total": 10,
  "all_valid": true,
  "platforms": {
    "sgx": 8,
    "sev": 2
  },
  "last_verification": "2024-01-15T10:00:00Z"
}
```

---

#### Verify Node Attestation

```http
GET /v1/security/attestation/{node_id}
```

**Response:** `200 OK`

```json
{
  "node_id": "grey-worker-7",
  "valid": true,
  "platform": "sgx",
  "mrenclave": "a1b2c3d4e5f6...",
  "mrsigner": "f6e5d4c3b2a1...",
  "verified_at": "2024-01-15T10:00:00Z",
  "expires_at": "2024-01-15T11:00:00Z"
}
```

---

#### Request Fresh Attestation

```http
POST /v1/security/attestation
```

**Request Body:**

```json
{
  "node_id": "grey-worker-7",
  "challenge": "optional-nonce"
}
```

**Response:** `200 OK`

```json
{
  "node_id": "grey-worker-7",
  "attestation": {
    "platform": "sgx",
    "quote": "base64-encoded-quote",
    "signature": "base64-encoded-signature"
  },
  "verified": true
}
```

---

#### Get Tenant Isolation Status

```http
GET /v1/security/isolation
```

**Response:** `200 OK`

```json
{
  "tenant_id": "tenant-abc",
  "isolation_level": "strict",
  "violations": 0,
  "last_checked": "2024-01-15T10:00:00Z",
  "features": {
    "network_isolation": true,
    "memory_encryption": true,
    "tee_enforcement": false
  }
}
```

---

### Governance API

#### Get Quotas

```http
GET /v1/governance/quotas
```

**Response:** `200 OK`

```json
{
  "tenant_id": "tenant-abc",
  "quotas": {
    "cpu_cores": 100,
    "memory_gb": 256,
    "max_concurrent_tasks": 10000,
    "max_queue_depth": 50000,
    "submit_rate_per_second": 1000
  }
}
```

---

#### Update Quotas (Admin)

```http
PATCH /v1/governance/quotas
```

**Request Body:**

```json
{
  "cpu_cores": 200,
  "reason": "Capacity increase for Q1"
}
```

**Response:** `200 OK`

---

#### Get Rate Limit Status

```http
GET /v1/governance/rate-limits
```

**Response:** `200 OK`

```json
{
  "submit": {
    "limit": 1000,
    "remaining": 750,
    "reset_at": "2024-01-15T10:31:00Z",
    "window_seconds": 60
  },
  "api": {
    "limit": 10000,
    "remaining": 8500,
    "reset_at": "2024-01-15T10:31:00Z",
    "window_seconds": 60
  }
}
```

---

## gRPC API

Grey Distributed provides gRPC API for low-latency communication.

### Service Definition

```protobuf
service GreyService {
  // Task operations
  rpc SubmitTask(SubmitTaskRequest) returns (SubmitTaskResponse);
  rpc GetTaskStatus(GetTaskStatusRequest) returns (TaskStatus);
  rpc CancelTask(CancelTaskRequest) returns (CancelTaskResponse);
  rpc StreamTaskStatus(StreamTaskRequest) returns (stream TaskStatusUpdate);
  
  // Cluster operations
  rpc GetClusterHealth(Empty) returns (ClusterHealth);
  rpc GetResourceUsage(Empty) returns (ResourceUsage);
  
  // Security operations
  rpc VerifyAttestation(AttestationRequest) returns (AttestationResponse);
  rpc GetTaskProof(GetProofRequest) returns (ProofResponse);
}
```

### Connection Setup

```python
import grpc
from grey_pb2_grpc import GreyServiceStub
from grey_pb2 import SubmitTaskRequest

# Create channel with TLS
credentials = grpc.ssl_channel_credentials()
channel = grpc.secure_channel('grpc.grey.io:443', credentials)

# Add authentication
auth_metadata = [('authorization', f'Bearer {api_key}')]
stub = GreyServiceStub(channel)

# Make call
request = SubmitTaskRequest(
    type='stateless',
    payload=b'{"data": "test"}'
)
response = stub.SubmitTask(request, metadata=auth_metadata)
```

### Streaming

```python
# Stream task updates
request = StreamTaskRequest(task_id='task_abc123')
for update in stub.StreamTaskStatus(request, metadata=auth_metadata):
    print(f"Status: {update.status}, Progress: {update.progress}%")
```

---

## Error Handling

### Error Response Format

```json
{
  "error": {
    "code": "error_code",
    "message": "Human-readable description",
    "details": {
      "field": "additional context"
    },
    "request_id": "req_abc123"
  }
}
```

### Error Codes

| HTTP Status | Code | Description |
|-------------|------|-------------|
| 400 | `bad_request` | Malformed request |
| 400 | `invalid_payload` | Invalid task payload |
| 400 | `invalid_parameter` | Invalid query parameter |
| 401 | `unauthorized` | Missing/invalid auth |
| 403 | `forbidden` | Insufficient permissions |
| 404 | `not_found` | Resource not found |
| 404 | `task_not_found` | Task doesn't exist |
| 409 | `conflict` | Resource conflict |
| 409 | `duplicate_task` | Idempotency conflict |
| 422 | `unprocessable` | Validation error |
| 429 | `rate_limited` | Rate limit exceeded |
| 503 | `service_unavailable` | Cluster unavailable |
| 503 | `queue_full` | Queue at capacity |

### Retry Behavior

| Error | Retry | Strategy |
|-------|-------|----------|
| 429 | Yes | Exponential backoff with `Retry-After` header |
| 500 | Yes | Exponential backoff |
| 502/503/504 | Yes | Exponential backoff |
| 4xx (other) | No | Fix request and retry |

---

## Rate Limiting

### Headers

All responses include rate limit headers:

```http
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 750
X-RateLimit-Reset: 1705318260
```

### 429 Response

```http
HTTP/1.1 429 Too Many Requests
Retry-After: 30
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 0
X-RateLimit-Reset: 1705318260

{
  "error": {
    "code": "rate_limited",
    "message": "Rate limit exceeded. Retry after 30 seconds.",
    "details": {
      "limit": 1000,
      "window_seconds": 60
    }
  }
}
```

### Limits by Endpoint

| Endpoint | Limit | Window |
|----------|-------|--------|
| `POST /v1/tasks` | 1000/min | 1 minute |
| `GET /v1/tasks/*` | 10000/min | 1 minute |
| `GET /v1/cluster/*` | 1000/min | 1 minute |
| Other | 5000/min | 1 minute |

---

## Webhooks

Configure webhooks to receive task completion notifications.

### Configure Webhook

```http
POST /v1/webhooks
```

**Request Body:**

```json
{
  "url": "https://your-server.com/grey-webhook",
  "events": ["task.completed", "task.failed"],
  "secret": "your-webhook-secret"
}
```

### Webhook Payload

```json
{
  "event": "task.completed",
  "timestamp": "2024-01-15T10:30:15Z",
  "task": {
    "task_id": "task_abc123",
    "status": "completed",
    "result": {...}
  }
}
```

### Signature Verification

Verify webhook signature using HMAC-SHA256:

```python
import hmac
import hashlib

def verify_webhook(payload, signature, secret):
    expected = hmac.new(
        secret.encode(),
        payload.encode(),
        hashlib.sha256
    ).hexdigest()
    return hmac.compare_digest(f"sha256={expected}", signature)

# Check X-Grey-Signature header
signature = request.headers['X-Grey-Signature']
if verify_webhook(request.body, signature, webhook_secret):
    process_webhook(request.body)
```

---

## SDK Endpoints

SDKs use these endpoints internally:

| SDK Method | Endpoint |
|------------|----------|
| `submit_task()` | `POST /v1/tasks` |
| `get_task_status()` | `GET /v1/tasks/{id}` |
| `wait_for_task()` | `GET /v1/tasks/{id}` (polling) |
| `stream_task()` | `GET /v1/tasks/{id}/stream` (SSE) |
| `cancel_task()` | `DELETE /v1/tasks/{id}` |
| `get_task_proof()` | `GET /v1/tasks/{id}/proof` |
| `cluster_health()` | `GET /v1/cluster/health` |
| `get_usage()` | `GET /v1/cluster/usage` |

---

*API Version: v1*  
*Last Updated: 2024*
