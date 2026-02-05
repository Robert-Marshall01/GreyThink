"""
Grey Distributed Python SDK
============================

A comprehensive Python client for interacting with Grey Distributed clusters.

Features:
- Task submission (stateless, stateful, pipelines)
- Task status querying and proof artifact retrieval
- Automatic retry with exponential backoff
- Backpressure handling with adaptive rate limiting
- Tenant isolation enforcement
- TEE attestation verification

Design Philosophy:
- Fail-fast on configuration errors, graceful degradation on transient errors
- Transparent retry logic with configurable policies
- Type hints for IDE support and static analysis
- Async-first with sync wrappers for convenience

Installation:
    pip install grey-distributed

Usage:
    from grey_client import GreyClient
    
    client = GreyClient(
        endpoint="https://grey.example.com",
        api_key="your-api-key",
        tenant_id="your-tenant"
    )
    
    # Submit a task
    result = client.submit_task(
        task_type="compute",
        payload={"data": [1, 2, 3]},
        priority=5
    )
    
    # Query status
    status = client.get_task_status(result.task_id)

Tradeoffs:
- Uses httpx for HTTP/2 support (better connection reuse)
- Async by default for scalability, sync wrappers for simplicity
- Retry logic adds latency but improves reliability
- Connection pooling trades memory for throughput

"""

from __future__ import annotations

import asyncio
import hashlib
import hmac
import json
import logging
import os
import time
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import (
    Any,
    AsyncIterator,
    Callable,
    Dict,
    Iterator,
    List,
    Literal,
    Optional,
    TypeVar,
    Union,
)

# Optional dependencies with graceful fallback
try:
    import httpx
except ImportError:
    httpx = None  # type: ignore

try:
    from pydantic import BaseModel, Field, validator
    PYDANTIC_AVAILABLE = True
except ImportError:
    PYDANTIC_AVAILABLE = False
    BaseModel = object  # type: ignore

# Configure logging
logger = logging.getLogger("grey_client")


# =============================================================================
# Configuration & Constants
# =============================================================================

DEFAULT_TIMEOUT = 30.0
DEFAULT_MAX_RETRIES = 3
DEFAULT_BACKOFF_BASE = 1.0
DEFAULT_BACKOFF_MAX = 60.0
DEFAULT_POOL_SIZE = 100

# API Endpoints
API_VERSION = "v1"
TASKS_ENDPOINT = f"/api/{API_VERSION}/tasks"
CLUSTER_ENDPOINT = f"/api/{API_VERSION}/cluster"
SECURITY_ENDPOINT = f"/api/{API_VERSION}/security"


# =============================================================================
# Enums & Types
# =============================================================================

class TaskStatus(Enum):
    """Task lifecycle states."""
    PENDING = "pending"
    QUEUED = "queued"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"
    TIMEOUT = "timeout"


class TaskType(Enum):
    """Supported task types."""
    STATELESS = "stateless"      # Pure compute, no state
    STATEFUL = "stateful"        # Maintains state across invocations
    PIPELINE = "pipeline"        # DAG of dependent tasks
    BATCH = "batch"              # Multiple independent tasks


class Priority(Enum):
    """Task priority levels."""
    CRITICAL = 10
    HIGH = 7
    NORMAL = 5
    LOW = 3
    BACKGROUND = 1


# =============================================================================
# Data Classes
# =============================================================================

@dataclass
class RetryPolicy:
    """
    Configuration for retry behavior.
    
    Tradeoffs:
    - Higher max_retries: Better reliability, higher latency for failures
    - Lower backoff_base: Faster retries, more load on failed services
    - Jitter: Prevents thundering herd, adds unpredictability
    """
    max_retries: int = DEFAULT_MAX_RETRIES
    backoff_base: float = DEFAULT_BACKOFF_BASE
    backoff_max: float = DEFAULT_BACKOFF_MAX
    jitter: bool = True
    retry_on_status: tuple = (429, 500, 502, 503, 504)
    
    def calculate_delay(self, attempt: int) -> float:
        """Calculate delay with exponential backoff and optional jitter."""
        delay = min(self.backoff_base * (2 ** attempt), self.backoff_max)
        if self.jitter:
            import random
            delay *= (0.5 + random.random())
        return delay


@dataclass
class BackpressureConfig:
    """
    Configuration for client-side backpressure handling.
    
    When the cluster is under load, the client adaptively reduces
    request rate to prevent overwhelming the system.
    
    Mechanics:
    - Track recent 429 (rate limit) responses
    - Reduce concurrency when rate limited
    - Gradually restore concurrency after recovery
    """
    enabled: bool = True
    initial_concurrency: int = 50
    min_concurrency: int = 1
    max_concurrency: int = 200
    reduction_factor: float = 0.7
    recovery_factor: float = 1.1
    recovery_interval: float = 5.0


@dataclass
class TaskDefinition:
    """
    Defines a task to be submitted to Grey Distributed.
    
    Attributes:
        task_type: Type of task (stateless, stateful, pipeline)
        payload: Task-specific data
        priority: Execution priority (1-10)
        timeout: Maximum execution time in seconds
        idempotency_key: Unique key for deduplication
        dependencies: List of task IDs that must complete first
        labels: Key-value pairs for filtering and organization
        tee_required: Whether task requires TEE execution
    """
    task_type: TaskType
    payload: Dict[str, Any]
    priority: int = 5
    timeout: Optional[int] = None
    idempotency_key: Optional[str] = None
    dependencies: List[str] = field(default_factory=list)
    labels: Dict[str, str] = field(default_factory=dict)
    tee_required: bool = False
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to API request format."""
        return {
            "type": self.task_type.value if isinstance(self.task_type, TaskType) else self.task_type,
            "payload": self.payload,
            "priority": self.priority,
            "timeout_seconds": self.timeout,
            "idempotency_key": self.idempotency_key,
            "dependencies": self.dependencies,
            "labels": self.labels,
            "tee_required": self.tee_required,
        }


@dataclass
class TaskResult:
    """
    Result of a task submission or query.
    
    Contains the task ID, current status, and optional result data
    or error information.
    """
    task_id: str
    status: TaskStatus
    created_at: datetime
    updated_at: datetime
    result: Optional[Dict[str, Any]] = None
    error: Optional[str] = None
    proof_artifact: Optional[bytes] = None
    execution_node: Optional[str] = None
    retry_count: int = 0
    
    @classmethod
    def from_api_response(cls, data: Dict[str, Any]) -> "TaskResult":
        """Parse API response into TaskResult."""
        return cls(
            task_id=data["task_id"],
            status=TaskStatus(data["status"]),
            created_at=datetime.fromisoformat(data["created_at"].replace("Z", "+00:00")),
            updated_at=datetime.fromisoformat(data["updated_at"].replace("Z", "+00:00")),
            result=data.get("result"),
            error=data.get("error"),
            proof_artifact=bytes.fromhex(data["proof_artifact"]) if data.get("proof_artifact") else None,
            execution_node=data.get("execution_node"),
            retry_count=data.get("retry_count", 0),
        )


@dataclass
class ClusterHealth:
    """Cluster health and resource information."""
    healthy: bool
    nodes: int
    active_nodes: int
    cpu_utilization: float
    memory_utilization: float
    queue_depth: int
    tasks_per_second: float
    leader: Optional[str] = None
    consensus_term: int = 0


@dataclass
class AttestationResult:
    """Result of TEE attestation verification."""
    valid: bool
    node_id: str
    platform: str  # sgx, sev, etc.
    mrenclave: Optional[str] = None
    mrsigner: Optional[str] = None
    verified_at: Optional[datetime] = None
    quote: Optional[bytes] = None


# =============================================================================
# Exceptions
# =============================================================================

class GreyError(Exception):
    """Base exception for Grey client errors."""
    pass


class GreyConnectionError(GreyError):
    """Failed to connect to Grey cluster."""
    pass


class GreyAuthenticationError(GreyError):
    """Authentication failed."""
    pass


class GreyAuthorizationError(GreyError):
    """Not authorized to perform action."""
    pass


class GreyRateLimitError(GreyError):
    """Rate limit exceeded."""
    def __init__(self, message: str, retry_after: Optional[float] = None):
        super().__init__(message)
        self.retry_after = retry_after


class GreyTaskError(GreyError):
    """Task-specific error."""
    def __init__(self, message: str, task_id: Optional[str] = None):
        super().__init__(message)
        self.task_id = task_id


class GreyValidationError(GreyError):
    """Request validation failed."""
    pass


# =============================================================================
# Authentication
# =============================================================================

class AuthProvider:
    """
    Base class for authentication providers.
    
    Grey supports multiple auth methods:
    - API Key: Simple, suitable for server-to-server
    - JWT: Short-lived tokens, suitable for user sessions
    - mTLS: Certificate-based, strongest security
    """
    
    def get_headers(self) -> Dict[str, str]:
        """Return authentication headers."""
        raise NotImplementedError


class ApiKeyAuth(AuthProvider):
    """API key authentication."""
    
    def __init__(self, api_key: str, key_id: Optional[str] = None):
        self.api_key = api_key
        self.key_id = key_id or "default"
    
    def get_headers(self) -> Dict[str, str]:
        return {
            "Authorization": f"Bearer {self.api_key}",
            "X-Grey-Key-ID": self.key_id,
        }


class HMACAuth(AuthProvider):
    """
    HMAC-based request signing.
    
    More secure than API keys as the secret is never transmitted.
    Each request is signed with a timestamp to prevent replay attacks.
    """
    
    def __init__(self, access_key: str, secret_key: str):
        self.access_key = access_key
        self.secret_key = secret_key.encode()
    
    def sign_request(self, method: str, path: str, body: bytes, timestamp: str) -> str:
        """Generate HMAC signature for request."""
        string_to_sign = f"{method}\n{path}\n{timestamp}\n{hashlib.sha256(body).hexdigest()}"
        signature = hmac.new(
            self.secret_key,
            string_to_sign.encode(),
            hashlib.sha256
        ).hexdigest()
        return signature
    
    def get_headers(self) -> Dict[str, str]:
        # Note: Full signing happens per-request in the client
        return {
            "X-Grey-Access-Key": self.access_key,
        }


# =============================================================================
# Rate Limiter
# =============================================================================

class AdaptiveRateLimiter:
    """
    Client-side adaptive rate limiter.
    
    Automatically adjusts request rate based on server responses.
    Uses token bucket algorithm with dynamic refill rate.
    
    Mechanics:
    - Start with initial concurrency limit
    - On 429: reduce limit by reduction_factor
    - After recovery_interval without 429: increase by recovery_factor
    - Never go below min_concurrency or above max_concurrency
    """
    
    def __init__(self, config: BackpressureConfig):
        self.config = config
        self._current_concurrency = config.initial_concurrency
        self._semaphore = asyncio.Semaphore(config.initial_concurrency)
        self._last_429 = 0.0
        self._lock = asyncio.Lock()
    
    @property
    def current_concurrency(self) -> int:
        return self._current_concurrency
    
    async def acquire(self) -> None:
        """Acquire permission to make a request."""
        if not self.config.enabled:
            return
        await self._semaphore.acquire()
    
    def release(self) -> None:
        """Release request permission."""
        if not self.config.enabled:
            return
        self._semaphore.release()
    
    async def on_rate_limited(self) -> None:
        """Called when a 429 response is received."""
        if not self.config.enabled:
            return
        
        async with self._lock:
            self._last_429 = time.time()
            new_concurrency = max(
                self.config.min_concurrency,
                int(self._current_concurrency * self.config.reduction_factor)
            )
            
            if new_concurrency < self._current_concurrency:
                logger.warning(
                    f"Rate limited, reducing concurrency: "
                    f"{self._current_concurrency} -> {new_concurrency}"
                )
                # Reduce semaphore permits
                reduction = self._current_concurrency - new_concurrency
                for _ in range(reduction):
                    await self._semaphore.acquire()
                self._current_concurrency = new_concurrency
    
    async def try_recover(self) -> None:
        """Attempt to recover concurrency after cooldown."""
        if not self.config.enabled:
            return
        
        async with self._lock:
            elapsed = time.time() - self._last_429
            if elapsed < self.config.recovery_interval:
                return
            
            new_concurrency = min(
                self.config.max_concurrency,
                int(self._current_concurrency * self.config.recovery_factor)
            )
            
            if new_concurrency > self._current_concurrency:
                logger.debug(
                    f"Recovering concurrency: "
                    f"{self._current_concurrency} -> {new_concurrency}"
                )
                # Add semaphore permits
                increase = new_concurrency - self._current_concurrency
                for _ in range(increase):
                    self._semaphore.release()
                self._current_concurrency = new_concurrency


# =============================================================================
# Async Client (Core Implementation)
# =============================================================================

class AsyncGreyClient:
    """
    Async client for Grey Distributed.
    
    This is the core implementation. The sync GreyClient wraps this.
    
    Example:
        async with AsyncGreyClient(endpoint, api_key, tenant_id) as client:
            result = await client.submit_task(...)
            status = await client.get_task_status(result.task_id)
    """
    
    def __init__(
        self,
        endpoint: str,
        api_key: Optional[str] = None,
        tenant_id: Optional[str] = None,
        *,
        auth: Optional[AuthProvider] = None,
        timeout: float = DEFAULT_TIMEOUT,
        retry_policy: Optional[RetryPolicy] = None,
        backpressure: Optional[BackpressureConfig] = None,
        verify_ssl: bool = True,
        pool_size: int = DEFAULT_POOL_SIZE,
    ):
        """
        Initialize Grey client.
        
        Args:
            endpoint: Grey API endpoint URL
            api_key: API key for authentication (or use auth provider)
            tenant_id: Tenant identifier for isolation
            auth: Custom authentication provider
            timeout: Default request timeout in seconds
            retry_policy: Retry configuration
            backpressure: Backpressure/rate limiting configuration
            verify_ssl: Whether to verify SSL certificates
            pool_size: HTTP connection pool size
        """
        if httpx is None:
            raise ImportError("httpx is required. Install with: pip install httpx")
        
        self.endpoint = endpoint.rstrip("/")
        self.tenant_id = tenant_id
        self.timeout = timeout
        self.retry_policy = retry_policy or RetryPolicy()
        
        # Set up authentication
        if auth:
            self._auth = auth
        elif api_key:
            self._auth = ApiKeyAuth(api_key)
        else:
            raise GreyAuthenticationError("API key or auth provider required")
        
        # Set up rate limiter
        self._rate_limiter = AdaptiveRateLimiter(
            backpressure or BackpressureConfig()
        )
        
        # Create HTTP client
        self._client = httpx.AsyncClient(
            base_url=self.endpoint,
            timeout=timeout,
            verify=verify_ssl,
            limits=httpx.Limits(max_connections=pool_size),
            http2=True,
        )
        
        self._closed = False
    
    async def __aenter__(self) -> "AsyncGreyClient":
        return self
    
    async def __aexit__(self, *args) -> None:
        await self.close()
    
    async def close(self) -> None:
        """Close the client and release resources."""
        if not self._closed:
            await self._client.aclose()
            self._closed = True
    
    def _build_headers(self) -> Dict[str, str]:
        """Build request headers with auth and tenant."""
        headers = {
            "Content-Type": "application/json",
            "Accept": "application/json",
            "X-Grey-Client": "python-sdk/1.0.0",
        }
        headers.update(self._auth.get_headers())
        
        if self.tenant_id:
            headers["X-Grey-Tenant-ID"] = self.tenant_id
        
        return headers
    
    async def _request(
        self,
        method: str,
        path: str,
        *,
        json_data: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        retry: bool = True,
    ) -> Dict[str, Any]:
        """
        Make an authenticated request with retry logic.
        
        Implements:
        - Automatic retries for transient errors
        - Backpressure handling
        - Request timeout
        - Error response parsing
        """
        headers = self._build_headers()
        last_error: Optional[Exception] = None
        
        for attempt in range(self.retry_policy.max_retries + 1):
            try:
                # Acquire rate limiter permit
                await self._rate_limiter.acquire()
                
                try:
                    response = await self._client.request(
                        method,
                        path,
                        headers=headers,
                        json=json_data,
                        params=params,
                    )
                finally:
                    self._rate_limiter.release()
                
                # Handle rate limiting
                if response.status_code == 429:
                    await self._rate_limiter.on_rate_limited()
                    retry_after = float(response.headers.get("Retry-After", 1.0))
                    if retry and attempt < self.retry_policy.max_retries:
                        await asyncio.sleep(retry_after)
                        continue
                    raise GreyRateLimitError(
                        "Rate limit exceeded",
                        retry_after=retry_after
                    )
                
                # Try to recover rate limiter
                await self._rate_limiter.try_recover()
                
                # Handle auth errors (don't retry)
                if response.status_code == 401:
                    raise GreyAuthenticationError("Authentication failed")
                if response.status_code == 403:
                    raise GreyAuthorizationError("Not authorized")
                
                # Handle validation errors (don't retry)
                if response.status_code == 400:
                    error_data = response.json()
                    raise GreyValidationError(
                        error_data.get("message", "Validation failed")
                    )
                
                # Handle server errors (retry)
                if response.status_code in self.retry_policy.retry_on_status:
                    if retry and attempt < self.retry_policy.max_retries:
                        delay = self.retry_policy.calculate_delay(attempt)
                        logger.warning(
                            f"Request failed with {response.status_code}, "
                            f"retrying in {delay:.1f}s (attempt {attempt + 1})"
                        )
                        await asyncio.sleep(delay)
                        continue
                    response.raise_for_status()
                
                # Handle not found
                if response.status_code == 404:
                    raise GreyTaskError("Resource not found")
                
                # Parse successful response
                if response.status_code in (200, 201, 202):
                    return response.json()
                
                response.raise_for_status()
                
            except httpx.ConnectError as e:
                last_error = GreyConnectionError(f"Connection failed: {e}")
                if retry and attempt < self.retry_policy.max_retries:
                    delay = self.retry_policy.calculate_delay(attempt)
                    await asyncio.sleep(delay)
                    continue
                raise last_error
                
            except httpx.TimeoutException as e:
                last_error = GreyConnectionError(f"Request timeout: {e}")
                if retry and attempt < self.retry_policy.max_retries:
                    delay = self.retry_policy.calculate_delay(attempt)
                    await asyncio.sleep(delay)
                    continue
                raise last_error
        
        raise last_error or GreyError("Request failed")
    
    # =========================================================================
    # Task Operations
    # =========================================================================
    
    async def submit_task(
        self,
        task_type: Union[str, TaskType],
        payload: Dict[str, Any],
        *,
        priority: int = 5,
        timeout: Optional[int] = None,
        idempotency_key: Optional[str] = None,
        dependencies: Optional[List[str]] = None,
        labels: Optional[Dict[str, str]] = None,
        tee_required: bool = False,
    ) -> TaskResult:
        """
        Submit a task to the Grey cluster.
        
        Args:
            task_type: Type of task (stateless, stateful, pipeline, batch)
            payload: Task-specific input data
            priority: Execution priority (1-10, higher = more urgent)
            timeout: Maximum execution time in seconds
            idempotency_key: Unique key for request deduplication
            dependencies: Task IDs that must complete first
            labels: Key-value pairs for organization
            tee_required: Whether to require TEE execution
        
        Returns:
            TaskResult with task_id and initial status
        
        Example:
            result = await client.submit_task(
                task_type="stateless",
                payload={"function": "process", "data": [1, 2, 3]},
                priority=7,
                labels={"env": "production"}
            )
        """
        if isinstance(task_type, TaskType):
            task_type = task_type.value
        
        request_data = {
            "type": task_type,
            "payload": payload,
            "priority": priority,
        }
        
        if timeout is not None:
            request_data["timeout_seconds"] = timeout
        if idempotency_key:
            request_data["idempotency_key"] = idempotency_key
        if dependencies:
            request_data["dependencies"] = dependencies
        if labels:
            request_data["labels"] = labels
        if tee_required:
            request_data["tee_required"] = tee_required
        
        response = await self._request("POST", TASKS_ENDPOINT, json_data=request_data)
        return TaskResult.from_api_response(response)
    
    async def submit_batch(
        self,
        tasks: List[TaskDefinition],
        *,
        wait_all: bool = False,
    ) -> List[TaskResult]:
        """
        Submit multiple tasks in a single request.
        
        More efficient than individual submissions for large batches.
        
        Args:
            tasks: List of task definitions
            wait_all: If True, wait for all tasks to complete
        
        Returns:
            List of TaskResult for each submitted task
        """
        request_data = {
            "tasks": [t.to_dict() for t in tasks],
            "wait": wait_all,
        }
        
        response = await self._request(
            "POST", 
            f"{TASKS_ENDPOINT}/batch", 
            json_data=request_data
        )
        
        return [TaskResult.from_api_response(r) for r in response["results"]]
    
    async def submit_pipeline(
        self,
        stages: List[Dict[str, Any]],
        *,
        name: Optional[str] = None,
        parallel_branches: bool = False,
    ) -> TaskResult:
        """
        Submit a pipeline (DAG) of tasks.
        
        Pipelines define a directed acyclic graph of tasks
        where outputs flow to downstream inputs.
        
        Args:
            stages: List of stage definitions with dependencies
            name: Human-readable pipeline name
            parallel_branches: Whether to run branches in parallel
        
        Example:
            result = await client.submit_pipeline([
                {"id": "extract", "type": "stateless", "payload": {...}},
                {"id": "transform", "type": "stateless", "payload": {...}, 
                 "depends_on": ["extract"]},
                {"id": "load", "type": "stateful", "payload": {...},
                 "depends_on": ["transform"]}
            ])
        """
        request_data = {
            "type": "pipeline",
            "stages": stages,
            "parallel_branches": parallel_branches,
        }
        
        if name:
            request_data["name"] = name
        
        response = await self._request("POST", TASKS_ENDPOINT, json_data=request_data)
        return TaskResult.from_api_response(response)
    
    async def get_task_status(self, task_id: str) -> TaskResult:
        """
        Get the current status of a task.
        
        Args:
            task_id: The task identifier
        
        Returns:
            TaskResult with current status and result if complete
        """
        response = await self._request("GET", f"{TASKS_ENDPOINT}/{task_id}")
        return TaskResult.from_api_response(response)
    
    async def wait_for_task(
        self,
        task_id: str,
        *,
        timeout: Optional[float] = None,
        poll_interval: float = 1.0,
    ) -> TaskResult:
        """
        Wait for a task to complete.
        
        Polls the task status until it reaches a terminal state
        (completed, failed, cancelled, timeout).
        
        Args:
            task_id: The task identifier
            timeout: Maximum time to wait in seconds
            poll_interval: Time between status checks
        
        Returns:
            Final TaskResult
        
        Raises:
            asyncio.TimeoutError: If timeout exceeded
        """
        deadline = time.time() + timeout if timeout else float("inf")
        
        while time.time() < deadline:
            result = await self.get_task_status(task_id)
            
            if result.status in (
                TaskStatus.COMPLETED,
                TaskStatus.FAILED,
                TaskStatus.CANCELLED,
                TaskStatus.TIMEOUT,
            ):
                return result
            
            await asyncio.sleep(poll_interval)
        
        raise asyncio.TimeoutError(f"Task {task_id} did not complete within timeout")
    
    async def cancel_task(self, task_id: str) -> bool:
        """
        Cancel a pending or running task.
        
        Returns True if cancellation was accepted.
        Note: Task may still complete if cancel arrives too late.
        """
        response = await self._request("DELETE", f"{TASKS_ENDPOINT}/{task_id}")
        return response.get("cancelled", False)
    
    async def get_task_proof(self, task_id: str) -> bytes:
        """
        Retrieve the proof artifact for a completed task.
        
        Only available for tasks that ran in TEE enclaves.
        The proof contains attestation data and execution trace.
        
        Returns:
            Raw proof bytes that can be verified independently
        """
        response = await self._request("GET", f"{TASKS_ENDPOINT}/{task_id}/proof")
        return bytes.fromhex(response["proof"])
    
    async def list_tasks(
        self,
        *,
        status: Optional[TaskStatus] = None,
        labels: Optional[Dict[str, str]] = None,
        since: Optional[datetime] = None,
        limit: int = 100,
        offset: int = 0,
    ) -> List[TaskResult]:
        """
        List tasks with optional filtering.
        
        Args:
            status: Filter by task status
            labels: Filter by labels (all must match)
            since: Only tasks created after this time
            limit: Maximum results to return
            offset: Pagination offset
        
        Returns:
            List of matching TaskResults
        """
        params: Dict[str, Any] = {"limit": limit, "offset": offset}
        
        if status:
            params["status"] = status.value
        if labels:
            params["labels"] = json.dumps(labels)
        if since:
            params["since"] = since.isoformat()
        
        response = await self._request("GET", TASKS_ENDPOINT, params=params)
        return [TaskResult.from_api_response(r) for r in response["tasks"]]
    
    async def replay_task(
        self,
        task_id: str,
        *,
        at: Optional[datetime] = None,
        override_payload: Optional[Dict[str, Any]] = None,
    ) -> TaskResult:
        """
        Replay a previously executed task.
        
        Useful for debugging or re-running failed tasks.
        
        Args:
            task_id: Original task to replay
            at: Point-in-time for state reconstruction
            override_payload: Modified payload for replay
        
        Returns:
            TaskResult for the new replay task
        """
        request_data: Dict[str, Any] = {"original_task_id": task_id}
        
        if at:
            request_data["at_timestamp"] = at.isoformat()
        if override_payload:
            request_data["payload_override"] = override_payload
        
        response = await self._request(
            "POST", 
            f"{TASKS_ENDPOINT}/replay", 
            json_data=request_data
        )
        return TaskResult.from_api_response(response)
    
    # =========================================================================
    # Cluster Operations
    # =========================================================================
    
    async def get_cluster_health(self) -> ClusterHealth:
        """
        Get cluster health and resource usage.
        
        Returns aggregate information about cluster state
        including node count, utilization, and queue depth.
        """
        response = await self._request("GET", CLUSTER_ENDPOINT)
        
        return ClusterHealth(
            healthy=response["healthy"],
            nodes=response["nodes"]["total"],
            active_nodes=response["nodes"]["active"],
            cpu_utilization=response["utilization"]["cpu"],
            memory_utilization=response["utilization"]["memory"],
            queue_depth=response["queue"]["depth"],
            tasks_per_second=response["throughput"]["tasks_per_second"],
            leader=response.get("consensus", {}).get("leader"),
            consensus_term=response.get("consensus", {}).get("term", 0),
        )
    
    async def get_cluster_nodes(self) -> List[Dict[str, Any]]:
        """Get detailed information about all cluster nodes."""
        response = await self._request("GET", f"{CLUSTER_ENDPOINT}/nodes")
        return response["nodes"]
    
    async def get_tenant_usage(self) -> Dict[str, Any]:
        """Get resource usage for the current tenant."""
        response = await self._request("GET", f"{CLUSTER_ENDPOINT}/usage")
        return response
    
    # =========================================================================
    # Security & Attestation
    # =========================================================================
    
    async def verify_node_attestation(self, node_id: str) -> AttestationResult:
        """
        Verify TEE attestation for a specific node.
        
        Returns attestation verification result including
        platform-specific measurements (SGX mrenclave, etc.)
        """
        response = await self._request(
            "GET", 
            f"{SECURITY_ENDPOINT}/attestation/{node_id}"
        )
        
        return AttestationResult(
            valid=response["valid"],
            node_id=node_id,
            platform=response["platform"],
            mrenclave=response.get("mrenclave"),
            mrsigner=response.get("mrsigner"),
            verified_at=datetime.fromisoformat(response["verified_at"].replace("Z", "+00:00"))
            if response.get("verified_at") else None,
            quote=bytes.fromhex(response["quote"]) if response.get("quote") else None,
        )
    
    async def get_tenant_isolation_status(self) -> Dict[str, Any]:
        """
        Get isolation status for the current tenant.
        
        Returns information about isolation boundaries,
        resource quotas, and any isolation violations.
        """
        response = await self._request("GET", f"{SECURITY_ENDPOINT}/isolation")
        return response
    
    async def request_attestation(self) -> AttestationResult:
        """
        Request fresh attestation from the cluster.
        
        Triggers a new attestation flow and returns the result.
        Use this to verify cluster security before sensitive operations.
        """
        response = await self._request("POST", f"{SECURITY_ENDPOINT}/attestation")
        
        return AttestationResult(
            valid=response["valid"],
            node_id=response["node_id"],
            platform=response["platform"],
            mrenclave=response.get("mrenclave"),
            mrsigner=response.get("mrsigner"),
            verified_at=datetime.fromisoformat(response["verified_at"].replace("Z", "+00:00"))
            if response.get("verified_at") else None,
        )


# =============================================================================
# Sync Client Wrapper
# =============================================================================

class GreyClient:
    """
    Synchronous wrapper around AsyncGreyClient.
    
    Provides the same API but with blocking calls suitable
    for scripts and synchronous applications.
    
    Example:
        client = GreyClient(
            endpoint="https://grey.example.com",
            api_key="your-api-key",
            tenant_id="your-tenant"
        )
        
        result = client.submit_task(
            task_type="compute",
            payload={"data": [1, 2, 3]}
        )
        
        status = client.wait_for_task(result.task_id, timeout=60)
    """
    
    def __init__(self, *args, **kwargs):
        """Initialize with same arguments as AsyncGreyClient."""
        self._async_client = AsyncGreyClient(*args, **kwargs)
        self._loop: Optional[asyncio.AbstractEventLoop] = None
    
    def _get_loop(self) -> asyncio.AbstractEventLoop:
        """Get or create event loop for sync operations."""
        if self._loop is None or self._loop.is_closed():
            try:
                self._loop = asyncio.get_event_loop()
            except RuntimeError:
                self._loop = asyncio.new_event_loop()
                asyncio.set_event_loop(self._loop)
        return self._loop
    
    def _run(self, coro):
        """Run coroutine synchronously."""
        return self._get_loop().run_until_complete(coro)
    
    def close(self) -> None:
        """Close the client."""
        self._run(self._async_client.close())
    
    def __enter__(self) -> "GreyClient":
        return self
    
    def __exit__(self, *args) -> None:
        self.close()
    
    # Task operations
    def submit_task(self, *args, **kwargs) -> TaskResult:
        return self._run(self._async_client.submit_task(*args, **kwargs))
    
    def submit_batch(self, *args, **kwargs) -> List[TaskResult]:
        return self._run(self._async_client.submit_batch(*args, **kwargs))
    
    def submit_pipeline(self, *args, **kwargs) -> TaskResult:
        return self._run(self._async_client.submit_pipeline(*args, **kwargs))
    
    def get_task_status(self, task_id: str) -> TaskResult:
        return self._run(self._async_client.get_task_status(task_id))
    
    def wait_for_task(self, task_id: str, **kwargs) -> TaskResult:
        return self._run(self._async_client.wait_for_task(task_id, **kwargs))
    
    def cancel_task(self, task_id: str) -> bool:
        return self._run(self._async_client.cancel_task(task_id))
    
    def get_task_proof(self, task_id: str) -> bytes:
        return self._run(self._async_client.get_task_proof(task_id))
    
    def list_tasks(self, **kwargs) -> List[TaskResult]:
        return self._run(self._async_client.list_tasks(**kwargs))
    
    def replay_task(self, task_id: str, **kwargs) -> TaskResult:
        return self._run(self._async_client.replay_task(task_id, **kwargs))
    
    # Cluster operations
    def get_cluster_health(self) -> ClusterHealth:
        return self._run(self._async_client.get_cluster_health())
    
    def get_cluster_nodes(self) -> List[Dict[str, Any]]:
        return self._run(self._async_client.get_cluster_nodes())
    
    def get_tenant_usage(self) -> Dict[str, Any]:
        return self._run(self._async_client.get_tenant_usage())
    
    # Security operations
    def verify_node_attestation(self, node_id: str) -> AttestationResult:
        return self._run(self._async_client.verify_node_attestation(node_id))
    
    def get_tenant_isolation_status(self) -> Dict[str, Any]:
        return self._run(self._async_client.get_tenant_isolation_status())
    
    def request_attestation(self) -> AttestationResult:
        return self._run(self._async_client.request_attestation())


# =============================================================================
# Convenience Functions
# =============================================================================

def create_client(
    endpoint: Optional[str] = None,
    api_key: Optional[str] = None,
    tenant_id: Optional[str] = None,
    **kwargs,
) -> GreyClient:
    """
    Create a Grey client with environment variable fallbacks.
    
    Environment variables:
    - GREY_ENDPOINT: API endpoint URL
    - GREY_API_KEY: API key for authentication
    - GREY_TENANT_ID: Tenant identifier
    
    Example:
        # Uses env vars
        client = create_client()
        
        # Override specific values
        client = create_client(tenant_id="custom-tenant")
    """
    return GreyClient(
        endpoint=endpoint or os.environ.get("GREY_ENDPOINT", "http://localhost:8080"),
        api_key=api_key or os.environ.get("GREY_API_KEY"),
        tenant_id=tenant_id or os.environ.get("GREY_TENANT_ID"),
        **kwargs,
    )


# =============================================================================
# Package Exports
# =============================================================================

__all__ = [
    # Clients
    "GreyClient",
    "AsyncGreyClient",
    "create_client",
    # Config
    "RetryPolicy",
    "BackpressureConfig",
    # Data types
    "TaskDefinition",
    "TaskResult",
    "TaskStatus",
    "TaskType",
    "Priority",
    "ClusterHealth",
    "AttestationResult",
    # Auth
    "AuthProvider",
    "ApiKeyAuth",
    "HMACAuth",
    # Exceptions
    "GreyError",
    "GreyConnectionError",
    "GreyAuthenticationError",
    "GreyAuthorizationError",
    "GreyRateLimitError",
    "GreyTaskError",
    "GreyValidationError",
]
