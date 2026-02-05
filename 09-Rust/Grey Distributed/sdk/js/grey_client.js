/**
 * Grey Distributed JavaScript/TypeScript SDK
 * ============================================
 * 
 * A comprehensive client for interacting with Grey Distributed clusters
 * from web browsers and Node.js applications.
 * 
 * Features:
 * - Task submission (stateless, stateful, pipelines)
 * - Real-time task status via WebSocket or polling
 * - Automatic retry with exponential backoff
 * - Tenant isolation enforcement
 * - Browser and Node.js compatible
 * - TypeScript definitions included
 * 
 * Installation:
 *   npm install @grey/client
 *   # or
 *   yarn add @grey/client
 * 
 * Usage:
 *   import { GreyClient } from '@grey/client';
 *   
 *   const client = new GreyClient({
 *     endpoint: 'https://grey.example.com',
 *     apiKey: 'your-api-key',
 *     tenantId: 'your-tenant'
 *   });
 *   
 *   const result = await client.submitTask({
 *     type: 'compute',
 *     payload: { data: [1, 2, 3] }
 *   });
 * 
 * Design Philosophy:
 * - Promise-based async API
 * - Zero dependencies in browser (optional fetch polyfill for older browsers)
 * - TypeScript-first with full type inference
 * - Fail-fast validation, graceful error handling
 * 
 * Tradeoffs:
 * - Uses native fetch (requires polyfill for Node.js < 18)
 * - WebSocket for real-time updates (falls back to polling)
 * - Retry logic increases latency for transient failures
 * - Connection pooling handled by browser/runtime
 * 
 * @module @grey/client
 * @version 1.0.0
 */

// ============================================================================
// Type Definitions
// ============================================================================

/**
 * Task lifecycle states
 */
const TaskStatus = {
  PENDING: 'pending',
  QUEUED: 'queued',
  RUNNING: 'running',
  COMPLETED: 'completed',
  FAILED: 'failed',
  CANCELLED: 'cancelled',
  TIMEOUT: 'timeout'
};

/**
 * Supported task types
 */
const TaskType = {
  STATELESS: 'stateless',
  STATEFUL: 'stateful',
  PIPELINE: 'pipeline',
  BATCH: 'batch'
};

/**
 * Task priority levels
 */
const Priority = {
  CRITICAL: 10,
  HIGH: 7,
  NORMAL: 5,
  LOW: 3,
  BACKGROUND: 1
};

// ============================================================================
// Error Classes
// ============================================================================

/**
 * Base error class for Grey SDK errors
 */
class GreyError extends Error {
  constructor(message, code = null, details = null) {
    super(message);
    this.name = 'GreyError';
    this.code = code;
    this.details = details;
  }
}

/**
 * Connection failed
 */
class GreyConnectionError extends GreyError {
  constructor(message, cause = null) {
    super(message, 'CONNECTION_ERROR');
    this.name = 'GreyConnectionError';
    this.cause = cause;
  }
}

/**
 * Authentication failed
 */
class GreyAuthenticationError extends GreyError {
  constructor(message = 'Authentication failed') {
    super(message, 'AUTH_ERROR');
    this.name = 'GreyAuthenticationError';
  }
}

/**
 * Not authorized for action
 */
class GreyAuthorizationError extends GreyError {
  constructor(message = 'Not authorized') {
    super(message, 'AUTHZ_ERROR');
    this.name = 'GreyAuthorizationError';
  }
}

/**
 * Rate limit exceeded
 */
class GreyRateLimitError extends GreyError {
  constructor(message = 'Rate limit exceeded', retryAfter = null) {
    super(message, 'RATE_LIMIT');
    this.name = 'GreyRateLimitError';
    this.retryAfter = retryAfter;
  }
}

/**
 * Request validation failed
 */
class GreyValidationError extends GreyError {
  constructor(message, fieldErrors = null) {
    super(message, 'VALIDATION_ERROR');
    this.name = 'GreyValidationError';
    this.fieldErrors = fieldErrors;
  }
}

/**
 * Task-specific error
 */
class GreyTaskError extends GreyError {
  constructor(message, taskId = null) {
    super(message, 'TASK_ERROR');
    this.name = 'GreyTaskError';
    this.taskId = taskId;
  }
}

// ============================================================================
// Configuration
// ============================================================================

const DEFAULT_CONFIG = {
  timeout: 30000,
  maxRetries: 3,
  retryDelay: 1000,
  retryMaxDelay: 60000,
  retryStatusCodes: [429, 500, 502, 503, 504],
  pollInterval: 1000,
  enableWebSocket: true
};

const API_VERSION = 'v1';

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Sleep for specified milliseconds
 */
const sleep = (ms) => new Promise(resolve => setTimeout(resolve, ms));

/**
 * Calculate exponential backoff delay with jitter
 */
function calculateBackoff(attempt, baseDelay, maxDelay) {
  const delay = Math.min(baseDelay * Math.pow(2, attempt), maxDelay);
  const jitter = delay * (0.5 + Math.random() * 0.5);
  return jitter;
}

/**
 * Validate required configuration
 */
function validateConfig(config) {
  if (!config.endpoint) {
    throw new GreyValidationError('endpoint is required');
  }
  if (!config.apiKey && !config.authProvider) {
    throw new GreyValidationError('apiKey or authProvider is required');
  }
}

// ============================================================================
// Authentication Providers
// ============================================================================

/**
 * API Key authentication provider
 */
class ApiKeyAuth {
  constructor(apiKey, keyId = 'default') {
    this.apiKey = apiKey;
    this.keyId = keyId;
  }

  async getHeaders() {
    return {
      'Authorization': `Bearer ${this.apiKey}`,
      'X-Grey-Key-ID': this.keyId
    };
  }
}

/**
 * JWT authentication with automatic refresh
 */
class JwtAuth {
  constructor(options) {
    this.getToken = options.getToken;
    this.refreshToken = options.refreshToken;
    this.tokenExpiry = null;
    this.currentToken = null;
  }

  async getHeaders() {
    if (!this.currentToken || this._isExpired()) {
      if (this.refreshToken && this.currentToken) {
        this.currentToken = await this.refreshToken();
      } else {
        this.currentToken = await this.getToken();
      }
    }

    return {
      'Authorization': `Bearer ${this.currentToken}`
    };
  }

  _isExpired() {
    if (!this.tokenExpiry) return false;
    return Date.now() >= this.tokenExpiry - 30000; // 30s buffer
  }
}

// ============================================================================
// Rate Limiter
// ============================================================================

/**
 * Client-side rate limiter with adaptive concurrency
 * 
 * Mechanics:
 * - Tracks in-flight requests
 * - Reduces concurrency on 429 responses
 * - Gradually recovers after cooldown
 */
class AdaptiveRateLimiter {
  constructor(options = {}) {
    this.maxConcurrency = options.maxConcurrency || 50;
    this.minConcurrency = options.minConcurrency || 1;
    this.currentConcurrency = options.initialConcurrency || 20;
    this.reductionFactor = options.reductionFactor || 0.7;
    this.recoveryFactor = options.recoveryFactor || 1.1;
    this.recoveryInterval = options.recoveryInterval || 5000;
    
    this.inFlight = 0;
    this.lastRateLimited = 0;
    this.queue = [];
  }

  async acquire() {
    return new Promise((resolve) => {
      if (this.inFlight < this.currentConcurrency) {
        this.inFlight++;
        resolve();
      } else {
        this.queue.push(resolve);
      }
    });
  }

  release() {
    this.inFlight--;
    this._tryRecover();
    
    if (this.queue.length > 0 && this.inFlight < this.currentConcurrency) {
      const next = this.queue.shift();
      this.inFlight++;
      next();
    }
  }

  onRateLimited() {
    this.lastRateLimited = Date.now();
    const newConcurrency = Math.max(
      this.minConcurrency,
      Math.floor(this.currentConcurrency * this.reductionFactor)
    );
    
    if (newConcurrency < this.currentConcurrency) {
      console.warn(`Rate limited, reducing concurrency: ${this.currentConcurrency} -> ${newConcurrency}`);
      this.currentConcurrency = newConcurrency;
    }
  }

  _tryRecover() {
    const elapsed = Date.now() - this.lastRateLimited;
    if (elapsed < this.recoveryInterval) return;
    
    const newConcurrency = Math.min(
      this.maxConcurrency,
      Math.ceil(this.currentConcurrency * this.recoveryFactor)
    );
    
    if (newConcurrency > this.currentConcurrency) {
      this.currentConcurrency = newConcurrency;
    }
  }
}

// ============================================================================
// WebSocket Manager
// ============================================================================

/**
 * WebSocket connection manager for real-time updates
 * 
 * Provides real-time task status updates instead of polling.
 * Automatically reconnects on connection loss.
 */
class WebSocketManager {
  constructor(endpoint, authProvider, options = {}) {
    this.endpoint = endpoint.replace(/^http/, 'ws') + '/ws';
    this.authProvider = authProvider;
    this.reconnectDelay = options.reconnectDelay || 1000;
    this.maxReconnectDelay = options.maxReconnectDelay || 30000;
    
    this.ws = null;
    this.reconnectAttempt = 0;
    this.subscriptions = new Map();
    this.connected = false;
    this.messageQueue = [];
  }

  async connect() {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      return;
    }

    const headers = await this.authProvider.getHeaders();
    const url = new URL(this.endpoint);
    url.searchParams.set('token', headers['Authorization']?.replace('Bearer ', ''));

    return new Promise((resolve, reject) => {
      this.ws = new WebSocket(url.toString());

      this.ws.onopen = () => {
        this.connected = true;
        this.reconnectAttempt = 0;
        
        // Flush queued messages
        while (this.messageQueue.length > 0) {
          const msg = this.messageQueue.shift();
          this.ws.send(JSON.stringify(msg));
        }
        
        // Resubscribe to active subscriptions
        for (const [taskId, callback] of this.subscriptions) {
          this._subscribe(taskId);
        }
        
        resolve();
      };

      this.ws.onmessage = (event) => {
        try {
          const message = JSON.parse(event.data);
          this._handleMessage(message);
        } catch (e) {
          console.error('Failed to parse WebSocket message:', e);
        }
      };

      this.ws.onerror = (error) => {
        console.error('WebSocket error:', error);
        reject(new GreyConnectionError('WebSocket connection failed'));
      };

      this.ws.onclose = () => {
        this.connected = false;
        this._scheduleReconnect();
      };
    });
  }

  _handleMessage(message) {
    if (message.type === 'task_update' && message.task_id) {
      const callback = this.subscriptions.get(message.task_id);
      if (callback) {
        callback(message.data);
      }
    }
  }

  _scheduleReconnect() {
    const delay = calculateBackoff(
      this.reconnectAttempt,
      this.reconnectDelay,
      this.maxReconnectDelay
    );
    
    this.reconnectAttempt++;
    setTimeout(() => this.connect().catch(() => {}), delay);
  }

  _subscribe(taskId) {
    const message = { type: 'subscribe', task_id: taskId };
    
    if (this.connected && this.ws) {
      this.ws.send(JSON.stringify(message));
    } else {
      this.messageQueue.push(message);
    }
  }

  subscribeToTask(taskId, callback) {
    this.subscriptions.set(taskId, callback);
    this._subscribe(taskId);
    
    return () => {
      this.subscriptions.delete(taskId);
      if (this.connected && this.ws) {
        this.ws.send(JSON.stringify({ type: 'unsubscribe', task_id: taskId }));
      }
    };
  }

  close() {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
    this.subscriptions.clear();
  }
}

// ============================================================================
// Main Client Class
// ============================================================================

/**
 * Grey Distributed Client
 * 
 * Main entry point for interacting with Grey clusters.
 * 
 * @example
 * const client = new GreyClient({
 *   endpoint: 'https://grey.example.com',
 *   apiKey: 'your-api-key',
 *   tenantId: 'your-tenant'
 * });
 * 
 * // Submit a task
 * const result = await client.submitTask({
 *   type: 'compute',
 *   payload: { data: [1, 2, 3] }
 * });
 * 
 * // Wait for completion
 * const finalResult = await client.waitForTask(result.taskId);
 */
class GreyClient {
  /**
   * Create a new Grey client
   * 
   * @param {Object} config - Configuration options
   * @param {string} config.endpoint - Grey API endpoint URL
   * @param {string} [config.apiKey] - API key for authentication
   * @param {string} [config.tenantId] - Tenant identifier for isolation
   * @param {Object} [config.authProvider] - Custom auth provider
   * @param {number} [config.timeout=30000] - Request timeout in ms
   * @param {number} [config.maxRetries=3] - Maximum retry attempts
   * @param {boolean} [config.enableWebSocket=true] - Enable WebSocket for updates
   */
  constructor(config) {
    validateConfig(config);

    this.endpoint = config.endpoint.replace(/\/$/, '');
    this.tenantId = config.tenantId;
    this.timeout = config.timeout || DEFAULT_CONFIG.timeout;
    this.maxRetries = config.maxRetries ?? DEFAULT_CONFIG.maxRetries;
    this.retryDelay = config.retryDelay || DEFAULT_CONFIG.retryDelay;
    this.retryMaxDelay = config.retryMaxDelay || DEFAULT_CONFIG.retryMaxDelay;
    this.retryStatusCodes = config.retryStatusCodes || DEFAULT_CONFIG.retryStatusCodes;
    this.pollInterval = config.pollInterval || DEFAULT_CONFIG.pollInterval;

    // Set up authentication
    if (config.authProvider) {
      this.authProvider = config.authProvider;
    } else if (config.apiKey) {
      this.authProvider = new ApiKeyAuth(config.apiKey);
    }

    // Set up rate limiter
    this.rateLimiter = new AdaptiveRateLimiter(config.rateLimiter);

    // Set up WebSocket (optional)
    this.wsManager = null;
    if (config.enableWebSocket !== false && typeof WebSocket !== 'undefined') {
      this.wsManager = new WebSocketManager(this.endpoint, this.authProvider);
    }
  }

  /**
   * Build request headers
   * @private
   */
  async _buildHeaders() {
    const headers = {
      'Content-Type': 'application/json',
      'Accept': 'application/json',
      'X-Grey-Client': 'javascript-sdk/1.0.0'
    };

    const authHeaders = await this.authProvider.getHeaders();
    Object.assign(headers, authHeaders);

    if (this.tenantId) {
      headers['X-Grey-Tenant-ID'] = this.tenantId;
    }

    return headers;
  }

  /**
   * Make an authenticated HTTP request with retry logic
   * @private
   */
  async _request(method, path, options = {}) {
    const url = `${this.endpoint}${path}`;
    let lastError = null;

    for (let attempt = 0; attempt <= this.maxRetries; attempt++) {
      try {
        await this.rateLimiter.acquire();

        const headers = await this._buildHeaders();
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), this.timeout);

        try {
          const response = await fetch(url, {
            method,
            headers,
            body: options.body ? JSON.stringify(options.body) : undefined,
            signal: controller.signal
          });

          clearTimeout(timeoutId);

          // Handle rate limiting
          if (response.status === 429) {
            this.rateLimiter.onRateLimited();
            const retryAfter = parseFloat(response.headers.get('Retry-After') || '1');
            
            if (attempt < this.maxRetries) {
              await sleep(retryAfter * 1000);
              continue;
            }
            throw new GreyRateLimitError('Rate limit exceeded', retryAfter);
          }

          // Handle auth errors (don't retry)
          if (response.status === 401) {
            throw new GreyAuthenticationError();
          }
          if (response.status === 403) {
            throw new GreyAuthorizationError();
          }

          // Handle validation errors (don't retry)
          if (response.status === 400) {
            const errorData = await response.json();
            throw new GreyValidationError(
              errorData.message || 'Validation failed',
              errorData.field_errors
            );
          }

          // Handle not found
          if (response.status === 404) {
            throw new GreyTaskError('Resource not found');
          }

          // Handle server errors (retry)
          if (this.retryStatusCodes.includes(response.status)) {
            if (attempt < this.maxRetries) {
              const delay = calculateBackoff(attempt, this.retryDelay, this.retryMaxDelay);
              console.warn(`Request failed with ${response.status}, retrying in ${delay}ms`);
              await sleep(delay);
              continue;
            }
            throw new GreyError(`Request failed with status ${response.status}`);
          }

          // Parse successful response
          if (response.ok) {
            const contentType = response.headers.get('Content-Type');
            if (contentType && contentType.includes('application/json')) {
              return await response.json();
            }
            return await response.text();
          }

          throw new GreyError(`Unexpected status ${response.status}`);

        } finally {
          this.rateLimiter.release();
          clearTimeout(timeoutId);
        }

      } catch (error) {
        lastError = error;

        if (error instanceof GreyError) {
          throw error;
        }

        if (error.name === 'AbortError') {
          lastError = new GreyConnectionError('Request timeout');
        } else if (error.name === 'TypeError' && error.message.includes('fetch')) {
          lastError = new GreyConnectionError('Network error', error);
        }

        if (attempt < this.maxRetries) {
          const delay = calculateBackoff(attempt, this.retryDelay, this.retryMaxDelay);
          await sleep(delay);
          continue;
        }
      }
    }

    throw lastError || new GreyError('Request failed');
  }

  // ===========================================================================
  // Task Operations
  // ===========================================================================

  /**
   * Submit a task to the Grey cluster
   * 
   * @param {Object} task - Task definition
   * @param {string} task.type - Task type (stateless, stateful, pipeline, batch)
   * @param {Object} task.payload - Task-specific input data
   * @param {number} [task.priority=5] - Execution priority (1-10)
   * @param {number} [task.timeout] - Maximum execution time in seconds
   * @param {string} [task.idempotencyKey] - Unique key for deduplication
   * @param {string[]} [task.dependencies] - Task IDs that must complete first
   * @param {Object} [task.labels] - Key-value pairs for organization
   * @param {boolean} [task.teeRequired=false] - Require TEE execution
   * @returns {Promise<Object>} Task result with taskId and status
   * 
   * @example
   * const result = await client.submitTask({
   *   type: 'stateless',
   *   payload: { function: 'process', data: [1, 2, 3] },
   *   priority: 7,
   *   labels: { env: 'production' }
   * });
   */
  async submitTask(task) {
    const body = {
      type: task.type,
      payload: task.payload,
      priority: task.priority || 5
    };

    if (task.timeout !== undefined) body.timeout_seconds = task.timeout;
    if (task.idempotencyKey) body.idempotency_key = task.idempotencyKey;
    if (task.dependencies) body.dependencies = task.dependencies;
    if (task.labels) body.labels = task.labels;
    if (task.teeRequired) body.tee_required = task.teeRequired;

    const response = await this._request('POST', `/api/${API_VERSION}/tasks`, { body });
    return this._parseTaskResponse(response);
  }

  /**
   * Submit multiple tasks in a batch
   * 
   * @param {Object[]} tasks - Array of task definitions
   * @param {Object} [options] - Batch options
   * @param {boolean} [options.waitAll=false] - Wait for all tasks to complete
   * @returns {Promise<Object[]>} Array of task results
   */
  async submitBatch(tasks, options = {}) {
    const body = {
      tasks: tasks.map(t => ({
        type: t.type,
        payload: t.payload,
        priority: t.priority || 5,
        timeout_seconds: t.timeout,
        idempotency_key: t.idempotencyKey,
        dependencies: t.dependencies,
        labels: t.labels,
        tee_required: t.teeRequired
      })),
      wait: options.waitAll || false
    };

    const response = await this._request('POST', `/api/${API_VERSION}/tasks/batch`, { body });
    return response.results.map(r => this._parseTaskResponse(r));
  }

  /**
   * Submit a pipeline (DAG) of tasks
   * 
   * @param {Object[]} stages - Pipeline stage definitions
   * @param {Object} [options] - Pipeline options
   * @returns {Promise<Object>} Pipeline task result
   * 
   * @example
   * const result = await client.submitPipeline([
   *   { id: 'extract', type: 'stateless', payload: { source: 'db' } },
   *   { id: 'transform', type: 'stateless', payload: { ... }, dependsOn: ['extract'] },
   *   { id: 'load', type: 'stateful', payload: { ... }, dependsOn: ['transform'] }
   * ]);
   */
  async submitPipeline(stages, options = {}) {
    const body = {
      type: 'pipeline',
      stages: stages.map(s => ({
        id: s.id,
        type: s.type,
        payload: s.payload,
        depends_on: s.dependsOn
      })),
      parallel_branches: options.parallelBranches || false,
      name: options.name
    };

    const response = await this._request('POST', `/api/${API_VERSION}/tasks`, { body });
    return this._parseTaskResponse(response);
  }

  /**
   * Get current status of a task
   * 
   * @param {string} taskId - Task identifier
   * @returns {Promise<Object>} Task status and result
   */
  async getTaskStatus(taskId) {
    const response = await this._request('GET', `/api/${API_VERSION}/tasks/${taskId}`);
    return this._parseTaskResponse(response);
  }

  /**
   * Wait for a task to complete
   * 
   * Uses WebSocket for real-time updates if available,
   * otherwise falls back to polling.
   * 
   * @param {string} taskId - Task identifier
   * @param {Object} [options] - Wait options
   * @param {number} [options.timeout] - Maximum wait time in ms
   * @param {number} [options.pollInterval] - Polling interval if no WebSocket
   * @param {Function} [options.onProgress] - Callback for status updates
   * @returns {Promise<Object>} Final task result
   */
  async waitForTask(taskId, options = {}) {
    const timeout = options.timeout;
    const pollInterval = options.pollInterval || this.pollInterval;
    const onProgress = options.onProgress;

    const deadline = timeout ? Date.now() + timeout : null;

    // Try WebSocket first
    if (this.wsManager) {
      try {
        await this.wsManager.connect();
        
        return new Promise((resolve, reject) => {
          const timeoutId = deadline
            ? setTimeout(() => {
                unsubscribe();
                reject(new GreyError(`Task ${taskId} did not complete within timeout`));
              }, timeout)
            : null;

          const unsubscribe = this.wsManager.subscribeToTask(taskId, (update) => {
            if (onProgress) onProgress(update);

            if (['completed', 'failed', 'cancelled', 'timeout'].includes(update.status)) {
              if (timeoutId) clearTimeout(timeoutId);
              unsubscribe();
              resolve(this._parseTaskResponse(update));
            }
          });
        });
      } catch (e) {
        console.warn('WebSocket unavailable, falling back to polling');
      }
    }

    // Fall back to polling
    while (true) {
      const result = await this.getTaskStatus(taskId);

      if (onProgress) onProgress(result);

      if (['completed', 'failed', 'cancelled', 'timeout'].includes(result.status)) {
        return result;
      }

      if (deadline && Date.now() >= deadline) {
        throw new GreyError(`Task ${taskId} did not complete within timeout`);
      }

      await sleep(pollInterval);
    }
  }

  /**
   * Cancel a pending or running task
   * 
   * @param {string} taskId - Task identifier
   * @returns {Promise<boolean>} True if cancellation was accepted
   */
  async cancelTask(taskId) {
    const response = await this._request('DELETE', `/api/${API_VERSION}/tasks/${taskId}`);
    return response.cancelled || false;
  }

  /**
   * Get proof artifact for a completed TEE task
   * 
   * @param {string} taskId - Task identifier
   * @returns {Promise<ArrayBuffer>} Raw proof bytes
   */
  async getTaskProof(taskId) {
    const response = await this._request('GET', `/api/${API_VERSION}/tasks/${taskId}/proof`);
    return this._hexToBytes(response.proof);
  }

  /**
   * List tasks with optional filtering
   * 
   * @param {Object} [filters] - Filter options
   * @param {string} [filters.status] - Filter by status
   * @param {Object} [filters.labels] - Filter by labels
   * @param {Date|string} [filters.since] - Only tasks after this time
   * @param {number} [filters.limit=100] - Maximum results
   * @param {number} [filters.offset=0] - Pagination offset
   * @returns {Promise<Object[]>} List of tasks
   */
  async listTasks(filters = {}) {
    const params = new URLSearchParams();
    
    if (filters.status) params.set('status', filters.status);
    if (filters.labels) params.set('labels', JSON.stringify(filters.labels));
    if (filters.since) {
      const since = filters.since instanceof Date ? filters.since.toISOString() : filters.since;
      params.set('since', since);
    }
    params.set('limit', String(filters.limit || 100));
    params.set('offset', String(filters.offset || 0));

    const response = await this._request('GET', `/api/${API_VERSION}/tasks?${params}`);
    return response.tasks.map(t => this._parseTaskResponse(t));
  }

  /**
   * Replay a previously executed task
   * 
   * @param {string} taskId - Original task to replay
   * @param {Object} [options] - Replay options
   * @param {Date|string} [options.at] - Point-in-time state reconstruction
   * @param {Object} [options.overridePayload] - Modified payload
   * @returns {Promise<Object>} New replay task result
   */
  async replayTask(taskId, options = {}) {
    const body = { original_task_id: taskId };
    
    if (options.at) {
      body.at_timestamp = options.at instanceof Date ? options.at.toISOString() : options.at;
    }
    if (options.overridePayload) {
      body.payload_override = options.overridePayload;
    }

    const response = await this._request('POST', `/api/${API_VERSION}/tasks/replay`, { body });
    return this._parseTaskResponse(response);
  }

  // ===========================================================================
  // Cluster Operations
  // ===========================================================================

  /**
   * Get cluster health and resource usage
   * 
   * @returns {Promise<Object>} Cluster health information
   */
  async getClusterHealth() {
    const response = await this._request('GET', `/api/${API_VERSION}/cluster`);
    
    return {
      healthy: response.healthy,
      nodes: response.nodes.total,
      activeNodes: response.nodes.active,
      cpuUtilization: response.utilization.cpu,
      memoryUtilization: response.utilization.memory,
      queueDepth: response.queue.depth,
      tasksPerSecond: response.throughput.tasks_per_second,
      leader: response.consensus?.leader,
      consensusTerm: response.consensus?.term || 0
    };
  }

  /**
   * Get detailed information about cluster nodes
   * 
   * @returns {Promise<Object[]>} List of node details
   */
  async getClusterNodes() {
    const response = await this._request('GET', `/api/${API_VERSION}/cluster/nodes`);
    return response.nodes;
  }

  /**
   * Get resource usage for current tenant
   * 
   * @returns {Promise<Object>} Tenant usage information
   */
  async getTenantUsage() {
    return this._request('GET', `/api/${API_VERSION}/cluster/usage`);
  }

  // ===========================================================================
  // Security & Attestation
  // ===========================================================================

  /**
   * Verify TEE attestation for a node
   * 
   * @param {string} nodeId - Node identifier
   * @returns {Promise<Object>} Attestation verification result
   */
  async verifyNodeAttestation(nodeId) {
    const response = await this._request('GET', `/api/${API_VERSION}/security/attestation/${nodeId}`);
    
    return {
      valid: response.valid,
      nodeId: nodeId,
      platform: response.platform,
      mrenclave: response.mrenclave,
      mrsigner: response.mrsigner,
      verifiedAt: response.verified_at ? new Date(response.verified_at) : null,
      quote: response.quote ? this._hexToBytes(response.quote) : null
    };
  }

  /**
   * Get isolation status for current tenant
   * 
   * @returns {Promise<Object>} Isolation status information
   */
  async getTenantIsolationStatus() {
    return this._request('GET', `/api/${API_VERSION}/security/isolation`);
  }

  /**
   * Request fresh attestation from cluster
   * 
   * @returns {Promise<Object>} Attestation result
   */
  async requestAttestation() {
    const response = await this._request('POST', `/api/${API_VERSION}/security/attestation`);
    
    return {
      valid: response.valid,
      nodeId: response.node_id,
      platform: response.platform,
      mrenclave: response.mrenclave,
      mrsigner: response.mrsigner,
      verifiedAt: response.verified_at ? new Date(response.verified_at) : null
    };
  }

  // ===========================================================================
  // Helper Methods
  // ===========================================================================

  /**
   * Parse API task response into consistent format
   * @private
   */
  _parseTaskResponse(data) {
    return {
      taskId: data.task_id,
      status: data.status,
      createdAt: new Date(data.created_at),
      updatedAt: new Date(data.updated_at),
      result: data.result || null,
      error: data.error || null,
      proofArtifact: data.proof_artifact ? this._hexToBytes(data.proof_artifact) : null,
      executionNode: data.execution_node || null,
      retryCount: data.retry_count || 0
    };
  }

  /**
   * Convert hex string to Uint8Array
   * @private
   */
  _hexToBytes(hex) {
    const bytes = new Uint8Array(hex.length / 2);
    for (let i = 0; i < bytes.length; i++) {
      bytes[i] = parseInt(hex.substr(i * 2, 2), 16);
    }
    return bytes;
  }

  /**
   * Close client and release resources
   */
  close() {
    if (this.wsManager) {
      this.wsManager.close();
    }
  }
}

// ============================================================================
// Factory Function
// ============================================================================

/**
 * Create a Grey client with environment variable fallbacks
 * 
 * Environment variables (Node.js):
 * - GREY_ENDPOINT: API endpoint URL
 * - GREY_API_KEY: API key for authentication
 * - GREY_TENANT_ID: Tenant identifier
 * 
 * @param {Object} [config] - Override config values
 * @returns {GreyClient} Configured client instance
 */
function createClient(config = {}) {
  const envConfig = {};
  
  // Node.js environment variables
  if (typeof process !== 'undefined' && process.env) {
    envConfig.endpoint = process.env.GREY_ENDPOINT;
    envConfig.apiKey = process.env.GREY_API_KEY;
    envConfig.tenantId = process.env.GREY_TENANT_ID;
  }

  return new GreyClient({
    ...envConfig,
    ...config
  });
}

// ============================================================================
// Exports
// ============================================================================

// CommonJS exports
if (typeof module !== 'undefined' && module.exports) {
  module.exports = {
    GreyClient,
    createClient,
    // Constants
    TaskStatus,
    TaskType,
    Priority,
    // Auth providers
    ApiKeyAuth,
    JwtAuth,
    // Errors
    GreyError,
    GreyConnectionError,
    GreyAuthenticationError,
    GreyAuthorizationError,
    GreyRateLimitError,
    GreyValidationError,
    GreyTaskError
  };
}

// ES Module exports
export {
  GreyClient,
  createClient,
  // Constants
  TaskStatus,
  TaskType,
  Priority,
  // Auth providers
  ApiKeyAuth,
  JwtAuth,
  // Errors
  GreyError,
  GreyConnectionError,
  GreyAuthenticationError,
  GreyAuthorizationError,
  GreyRateLimitError,
  GreyValidationError,
  GreyTaskError
};

export default GreyClient;
