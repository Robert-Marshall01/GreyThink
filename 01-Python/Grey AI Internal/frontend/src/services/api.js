/**
 * API Service
 * ===========
 * Centralized API client for Grey AI Internal backend.
 * 
 * Complete Flow:
 * 1. uploadFile() → POST /api/upload/ → Returns upload with metrics
 * 2. triggerAnalysis() → POST /api/ai/analyze/{id} → Returns AI insights
 * 3. fetchInsights() → GET /api/ai/insights/{id} → Returns cached insights
 * 
 * All endpoints return JSON with consistent error handling.
 */

const API_BASE = '/api'

// ---------------------------------------------------------------------------
// Error Handling
// ---------------------------------------------------------------------------

/**
 * Custom error class for API errors.
 * Includes HTTP status and response data for debugging.
 */
class APIError extends Error {
  constructor(message, status, data = null) {
    super(message)
    this.name = 'APIError'
    this.status = status
    this.data = data
  }
}

/**
 * Handle API response - extract data or throw error.
 * Parses JSON for success/error responses.
 */
async function handleResponse(response) {
  const contentType = response.headers.get('content-type')
  const isJson = contentType && contentType.includes('application/json')
  const data = isJson ? await response.json() : await response.text()
  
  if (!response.ok) {
    const message = typeof data === 'object' ? data.detail || 'Request failed' : data
    throw new APIError(message, response.status, data)
  }
  
  return data
}

// ---------------------------------------------------------------------------
// Upload Endpoints (/api/upload)
// ---------------------------------------------------------------------------

/**
 * Upload a file for analysis.
 * 
 * Flow:
 * 1. Sends file to backend via POST
 * 2. Backend saves file and parses metrics
 * 3. Returns upload info with basic metrics
 * 
 * POST /api/upload/
 * 
 * @param {File} file - The file to upload (.csv or .txt)
 * @returns {Promise<Object>} Upload response with id, filename, row_count, etc.
 * 
 * @example
 * const result = await uploadFile(file)
 * console.log(result.id, result.row_count)
 */
export async function uploadFile(file) {
  const formData = new FormData()
  formData.append('file', file)
  
  const response = await fetch(`${API_BASE}/upload/`, {
    method: 'POST',
    body: formData,
  })
  
  return handleResponse(response)
}

/**
 * Fetch paginated list of uploads.
 * 
 * GET /api/upload/
 * 
 * @param {Object} options - Query parameters
 * @param {number} options.page - Page number (default: 1)
 * @param {number} options.pageSize - Items per page (default: 20)
 * @param {string} options.status - Filter by status (optional)
 * @returns {Promise<Object>} Paginated upload history
 */
export async function fetchUploads({ page = 1, pageSize = 20, status = null } = {}) {
  let url = `${API_BASE}/upload/?page=${page}&page_size=${pageSize}`
  if (status) {
    url += `&status_filter=${status}`
  }
  const response = await fetch(url)
  return handleResponse(response)
}

/**
 * Get detailed upload info including metrics.
 * 
 * GET /api/upload/{id}
 * 
 * @param {number|string} id - Upload ID
 * @returns {Promise<Object>} Upload details with metrics
 */
export async function getUploadDetails(id) {
  const response = await fetch(`${API_BASE}/upload/${id}`)
  return handleResponse(response)
}

/**
 * Fetch metrics for an upload.
 * Extracts metrics from upload details.
 * 
 * @param {number|string} id - Upload ID
 * @returns {Promise<Object>} Metrics object
 */
export async function fetchMetrics(id) {
  const details = await getUploadDetails(id)
  return details.metrics || {
    row_count: details.row_count,
    column_count: details.column_count,
  }
}

/**
 * Delete an upload and its associated data.
 * 
 * DELETE /api/upload/{id}
 * 
 * @param {number|string} id - Upload ID
 * @returns {Promise<void>}
 */
export async function deleteUpload(id) {
  const response = await fetch(`${API_BASE}/upload/${id}`, {
    method: 'DELETE',
  })
  
  if (!response.ok && response.status !== 204) {
    const data = await response.json().catch(() => ({}))
    throw new APIError(data.detail || 'Delete failed', response.status)
  }
}

// ---------------------------------------------------------------------------
// AI Endpoints (/api/ai)
// ---------------------------------------------------------------------------

/**
 * Trigger AI analysis on an upload.
 * 
 * Complete Flow:
 * 1. Fetches upload metrics from database
 * 2. Calls Ollama with data summary
 * 3. Parses and stores AI insights
 * 4. Returns structured insights
 * 
 * POST /api/ai/analyze/{id}
 * 
 * @param {number|string} id - Upload ID
 * @param {string|null} customPrompt - Optional custom analysis instructions
 * @returns {Promise<Object>} Insights response with summary, insights, recommendations
 * 
 * @example
 * const insights = await triggerAnalysis(uploadId, "Focus on sales trends")
 * console.log(insights.summary, insights.insights)
 */
export async function triggerAnalysis(id, customPrompt = null) {
  const body = customPrompt 
    ? JSON.stringify({ custom_prompt: customPrompt }) 
    : JSON.stringify({})  // Send empty object for proper JSON
  
  const response = await fetch(`${API_BASE}/ai/analyze/${id}`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body,
  })
  
  return handleResponse(response)
}

// Legacy alias for backwards compatibility
export const analyzeUpload = triggerAnalysis

/**
 * Fetch cached insights for an upload.
 * Returns null if no insights exist (instead of throwing).
 * 
 * GET /api/ai/insights/{id}
 * 
 * @param {number|string} id - Upload ID
 * @returns {Promise<Object|null>} Insights or null if not found
 */
export async function fetchInsights(id) {
  try {
    const response = await fetch(`${API_BASE}/ai/insights/${id}`)
    if (response.status === 404) {
      return null
    }
    return handleResponse(response)
  } catch (err) {
    if (err instanceof APIError && err.status === 404) {
      return null
    }
    throw err
  }
}

// Legacy alias
export const getInsights = fetchInsights

/**
 * Check AI service (Ollama) status.
 * 
 * GET /api/ai/status
 * 
 * @returns {Promise<Object>} Status with available flag and models list
 */
export async function checkAIStatus() {
  const response = await fetch(`${API_BASE}/ai/status`)
  return handleResponse(response)
}

/**
 * Raw AI inference endpoint for custom prompts.
 * 
 * POST /api/ai/infer
 * 
 * @param {string} prompt - User prompt
 * @param {string|null} systemPrompt - Optional system instructions
 * @returns {Promise<Object>} AI response
 */
export async function rawInference(prompt, systemPrompt = null) {
  const body = {
    prompt,
    ...(systemPrompt && { system_prompt: systemPrompt }),
  }
  
  const response = await fetch(`${API_BASE}/ai/infer`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(body),
  })
  
  return handleResponse(response)
}

// ---------------------------------------------------------------------------
// Health Check
// ---------------------------------------------------------------------------

/**
 * Backend health check.
 * 
 * GET /health
 * 
 * @returns {Promise<boolean>} True if backend is healthy
 */
export async function checkHealth() {
  try {
    const response = await fetch('/health')
    return response.ok
  } catch {
    return false
  }
}

// ---------------------------------------------------------------------------
// Image Generation Endpoints (/api/images)
// ---------------------------------------------------------------------------

/**
 * Check Stable Diffusion service status.
 * 
 * GET /api/images/status
 * 
 * @returns {Promise<Object>} Status with available flag, models, samplers
 */
export async function checkSDStatus() {
  const response = await fetch(`${API_BASE}/images/status`)
  return handleResponse(response)
}

/**
 * Get available image styles.
 * 
 * GET /api/images/styles
 * 
 * @returns {Promise<Array>} List of style presets
 */
export async function getImageStyles() {
  const response = await fetch(`${API_BASE}/images/styles`)
  return handleResponse(response)
}

/**
 * Get quality presets.
 * 
 * GET /api/images/presets
 * 
 * @returns {Promise<Array>} List of quality presets
 */
export async function getQualityPresets() {
  const response = await fetch(`${API_BASE}/images/presets`)
  return handleResponse(response)
}

/**
 * Generate image from text prompt (text-to-image).
 * 
 * POST /api/images/generate
 * 
 * @param {Object} params - Generation parameters
 * @param {string} params.prompt - Text description
 * @param {string} params.negative_prompt - What to avoid
 * @param {string} params.style - Style preset
 * @param {string} params.quality - Quality preset
 * @param {number} params.width - Image width
 * @param {number} params.height - Image height
 * @param {number} params.seed - Random seed (-1 for random)
 * @returns {Promise<Object>} Generated image with metadata
 */
export async function generateImage({
  prompt,
  negative_prompt = null,
  style = 'photorealistic',
  quality = 'balanced',
  width = 1024,
  height = 1024,
  seed = -1,
  sampler = 'DPM++ 2M Karras',
}) {
  const response = await fetch(`${API_BASE}/images/generate`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      prompt,
      negative_prompt,
      style,
      quality,
      width,
      height,
      seed,
      sampler,
    }),
  })
  return handleResponse(response)
}

/**
 * Transform an existing image (image-to-image).
 * 
 * POST /api/images/transform
 * 
 * @param {Object} params - Transformation parameters
 * @param {string} params.image_base64 - Base64 encoded source image
 * @param {string} params.prompt - Transformation description
 * @param {string} params.style - Style preset
 * @param {number} params.denoising_strength - How much to change (0.1-1.0)
 * @returns {Promise<Object>} Transformed image with metadata
 */
export async function transformImage({
  image_base64,
  prompt,
  negative_prompt = null,
  style = 'photorealistic',
  quality = 'balanced',
  denoising_strength = 0.75,
  width = null,
  height = null,
  seed = -1,
}) {
  const response = await fetch(`${API_BASE}/images/transform`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      image_base64,
      prompt,
      negative_prompt,
      style,
      quality,
      denoising_strength,
      width,
      height,
      seed,
    }),
  })
  return handleResponse(response)
}

// ---------------------------------------------------------------------------
// Utility Functions
// ---------------------------------------------------------------------------

/**
 * Format file size for display.
 * 
 * @param {number} bytes - File size in bytes
 * @returns {string} Formatted size string
 */
export function formatFileSize(bytes) {
  if (!bytes || bytes === 0) return '0 B'
  if (bytes < 1024) return bytes + ' B'
  if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB'
  return (bytes / (1024 * 1024)).toFixed(2) + ' MB'
}

/**
 * Format date for display.
 * 
 * @param {string|Date} date - Date to format
 * @returns {string} Formatted date string
 */
export function formatDate(date) {
  if (!date) return ''
  const d = new Date(date)
  return d.toLocaleDateString('en-US', {
    month: 'short',
    day: 'numeric',
    year: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  })
}

// Export error class and utilities
export { APIError }
