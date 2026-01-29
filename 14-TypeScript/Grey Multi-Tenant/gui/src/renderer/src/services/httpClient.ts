/**
 * HTTP Client - Low-level HTTP API wrapper
 * All HTTP requests to the Grey Core API go through this client.
 */
import type { ApiResponse } from '../types'

export interface HttpRequestOptions {
  method: 'GET' | 'POST' | 'PUT' | 'DELETE' | 'PATCH'
  path: string
  body?: unknown
  headers?: Record<string, string>
  timeout?: number
}

/**
 * HTTP Client class for making API requests
 */
export class HttpClient {
  private baseUrl: string
  private defaultHeaders: Record<string, string>
  private authToken: string | null = null

  constructor(baseUrl: string = 'http://localhost:8080') {
    this.baseUrl = baseUrl
    this.defaultHeaders = {
      'Content-Type': 'application/json'
    }
  }

  /**
   * Set the base URL for API requests
   */
  setBaseUrl(url: string): void {
    this.baseUrl = url
  }

  /**
   * Set the authentication token
   */
  setAuthToken(token: string | null): void {
    this.authToken = token
  }

  /**
   * Get the current auth token
   */
  getAuthToken(): string | null {
    return this.authToken
  }

  /**
   * Make an HTTP request
   */
  async request<T = unknown>(options: HttpRequestOptions): Promise<ApiResponse<T>> {
    const headers: Record<string, string> = {
      ...this.defaultHeaders,
      ...options.headers
    }

    if (this.authToken) {
      headers['Authorization'] = `Bearer ${this.authToken}`
    }

    try {
      const response = await window.electronAPI.api.request({
        method: options.method,
        path: options.path,
        body: options.body,
        headers
      })

      return {
        ok: response.ok,
        status: response.status,
        statusText: response.statusText,
        data: response.data as T,
        error: response.error
      }
    } catch (error) {
      return {
        ok: false,
        status: 0,
        statusText: 'Network Error',
        error: String(error)
      }
    }
  }

  /**
   * GET request
   */
  async get<T = unknown>(path: string, headers?: Record<string, string>): Promise<ApiResponse<T>> {
    return this.request<T>({ method: 'GET', path, headers })
  }

  /**
   * POST request
   */
  async post<T = unknown>(path: string, body?: unknown, headers?: Record<string, string>): Promise<ApiResponse<T>> {
    return this.request<T>({ method: 'POST', path, body, headers })
  }

  /**
   * PUT request
   */
  async put<T = unknown>(path: string, body?: unknown, headers?: Record<string, string>): Promise<ApiResponse<T>> {
    return this.request<T>({ method: 'PUT', path, body, headers })
  }

  /**
   * DELETE request
   */
  async delete<T = unknown>(path: string, headers?: Record<string, string>): Promise<ApiResponse<T>> {
    return this.request<T>({ method: 'DELETE', path, headers })
  }
}

// Singleton instance
export const httpClient = new HttpClient()

/**
 * Initialize HTTP client with config from electron store
 */
export async function initHttpClient(): Promise<void> {
  const endpoint = await window.electronAPI.config.get('httpEndpoint') as string
  if (endpoint) {
    httpClient.setBaseUrl(endpoint)
  }

  const token = await window.electronAPI.auth.getToken()
  if (token) {
    httpClient.setAuthToken(token)
  }
}
