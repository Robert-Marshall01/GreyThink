/**
 * API Client - HTTP API Wrapper
 * Wraps all HTTP API calls to the Grey Core API.
 */
import type { User, Project, ApiResponse } from '../types'

const api = window.electronAPI.api

/**
 * Auth API
 */
export const authApi = {
  /**
   * Login with email and password
   */
  async login(email: string, password: string) {
    console.log('[api.login] calling IPC')
    const result = await window.electronAPI.auth.login({ email, password })
    console.log('[api.login] IPC result:', JSON.stringify(result))
    return result
  },

  /**
   * Sign up / register new user
   */
  async signUp(email: string, password: string, name: string) {
    return window.electronAPI.auth.signUp({ email, password, name })
  },

  /**
   * Logout current user
   */
  async logout() {
    return window.electronAPI.auth.logout()
  },

  /**
   * Refresh authentication token
   */
  async refresh() {
    return window.electronAPI.auth.refresh()
  },

  /**
   * Get current auth token
   */
  async getToken() {
    return window.electronAPI.auth.getToken()
  }
}

/**
 * User API
 */
export const userApi = {
  /**
   * Get current user information
   */
  async getUser(): Promise<ApiResponse<User>> {
    const token = await authApi.getToken()
    return api.request<User>({
      method: 'GET',
      path: '/api/v1/user',
      headers: token ? { Authorization: `Bearer ${token}` } : undefined
    })
  }
}

/**
 * Projects API
 */
export const projectsApi = {
  /**
   * List all projects for current tenant
   */
  async listProjects(): Promise<ApiResponse<{ projects: Project[] }>> {
    const token = await authApi.getToken()
    return api.request<{ projects: Project[] }>({
      method: 'GET',
      path: '/api/v1/projects',
      headers: token ? { Authorization: `Bearer ${token}` } : undefined
    })
  },

  /**
   * Create a new project
   */
  async createProject(data: { name: string; description?: string }): Promise<ApiResponse<Project>> {
    const token = await authApi.getToken()
    return api.request<Project>({
      method: 'POST',
      path: '/api/v1/projects',
      body: data,
      headers: token ? { Authorization: `Bearer ${token}` } : undefined
    })
  },

  /**
   * Get a single project by ID
   */
  async getProject(id: string): Promise<ApiResponse<Project>> {
    const token = await authApi.getToken()
    return api.request<Project>({
      method: 'GET',
      path: `/api/v1/projects/${id}`,
      headers: token ? { Authorization: `Bearer ${token}` } : undefined
    })
  },

  /**
   * Update a project
   */
  async updateProject(id: string, data: { name?: string; description?: string }): Promise<ApiResponse<Project>> {
    const token = await authApi.getToken()
    return api.request<Project>({
      method: 'PUT',
      path: `/api/v1/projects/${id}`,
      body: data,
      headers: token ? { Authorization: `Bearer ${token}` } : undefined
    })
  },

  /**
   * Delete a project
   */
  async deleteProject(id: string): Promise<ApiResponse<void>> {
    const token = await authApi.getToken()
    return api.request<void>({
      method: 'DELETE',
      path: `/api/v1/projects/${id}`,
      headers: token ? { Authorization: `Bearer ${token}` } : undefined
    })
  }
}

/**
 * Query API
 */
export const queryApi = {
  /**
   * Execute a query against the adapter core
   */
  async query(queryString: string, variables?: Record<string, unknown>): Promise<ApiResponse<unknown>> {
    const token = await authApi.getToken()
    return api.request({
      method: 'POST',
      path: '/api/v1/query',
      body: { query: queryString, variables },
      headers: token ? { Authorization: `Bearer ${token}` } : undefined
    })
  }
}

/**
 * Mutation API
 */
export const mutationApi = {
  /**
   * Execute a mutation against the adapter core
   */
  async mutate(mutation: string, variables?: Record<string, unknown>): Promise<ApiResponse<unknown>> {
    const token = await authApi.getToken()
    return api.request({
      method: 'POST',
      path: '/api/v1/mutate',
      body: { mutation, variables },
      headers: token ? { Authorization: `Bearer ${token}` } : undefined
    })
  }
}

/**
 * Health API
 */
export const healthApi = {
  /**
   * Check API health
   */
  async check(): Promise<ApiResponse<{ status: string; time: string }>> {
    return api.request<{ status: string; time: string }>({
      method: 'GET',
      path: '/health'
    })
  }
}
