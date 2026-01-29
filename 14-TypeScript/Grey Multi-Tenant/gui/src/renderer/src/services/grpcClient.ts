/**
 * gRPC Client - Low-level gRPC wrapper
 * All gRPC requests to the Grey Core API go through this client.
 * 
 * Note: gRPC calls are proxied through the Electron main process
 * because gRPC-js requires Node.js runtime.
 */
import type { ApiResponse } from '../types'

export interface GrpcRequestOptions {
  service: string
  method: string
  data?: unknown
  metadata?: Record<string, string>
  timeout?: number
}

export interface GrpcServiceMethods {
  // Auth service methods
  'auth.Login': { email: string; password: string }
  'auth.Logout': { token: string }
  'auth.Refresh': { refreshToken: string }
  
  // User service methods
  'user.GetUser': { id?: string }
  'user.UpdateUser': { id: string; name?: string; email?: string }
  
  // Projects service methods
  'projects.List': { tenantId?: string }
  'projects.Get': { id: string }
  'projects.Create': { name: string; description?: string }
  'projects.Update': { id: string; name?: string; description?: string }
  'projects.Delete': { id: string }
  
  // Query service methods
  'query.Execute': { query: string; variables?: Record<string, unknown> }
  
  // Mutation service methods
  'mutation.Execute': { mutation: string; variables?: Record<string, unknown> }
  
  // Health service methods
  'health.Check': Record<string, never>
}

/**
 * gRPC Client class for making gRPC requests
 */
export class GrpcClient {
  private endpoint: string
  private authToken: string | null = null

  constructor(endpoint: string = 'localhost:50051') {
    this.endpoint = endpoint
  }

  /**
   * Set the gRPC endpoint
   */
  setEndpoint(endpoint: string): void {
    this.endpoint = endpoint
  }

  /**
   * Get the current endpoint
   */
  getEndpoint(): string {
    return this.endpoint
  }

  /**
   * Set the authentication token
   */
  setAuthToken(token: string | null): void {
    this.authToken = token
  }

  /**
   * Make a gRPC request through the main process
   */
  async call<T = unknown>(options: GrpcRequestOptions): Promise<ApiResponse<T>> {
    const metadata = { ...options.metadata }
    
    if (this.authToken) {
      metadata['authorization'] = `Bearer ${this.authToken}`
    }

    try {
      // gRPC calls go through Electron's IPC to main process
      const response = await window.electronAPI.api.request({
        method: 'POST',
        path: `/_grpc/${options.service}/${options.method}`,
        body: {
          data: options.data,
          metadata,
          timeout: options.timeout || 30000
        }
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
        statusText: 'gRPC Error',
        error: String(error)
      }
    }
  }

  /**
   * Auth service calls
   */
  auth = {
    login: async (email: string, password: string) => {
      return this.call({ service: 'auth', method: 'Login', data: { email, password } })
    },
    logout: async (token: string) => {
      return this.call({ service: 'auth', method: 'Logout', data: { token } })
    },
    refresh: async (refreshToken: string) => {
      return this.call({ service: 'auth', method: 'Refresh', data: { refreshToken } })
    }
  }

  /**
   * User service calls
   */
  user = {
    getUser: async (id?: string) => {
      return this.call({ service: 'user', method: 'GetUser', data: { id } })
    },
    updateUser: async (id: string, data: { name?: string; email?: string }) => {
      return this.call({ service: 'user', method: 'UpdateUser', data: { id, ...data } })
    }
  }

  /**
   * Projects service calls
   */
  projects = {
    list: async (tenantId?: string) => {
      return this.call({ service: 'projects', method: 'List', data: { tenantId } })
    },
    get: async (id: string) => {
      return this.call({ service: 'projects', method: 'Get', data: { id } })
    },
    create: async (name: string, description?: string) => {
      return this.call({ service: 'projects', method: 'Create', data: { name, description } })
    },
    update: async (id: string, data: { name?: string; description?: string }) => {
      return this.call({ service: 'projects', method: 'Update', data: { id, ...data } })
    },
    delete: async (id: string) => {
      return this.call({ service: 'projects', method: 'Delete', data: { id } })
    }
  }

  /**
   * Query service calls
   */
  query = {
    execute: async (query: string, variables?: Record<string, unknown>) => {
      return this.call({ service: 'query', method: 'Execute', data: { query, variables } })
    }
  }

  /**
   * Mutation service calls
   */
  mutation = {
    execute: async (mutation: string, variables?: Record<string, unknown>) => {
      return this.call({ service: 'mutation', method: 'Execute', data: { mutation, variables } })
    }
  }

  /**
   * Health service calls
   */
  health = {
    check: async () => {
      return this.call({ service: 'health', method: 'Check', data: {} })
    }
  }
}

// Singleton instance
export const grpcClient = new GrpcClient()

/**
 * Initialize gRPC client with config from electron store
 */
export async function initGrpcClient(): Promise<void> {
  const endpoint = await window.electronAPI.config.get('grpcEndpoint') as string
  if (endpoint) {
    grpcClient.setEndpoint(endpoint)
  }

  const token = await window.electronAPI.auth.getToken()
  if (token) {
    grpcClient.setAuthToken(token)
  }
}
