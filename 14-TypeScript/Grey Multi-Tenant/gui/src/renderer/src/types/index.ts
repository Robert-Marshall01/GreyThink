/**
 * Global Type Definitions
 */

export interface ServiceStatus {
  name: string
  status: 'running' | 'stopped' | 'error' | 'unknown'
  port: number
  message?: string
  lastChecked: Date
}

export interface User {
  id: string
  email: string
  name: string
  role: string
  tenantId: string
}

export interface Project {
  id: string
  name: string
  description: string
  tenantId: string
  createdAt: string
  updatedAt: string
}

export interface LogEntry {
  timestamp: Date
  level: 'info' | 'warn' | 'error' | 'debug'
  message: string
}

export interface ApiResponse<T = unknown> {
  ok: boolean
  status: number
  statusText: string
  data?: T
  error?: string
}

export interface AuthResponse {
  ok: boolean
  data?: {
    access_token?: string
    refresh_token?: string
    expires_in?: number
    error?: string
    message?: string
    user?: User
  }
  error?: string
}

export interface Config {
  httpEndpoint: string
  grpcEndpoint: string
  autoStartServices: boolean
  theme: 'light' | 'dark' | 'system'
  windowBounds: { width: number; height: number }
}

// Extend Window interface for electron API
declare global {
  interface Window {
    electronAPI: {
      config: {
        get: (key: string) => Promise<unknown>
        set: (key: string, value: unknown) => Promise<boolean>
        getAll: () => Promise<Config>
      }
      services: {
        getStatus: () => Promise<ServiceStatus[]>
        start: () => Promise<ServiceStatus[]>
        stop: () => Promise<ServiceStatus[]>
        onStatusUpdate: (callback: (statuses: ServiceStatus[]) => void) => () => void
      }
      api: {
        request: <T = unknown>(options: {
          method: string
          path: string
          body?: unknown
          headers?: Record<string, string>
        }) => Promise<ApiResponse<T>>
      }
      auth: {
        login: (credentials: { email: string; password: string }) => Promise<AuthResponse>
        signUp: (data: { email: string; password: string; name: string }) => Promise<AuthResponse>
        logout: () => Promise<{ ok: boolean }>
        refresh: () => Promise<AuthResponse>
        getToken: () => Promise<string | null>
      }
      logs: {
        get: () => Promise<LogEntry[]>
        add: (log: { level: string; message: string }) => Promise<boolean>
        clear: () => Promise<boolean>
      }
      navigation: {
        onNavigate: (callback: (path: string) => void) => () => void
      }
      app: {
        onCheckServices: (callback: () => void) => () => void
      }
    }
  }
}

export {}
