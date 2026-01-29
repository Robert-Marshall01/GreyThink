/**
 * Preload Script
 * Exposes a secure API to the renderer process via contextBridge.
 */
import { contextBridge, ipcRenderer } from 'electron'

// Type definitions for the exposed API
export interface ElectronAPI {
  config: {
    get: (key: string) => Promise<unknown>
    set: (key: string, value: unknown) => Promise<boolean>
    getAll: () => Promise<Record<string, unknown>>
  }
  services: {
    getStatus: () => Promise<ServiceStatus[]>
    start: () => Promise<ServiceStatus[]>
    stop: () => Promise<ServiceStatus[]>
    onStatusUpdate: (callback: (statuses: ServiceStatus[]) => void) => () => void
  }
  api: {
    request: (options: ApiRequestOptions) => Promise<ApiResponse>
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

interface ServiceStatus {
  name: string
  status: 'running' | 'stopped' | 'error' | 'unknown'
  port: number
  message?: string
  lastChecked: Date
}

interface ApiRequestOptions {
  method: string
  path: string
  body?: unknown
  headers?: Record<string, string>
}

interface ApiResponse {
  ok: boolean
  status: number
  statusText: string
  data?: unknown
  error?: string
}

interface AuthResponse {
  ok: boolean
  data?: {
    token?: string
    refreshToken?: string
    user?: unknown
  }
  error?: string
}

interface LogEntry {
  timestamp: Date
  level: string
  message: string
}

// Expose the API to the renderer
contextBridge.exposeInMainWorld('electronAPI', {
  config: {
    get: (key: string) => ipcRenderer.invoke('config:get', key),
    set: (key: string, value: unknown) => ipcRenderer.invoke('config:set', key, value),
    getAll: () => ipcRenderer.invoke('config:getAll')
  },
  
  services: {
    getStatus: () => ipcRenderer.invoke('services:getStatus'),
    start: () => ipcRenderer.invoke('services:start'),
    stop: () => ipcRenderer.invoke('services:stop'),
    onStatusUpdate: (callback: (statuses: ServiceStatus[]) => void) => {
      const handler = (_event: Electron.IpcRendererEvent, statuses: ServiceStatus[]) => callback(statuses)
      ipcRenderer.on('services:status-update', handler)
      return () => ipcRenderer.removeListener('services:status-update', handler)
    }
  },
  
  api: {
    request: (options: ApiRequestOptions) => ipcRenderer.invoke('api:request', options)
  },
  
  auth: {
    login: (credentials: { email: string; password: string }) => ipcRenderer.invoke('auth:login', credentials),
    signUp: (data: { email: string; password: string; name: string }) => ipcRenderer.invoke('auth:signUp', data),
    logout: () => ipcRenderer.invoke('auth:logout'),
    refresh: () => ipcRenderer.invoke('auth:refresh'),
    getToken: () => ipcRenderer.invoke('auth:getToken')
  },
  
  logs: {
    get: () => ipcRenderer.invoke('logs:get'),
    add: (log: { level: string; message: string }) => ipcRenderer.invoke('logs:add', log),
    clear: () => ipcRenderer.invoke('logs:clear')
  },
  
  navigation: {
    onNavigate: (callback: (path: string) => void) => {
      const handler = (_event: Electron.IpcRendererEvent, path: string) => callback(path)
      ipcRenderer.on('navigate', handler)
      return () => ipcRenderer.removeListener('navigate', handler)
    }
  },
  
  app: {
    onCheckServices: (callback: () => void) => {
      const handler = () => callback()
      ipcRenderer.on('check-services', handler)
      return () => ipcRenderer.removeListener('check-services', handler)
    }
  }
} as ElectronAPI)
