/**
 * IPC Handlers
 * Handles communication between main and renderer processes.
 */
import { ipcMain } from 'electron'
import Store from 'electron-store'
import { ServiceManager, ServiceStatus } from './serviceManager'

export function setupIpcHandlers(store: Store, serviceManager: ServiceManager): void {
  // Configuration handlers
  ipcMain.handle('config:get', (_event, key: string) => {
    return store.get(key)
  })

  ipcMain.handle('config:set', (_event, key: string, value: unknown) => {
    store.set(key, value)
    return true
  })

  ipcMain.handle('config:getAll', () => {
    return store.store
  })

  // Service status handlers
  ipcMain.handle('services:getStatus', async (): Promise<ServiceStatus[]> => {
    return serviceManager.getAllStatus()
  })

  ipcMain.handle('services:start', async () => {
    await serviceManager.startAll()
    return serviceManager.getAllStatus()
  })

  ipcMain.handle('services:stop', async () => {
    await serviceManager.stopAll()
    return serviceManager.getAllStatus()
  })

  // HTTP API proxy handlers
  ipcMain.handle('api:request', async (_event, options: {
    method: string
    path: string
    body?: unknown
    headers?: Record<string, string>
  }) => {
    const endpoint = store.get('httpEndpoint') as string
    const url = `${endpoint}${options.path}`
    console.log('[api:request]', options.method, options.path)
    
    try {
      const response = await fetch(url, {
        method: options.method,
        headers: {
          'Content-Type': 'application/json',
          ...options.headers
        },
        body: options.body ? JSON.stringify(options.body) : undefined
      })
      
      const data = await response.json().catch(() => null)
      console.log('[api:request] Response:', response.status, response.ok)
      
      return {
        ok: response.ok,
        status: response.status,
        statusText: response.statusText,
        data
      }
    } catch (error) {
      console.error('[api:request] Error:', error)
      return {
        ok: false,
        status: 0,
        statusText: 'Network Error',
        error: String(error)
      }
    }
  })

  // Auth handlers
  ipcMain.handle('auth:login', async (_event, credentials: { email: string; password: string }) => {
    const endpoint = store.get('httpEndpoint') as string
    console.log('[IPC:auth:login] ========== LOGIN ATTEMPT ==========')
    console.log('[IPC:auth:login] Endpoint:', endpoint)
    console.log('[IPC:auth:login] Email:', credentials.email)
    
    try {
      const url = `${endpoint}/api/v1/auth/login`
      console.log('[IPC:auth:login] Fetching:', url)
      
      const response = await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(credentials)
      })
      
      console.log('[IPC:auth:login] Response status:', response.status)
      console.log('[IPC:auth:login] Response ok:', response.ok)
      
      const data = await response.json()
      console.log('[IPC:auth:login] Response data keys:', Object.keys(data))
      console.log('[IPC:auth:login] Has access_token:', !!data.access_token)
      
      if (response.ok && data.access_token) {
        console.log('[IPC:auth:login] SUCCESS! Storing tokens...')
        store.set('authToken', data.access_token)
        store.set('refreshToken', data.refresh_token)
        console.log('[IPC:auth:login] Tokens stored.')
        return { ok: true, data }
      }
      
      // Extract error message for failed login
      const errorMessage = data.message || data.error || 'Login failed'
      console.log('[IPC:auth:login] FAILED:', errorMessage)
      console.log('[IPC:auth:login] ========== END LOGIN ==========')
      return { ok: false, error: errorMessage, data }
    } catch (error) {
      console.error('[IPC:auth:login] EXCEPTION:', error)
      console.log('[IPC:auth:login] ========== END LOGIN (ERROR) ==========')
      return { ok: false, error: String(error) }
    }
  })

  ipcMain.handle('auth:signUp', async (_event, data: { email: string; password: string; name: string }) => {
    const endpoint = store.get('httpEndpoint') as string
    console.log('[signUp] Endpoint:', endpoint)
    console.log('[signUp] Data:', JSON.stringify(data))
    
    try {
      const response = await fetch(`${endpoint}/api/v1/auth/register`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data)
      })
      
      console.log('[signUp] Response status:', response.status, response.ok)
      const result = await response.json()
      console.log('[signUp] Response body:', JSON.stringify(result))
      
      if (response.ok && result.access_token) {
        console.log('[signUp] Success! Storing tokens')
        store.set('authToken', result.access_token)
        store.set('refreshToken', result.refresh_token)
        return { ok: true, data: result }
      }
      
      // Extract error message from backend response for any non-ok response
      const errorMessage = result.message || result.error || 'Registration failed'
      console.log('[signUp] Error from backend:', errorMessage)
      return { ok: false, error: errorMessage, data: result }
    } catch (error) {
      console.error('[signUp] Exception:', error)
      return { ok: false, error: String(error) }
    }
  })

  ipcMain.handle('auth:logout', async () => {
    const endpoint = store.get('httpEndpoint') as string
    const token = store.get('authToken') as string
    
    try {
      await fetch(`${endpoint}/api/v1/auth/logout`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${token}`
        }
      })
    } catch {
      // Ignore logout errors
    }
    
    store.delete('authToken')
    store.delete('refreshToken')
    return { ok: true }
  })

  ipcMain.handle('auth:refresh', async () => {
    const endpoint = store.get('httpEndpoint') as string
    const refreshToken = store.get('refreshToken') as string
    
    if (!refreshToken) {
      return { ok: false, error: 'No refresh token' }
    }
    
    try {
      const response = await fetch(`${endpoint}/api/v1/auth/refresh`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ refreshToken })
      })
      
      const data = await response.json()
      
      if (response.ok && data.token) {
        store.set('authToken', data.token)
        if (data.refreshToken) {
          store.set('refreshToken', data.refreshToken)
        }
      }
      
      return { ok: response.ok, data }
    } catch (error) {
      return { ok: false, error: String(error) }
    }
  })

  ipcMain.handle('auth:getToken', () => {
    const token = store.get('authToken') || null
    console.log('[getToken] Token exists:', !!token)
    return token
  })

  // Logging handlers
  const logs: Array<{ timestamp: Date; level: string; message: string }> = []
  
  ipcMain.handle('logs:get', () => {
    return logs.slice(-100) // Return last 100 logs
  })

  ipcMain.handle('logs:add', (_event, log: { level: string; message: string }) => {
    logs.push({ ...log, timestamp: new Date() })
    if (logs.length > 1000) {
      logs.shift() // Remove oldest
    }
    return true
  })

  ipcMain.handle('logs:clear', () => {
    logs.length = 0
    return true
  })

  // Forward service status updates to renderer
  serviceManager.on('status-update', (statuses: ServiceStatus[]) => {
    const { BrowserWindow } = require('electron')
    BrowserWindow.getAllWindows().forEach(window => {
      window.webContents.send('services:status-update', statuses)
    })
  })

  serviceManager.on('error', (error: string) => {
    logs.push({ timestamp: new Date(), level: 'error', message: error })
  })
}
