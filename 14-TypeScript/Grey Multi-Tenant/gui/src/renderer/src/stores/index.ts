/**
 * Application Stores - Zustand State Management
 */
import { create } from 'zustand'
import type { User, Project, ServiceStatus, LogEntry, Config } from '../types'
import { authApi, userApi, projectsApi } from '../services/api'

/**
 * Auth Store
 */
interface AuthState {
  user: User | null
  isAuthenticated: boolean
  isLoading: boolean
  error: string | null
  login: (email: string, password: string) => Promise<boolean>
  signUp: (email: string, password: string, name: string) => Promise<boolean>
  logout: () => Promise<void>
  checkAuth: () => Promise<void>
}

export const useAuthStore = create<AuthState>((set) => ({
  user: null,
  isAuthenticated: false,
  isLoading: true,
  error: null,

  login: async (email: string, password: string) => {
    console.log('[Store] ========= LOGIN START =========')
    console.log('[Store] login called with:', email)
    set({ isLoading: true, error: null })
    try {
      console.log('[Store] Calling authApi.login...')
      const result = await authApi.login(email, password)
      console.log('[Store] authApi.login returned:', result)
      console.log('[Store] result.ok =', result.ok)
      console.log('[Store] typeof result.ok =', typeof result.ok)
      // Simplified check - just check if ok is true
      if (result.ok) {
        console.log('[Store] login success! Setting isAuthenticated=true')
        set({ isAuthenticated: true, isLoading: false, error: null })
        console.log('[Store] State updated, returning true')
        return true
      } else {
        const errMsg = result.data?.message || result.error || 'Login failed'
        console.log('[Store] login failed:', errMsg)
        set({ error: errMsg, isLoading: false })
        return false
      }
    } catch (error) {
      console.error('[Store] login exception:', error)
      set({ error: String(error), isLoading: false })
      return false
    }
  },

  signUp: async (email: string, password: string, name: string) => {
    set({ isLoading: true, error: null })
    try {
      const result = await authApi.signUp(email, password, name)
      if (result.ok && result.data?.access_token) {
        // Registration succeeded - user is now authenticated
        set({ isAuthenticated: true, isLoading: false })
        return true
      } else {
        const errMsg = result.data?.message || result.error || 'Registration failed'
        set({ error: errMsg, isLoading: false })
        return false
      }
    } catch (error) {
      set({ error: String(error), isLoading: false })
      return false
    }
  },

  logout: async () => {
    await authApi.logout()
    set({ user: null, isAuthenticated: false })
  },

  checkAuth: async () => {
    set({ isLoading: true })
    try {
      const token = await authApi.getToken()
      if (!token) {
        set({ isAuthenticated: false, isLoading: false })
        return
      }
      // If we have a token, consider the user authenticated
      // Try to fetch user info but don't fail if endpoint doesn't exist
      try {
        const result = await userApi.getUser()
        if (result.ok && result.data) {
          set({ user: result.data, isAuthenticated: true, isLoading: false })
          return
        }
      } catch {
        // User endpoint may not exist - that's OK
      }
      // Token exists, mark as authenticated even without user data
      set({ isAuthenticated: true, isLoading: false })
    } catch {
      set({ isAuthenticated: false, isLoading: false })
    }
  }
}))

/**
 * Services Store
 */
interface ServicesState {
  statuses: ServiceStatus[]
  isChecking: boolean
  lastChecked: Date | null
  checkStatus: () => Promise<void>
  startServices: () => Promise<void>
  stopServices: () => Promise<void>
  setStatuses: (statuses: ServiceStatus[]) => void
}

export const useServicesStore = create<ServicesState>((set) => ({
  statuses: [],
  isChecking: false,
  lastChecked: null,

  checkStatus: async () => {
    set({ isChecking: true })
    try {
      const statuses = await window.electronAPI.services.getStatus()
      set({ statuses, lastChecked: new Date(), isChecking: false })
    } catch {
      set({ isChecking: false })
    }
  },

  startServices: async () => {
    set({ isChecking: true })
    try {
      const statuses = await window.electronAPI.services.start()
      set({ statuses, lastChecked: new Date(), isChecking: false })
    } catch {
      set({ isChecking: false })
    }
  },

  stopServices: async () => {
    set({ isChecking: true })
    try {
      const statuses = await window.electronAPI.services.stop()
      set({ statuses, lastChecked: new Date(), isChecking: false })
    } catch {
      set({ isChecking: false })
    }
  },

  setStatuses: (statuses: ServiceStatus[]) => {
    set({ statuses, lastChecked: new Date() })
  }
}))

/**
 * Projects Store
 */
interface ProjectsState {
  projects: Project[]
  isLoading: boolean
  error: string | null
  fetchProjects: () => Promise<void>
  createProject: (name: string, description?: string) => Promise<Project | null>
}

export const useProjectsStore = create<ProjectsState>((set, get) => ({
  projects: [],
  isLoading: false,
  error: null,

  fetchProjects: async () => {
    set({ isLoading: true, error: null })
    try {
      const result = await projectsApi.listProjects()
      if (result.ok && result.data) {
        set({ projects: result.data.projects, isLoading: false })
      } else {
        set({ error: result.error || 'Failed to fetch projects', isLoading: false })
      }
    } catch (error) {
      set({ error: String(error), isLoading: false })
    }
  },

  createProject: async (name: string, description?: string) => {
    try {
      const result = await projectsApi.createProject({ name, description })
      if (result.ok && result.data) {
        set({ projects: [...get().projects, result.data] })
        return result.data
      }
      return null
    } catch {
      return null
    }
  }
}))

/**
 * Logs Store
 */
interface LogsState {
  logs: LogEntry[]
  fetchLogs: () => Promise<void>
  addLog: (level: string, message: string) => Promise<void>
  clearLogs: () => Promise<void>
}

export const useLogsStore = create<LogsState>((set, get) => ({
  logs: [],

  fetchLogs: async () => {
    const logs = await window.electronAPI.logs.get()
    set({ logs })
  },

  addLog: async (level: string, message: string) => {
    await window.electronAPI.logs.add({ level, message })
    const logs = [...get().logs, { timestamp: new Date(), level: level as LogEntry['level'], message }]
    set({ logs: logs.slice(-100) }) // Keep last 100
  },

  clearLogs: async () => {
    await window.electronAPI.logs.clear()
    set({ logs: [] })
  }
}))

/**
 * Config Store
 */
interface ConfigState {
  config: Config | null
  isLoading: boolean
  fetchConfig: () => Promise<void>
  updateConfig: <K extends keyof Config>(key: K, value: Config[K]) => Promise<void>
}

export const useConfigStore = create<ConfigState>((set, get) => ({
  config: null,
  isLoading: true,

  fetchConfig: async () => {
    set({ isLoading: true })
    const config = await window.electronAPI.config.getAll()
    set({ config, isLoading: false })
  },

  updateConfig: async <K extends keyof Config>(key: K, value: Config[K]) => {
    await window.electronAPI.config.set(key, value)
    const current = get().config
    if (current) {
      set({ config: { ...current, [key]: value } })
    }
  }
}))
