/**
 * Service Manager
 * Handles starting, stopping, and health checking of backend services.
 */
import { spawn, ChildProcess, exec, execSync } from 'child_process'
import { platform } from 'os'
import { EventEmitter } from 'events'
import * as net from 'net'
import * as path from 'path'
import * as fs from 'fs'
import { app } from 'electron'

export interface ServiceStatus {
  name: string
  status: 'running' | 'stopped' | 'error' | 'unknown'
  port: number
  message?: string
  lastChecked: Date
}

export class ServiceManager extends EventEmitter {
  private processes: Map<string, ChildProcess> = new Map()
  private healthCheckInterval: NodeJS.Timeout | null = null
  
  private services = {
    'adapter-core': { port: 8080, command: 'grey-core-api' },
    'http-gateway': { port: 8080, command: 'grey-core-api' },
    'grpc-service': { port: 50051, command: 'grey-core-api' }
  }

  private projectDir: string

  constructor() {
    super()
    // Find the project directory (parent of gui folder)
    this.projectDir = this.findProjectDir()
    console.log('[ServiceManager] Project directory:', this.projectDir)
    this.startHealthChecks()
  }

  /**
   * Find the project root directory containing docker-compose.yml
   * Works for both dev mode and installed app scenarios
   */
  private findProjectDir(): string {
    const searchPaths: string[] = []
    const home = process.env.USERPROFILE || process.env.HOME || ''
    
    // 1. Electron app path and ancestors (most reliable for packaged apps)
    let appPath = app.getAppPath()
    for (let i = 0; i < 10; i++) {
      searchPaths.push(appPath)
      appPath = path.dirname(appPath)
    }
    
    // 2. Try process.cwd() and ancestors
    let dir = process.cwd()
    for (let i = 0; i < 8; i++) {
      searchPaths.push(dir)
      dir = path.dirname(dir)
    }
    
    // 3. Try __dirname and ancestors
    dir = __dirname
    for (let i = 0; i < 10; i++) {
      searchPaths.push(dir)
      dir = path.dirname(dir)
    }
    
    // 4. Try GREY_PROJECT_DIR environment variable if set
    if (process.env.GREY_PROJECT_DIR) {
      searchPaths.push(process.env.GREY_PROJECT_DIR)
    }
    
    // 5. Check for 'release' folder pattern (packaged app)
    // If in release\win-unpacked\..., go up to find gui folder's parent
    const releasePaths = searchPaths.filter(p => p.includes('release'))
    for (const releasePath of releasePaths) {
      const releaseIndex = releasePath.indexOf('release')
      if (releaseIndex > 0) {
        const guiPath = releasePath.substring(0, releaseIndex)
        const projectPath = path.dirname(guiPath.replace(/[\\/]$/, ''))
        if (projectPath) {
          searchPaths.push(projectPath)
        }
      }
    }
    
    // 6. Try common installation/development paths
    searchPaths.push(
      path.join(home, 'OneDrive', 'Desktop', 'Grey Multi-Tenant'),
      path.join(home, 'Desktop', 'Grey Multi-Tenant'),
      path.join(home, 'Grey Multi-Tenant'),
      'C:\\Grey Multi-Tenant',
      'C:\\Users\\Rober\\OneDrive\\Desktop\\Grey Multi-Tenant', // Explicit fallback
      'C:\\Program Files\\GreyMultiTenant',
      '/opt/grey-multitenant',
      '/usr/local/grey-multitenant'
    )
    
    // Find first path with docker-compose.yml
    for (const searchPath of searchPaths) {
      try {
        const composePath = path.join(searchPath, 'docker-compose.yml')
        if (fs.existsSync(composePath)) {
          console.log('[ServiceManager] Found docker-compose.yml at:', searchPath)
          return searchPath
        }
      } catch {
        // Ignore errors
      }
    }
    
    // Last resort fallback - use explicit known path
    const fallback = path.join(home, 'OneDrive', 'Desktop', 'Grey Multi-Tenant')
    console.warn('[ServiceManager] Could not find docker-compose.yml, using fallback path:', fallback)
    return fallback
  }

  /**
   * Check if a port is in use (service is running)
   */
  async checkPort(port: number): Promise<boolean> {
    return new Promise((resolve) => {
      const socket = new net.Socket()
      socket.setTimeout(1000)
      
      const cleanup = () => {
        socket.removeAllListeners()
        socket.destroy()
      }
      
      socket.on('connect', () => {
        cleanup()
        resolve(true)
      })
      
      socket.on('timeout', () => {
        cleanup()
        resolve(false)
      })
      
      socket.on('error', () => {
        cleanup()
        resolve(false)
      })
      
      socket.on('close', () => {
        // Already handled by other events
      })
      
      try {
        socket.connect(port, '127.0.0.1')
      } catch {
        cleanup()
        resolve(false)
      }
    })
  }

  /**
   * Check HTTP health endpoint
   */
  async checkHttpHealth(endpoint: string): Promise<{ healthy: boolean; message?: string }> {
    try {
      const controller = new AbortController()
      const timeout = setTimeout(() => controller.abort(), 5000)
      
      const response = await fetch(`${endpoint}/health`, {
        signal: controller.signal
      })
      
      clearTimeout(timeout)
      
      if (response.ok) {
        const data = await response.json()
        return { healthy: true, message: data.status || 'OK' }
      }
      return { healthy: false, message: `HTTP ${response.status}` }
    } catch (error) {
      return { healthy: false, message: String(error) }
    }
  }

  /**
   * Get status of all services
   */
  async getAllStatus(): Promise<ServiceStatus[]> {
    const statuses: ServiceStatus[] = []
    
    // Check HTTP/Adapter Core (same service on port 8080)
    const httpHealthy = await this.checkPort(8080)
    let httpMessage = 'Not responding'
    let apiHealthy = false
    
    if (httpHealthy) {
      const health = await this.checkHttpHealth('http://localhost:8080')
      httpMessage = health.message || 'OK'
      apiHealthy = health.healthy
    }
    
    statuses.push({
      name: 'Adapter Core',
      status: apiHealthy ? 'running' : 'stopped',
      port: 8080,
      message: httpMessage,
      lastChecked: new Date()
    })
    
    statuses.push({
      name: 'HTTP Gateway',
      status: apiHealthy ? 'running' : 'stopped',
      port: 8080,
      message: httpMessage,
      lastChecked: new Date()
    })
    
    // gRPC service is part of grey-core-api, so it's running if the API is healthy
    // Port 50051 check alone is unreliable as other processes may use it
    statuses.push({
      name: 'gRPC Service',
      status: apiHealthy ? 'running' : 'stopped',
      port: 50051,
      message: apiHealthy ? 'Accepting connections' : 'Not responding',
      lastChecked: new Date()
    })
    
    return statuses
  }

  /**
   * Start health check polling
   */
  startHealthChecks(): void {
    if (this.healthCheckInterval) return
    
    this.healthCheckInterval = setInterval(async () => {
      const statuses = await this.getAllStatus()
      this.emit('status-update', statuses)
    }, 10000) // Check every 10 seconds
  }

  /**
   * Stop health check polling
   */
  stopHealthChecks(): void {
    if (this.healthCheckInterval) {
      clearInterval(this.healthCheckInterval)
      this.healthCheckInterval = null
    }
  }

  /**
   * Start all services using platform-specific commands
   */
  async startAll(): Promise<void> {
    console.log('[ServiceManager] Starting services from:', this.projectDir)
    
    try {
      // Start PostgreSQL container
      console.log('[ServiceManager] Starting PostgreSQL...')
      await this.executeCommand('docker compose up -d postgres', this.projectDir)
      
      // Wait for PostgreSQL to be ready
      await new Promise(resolve => setTimeout(resolve, 3000))
      
      // Check if API is already running on port 8080
      const apiRunning = await this.checkPort(8080)
      if (!apiRunning) {
        console.log('[ServiceManager] API not running, starting...')
        await this.startBackendApi()
        // Wait for the API to start
        await new Promise(resolve => setTimeout(resolve, 2000))
      }
      
      console.log('[ServiceManager] Services started')
      this.emit('services-started')
    } catch (error) {
      console.error('[ServiceManager] Failed to start services:', error)
      this.emit('error', `Failed to start services: ${error}`)
    }
  }

  /**
   * Start the backend API (grey-core-api)
   */
  private async startBackendApi(): Promise<void> {
    const os = platform()
    const exe = os === 'win32' ? '.exe' : ''
    
    // Search for API binary in multiple locations
    const searchPaths = [
      // Installed paths
      path.join('C:', 'Program Files', 'GreyMultiTenant', 'bin', `grey-core-api${exe}`),
      path.join('/usr', 'local', 'bin', `grey-core-api`),
      path.join('/opt', 'grey-multitenant', 'bin', `grey-core-api`),
      // Development paths
      path.join(this.projectDir, 'services', 'grey-core-api', `grey-core-api${exe}`),
      // Parent of gui when in dev mode
      path.join(process.cwd(), '..', 'services', 'grey-core-api', `grey-core-api${exe}`),
      path.join(__dirname, '..', '..', '..', 'services', 'grey-core-api', `grey-core-api${exe}`),
    ]
    
    let apiPath: string | null = null
    for (const p of searchPaths) {
      try {
        const normalizedPath = path.normalize(p)
        console.log('[ServiceManager] Checking for API at:', normalizedPath)
        if (fs.existsSync(normalizedPath)) {
          apiPath = normalizedPath
          break
        }
      } catch {
        // Ignore errors
      }
    }
    
    if (!apiPath) {
      console.error('[ServiceManager] API binary not found in any of:', searchPaths)
      throw new Error('API binary not found. Please run the installer or build the backend first.')
    }
    
    console.log('[ServiceManager] Starting API from:', apiPath)
    const workDir = path.dirname(apiPath)
    
    // Use shell command to start detached process - more reliable than spawn
    if (os === 'win32') {
      // On Windows, use PowerShell Start-Process
      const cmd = `powershell -Command "Start-Process -FilePath '${apiPath.replace(/'/g, "''")}' -WorkingDirectory '${workDir.replace(/'/g, "''")}' -WindowStyle Hidden"`
      console.log('[ServiceManager] Executing:', cmd)
      exec(cmd, (error, stdout, stderr) => {
        if (error) {
          console.error('[ServiceManager] Failed to start API:', stderr || error.message)
        } else {
          console.log('[ServiceManager] API started via PowerShell')
        }
      })
    } else {
      // On Unix, spawn detached
      const proc = spawn(apiPath, [], {
        cwd: workDir,
        detached: true,
        stdio: 'ignore'
      })
      proc.unref()
      console.log('[ServiceManager] API spawned with PID:', proc.pid)
    }
  }

  /**
   * Ensure PostgreSQL and backend API are running (called on app startup)
   */
  async ensurePostgres(): Promise<boolean> {
    console.log('[ServiceManager] Ensuring services are running...')
    try {
      // Start PostgreSQL
      await this.executeCommand('docker compose up -d postgres', this.projectDir)
      // Wait for container to start and become healthy
      await new Promise(resolve => setTimeout(resolve, 3000))
      
      // Check if API is running
      let apiRunning = await this.checkPort(8080)
      if (!apiRunning) {
        console.log('[ServiceManager] Starting backend API...')
        try {
          await this.startBackendApi()
          // Wait longer for API to initialize
          await new Promise(resolve => setTimeout(resolve, 4000))
          
          // Retry check
          apiRunning = await this.checkPort(8080)
          if (!apiRunning) {
            console.log('[ServiceManager] API still not responding, waiting more...')
            await new Promise(resolve => setTimeout(resolve, 3000))
          }
        } catch (err) {
          console.error('[ServiceManager] Could not start API:', err)
        }
      }
      
      // Verify health
      const health = await this.checkHttpHealth('http://localhost:8080')
      console.log('[ServiceManager] Health check:', health)
      return health.healthy
    } catch (error) {
      console.error('[ServiceManager] Error starting services:', error)
      return false
    }
  }

  /**
   * Stop all services
   */
  async stopAll(): Promise<void> {
    try {
      await this.executeCommand('docker compose down')
      
      // Kill any spawned processes
      for (const [name, proc] of this.processes) {
        proc.kill()
        this.processes.delete(name)
      }
      
      this.emit('services-stopped')
    } catch (error) {
      this.emit('error', `Failed to stop services: ${error}`)
    }
  }

  /**
   * Execute a shell command
   * Note: docker compose writes to stderr even on success, so we check exit code
   */
  private executeCommand(command: string, cwd?: string): Promise<string> {
    const workDir = cwd || this.projectDir
    console.log(`[ServiceManager] Executing: ${command} in ${workDir}`)
    
    return new Promise((resolve, reject) => {
      exec(command, { cwd: workDir }, (error, stdout, stderr) => {
        // Docker compose writes progress/warnings to stderr even on success
        // Only treat as error if there's an actual error AND no stdout
        const combinedOutput = stdout + stderr
        
        if (error && !stdout && stderr.includes('error')) {
          console.error(`[ServiceManager] Command error:`, stderr || error.message)
          reject(new Error(stderr || error.message))
        } else {
          console.log(`[ServiceManager] Command output:`, combinedOutput.trim())
          resolve(combinedOutput)
        }
      })
    })
  }

  /**
   * Get platform-specific service path
   */
  getServicePath(): string {
    const os = platform()
    if (os === 'win32') {
      return 'C:\\Program Files\\GreyMultiTenant\\bin\\grey-core-api.exe'
    } else if (os === 'darwin') {
      return '/usr/local/bin/grey-core-api'
    } else {
      return '/usr/local/bin/grey-core-api'
    }
  }
}
