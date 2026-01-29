/**
 * Service Health Checker
 * Monitors the health of all backend services.
 */
import { httpClient } from './httpClient'
import type { ServiceStatus } from '../types'

export interface HealthCheckResult {
  service: string
  status: 'healthy' | 'unhealthy' | 'unknown'
  responseTime: number
  message?: string
  lastChecked: Date
  details?: Record<string, unknown>
}

export interface AllServicesHealth {
  overall: 'healthy' | 'degraded' | 'unhealthy'
  services: HealthCheckResult[]
  timestamp: Date
}

/**
 * Service Health Checker class
 */
export class ServiceHealthChecker {
  private checkInterval: NodeJS.Timeout | null = null
  private listeners: Set<(health: AllServicesHealth) => void> = new Set()

  /**
   * Check HTTP API health
   */
  async checkHttpHealth(): Promise<HealthCheckResult> {
    const start = Date.now()
    try {
      const response = await httpClient.get<{ status: string; time: string }>('/health')
      const responseTime = Date.now() - start

      if (response.ok && response.data?.status === 'healthy') {
        return {
          service: 'HTTP Gateway',
          status: 'healthy',
          responseTime,
          message: 'API responding normally',
          lastChecked: new Date(),
          details: response.data
        }
      }

      return {
        service: 'HTTP Gateway',
        status: 'unhealthy',
        responseTime,
        message: response.error || 'Health check failed',
        lastChecked: new Date()
      }
    } catch (error) {
      return {
        service: 'HTTP Gateway',
        status: 'unhealthy',
        responseTime: Date.now() - start,
        message: String(error),
        lastChecked: new Date()
      }
    }
  }

  /**
   * Check Adapter Core health (same as HTTP for now)
   */
  async checkAdapterCoreHealth(): Promise<HealthCheckResult> {
    const httpResult = await this.checkHttpHealth()
    return {
      ...httpResult,
      service: 'Adapter Core'
    }
  }

  /**
   * Check gRPC service health
   */
  async checkGrpcHealth(): Promise<HealthCheckResult> {
    const start = Date.now()
    try {
      // Check via Electron's service manager
      const statuses = await window.electronAPI.services.getStatus()
      const grpcStatus = statuses.find(s => s.name === 'gRPC Service')
      const responseTime = Date.now() - start

      if (grpcStatus?.status === 'running') {
        return {
          service: 'gRPC Service',
          status: 'healthy',
          responseTime,
          message: grpcStatus.message || 'Service running',
          lastChecked: new Date()
        }
      }

      return {
        service: 'gRPC Service',
        status: 'unhealthy',
        responseTime,
        message: grpcStatus?.message || 'Service not running',
        lastChecked: new Date()
      }
    } catch (error) {
      return {
        service: 'gRPC Service',
        status: 'unknown',
        responseTime: Date.now() - start,
        message: String(error),
        lastChecked: new Date()
      }
    }
  }

  /**
   * Check database connectivity (via API endpoint)
   */
  async checkDatabaseHealth(): Promise<HealthCheckResult> {
    const start = Date.now()
    try {
      const response = await httpClient.get<{ status: string; database?: string }>('/health')
      const responseTime = Date.now() - start

      // If HTTP is healthy, database is implied to be healthy
      if (response.ok) {
        return {
          service: 'Database (PostgreSQL)',
          status: 'healthy',
          responseTime,
          message: 'Database connected',
          lastChecked: new Date()
        }
      }

      return {
        service: 'Database (PostgreSQL)',
        status: 'unhealthy',
        responseTime,
        message: 'Database connection failed',
        lastChecked: new Date()
      }
    } catch (error) {
      return {
        service: 'Database (PostgreSQL)',
        status: 'unknown',
        responseTime: Date.now() - start,
        message: String(error),
        lastChecked: new Date()
      }
    }
  }

  /**
   * Check all services health
   */
  async checkAll(): Promise<AllServicesHealth> {
    const [adapterCore, httpGateway, grpc, database] = await Promise.all([
      this.checkAdapterCoreHealth(),
      this.checkHttpHealth(),
      this.checkGrpcHealth(),
      this.checkDatabaseHealth()
    ])

    const services = [adapterCore, httpGateway, grpc, database]
    
    // Determine overall health
    const unhealthyCount = services.filter(s => s.status === 'unhealthy').length
    const unknownCount = services.filter(s => s.status === 'unknown').length
    
    let overall: 'healthy' | 'degraded' | 'unhealthy'
    if (unhealthyCount === 0 && unknownCount === 0) {
      overall = 'healthy'
    } else if (unhealthyCount >= services.length / 2) {
      overall = 'unhealthy'
    } else {
      overall = 'degraded'
    }

    const result: AllServicesHealth = {
      overall,
      services,
      timestamp: new Date()
    }

    // Notify listeners
    this.listeners.forEach(listener => listener(result))

    return result
  }

  /**
   * Start periodic health checks
   */
  startPeriodicChecks(intervalMs: number = 30000): void {
    if (this.checkInterval) return

    // Initial check
    this.checkAll()

    // Periodic checks
    this.checkInterval = setInterval(() => {
      this.checkAll()
    }, intervalMs)
  }

  /**
   * Stop periodic health checks
   */
  stopPeriodicChecks(): void {
    if (this.checkInterval) {
      clearInterval(this.checkInterval)
      this.checkInterval = null
    }
  }

  /**
   * Subscribe to health check updates
   */
  subscribe(callback: (health: AllServicesHealth) => void): () => void {
    this.listeners.add(callback)
    return () => this.listeners.delete(callback)
  }
}

// Singleton instance
export const serviceHealthChecker = new ServiceHealthChecker()

/**
 * Convert ServiceStatus array to HealthCheckResult array
 */
export function statusesToHealthResults(statuses: ServiceStatus[]): HealthCheckResult[] {
  return statuses.map(status => ({
    service: status.name,
    status: status.status === 'running' ? 'healthy' : 
            status.status === 'error' ? 'unhealthy' : 'unknown',
    responseTime: 0,
    message: status.message,
    lastChecked: new Date(status.lastChecked)
  }))
}
