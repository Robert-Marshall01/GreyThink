/**
 * Service Health Screen
 * Displays health status of all backend services.
 */
import React, { useEffect, useState, useCallback } from 'react'
import { useServicesStore, useLogsStore } from '../../stores'
import { serviceHealthChecker, type AllServicesHealth, type HealthCheckResult } from '../../services/serviceHealth'
import './ServiceHealthScreen.css'

/**
 * Status indicator component
 */
function StatusBadge({ status }: { status: 'healthy' | 'unhealthy' | 'unknown' | 'running' | 'stopped' | 'error' }) {
  const statusClass = {
    healthy: 'status-healthy',
    running: 'status-healthy',
    unhealthy: 'status-unhealthy',
    error: 'status-unhealthy',
    stopped: 'status-stopped',
    unknown: 'status-unknown'
  }[status] || 'status-unknown'

  const statusText = {
    healthy: 'Healthy',
    running: 'Running',
    unhealthy: 'Unhealthy',
    error: 'Error',
    stopped: 'Stopped',
    unknown: 'Unknown'
  }[status] || 'Unknown'

  return <span className={`status-badge ${statusClass}`}>{statusText}</span>
}

/**
 * Service card component
 */
function ServiceCard({ result }: { result: HealthCheckResult }) {
  return (
    <div className={`service-card service-${result.status}`}>
      <div className="service-header">
        <h3>{result.service}</h3>
        <StatusBadge status={result.status} />
      </div>
      <div className="service-details">
        <p className="service-message">{result.message || 'No message'}</p>
        {result.responseTime > 0 && (
          <p className="service-response-time">Response time: {result.responseTime}ms</p>
        )}
        <p className="service-last-checked">
          Last checked: {new Date(result.lastChecked).toLocaleTimeString()}
        </p>
      </div>
    </div>
  )
}

/**
 * Overall status banner
 */
function OverallStatusBanner({ health }: { health: AllServicesHealth | null }) {
  if (!health) {
    return (
      <div className="overall-status overall-checking">
        <div className="status-icon">⏳</div>
        <div className="status-text">
          <h2>Checking Services...</h2>
          <p>Please wait while we check the status of all services.</p>
        </div>
      </div>
    )
  }

  const statusConfig = {
    healthy: {
      icon: '✅',
      title: 'All Systems Operational',
      description: 'All services are running normally.'
    },
    degraded: {
      icon: '⚠️',
      title: 'Degraded Performance',
      description: 'Some services may not be functioning properly.'
    },
    unhealthy: {
      icon: '❌',
      title: 'Systems Unavailable',
      description: 'Critical services are not responding.'
    }
  }

  const config = statusConfig[health.overall]

  return (
    <div className={`overall-status overall-${health.overall}`}>
      <div className="status-icon">{config.icon}</div>
      <div className="status-text">
        <h2>{config.title}</h2>
        <p>{config.description}</p>
        <p className="timestamp">
          Last updated: {health.timestamp.toLocaleString()}
        </p>
      </div>
    </div>
  )
}

/**
 * Service Health Screen component
 */
export function ServiceHealthScreen() {
  const { statuses, isChecking, startServices, stopServices, checkStatus } = useServicesStore()
  const { addLog } = useLogsStore()
  const [health, setHealth] = useState<AllServicesHealth | null>(null)
  const [isStarting, setIsStarting] = useState(false)
  const [isStopping, setIsStopping] = useState(false)

  // Load health status
  const loadHealth = useCallback(async () => {
    try {
      const result = await serviceHealthChecker.checkAll()
      setHealth(result)
    } catch (error) {
      await addLog('error', `Health check failed: ${error}`)
    }
  }, [addLog])

  // Initial load and periodic refresh
  useEffect(() => {
    loadHealth()
    checkStatus()

    // Subscribe to health updates
    const unsubscribe = serviceHealthChecker.subscribe(setHealth)
    serviceHealthChecker.startPeriodicChecks(30000)

    return () => {
      unsubscribe()
      serviceHealthChecker.stopPeriodicChecks()
    }
  }, [loadHealth, checkStatus])

  // Handle start services
  const handleStartServices = async () => {
    setIsStarting(true)
    await addLog('info', 'Starting all services...')
    try {
      await startServices()
      await addLog('info', 'Services started successfully')
      await loadHealth()
    } catch (error) {
      await addLog('error', `Failed to start services: ${error}`)
    } finally {
      setIsStarting(false)
    }
  }

  // Handle stop services
  const handleStopServices = async () => {
    setIsStopping(true)
    await addLog('info', 'Stopping all services...')
    try {
      await stopServices()
      await addLog('info', 'Services stopped')
      await loadHealth()
    } catch (error) {
      await addLog('error', `Failed to stop services: ${error}`)
    } finally {
      setIsStopping(false)
    }
  }

  // Handle refresh
  const handleRefresh = async () => {
    await checkStatus()
    await loadHealth()
  }

  return (
    <div className="service-health-screen">
      <header className="screen-header">
        <h1>Service Health</h1>
        <p>Monitor the status of all backend services</p>
      </header>

      <OverallStatusBanner health={health} />

      <div className="actions-bar">
        <button
          className="btn btn-primary"
          onClick={handleRefresh}
          disabled={isChecking}
        >
          {isChecking ? '🔄 Checking...' : '🔄 Refresh Status'}
        </button>
        <button
          className="btn btn-success"
          onClick={handleStartServices}
          disabled={isStarting || health?.overall === 'healthy'}
        >
          {isStarting ? '⏳ Starting...' : '▶️ Start All Services'}
        </button>
        <button
          className="btn btn-danger"
          onClick={handleStopServices}
          disabled={isStopping || health?.overall === 'unhealthy'}
        >
          {isStopping ? '⏳ Stopping...' : '⏹️ Stop All Services'}
        </button>
      </div>

      <section className="services-grid">
        {health?.services.map((result, index) => (
          <ServiceCard key={index} result={result} />
        ))}
      </section>

      {health?.overall !== 'healthy' && (
        <section className="troubleshooting">
          <h2>Troubleshooting</h2>
          <div className="troubleshooting-tips">
            <div className="tip">
              <h3>🐳 Check Docker</h3>
              <p>Ensure Docker Desktop is running and the PostgreSQL container is active.</p>
              <code>docker ps | grep postgres</code>
            </div>
            <div className="tip">
              <h3>🔧 Check Configuration</h3>
              <p>Verify the endpoints in Settings match your backend configuration.</p>
            </div>
            <div className="tip">
              <h3>📋 Check Logs</h3>
              <p>Review the application logs for detailed error messages.</p>
            </div>
          </div>
        </section>
      )}

      <section className="service-details-table">
        <h2>Service Details</h2>
        <table>
          <thead>
            <tr>
              <th>Service</th>
              <th>Status</th>
              <th>Port</th>
              <th>Message</th>
              <th>Last Checked</th>
            </tr>
          </thead>
          <tbody>
            {statuses.map((status, index) => (
              <tr key={index} className={`row-${status.status}`}>
                <td>{status.name}</td>
                <td><StatusBadge status={status.status as any} /></td>
                <td>{status.port}</td>
                <td>{status.message || '-'}</td>
                <td>{new Date(status.lastChecked).toLocaleTimeString()}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </section>
    </div>
  )
}
