import { useState, useEffect, useCallback } from 'react'
import Dashboard from './components/Dashboard'
import Header from './components/Header'
import AlertBanner from './components/AlertBanner'
import { useWebSocket } from './hooks/useWebSocket'

/**
 * Grey Optimizer Dashboard Application
 * 
 * Real-time monitoring and control interface for the Grey Optimizer daemon.
 * Displays live telemetry, before/after comparisons, and enforcement controls.
 */
function App() {
  // State for metrics and configuration
  const [metrics, setMetrics] = useState(null)
  const [metricsHistory, setMetricsHistory] = useState([])
  const [baseline, setBaseline] = useState(null)
  const [status, setStatus] = useState({
    connected: false,
    simulation_mode: true,
    version: '1.0.0'
  })
  const [alerts, setAlerts] = useState([])
  const [config, setConfig] = useState({
    cpu_enabled: true,
    ram_enabled: true,
    disk_enabled: true
  })

  // WebSocket connection for real-time updates
  const { lastMessage, connectionStatus, sendMessage } = useWebSocket(
    `ws://${window.location.hostname}:8080/ws`
  )

  // Handle incoming WebSocket messages
  useEffect(() => {
    if (!lastMessage) return

    try {
      const data = JSON.parse(lastMessage)

      switch (data.type) {
        case 'connected':
          setStatus(prev => ({
            ...prev,
            connected: true,
            simulation_mode: data.data.simulation_mode
          }))
          break

        case 'metrics':
          setMetrics(data.data)
          setMetricsHistory(prev => {
            const updated = [...prev, data.data].slice(-60) // Keep 60 samples
            return updated
          })
          break

        case 'alert':
          setAlerts(prev => [...prev, { ...data.data, id: Date.now() }])
          break

        default:
          console.log('Unknown message type:', data.type)
      }
    } catch (e) {
      console.error('Failed to parse WebSocket message:', e)
    }
  }, [lastMessage])

  // Update connection status
  useEffect(() => {
    setStatus(prev => ({
      ...prev,
      connected: connectionStatus === 'connected'
    }))
  }, [connectionStatus])

  // Fetch initial data on mount
  useEffect(() => {
    fetchStatus()
    fetchConfig()
    fetchMetricsHistory()
  }, [])

  // API calls
  const fetchStatus = async () => {
    try {
      const res = await fetch('/api/status')
      if (res.ok) {
        const data = await res.json()
        setStatus(prev => ({ ...prev, ...data }))
      }
    } catch (e) {
      console.error('Failed to fetch status:', e)
    }
  }

  const fetchConfig = async () => {
    try {
      const res = await fetch('/api/config')
      if (res.ok) {
        const data = await res.json()
        setConfig(data)
      }
    } catch (e) {
      console.error('Failed to fetch config:', e)
    }
  }

  const fetchMetricsHistory = async () => {
    try {
      const res = await fetch('/api/metrics/history?seconds=60')
      if (res.ok) {
        const data = await res.json()
        setMetricsHistory(data.history || [])
      }
    } catch (e) {
      console.error('Failed to fetch metrics history:', e)
    }
  }

  // Handle rollback request
  const handleRollback = useCallback(async () => {
    if (!confirm('Are you sure you want to rollback all enforcement actions?')) {
      return
    }

    try {
      const res = await fetch('/api/rollback', { method: 'POST' })
      if (res.ok) {
        setAlerts(prev => [...prev, {
          id: Date.now(),
          type: 'info',
          message: 'Rollback completed successfully',
          timestamp: new Date().toISOString()
        }])
      }
    } catch (e) {
      console.error('Rollback failed:', e)
      setAlerts(prev => [...prev, {
        id: Date.now(),
        type: 'error',
        message: 'Rollback failed: ' + e.message,
        timestamp: new Date().toISOString()
      }])
    }
  }, [])

  // Handle config toggle
  const handleToggle = useCallback(async (subsystem) => {
    const newConfig = {
      ...config,
      [`${subsystem}_enabled`]: !config[`${subsystem}_enabled`]
    }

    try {
      const res = await fetch('/api/config', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(newConfig)
      })
      if (res.ok) {
        setConfig(newConfig)
      }
    } catch (e) {
      console.error('Failed to update config:', e)
    }
  }, [config])

  // Dismiss alert
  const dismissAlert = useCallback((id) => {
    setAlerts(prev => prev.filter(a => a.id !== id))
  }, [])

  return (
    <div className="min-h-screen bg-grey-900">
      {/* Alert banners */}
      {alerts.map(alert => (
        <AlertBanner
          key={alert.id}
          alert={alert}
          onDismiss={() => dismissAlert(alert.id)}
        />
      ))}

      {/* Header with status */}
      <Header
        status={status}
        onRollback={handleRollback}
      />

      {/* Main dashboard */}
      <main className="container mx-auto px-4 py-6">
        <Dashboard
          metrics={metrics}
          metricsHistory={metricsHistory}
          baseline={baseline}
          config={config}
          onToggle={handleToggle}
          simulationMode={status.simulation_mode}
        />
      </main>

      {/* Footer */}
      <footer className="text-center py-4 text-grey-500 text-sm">
        Grey Optimizer v{status.version} • 
        {status.simulation_mode ? ' Simulation Mode' : ' Live Mode'}
      </footer>
    </div>
  )
}

export default App
