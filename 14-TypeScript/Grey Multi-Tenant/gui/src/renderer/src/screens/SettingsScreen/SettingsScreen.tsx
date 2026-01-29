/**
 * Settings Screen
 */
import React, { useState, useEffect } from 'react'
import { Card, Button, Input, LogPanel } from '../../components'
import { useConfigStore, useServicesStore } from '../../stores'
import './SettingsScreen.css'

export function SettingsScreen() {
  const { config, fetchConfig, updateConfig, isLoading } = useConfigStore()
  const { startServices, stopServices, isChecking } = useServicesStore()
  
  const [httpEndpoint, setHttpEndpoint] = useState('')
  const [grpcEndpoint, setGrpcEndpoint] = useState('')
  const [autoStart, setAutoStart] = useState(false)
  const [saved, setSaved] = useState(false)

  useEffect(() => {
    fetchConfig()
  }, [fetchConfig])

  useEffect(() => {
    if (config) {
      setHttpEndpoint(config.httpEndpoint)
      setGrpcEndpoint(config.grpcEndpoint)
      setAutoStart(config.autoStartServices)
    }
  }, [config])

  const handleSave = async () => {
    await updateConfig('httpEndpoint', httpEndpoint)
    await updateConfig('grpcEndpoint', grpcEndpoint)
    await updateConfig('autoStartServices', autoStart)
    setSaved(true)
    setTimeout(() => setSaved(false), 2000)
  }

  const handleReset = () => {
    if (config) {
      setHttpEndpoint(config.httpEndpoint)
      setGrpcEndpoint(config.grpcEndpoint)
      setAutoStart(config.autoStartServices)
    }
  }

  if (isLoading) {
    return (
      <div className="settings-screen">
        <div className="loading-state">Loading settings...</div>
      </div>
    )
  }

  return (
    <div className="settings-screen">
      <header className="settings-header">
        <h1>Settings</h1>
        <p>Configure endpoints, ports, and application behavior</p>
      </header>

      <div className="settings-grid">
        {/* Endpoints Card */}
        <Card title="Service Endpoints" className="settings-card">
          <div className="settings-form">
            <Input
              label="HTTP API Endpoint"
              value={httpEndpoint}
              onChange={(e) => setHttpEndpoint(e.target.value)}
              placeholder="http://localhost:8080"
              helperText="URL for the HTTP JSON gateway"
            />
            <Input
              label="gRPC Endpoint"
              value={grpcEndpoint}
              onChange={(e) => setGrpcEndpoint(e.target.value)}
              placeholder="localhost:50051"
              helperText="Host:port for the gRPC service"
            />
          </div>
        </Card>

        {/* Behavior Card */}
        <Card title="Behavior" className="settings-card">
          <div className="settings-form">
            <label className="checkbox-label">
              <input
                type="checkbox"
                checked={autoStart}
                onChange={(e) => setAutoStart(e.target.checked)}
              />
              <span>Auto-start services on launch</span>
            </label>
            <p className="setting-description">
              Automatically attempt to start backend services when the application launches.
            </p>
          </div>
        </Card>

        {/* Service Management Card */}
        <Card title="Service Management" className="settings-card">
          <div className="service-controls">
            <Button
              variant="primary"
              onClick={startServices}
              loading={isChecking}
            >
              Start All Services
            </Button>
            <Button
              variant="danger"
              onClick={stopServices}
              loading={isChecking}
            >
              Stop All Services
            </Button>
          </div>
          <p className="setting-description">
            Start or stop all backend services. Services include the adapter core, HTTP gateway, and gRPC service.
          </p>
        </Card>

        {/* About Card */}
        <Card title="About" className="settings-card">
          <div className="about-info">
            <div className="about-item">
              <span className="about-label">Application</span>
              <span className="about-value">Grey Multi-Tenant</span>
            </div>
            <div className="about-item">
              <span className="about-label">Version</span>
              <span className="about-value">1.0.0</span>
            </div>
            <div className="about-item">
              <span className="about-label">Platform</span>
              <span className="about-value">{navigator.platform}</span>
            </div>
          </div>
        </Card>
      </div>

      {/* Save Actions */}
      <div className="settings-actions">
        <Button variant="secondary" onClick={handleReset}>
          Reset
        </Button>
        <Button variant="primary" onClick={handleSave}>
          {saved ? '✓ Saved' : 'Save Settings'}
        </Button>
      </div>

      {/* Log Panel */}
      <section className="settings-logs">
        <h2>Application Logs</h2>
        <LogPanel />
      </section>
    </div>
  )
}
