/**
 * Dashboard Screen
 */
import React from 'react'
import { Card, StatusIndicator, Button, LogPanel } from '../../components'
import { useAuthStore, useServicesStore, useProjectsStore } from '../../stores'
import './DashboardScreen.css'

export function DashboardScreen() {
  const { user } = useAuthStore()
  const { statuses, checkStatus, startServices, isChecking } = useServicesStore()
  const { projects, fetchProjects, isLoading: projectsLoading } = useProjectsStore()

  React.useEffect(() => {
    checkStatus()
    fetchProjects()
  }, [checkStatus, fetchProjects])

  const runningCount = statuses.filter(s => s.status === 'running').length
  const totalCount = statuses.length

  return (
    <div className="dashboard-screen">
      <header className="dashboard-header">
        <div>
          <h1>Dashboard</h1>
          <p>Welcome back, {user?.name || 'User'}</p>
        </div>
        <Button onClick={checkStatus} loading={isChecking} variant="secondary">
          Refresh Status
        </Button>
      </header>

      <div className="dashboard-grid">
        {/* User Info Card */}
        <Card title="User Information" className="dashboard-card">
          <div className="user-info-grid">
            <div className="info-item">
              <span className="info-label">Name</span>
              <span className="info-value">{user?.name || 'N/A'}</span>
            </div>
            <div className="info-item">
              <span className="info-label">Email</span>
              <span className="info-value">{user?.email || 'N/A'}</span>
            </div>
            <div className="info-item">
              <span className="info-label">Role</span>
              <span className="info-value">{user?.role || 'N/A'}</span>
            </div>
            <div className="info-item">
              <span className="info-label">Tenant ID</span>
              <span className="info-value info-value--mono">{user?.tenantId || 'N/A'}</span>
            </div>
          </div>
        </Card>

        {/* Services Status Card */}
        <Card 
          title="Service Status" 
          className="dashboard-card"
          actions={
            <Button size="sm" variant="ghost" onClick={startServices}>
              Start All
            </Button>
          }
        >
          <div className="services-status">
            <div className="services-summary">
              <span className="services-count">{runningCount}/{totalCount}</span>
              <span className="services-label">services running</span>
            </div>
            <div className="services-detail-list">
              {statuses.map((service) => (
                <div key={service.name} className="service-detail-item">
                  <StatusIndicator status={service.status} showLabel={false} />
                  <span className="service-name">{service.name}</span>
                  <span className="service-message">{service.message}</span>
                </div>
              ))}
            </div>
          </div>
        </Card>

        {/* Projects Overview Card */}
        <Card title="Projects" className="dashboard-card">
          {projectsLoading ? (
            <div className="loading-state">Loading projects...</div>
          ) : (
            <div className="projects-overview">
              <div className="projects-count">
                <span className="count-number">{projects.length}</span>
                <span className="count-label">Total Projects</span>
              </div>
              {projects.slice(0, 3).map((project) => (
                <div key={project.id} className="project-preview">
                  <span className="project-name">{project.name}</span>
                  <span className="project-date">
                    {new Date(project.createdAt).toLocaleDateString()}
                  </span>
                </div>
              ))}
              {projects.length > 3 && (
                <div className="projects-more">
                  +{projects.length - 3} more projects
                </div>
              )}
            </div>
          )}
        </Card>

        {/* Quick Actions Card */}
        <Card title="Quick Actions" className="dashboard-card">
          <div className="quick-actions">
            <Button variant="secondary" fullWidth>
              📁 New Project
            </Button>
            <Button variant="secondary" fullWidth>
              🔍 Run Query
            </Button>
            <Button variant="secondary" fullWidth>
              ⚙️ Settings
            </Button>
          </div>
        </Card>
      </div>

      {/* Log Panel */}
      <section className="dashboard-logs">
        <h2>Activity Log</h2>
        <LogPanel />
      </section>
    </div>
  )
}
