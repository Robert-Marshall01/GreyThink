/**
 * Layout Component
 */
import React from 'react'
import { NavLink } from 'react-router-dom'
import { useServicesStore, useAuthStore } from '../../stores'
import { StatusIndicator } from '../StatusIndicator/StatusIndicator'
import './Layout.css'

interface LayoutProps {
  children: React.ReactNode
}

export function Layout({ children }: LayoutProps) {
  const { statuses, checkStatus } = useServicesStore()
  const { user, logout } = useAuthStore()

  React.useEffect(() => {
    checkStatus()
  }, [checkStatus])

  return (
    <div className="layout">
      <aside className="sidebar">
        <div className="sidebar-header">
          <div className="sidebar-logo">
            <span className="logo-icon">◇</span>
            <span className="logo-text">Grey</span>
          </div>
        </div>

        <nav className="sidebar-nav">
          <NavLink to="/dashboard" className={({ isActive }) => `nav-link ${isActive ? 'nav-link--active' : ''}`}>
            <span className="nav-icon">📊</span>
            Dashboard
          </NavLink>
          <NavLink to="/projects" className={({ isActive }) => `nav-link ${isActive ? 'nav-link--active' : ''}`}>
            <span className="nav-icon">📁</span>
            Projects
          </NavLink>
          <NavLink to="/query" className={({ isActive }) => `nav-link ${isActive ? 'nav-link--active' : ''}`}>
            <span className="nav-icon">🔍</span>
            Query Console
          </NavLink>
          <NavLink to="/mutation" className={({ isActive }) => `nav-link ${isActive ? 'nav-link--active' : ''}`}>
            <span className="nav-icon">✏️</span>
            Mutation Console
          </NavLink>
          <NavLink to="/services" className={({ isActive }) => `nav-link ${isActive ? 'nav-link--active' : ''}`}>
            <span className="nav-icon">🔧</span>
            Service Health
          </NavLink>
          <NavLink to="/settings" className={({ isActive }) => `nav-link ${isActive ? 'nav-link--active' : ''}`}>
            <span className="nav-icon">⚙️</span>
            Settings
          </NavLink>
        </nav>

        <div className="sidebar-services">
          <div className="services-header">
            <span>Services</span>
            <button className="refresh-btn" onClick={checkStatus} title="Refresh">↻</button>
          </div>
          <div className="services-list">
            {statuses.map((service) => (
              <div key={service.name} className="service-item">
                <StatusIndicator status={service.status} showLabel={false} />
                <span className="service-name">{service.name}</span>
                <span className="service-port">:{service.port}</span>
              </div>
            ))}
            {statuses.length === 0 && (
              <div className="service-item service-item--empty">
                No services detected
              </div>
            )}
          </div>
        </div>

        <div className="sidebar-footer">
          {user && (
            <div className="user-info">
              <div className="user-avatar">{user.name.charAt(0).toUpperCase()}</div>
              <div className="user-details">
                <span className="user-name">{user.name}</span>
                <span className="user-email">{user.email}</span>
              </div>
              <button className="logout-btn" onClick={logout} title="Logout">⏻</button>
            </div>
          )}
        </div>
      </aside>

      <main className="main-content">
        {children}
      </main>
    </div>
  )
}
