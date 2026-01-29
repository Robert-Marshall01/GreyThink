/**
 * App Component - Main Application with Routing
 */
import React, { useEffect } from 'react'
import { HashRouter, Routes, Route, Navigate, useNavigate } from 'react-router-dom'
import { Layout } from './components/Layout/Layout'
import {
  DashboardScreen,
  ProjectsScreen,
  QueryScreen,
  MutationScreen,
  SettingsScreen,
  ServiceHealthScreen
} from './screens'
import { useServicesStore, useConfigStore } from './stores'

/**
 * Route Wrapper - No authentication required
 */
function AppRoute({ children }: { children: React.ReactNode }) {
  return <Layout>{children}</Layout>
}

/**
 * Navigation Listener
 * Listens for navigation events from main process
 */
function NavigationListener() {
  const navigate = useNavigate()

  useEffect(() => {
    const unsubscribe = window.electronAPI.navigation.onNavigate((path) => {
      navigate(path)
    })
    return unsubscribe
  }, [navigate])

  useEffect(() => {
    const unsubscribe = window.electronAPI.app.onCheckServices(() => {
      // Trigger service check - handled by store
      useServicesStore.getState().checkStatus()
    })
    return unsubscribe
  }, [])

  return null
}

/**
 * App Initializer
 * Loads initial config and checks services
 */
function AppInitializer() {
  const { fetchConfig } = useConfigStore()
  const { checkStatus } = useServicesStore()

  useEffect(() => {
    fetchConfig()
    checkStatus()
    
    // Set up periodic status checks
    const unsubscribe = window.electronAPI.services.onStatusUpdate((statuses) => {
      useServicesStore.getState().setStatuses(statuses)
    })
    
    return unsubscribe
  }, [fetchConfig, checkStatus])

  return null
}

/**
 * Main App Component
 */
export function App() {
  return (
    <HashRouter>
      <NavigationListener />
      <AppInitializer />
      <Routes>
        <Route
          path="/dashboard"
          element={
            <AppRoute>
              <DashboardScreen />
            </AppRoute>
          }
        />
        <Route
          path="/projects"
          element={
            <AppRoute>
              <ProjectsScreen />
            </AppRoute>
          }
        />
        <Route
          path="/query"
          element={
            <AppRoute>
              <QueryScreen />
            </AppRoute>
          }
        />
        <Route
          path="/mutation"
          element={
            <AppRoute>
              <MutationScreen />
            </AppRoute>
          }
        />
        <Route
          path="/settings"
          element={
            <AppRoute>
              <SettingsScreen />
            </AppRoute>
          }
        />
        <Route
          path="/services"
          element={
            <AppRoute>
              <ServiceHealthScreen />
            </AppRoute>
          }
        />
        <Route path="/" element={<Navigate to="/dashboard" replace />} />
        <Route path="*" element={<Navigate to="/dashboard" replace />} />
      </Routes>
    </HashRouter>
  )
}
