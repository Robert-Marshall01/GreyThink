/**
 * Grey Reference App - Main App Component
 * 
 * Defines the routing structure and layout.
 * Uses React Router for navigation and ProtectedRoute for auth guards.
 */

import { Routes, Route, Navigate } from 'react-router-dom';
import { useAuth } from '@grey/react';
import { Login } from './pages/Login';
import { Dashboard } from './pages/Dashboard';
import { Organizations } from './pages/Organizations';
import { Projects } from './pages/Projects';
import { ProjectDetail } from './pages/ProjectDetail';
import { Layout } from './components/Layout';
import { ProtectedRoute } from './components/ProtectedRoute';

/**
 * App Component
 * 
 * Handles routing and authentication guards.
 * All routes except /login require authentication.
 */
export function App() {
  const { isAuthenticated } = useAuth();

  return (
    <Routes>
      {/* Public routes */}
      <Route
        path="/login"
        element={
          isAuthenticated ? <Navigate to="/dashboard" replace /> : <Login />
        }
      />

      {/* Protected routes - wrapped in Layout for consistent navigation */}
      <Route
        element={
          <ProtectedRoute>
            <Layout />
          </ProtectedRoute>
        }
      >
        <Route path="/dashboard" element={<Dashboard />} />
        <Route path="/organizations" element={<Organizations />} />
        <Route path="/projects" element={<Projects />} />
        <Route path="/projects/:projectId" element={<ProjectDetail />} />
      </Route>

      {/* Default redirect */}
      <Route
        path="*"
        element={
          <Navigate to={isAuthenticated ? '/dashboard' : '/login'} replace />
        }
      />
    </Routes>
  );
}
