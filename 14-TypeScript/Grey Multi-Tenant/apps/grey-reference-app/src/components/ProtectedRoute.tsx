/**
 * ProtectedRoute Component
 * 
 * Wraps routes that require authentication.
 * Redirects to login if user is not authenticated.
 */

import { Navigate, useLocation } from 'react-router-dom';
import { useAuth } from '@grey/react';
import type { ReactNode } from 'react';

interface ProtectedRouteProps {
  children: ReactNode;
}

/**
 * ProtectedRoute
 * 
 * Checks authentication status and redirects to login if needed.
 * Preserves the intended destination URL for post-login redirect.
 * 
 * @example
 * ```tsx
 * <Route element={<ProtectedRoute><Layout /></ProtectedRoute>}>
 *   <Route path="/dashboard" element={<Dashboard />} />
 * </Route>
 * ```
 */
export function ProtectedRoute({ children }: ProtectedRouteProps) {
  const { isAuthenticated, loading } = useAuth();
  const location = useLocation();

  // Show loading state while checking auth
  // TODO: Replace with a proper loading spinner component
  if (loading) {
    return (
      <div style={{ 
        display: 'flex', 
        alignItems: 'center', 
        justifyContent: 'center',
        minHeight: '100vh',
        fontSize: '1.25rem',
        color: '#666'
      }}>
        Checking authentication...
      </div>
    );
  }

  // Redirect to login if not authenticated
  if (!isAuthenticated) {
    // Preserve the attempted URL for redirect after login
    return <Navigate to="/login" state={{ from: location }} replace />;
  }

  // Render protected content
  return <>{children}</>;
}
