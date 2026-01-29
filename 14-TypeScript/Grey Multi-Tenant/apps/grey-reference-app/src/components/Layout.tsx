/**
 * Layout Component
 * 
 * Provides the main app layout with navigation header.
 * Uses React Router's Outlet for nested route rendering.
 */

import { Outlet, Link, useNavigate } from 'react-router-dom';
import { useAuth, useUser } from '@grey/react';
import { useOrganization } from '../context/OrganizationContext';

/**
 * Layout
 * 
 * Main application shell with:
 * - Navigation header
 * - User info display
 * - Active organization indicator
 * - Logout button
 * 
 * TODO: In a real app, replace this minimal UI with a proper design system.
 */
export function Layout() {
  const { logout } = useAuth();
  const { user } = useUser();
  const { activeOrganization, clearActiveOrganization } = useOrganization();
  const navigate = useNavigate();

  const handleLogout = async () => {
    // Clear organization selection on logout
    clearActiveOrganization();
    
    await logout();
    navigate('/login');
  };

  return (
    <div style={{ minHeight: '100vh', display: 'flex', flexDirection: 'column' }}>
      {/* Navigation Header */}
      <header style={{
        background: '#1a1a2e',
        color: 'white',
        padding: '1rem 2rem',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
      }}>
        <nav style={{ display: 'flex', gap: '1.5rem' }}>
          <Link to="/dashboard" style={{ color: 'white', textDecoration: 'none' }}>
            Dashboard
          </Link>
          <Link to="/organizations" style={{ color: 'white', textDecoration: 'none' }}>
            Organizations
          </Link>
          <Link to="/projects" style={{ color: 'white', textDecoration: 'none' }}>
            Projects
          </Link>
        </nav>

        <div style={{ display: 'flex', alignItems: 'center', gap: '1rem' }}>
          {/* Active organization indicator */}
          {activeOrganization && (
            <span style={{ fontSize: '0.875rem', opacity: 0.8 }}>
              Org: {activeOrganization.name}
            </span>
          )}

          {/* User info */}
          {user && (
            <span style={{ fontSize: '0.875rem' }}>
              {user.email}
            </span>
          )}

          <button
            onClick={handleLogout}
            style={{
              background: 'transparent',
              border: '1px solid rgba(255,255,255,0.3)',
              color: 'white',
              padding: '0.5rem 1rem',
              borderRadius: '4px',
              cursor: 'pointer',
            }}
          >
            Logout
          </button>
        </div>
      </header>

      {/* Main Content Area */}
      <main style={{ flex: 1, padding: '2rem' }}>
        {/* Outlet renders the matched child route */}
        <Outlet />
      </main>

      {/* Footer */}
      <footer style={{
        background: '#f0f0f0',
        padding: '1rem 2rem',
        textAlign: 'center',
        fontSize: '0.875rem',
        color: '#666',
      }}>
        Grey Multi-Tenant Reference App
      </footer>
    </div>
  );
}
