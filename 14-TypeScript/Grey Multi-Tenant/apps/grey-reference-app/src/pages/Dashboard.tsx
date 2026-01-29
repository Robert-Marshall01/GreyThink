/**
 * Dashboard Page
 * 
 * Main landing page after login.
 * Displays user info and quick stats.
 * Uses useUser() and useProjects() hooks from grey-react.
 */

import { useEffect } from 'react';
import { Link } from 'react-router-dom';
import { useUser, useProjects } from '@grey/react';
import { useOrganization } from '../context/OrganizationContext';

/**
 * Dashboard Component
 * 
 * Shows:
 * - Current user information
 * - Active organization
 * - Quick stats (project count, etc.)
 * - Quick links to main sections
 * 
 * TODO: Add real dashboard widgets
 * TODO: Add recent activity feed
 * TODO: Add charts/graphs for analytics
 */
export function Dashboard() {
  const { user, loading: userLoading, error: userError, refetch: refetchUser } = useUser();
  const { projects, loading: projectsLoading, error: projectsError, list: listProjects } = useProjects();
  const { activeOrganization } = useOrganization();

  // Fetch projects on mount
  useEffect(() => {
    listProjects(1, 10);
  }, [listProjects, activeOrganization?.id]);

  const isLoading = userLoading || projectsLoading;
  const error = userError || projectsError;

  return (
    <div>
      <h1 style={{ marginBottom: '1.5rem' }}>Dashboard</h1>

      {/* Error state */}
      {error && (
        <div style={{
          padding: '1rem',
          background: '#fee2e2',
          border: '1px solid #fecaca',
          borderRadius: '4px',
          color: '#dc2626',
          marginBottom: '1rem',
        }}>
          Error: {error.message}
          <button 
            onClick={() => refetchUser()} 
            style={{ marginLeft: '1rem', textDecoration: 'underline', background: 'none', border: 'none', cursor: 'pointer', color: 'inherit' }}
          >
            Retry
          </button>
        </div>
      )}

      {/* Loading state */}
      {isLoading && (
        <div style={{ color: '#666', marginBottom: '1rem' }}>
          Loading dashboard data...
        </div>
      )}

      {/* User info card */}
      <div style={{
        background: 'white',
        padding: '1.5rem',
        borderRadius: '8px',
        boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
        marginBottom: '1.5rem',
      }}>
        <h2 style={{ fontSize: '1.25rem', marginBottom: '1rem' }}>Welcome Back</h2>
        
        {user ? (
          <div>
            <p><strong>Name:</strong> {user.name || 'Not set'}</p>
            <p><strong>Email:</strong> {user.email}</p>
            <p><strong>User ID:</strong> <code style={{ fontSize: '0.875rem' }}>{user.id}</code></p>
            {/* TODO: Add more user fields like role, created date, etc. */}
          </div>
        ) : !isLoading ? (
          <p style={{ color: '#666' }}>User data not available</p>
        ) : null}
      </div>

      {/* Active organization card */}
      <div style={{
        background: 'white',
        padding: '1.5rem',
        borderRadius: '8px',
        boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
        marginBottom: '1.5rem',
      }}>
        <h2 style={{ fontSize: '1.25rem', marginBottom: '1rem' }}>Active Organization</h2>
        
        {activeOrganization ? (
          <div>
            <p><strong>Name:</strong> {activeOrganization.name}</p>
            <p><strong>ID:</strong> <code style={{ fontSize: '0.875rem' }}>{activeOrganization.id}</code></p>
          </div>
        ) : (
          <div>
            <p style={{ color: '#666', marginBottom: '0.5rem' }}>No organization selected</p>
            <Link 
              to="/organizations" 
              style={{ color: '#1a1a2e', textDecoration: 'underline' }}
            >
              Select an organization
            </Link>
          </div>
        )}
      </div>

      {/* Quick stats grid */}
      <div style={{
        display: 'grid',
        gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))',
        gap: '1rem',
        marginBottom: '1.5rem',
      }}>
        {/* Projects count card */}
        <div style={{
          background: 'white',
          padding: '1.5rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
          textAlign: 'center',
        }}>
          <div style={{ fontSize: '2rem', fontWeight: 'bold', color: '#1a1a2e' }}>
            {projects.length}
          </div>
          <div style={{ color: '#666' }}>Projects</div>
        </div>

        {/* TODO: Add more stat cards */}
        <div style={{
          background: 'white',
          padding: '1.5rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
          textAlign: 'center',
        }}>
          <div style={{ fontSize: '2rem', fontWeight: 'bold', color: '#1a1a2e' }}>
            —
          </div>
          <div style={{ color: '#666' }}>Team Members</div>
        </div>

        <div style={{
          background: 'white',
          padding: '1.5rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
          textAlign: 'center',
        }}>
          <div style={{ fontSize: '2rem', fontWeight: 'bold', color: '#1a1a2e' }}>
            —
          </div>
          <div style={{ color: '#666' }}>API Calls (24h)</div>
        </div>
      </div>

      {/* Quick links */}
      <div style={{
        display: 'flex',
        gap: '1rem',
        flexWrap: 'wrap',
      }}>
        <Link
          to="/organizations"
          style={{
            display: 'inline-block',
            padding: '0.75rem 1.5rem',
            background: '#1a1a2e',
            color: 'white',
            textDecoration: 'none',
            borderRadius: '4px',
          }}
        >
          Manage Organizations
        </Link>
        
        <Link
          to="/projects"
          style={{
            display: 'inline-block',
            padding: '0.75rem 1.5rem',
            background: '#1a1a2e',
            color: 'white',
            textDecoration: 'none',
            borderRadius: '4px',
          }}
        >
          View Projects
        </Link>
      </div>
    </div>
  );
}
