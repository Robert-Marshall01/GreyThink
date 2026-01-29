/**
 * Project Detail Page
 * 
 * Shows details for a single project.
 * Uses useQuery() hook from grey-react for data fetching.
 */

import { Link, useParams, useNavigate } from 'react-router-dom';
import { useQuery, useProjects, useGreyContext, type Project } from '@grey/react';
import { useOrganization } from '../context/OrganizationContext';

/**
 * ProjectDetail Component
 * 
 * Features:
 * - Display project details
 * - Edit project (future)
 * - Delete project with confirmation
 * 
 * TODO: Add edit form
 * TODO: Add project settings
 * TODO: Add team/members management
 * TODO: Add activity log
 */
export function ProjectDetail() {
  const { projectId } = useParams<{ projectId: string }>();
  const navigate = useNavigate();
  const { activeOrganization } = useOrganization();
  const { baseUrl } = useGreyContext();
  const { remove } = useProjects();

  // Fetch project details using useQuery
  const {
    data: project,
    loading,
    error,
    refetch,
  } = useQuery<Project>({
    key: ['project', projectId || ''],
    queryFn: async () => {
      if (!projectId) {
        throw new Error('Project ID is required');
      }

      // TODO: Use SDK client when available
      // const result = await client.projects.get(projectId);
      // return result.data;

      const response = await fetch(`${baseUrl}/projects/${projectId}`, {
        headers: {
          'Content-Type': 'application/json',
          // Token would be injected by SDK normally
        },
      });

      if (!response.ok) {
        if (response.status === 404) {
          throw new Error('Project not found');
        }
        throw new Error('Failed to fetch project');
      }

      const result = await response.json();
      return result.data;
    },
    enabled: !!projectId,
  });

  const handleDelete = async () => {
    if (!project) return;

    const confirmed = window.confirm(
      `Are you sure you want to delete "${project.name}"? This action cannot be undone.`
    );
    if (!confirmed) return;

    try {
      await remove(project.id);
      navigate('/projects');
    } catch (err) {
      console.error('Failed to delete project:', err);
      alert('Failed to delete project');
    }
  };

  // No organization selected
  if (!activeOrganization) {
    return (
      <div>
        <h1 style={{ marginBottom: '1rem' }}>Project Detail</h1>
        <div style={{
          background: '#fef3c7',
          border: '1px solid #fcd34d',
          borderRadius: '8px',
          padding: '1.5rem',
        }}>
          <p>Please select an organization to view project details.</p>
          <Link to="/organizations" style={{ color: '#1a1a2e' }}>
            Go to Organizations
          </Link>
        </div>
      </div>
    );
  }

  // Loading state
  if (loading) {
    return (
      <div>
        <div style={{ marginBottom: '1rem' }}>
          <Link to="/projects" style={{ color: '#666', textDecoration: 'none' }}>
            ← Back to Projects
          </Link>
        </div>
        <div style={{
          background: 'white',
          padding: '2rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
          textAlign: 'center',
          color: '#666',
        }}>
          Loading project details...
        </div>
      </div>
    );
  }

  // Error state
  if (error) {
    return (
      <div>
        <div style={{ marginBottom: '1rem' }}>
          <Link to="/projects" style={{ color: '#666', textDecoration: 'none' }}>
            ← Back to Projects
          </Link>
        </div>
        <div style={{
          background: '#fee2e2',
          border: '1px solid #fecaca',
          borderRadius: '8px',
          padding: '1.5rem',
          color: '#dc2626',
        }}>
          <p>Error: {error.message}</p>
          <button
            onClick={() => refetch()}
            style={{
              marginTop: '0.5rem',
              padding: '0.5rem 1rem',
              background: '#dc2626',
              color: 'white',
              border: 'none',
              borderRadius: '4px',
              cursor: 'pointer',
            }}
          >
            Retry
          </button>
        </div>
      </div>
    );
  }

  // Not found
  if (!project) {
    return (
      <div>
        <div style={{ marginBottom: '1rem' }}>
          <Link to="/projects" style={{ color: '#666', textDecoration: 'none' }}>
            ← Back to Projects
          </Link>
        </div>
        <div style={{
          background: 'white',
          padding: '2rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
          textAlign: 'center',
        }}>
          <p>Project not found.</p>
        </div>
      </div>
    );
  }

  return (
    <div>
      {/* Breadcrumb */}
      <div style={{ marginBottom: '1rem' }}>
        <Link to="/projects" style={{ color: '#666', textDecoration: 'none' }}>
          ← Back to Projects
        </Link>
      </div>

      {/* Header */}
      <div style={{ 
        display: 'flex', 
        justifyContent: 'space-between', 
        alignItems: 'flex-start',
        marginBottom: '1.5rem',
      }}>
        <div>
          <h1>{project.name}</h1>
          <p style={{ color: '#666', fontSize: '0.875rem', marginTop: '0.25rem' }}>
            Organization: {activeOrganization.name}
          </p>
        </div>
        
        <div style={{ display: 'flex', gap: '0.5rem' }}>
          {/* TODO: Add edit button */}
          <button
            onClick={() => alert('Edit functionality coming soon')}
            style={{
              padding: '0.5rem 1rem',
              background: '#e5e7eb',
              color: '#374151',
              border: 'none',
              borderRadius: '4px',
              cursor: 'pointer',
            }}
          >
            Edit
          </button>
          
          <button
            onClick={handleDelete}
            style={{
              padding: '0.5rem 1rem',
              background: '#fee2e2',
              color: '#dc2626',
              border: 'none',
              borderRadius: '4px',
              cursor: 'pointer',
            }}
          >
            Delete
          </button>
        </div>
      </div>

      {/* Project details card */}
      <div style={{
        background: 'white',
        padding: '1.5rem',
        borderRadius: '8px',
        boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
        marginBottom: '1.5rem',
      }}>
        <h2 style={{ fontSize: '1.25rem', marginBottom: '1rem' }}>Project Details</h2>
        
        <dl style={{ margin: 0 }}>
          <div style={{ marginBottom: '1rem' }}>
            <dt style={{ fontWeight: 500, color: '#666', fontSize: '0.875rem' }}>Project ID</dt>
            <dd style={{ margin: '0.25rem 0 0' }}>
              <code style={{ 
                background: '#f3f4f6', 
                padding: '0.25rem 0.5rem', 
                borderRadius: '4px',
                fontSize: '0.875rem',
              }}>
                {project.id}
              </code>
            </dd>
          </div>
          
          <div style={{ marginBottom: '1rem' }}>
            <dt style={{ fontWeight: 500, color: '#666', fontSize: '0.875rem' }}>Name</dt>
            <dd style={{ margin: '0.25rem 0 0' }}>{project.name}</dd>
          </div>
          
          <div style={{ marginBottom: '1rem' }}>
            <dt style={{ fontWeight: 500, color: '#666', fontSize: '0.875rem' }}>Description</dt>
            <dd style={{ margin: '0.25rem 0 0', color: project.description ? 'inherit' : '#999' }}>
              {project.description || 'No description'}
            </dd>
          </div>

          {/* TODO: Add more fields like created_at, updated_at, owner, etc. */}
          <div style={{ marginBottom: '1rem' }}>
            <dt style={{ fontWeight: 500, color: '#666', fontSize: '0.875rem' }}>Created</dt>
            <dd style={{ margin: '0.25rem 0 0', color: '#999' }}>
              {/* TODO: Format date when available */}
              —
            </dd>
          </div>
        </dl>
      </div>

      {/* Placeholder sections for future features */}
      <div style={{
        display: 'grid',
        gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))',
        gap: '1rem',
      }}>
        {/* Settings placeholder */}
        <div style={{
          background: 'white',
          padding: '1.5rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
        }}>
          <h3 style={{ fontSize: '1rem', marginBottom: '1rem' }}>Project Settings</h3>
          <p style={{ color: '#666', fontSize: '0.875rem' }}>
            {/* TODO: Add project settings management */}
            Settings configuration will be available here.
          </p>
        </div>

        {/* Team placeholder */}
        <div style={{
          background: 'white',
          padding: '1.5rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
        }}>
          <h3 style={{ fontSize: '1rem', marginBottom: '1rem' }}>Team Members</h3>
          <p style={{ color: '#666', fontSize: '0.875rem' }}>
            {/* TODO: Add team member management */}
            Team member management will be available here.
          </p>
        </div>

        {/* Activity placeholder */}
        <div style={{
          background: 'white',
          padding: '1.5rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
        }}>
          <h3 style={{ fontSize: '1rem', marginBottom: '1rem' }}>Recent Activity</h3>
          <p style={{ color: '#666', fontSize: '0.875rem' }}>
            {/* TODO: Add activity log */}
            Activity log will be available here.
          </p>
        </div>

        {/* API Keys placeholder */}
        <div style={{
          background: 'white',
          padding: '1.5rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
        }}>
          <h3 style={{ fontSize: '1rem', marginBottom: '1rem' }}>API Keys</h3>
          <p style={{ color: '#666', fontSize: '0.875rem' }}>
            {/* TODO: Add API key management */}
            API key management will be available here.
          </p>
        </div>
      </div>
    </div>
  );
}
