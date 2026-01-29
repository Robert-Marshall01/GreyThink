/**
 * Projects Page
 * 
 * Lists all projects for the active organization.
 * Uses useProjects() hook from grey-react.
 */

import { useState, useEffect, type FormEvent } from 'react';
import { Link } from 'react-router-dom';
import { useProjects } from '@grey/react';
import { useOrganization } from '../context/OrganizationContext';

/**
 * Projects Component
 * 
 * Features:
 * - List projects with pagination
 * - Create new project
 * - Navigate to project detail
 * - Delete project with confirmation
 * 
 * TODO: Add search/filter
 * TODO: Add sorting options
 * TODO: Add bulk actions
 */
export function Projects() {
  const { activeOrganization } = useOrganization();
  const { 
    projects, 
    pagination, 
    loading, 
    error, 
    list, 
    create, 
    remove, 
    refetch 
  } = useProjects();

  // Form state
  const [showCreateForm, setShowCreateForm] = useState(false);
  const [newProjectName, setNewProjectName] = useState('');
  const [newProjectDescription, setNewProjectDescription] = useState('');
  const [createLoading, setCreateLoading] = useState(false);
  const [createError, setCreateError] = useState<string | null>(null);

  // Pagination state
  const [currentPage, setCurrentPage] = useState(1);
  const pageSize = 10;

  // Fetch projects when page or organization changes
  useEffect(() => {
    list(currentPage, pageSize);
  }, [list, currentPage, activeOrganization?.id]);

  const handleCreateProject = async (e: FormEvent) => {
    e.preventDefault();
    
    if (!newProjectName.trim()) {
      setCreateError('Project name is required');
      return;
    }

    setCreateLoading(true);
    setCreateError(null);

    try {
      await create({
        name: newProjectName.trim(),
        description: newProjectDescription.trim() || undefined,
      });

      // Clear form and close
      setNewProjectName('');
      setNewProjectDescription('');
      setShowCreateForm(false);
    } catch (err) {
      setCreateError(err instanceof Error ? err.message : 'Failed to create project');
    } finally {
      setCreateLoading(false);
    }
  };

  const handleDeleteProject = async (projectId: string, projectName: string) => {
    // Simple confirmation - in production, use a proper modal
    const confirmed = window.confirm(`Are you sure you want to delete "${projectName}"?`);
    if (!confirmed) return;

    try {
      await remove(projectId);
    } catch (err) {
      console.error('Failed to delete project:', err);
      alert('Failed to delete project');
    }
  };

  const handlePageChange = (newPage: number) => {
    setCurrentPage(newPage);
  };

  // Show message if no organization selected
  if (!activeOrganization) {
    return (
      <div>
        <h1 style={{ marginBottom: '1.5rem' }}>Projects</h1>
        <div style={{
          background: '#fef3c7',
          border: '1px solid #fcd34d',
          borderRadius: '8px',
          padding: '1.5rem',
          textAlign: 'center',
        }}>
          <p style={{ marginBottom: '1rem' }}>
            Please select an organization first to view projects.
          </p>
          <Link
            to="/organizations"
            style={{
              display: 'inline-block',
              padding: '0.5rem 1rem',
              background: '#1a1a2e',
              color: 'white',
              textDecoration: 'none',
              borderRadius: '4px',
            }}
          >
            Go to Organizations
          </Link>
        </div>
      </div>
    );
  }

  return (
    <div>
      <div style={{ 
        display: 'flex', 
        justifyContent: 'space-between', 
        alignItems: 'center',
        marginBottom: '1.5rem',
      }}>
        <div>
          <h1>Projects</h1>
          <p style={{ color: '#666', fontSize: '0.875rem', marginTop: '0.25rem' }}>
            Organization: {activeOrganization.name}
          </p>
        </div>
        
        <button
          onClick={() => setShowCreateForm(!showCreateForm)}
          style={{
            padding: '0.5rem 1rem',
            background: showCreateForm ? '#6b7280' : '#1a1a2e',
            color: 'white',
            border: 'none',
            borderRadius: '4px',
            cursor: 'pointer',
          }}
        >
          {showCreateForm ? 'Cancel' : '+ Create Project'}
        </button>
      </div>

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
            onClick={() => refetch()} 
            style={{ 
              marginLeft: '1rem', 
              textDecoration: 'underline', 
              background: 'none', 
              border: 'none', 
              cursor: 'pointer', 
              color: 'inherit' 
            }}
          >
            Retry
          </button>
        </div>
      )}

      {/* Create project form */}
      {showCreateForm && (
        <div style={{
          background: 'white',
          padding: '1.5rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
          marginBottom: '1.5rem',
        }}>
          <h2 style={{ fontSize: '1.25rem', marginBottom: '1rem' }}>Create New Project</h2>
          
          <form onSubmit={handleCreateProject}>
            <div style={{ marginBottom: '1rem' }}>
              <label 
                htmlFor="projectName"
                style={{ display: 'block', marginBottom: '0.5rem', fontWeight: 500 }}
              >
                Project Name *
              </label>
              <input
                id="projectName"
                type="text"
                value={newProjectName}
                onChange={(e) => setNewProjectName(e.target.value)}
                placeholder="My Project"
                disabled={createLoading}
                style={{
                  width: '100%',
                  padding: '0.75rem',
                  border: '1px solid #ddd',
                  borderRadius: '4px',
                  fontSize: '1rem',
                }}
              />
            </div>

            <div style={{ marginBottom: '1rem' }}>
              <label 
                htmlFor="projectDescription"
                style={{ display: 'block', marginBottom: '0.5rem', fontWeight: 500 }}
              >
                Description (optional)
              </label>
              <textarea
                id="projectDescription"
                value={newProjectDescription}
                onChange={(e) => setNewProjectDescription(e.target.value)}
                placeholder="A brief description of this project"
                disabled={createLoading}
                rows={3}
                style={{
                  width: '100%',
                  padding: '0.75rem',
                  border: '1px solid #ddd',
                  borderRadius: '4px',
                  fontSize: '1rem',
                  resize: 'vertical',
                }}
              />
            </div>

            {createError && (
              <div style={{
                padding: '0.75rem',
                background: '#fee2e2',
                border: '1px solid #fecaca',
                borderRadius: '4px',
                color: '#dc2626',
                marginBottom: '1rem',
                fontSize: '0.875rem',
              }}>
                {createError}
              </div>
            )}

            <button
              type="submit"
              disabled={createLoading || !newProjectName.trim()}
              style={{
                padding: '0.75rem 1.5rem',
                background: createLoading ? '#9ca3af' : '#1a1a2e',
                color: 'white',
                border: 'none',
                borderRadius: '4px',
                cursor: createLoading ? 'not-allowed' : 'pointer',
                fontWeight: 500,
              }}
            >
              {createLoading ? 'Creating...' : 'Create Project'}
            </button>
          </form>
        </div>
      )}

      {/* Projects list */}
      <div style={{
        background: 'white',
        borderRadius: '8px',
        boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
        overflow: 'hidden',
      }}>
        {loading ? (
          <div style={{ padding: '2rem', textAlign: 'center', color: '#666' }}>
            Loading projects...
          </div>
        ) : projects.length > 0 ? (
          <>
            <ul style={{ listStyle: 'none', margin: 0, padding: 0 }}>
              {projects.map((project) => (
                <li 
                  key={project.id}
                  style={{
                    padding: '1rem 1.5rem',
                    borderBottom: '1px solid #e5e7eb',
                    display: 'flex',
                    justifyContent: 'space-between',
                    alignItems: 'center',
                  }}
                >
                  <div style={{ flex: 1 }}>
                    <Link
                      to={`/projects/${project.id}`}
                      style={{ 
                        fontWeight: 500, 
                        color: '#1a1a2e', 
                        textDecoration: 'none',
                      }}
                    >
                      {project.name}
                    </Link>
                    {project.description && (
                      <div style={{ fontSize: '0.875rem', color: '#666', marginTop: '0.25rem' }}>
                        {project.description}
                      </div>
                    )}
                    <div style={{ fontSize: '0.75rem', color: '#999', marginTop: '0.25rem' }}>
                      ID: {project.id}
                    </div>
                  </div>

                  <div style={{ display: 'flex', gap: '0.5rem' }}>
                    <Link
                      to={`/projects/${project.id}`}
                      style={{
                        padding: '0.5rem 1rem',
                        background: '#e5e7eb',
                        color: '#374151',
                        textDecoration: 'none',
                        borderRadius: '4px',
                        fontSize: '0.875rem',
                      }}
                    >
                      View
                    </Link>
                    
                    <button
                      onClick={() => handleDeleteProject(project.id, project.name)}
                      style={{
                        padding: '0.5rem 1rem',
                        background: '#fee2e2',
                        color: '#dc2626',
                        border: 'none',
                        borderRadius: '4px',
                        cursor: 'pointer',
                        fontSize: '0.875rem',
                      }}
                    >
                      Delete
                    </button>
                  </div>
                </li>
              ))}
            </ul>

            {/* Pagination */}
            {pagination && pagination.total_pages > 1 && (
              <div style={{
                padding: '1rem 1.5rem',
                borderTop: '1px solid #e5e7eb',
                display: 'flex',
                justifyContent: 'space-between',
                alignItems: 'center',
              }}>
                <span style={{ fontSize: '0.875rem', color: '#666' }}>
                  Page {pagination.page} of {pagination.total_pages} ({pagination.total_items} total)
                </span>

                <div style={{ display: 'flex', gap: '0.5rem' }}>
                  <button
                    onClick={() => handlePageChange(currentPage - 1)}
                    disabled={currentPage <= 1}
                    style={{
                      padding: '0.5rem 1rem',
                      background: currentPage <= 1 ? '#e5e7eb' : '#1a1a2e',
                      color: currentPage <= 1 ? '#9ca3af' : 'white',
                      border: 'none',
                      borderRadius: '4px',
                      cursor: currentPage <= 1 ? 'not-allowed' : 'pointer',
                    }}
                  >
                    Previous
                  </button>
                  
                  <button
                    onClick={() => handlePageChange(currentPage + 1)}
                    disabled={currentPage >= pagination.total_pages}
                    style={{
                      padding: '0.5rem 1rem',
                      background: currentPage >= pagination.total_pages ? '#e5e7eb' : '#1a1a2e',
                      color: currentPage >= pagination.total_pages ? '#9ca3af' : 'white',
                      border: 'none',
                      borderRadius: '4px',
                      cursor: currentPage >= pagination.total_pages ? 'not-allowed' : 'pointer',
                    }}
                  >
                    Next
                  </button>
                </div>
              </div>
            )}
          </>
        ) : (
          <div style={{ padding: '2rem', textAlign: 'center', color: '#666' }}>
            <p>No projects found.</p>
            <p style={{ fontSize: '0.875rem', marginTop: '0.5rem' }}>
              Create your first project to get started.
            </p>
          </div>
        )}
      </div>
    </div>
  );
}
