/**
 * Projects Screen
 */
import React, { useState } from 'react'
import { Card, Button, Input } from '../../components'
import { useProjectsStore } from '../../stores'
import './ProjectsScreen.css'

export function ProjectsScreen() {
  const { projects, fetchProjects, createProject, isLoading, error } = useProjectsStore()
  const [showCreate, setShowCreate] = useState(false)
  const [newName, setNewName] = useState('')
  const [newDescription, setNewDescription] = useState('')
  const [creating, setCreating] = useState(false)

  React.useEffect(() => {
    fetchProjects()
  }, [fetchProjects])

  const handleCreate = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!newName.trim()) return
    
    setCreating(true)
    const project = await createProject(newName, newDescription)
    setCreating(false)
    
    if (project) {
      setShowCreate(false)
      setNewName('')
      setNewDescription('')
    }
  }

  return (
    <div className="projects-screen">
      <header className="projects-header">
        <div>
          <h1>Projects</h1>
          <p>Manage your multi-tenant projects</p>
        </div>
        <Button onClick={() => setShowCreate(true)} variant="primary">
          + New Project
        </Button>
      </header>

      {error && (
        <div className="projects-error">
          {error}
        </div>
      )}

      {/* Create Project Modal */}
      {showCreate && (
        <div className="modal-overlay">
          <Card title="Create New Project" className="create-modal">
            <form onSubmit={handleCreate} className="create-form">
              <Input
                label="Project Name"
                value={newName}
                onChange={(e) => setNewName(e.target.value)}
                placeholder="Enter project name"
                required
                autoFocus
              />
              <Input
                label="Description"
                value={newDescription}
                onChange={(e) => setNewDescription(e.target.value)}
                placeholder="Enter project description (optional)"
              />
              <div className="create-actions">
                <Button
                  type="button"
                  variant="secondary"
                  onClick={() => setShowCreate(false)}
                >
                  Cancel
                </Button>
                <Button
                  type="submit"
                  variant="primary"
                  loading={creating}
                  disabled={!newName.trim()}
                >
                  Create Project
                </Button>
              </div>
            </form>
          </Card>
        </div>
      )}

      {/* Projects List */}
      <div className="projects-list">
        {isLoading ? (
          <div className="loading-state">Loading projects...</div>
        ) : projects.length === 0 ? (
          <div className="empty-state">
            <p>No projects found.</p>
            <Button onClick={() => setShowCreate(true)} variant="primary">
              Create your first project
            </Button>
          </div>
        ) : (
          projects.map((project) => (
            <Card key={project.id} className="project-card">
              <div className="project-content">
                <div className="project-info">
                  <h3>{project.name}</h3>
                  <p>{project.description || 'No description'}</p>
                </div>
                <div className="project-meta">
                  <span className="project-id">ID: {project.id}</span>
                  <span className="project-date">
                    Created: {new Date(project.createdAt).toLocaleDateString()}
                  </span>
                </div>
                <div className="project-actions">
                  <Button variant="ghost" size="sm">View</Button>
                  <Button variant="ghost" size="sm">Edit</Button>
                  <Button variant="ghost" size="sm">Delete</Button>
                </div>
              </div>
            </Card>
          ))
        )}
      </div>
    </div>
  )
}
