// Package projects provides project management functionality.
package projects

import (
	"errors"
	"time"

	"github.com/google/uuid"
)

// Common errors for project operations.
var (
	ErrProjectNotFound = errors.New("project not found")
)

// Project represents a project within an organization.
// Matches the OpenAPI Project schema.
type Project struct {
	ID             uuid.UUID `json:"id"`
	OrganizationID uuid.UUID `json:"organization_id"`
	Name           string    `json:"name"`
	Description    *string   `json:"description,omitempty"`
	CreatedAt      time.Time `json:"created_at"`
	UpdatedAt      time.Time `json:"updated_at"`
}

// CreateProjectRequest represents the API request for creating a new project.
// Matches OpenAPI CreateProjectRequest schema.
type CreateProjectRequest struct {
	Name        string  `json:"name" validate:"required,min=2,max=255"`
	Description *string `json:"description,omitempty"`
}

// CreateProjectInput represents the input for creating a new project (internal).
type CreateProjectInput struct {
	OrganizationID uuid.UUID
	Name           string
	Description    *string
}

// UpdateProjectInput represents the input for updating a project.
type UpdateProjectInput struct {
	Name        *string `json:"name,omitempty"`
	Description *string `json:"description,omitempty"`
}

// ProjectResponse represents the API response for a project.
// Matches OpenAPI Project schema.
type ProjectResponse struct {
	ID             uuid.UUID `json:"id"`
	OrganizationID uuid.UUID `json:"organization_id"`
	Name           string    `json:"name"`
	Description    *string   `json:"description,omitempty"`
	CreatedAt      time.Time `json:"created_at"`
	UpdatedAt      time.Time `json:"updated_at"`
}

// ToResponse converts a Project to a ProjectResponse.
func (p *Project) ToResponse() *ProjectResponse {
	return &ProjectResponse{
		ID:             p.ID,
		OrganizationID: p.OrganizationID,
		Name:           p.Name,
		Description:    p.Description,
		CreatedAt:      p.CreatedAt,
		UpdatedAt:      p.UpdatedAt,
	}
}
