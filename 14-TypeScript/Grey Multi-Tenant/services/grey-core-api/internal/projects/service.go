// Package projects provides project management functionality.
package projects

import (
	"context"

	"github.com/google/uuid"
)

// Service provides project business logic.
type Service struct {
	repo Repository
}

// NewService creates a new project service.
func NewService(repo Repository) *Service {
	return &Service{repo: repo}
}

// Create creates a new project.
func (s *Service) Create(ctx context.Context, input *CreateProjectInput) (*Project, error) {
	project := &Project{
		OrganizationID: input.OrganizationID,
		Name:           input.Name,
		Description:    input.Description,
	}

	if err := s.repo.Create(ctx, project); err != nil {
		return nil, err
	}

	return project, nil
}

// GetByID retrieves a project by its ID.
func (s *Service) GetByID(ctx context.Context, id uuid.UUID) (*Project, error) {
	return s.repo.GetByID(ctx, id)
}

// GetByIDAndOrg retrieves a project by ID with organization validation.
// This enforces multi-tenant isolation.
func (s *Service) GetByIDAndOrg(ctx context.Context, id, orgID uuid.UUID) (*Project, error) {
	return s.repo.GetByIDAndOrg(ctx, id, orgID)
}

// Update updates a project's information.
func (s *Service) Update(ctx context.Context, id uuid.UUID, input *UpdateProjectInput) (*Project, error) {
	project, err := s.repo.GetByID(ctx, id)
	if err != nil {
		return nil, err
	}

	// Apply updates
	if input.Name != nil {
		project.Name = *input.Name
	}
	if input.Description != nil {
		project.Description = input.Description
	}

	if err := s.repo.Update(ctx, project); err != nil {
		return nil, err
	}

	return project, nil
}

// Delete removes a project.
func (s *Service) Delete(ctx context.Context, id uuid.UUID) error {
	return s.repo.Delete(ctx, id)
}

// ListByOrganization retrieves paginated projects for an organization.
func (s *Service) ListByOrganization(ctx context.Context, orgID uuid.UUID, limit, offset int) ([]*Project, int, error) {
	return s.repo.ListByOrganization(ctx, orgID, limit, offset)
}
