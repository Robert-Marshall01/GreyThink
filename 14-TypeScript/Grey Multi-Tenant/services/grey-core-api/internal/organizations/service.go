// Package organizations provides organization (tenant) management functionality.
package organizations

import (
	"context"

	"github.com/google/uuid"
)

// Service provides organization business logic.
type Service struct {
	repo Repository
}

// NewService creates a new organization service.
func NewService(repo Repository) *Service {
	return &Service{repo: repo}
}

// Create creates a new organization.
func (s *Service) Create(ctx context.Context, input *CreateOrganizationInput) (*Organization, error) {
	org := &Organization{
		Name: input.Name,
	}

	if err := s.repo.Create(ctx, org); err != nil {
		return nil, err
	}

	return org, nil
}

// GetByID retrieves an organization by its ID.
func (s *Service) GetByID(ctx context.Context, id uuid.UUID) (*Organization, error) {
	return s.repo.GetByID(ctx, id)
}

// Update updates an organization's information.
func (s *Service) Update(ctx context.Context, id uuid.UUID, input *UpdateOrganizationInput) (*Organization, error) {
	org, err := s.repo.GetByID(ctx, id)
	if err != nil {
		return nil, err
	}

	// Apply updates
	if input.Name != nil {
		org.Name = *input.Name
	}

	if err := s.repo.Update(ctx, org); err != nil {
		return nil, err
	}

	return org, nil
}

// Delete removes an organization.
func (s *Service) Delete(ctx context.Context, id uuid.UUID) error {
	return s.repo.Delete(ctx, id)
}

// List retrieves paginated organizations.
func (s *Service) List(ctx context.Context, limit, offset int) ([]*Organization, int, error) {
	return s.repo.List(ctx, limit, offset)
}
