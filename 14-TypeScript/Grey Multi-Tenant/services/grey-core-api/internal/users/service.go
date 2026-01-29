// Package users provides user management functionality.
package users

import (
	"context"
	"fmt"

	"github.com/google/uuid"
	"github.com/grey-platform/grey-core-api/internal/crypto"
)

// Service provides user business logic.
type Service struct {
	repo Repository
}

// NewService creates a new user service.
func NewService(repo Repository) *Service {
	return &Service{repo: repo}
}

// Create creates a new user with a hashed password.
func (s *Service) Create(ctx context.Context, input *CreateUserInput) (*User, error) {
	user := &User{
		OrganizationID: input.OrganizationID,
		Email:          input.Email,
		Name:           input.Name,
		PasswordHash:   input.PasswordHash,
	}

	if err := s.repo.Create(ctx, user); err != nil {
		return nil, err
	}

	return user, nil
}

// CreateWithPassword creates a user with a raw password that will be hashed.
func (s *Service) CreateWithPassword(ctx context.Context, orgID uuid.UUID, email, name, password string) (*User, error) {
	hashedPassword, err := crypto.HashPassword(password)
	if err != nil {
		return nil, fmt.Errorf("hashing password: %w", err)
	}

	return s.Create(ctx, &CreateUserInput{
		OrganizationID: orgID,
		Email:          email,
		Name:           name,
		PasswordHash:   hashedPassword,
	})
}

// GetByID retrieves a user by their ID.
func (s *Service) GetByID(ctx context.Context, id uuid.UUID) (*User, error) {
	return s.repo.GetByID(ctx, id)
}

// GetByEmail retrieves a user by email within an organization.
func (s *Service) GetByEmail(ctx context.Context, orgID uuid.UUID, email string) (*User, error) {
	return s.repo.GetByEmail(ctx, orgID, email)
}

// GetByEmailAnyOrg retrieves a user by email across all organizations.
func (s *Service) GetByEmailAnyOrg(ctx context.Context, email string) (*User, error) {
	return s.repo.GetByEmailAnyOrg(ctx, email)
}

// Update updates a user's information.
func (s *Service) Update(ctx context.Context, id uuid.UUID, input *UpdateUserInput) (*User, error) {
	user, err := s.repo.GetByID(ctx, id)
	if err != nil {
		return nil, err
	}

	// Apply updates
	if input.Name != nil {
		user.Name = *input.Name
	}

	if err := s.repo.Update(ctx, user); err != nil {
		return nil, err
	}

	return user, nil
}

// Delete removes a user.
func (s *Service) Delete(ctx context.Context, id uuid.UUID) error {
	return s.repo.Delete(ctx, id)
}

// ListByOrganization retrieves paginated users for an organization.
func (s *Service) ListByOrganization(ctx context.Context, orgID uuid.UUID, limit, offset int) ([]*User, int, error) {
	return s.repo.ListByOrganization(ctx, orgID, limit, offset)
}

// Authenticate validates user credentials and returns the user if valid.
func (s *Service) Authenticate(ctx context.Context, email, password string) (*User, error) {
	user, err := s.repo.GetByEmailAnyOrg(ctx, email)
	if err != nil {
		return nil, ErrInvalidCredentials
	}

	if !crypto.CheckPassword(password, user.PasswordHash) {
		return nil, ErrInvalidCredentials
	}

	return user, nil
}
