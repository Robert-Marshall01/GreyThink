// Package organizations provides organization (tenant) management functionality.
package organizations

import (
	"errors"
	"time"

	"github.com/google/uuid"
)

// Common errors for organization operations.
var (
	ErrOrganizationNotFound = errors.New("organization not found")
)

// Organization represents a tenant in the multi-tenant system.
// Matches the OpenAPI Organization schema.
type Organization struct {
	ID        uuid.UUID `json:"id"`
	Name      string    `json:"name"`
	CreatedAt time.Time `json:"created_at"`
	UpdatedAt time.Time `json:"updated_at"`
}

// CreateOrganizationRequest represents the API request for creating a new organization.
// Matches OpenAPI CreateOrganizationRequest schema.
type CreateOrganizationRequest struct {
	Name string `json:"name" validate:"required,min=2,max=255"`
}

// CreateOrganizationInput represents the input for creating a new organization (internal).
type CreateOrganizationInput struct {
	Name string
}

// UpdateOrganizationInput represents the input for updating an organization.
type UpdateOrganizationInput struct {
	Name *string `json:"name,omitempty"`
}

// OrganizationResponse represents the API response for an organization.
// Matches OpenAPI Organization schema.
type OrganizationResponse struct {
	ID        uuid.UUID `json:"id"`
	Name      string    `json:"name"`
	CreatedAt time.Time `json:"created_at"`
	UpdatedAt time.Time `json:"updated_at"`
}

// ToResponse converts an Organization to an OrganizationResponse.
func (o *Organization) ToResponse() *OrganizationResponse {
	return &OrganizationResponse{
		ID:        o.ID,
		Name:      o.Name,
		CreatedAt: o.CreatedAt,
		UpdatedAt: o.UpdatedAt,
	}
}
