// Package users provides user management functionality.
package users

import (
	"errors"
	"time"

	"github.com/google/uuid"
)

// Common errors for user operations.
var (
	ErrUserNotFound       = errors.New("user not found")
	ErrUserEmailExists    = errors.New("email already exists")
	ErrInvalidCredentials = errors.New("invalid credentials")
)

// User represents a user in the system.
// Matches the OpenAPI User schema.
type User struct {
	ID             uuid.UUID `json:"id"`
	OrganizationID uuid.UUID `json:"organization_id"`
	Email          string    `json:"email"`
	Name           string    `json:"name"`
	PasswordHash   string    `json:"-"` // Never serialize password
	CreatedAt      time.Time `json:"created_at"`
	UpdatedAt      time.Time `json:"updated_at"`
}

// CreateUserRequest represents the API request for creating a new user.
// Matches OpenAPI CreateUserRequest schema.
type CreateUserRequest struct {
	Email    string `json:"email" validate:"required,email"`
	Name     string `json:"name" validate:"required,min=1,max=255"`
	Password string `json:"password" validate:"required,min=8"`
}

// CreateUserInput represents the input for creating a new user (internal).
type CreateUserInput struct {
	OrganizationID uuid.UUID
	Email          string
	Name           string
	PasswordHash   string
}

// UpdateUserInput represents the input for updating a user.
type UpdateUserInput struct {
	Name *string `json:"name,omitempty"`
}

// UserResponse represents the API response for a user.
// Matches OpenAPI User schema.
type UserResponse struct {
	ID             uuid.UUID `json:"id"`
	Email          string    `json:"email"`
	Name           string    `json:"name"`
	OrganizationID uuid.UUID `json:"organization_id"`
	CreatedAt      time.Time `json:"created_at"`
	UpdatedAt      time.Time `json:"updated_at"`
}

// ToResponse converts a User to a UserResponse.
func (u *User) ToResponse() *UserResponse {
	return &UserResponse{
		ID:             u.ID,
		Email:          u.Email,
		Name:           u.Name,
		OrganizationID: u.OrganizationID,
		CreatedAt:      u.CreatedAt,
		UpdatedAt:      u.UpdatedAt,
	}
}
