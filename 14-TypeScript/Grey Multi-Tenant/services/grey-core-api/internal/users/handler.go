// Package users provides handler implementations for user endpoints.
package users

import (
	"encoding/json"
	"errors"
	"net/http"

	"github.com/google/uuid"
	"github.com/grey-platform/grey-core-api/internal/crypto"
	"github.com/grey-platform/grey-core-api/internal/http/middleware"
	"github.com/grey-platform/grey-core-api/internal/response"
	"go.uber.org/zap"
)

// Handler handles HTTP requests for user operations.
type Handler struct {
	service *Service
	logger  *zap.Logger
}

// NewHandler creates a new user handler.
func NewHandler(service *Service, logger *zap.Logger) *Handler {
	return &Handler{
		service: service,
		logger:  logger,
	}
}

// Create handles POST /users - creates a new user.
//
// Request body:
//   - email: string (required)
//   - name: string (required)
//   - password: string (required, min 8 chars)
//
// Response: UserResponse
func (h *Handler) Create(w http.ResponseWriter, r *http.Request) {
	var req CreateUserRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		response.WriteError(w, http.StatusBadRequest, "invalid_request", "Invalid JSON body")
		return
	}

	// Basic validation
	if req.Email == "" || req.Name == "" || req.Password == "" {
		response.WriteError(w, http.StatusBadRequest, "validation_error", "Email, name, and password are required")
		return
	}

	if len(req.Password) < 8 {
		response.WriteError(w, http.StatusBadRequest, "validation_error", "Password must be at least 8 characters")
		return
	}

	// Get organization ID from context
	orgID, ok := middleware.GetOrganizationID(r.Context())
	if !ok {
		response.WriteError(w, http.StatusUnauthorized, "unauthorized", "Organization context not found")
		return
	}

	// Hash password
	passwordHash, err := crypto.HashPassword(req.Password)
	if err != nil {
		h.logger.Error("failed to hash password", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to process request")
		return
	}

	// Create user
	user, err := h.service.Create(r.Context(), &CreateUserInput{
		OrganizationID: orgID,
		Email:          req.Email,
		Name:           req.Name,
		PasswordHash:   passwordHash,
	})
	if err != nil {
		if errors.Is(err, ErrUserEmailExists) {
			response.WriteError(w, http.StatusConflict, "email_exists", "A user with this email already exists")
			return
		}
		h.logger.Error("failed to create user", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to create user")
		return
	}

	response.WriteJSON(w, http.StatusCreated, user.ToResponse())
}

// GetCurrent handles GET /users/me - returns the authenticated user.
//
// Response: UserResponse
func (h *Handler) GetCurrent(w http.ResponseWriter, r *http.Request) {
	userID, ok := middleware.GetUserID(r.Context())
	if !ok {
		response.WriteError(w, http.StatusUnauthorized, "unauthorized", "User not authenticated")
		return
	}

	user, err := h.service.GetByID(r.Context(), userID)
	if err != nil {
		if errors.Is(err, ErrUserNotFound) {
			response.WriteError(w, http.StatusNotFound, "not_found", "User not found")
			return
		}
		h.logger.Error("failed to get user", zap.Error(err), zap.String("user_id", userID.String()))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to get user")
		return
	}

	response.WriteJSON(w, http.StatusOK, user.ToResponse())
}

// GetByID handles GET /users/{id} - returns a user by ID.
// Validates multi-tenant access.
func (h *Handler) GetByID(w http.ResponseWriter, r *http.Request, id uuid.UUID) {
	orgID, ok := middleware.GetOrganizationID(r.Context())
	if !ok {
		response.WriteError(w, http.StatusUnauthorized, "unauthorized", "Organization context not found")
		return
	}

	user, err := h.service.GetByID(r.Context(), id)
	if err != nil {
		if errors.Is(err, ErrUserNotFound) {
			response.WriteError(w, http.StatusNotFound, "not_found", "User not found")
			return
		}
		h.logger.Error("failed to get user", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to get user")
		return
	}

	// Verify multi-tenant access
	if user.OrganizationID != orgID {
		response.WriteError(w, http.StatusNotFound, "not_found", "User not found")
		return
	}

	response.WriteJSON(w, http.StatusOK, user.ToResponse())
}
