// Package organizations provides handler implementations for organization endpoints.
package organizations

import (
	"encoding/json"
	"errors"
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/google/uuid"
	"github.com/grey-platform/grey-core-api/internal/response"
	"go.uber.org/zap"
)

// Handler handles HTTP requests for organization operations.
type Handler struct {
	service *Service
	logger  *zap.Logger
}

// NewHandler creates a new organization handler.
func NewHandler(service *Service, logger *zap.Logger) *Handler {
	return &Handler{
		service: service,
		logger:  logger,
	}
}

// Create handles POST /organizations - creates a new organization.
//
// Request body:
//   - name: string (required)
//
// Response: OrganizationResponse
func (h *Handler) Create(w http.ResponseWriter, r *http.Request) {
	var req CreateOrganizationRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		response.WriteError(w, http.StatusBadRequest, "invalid_request", "Invalid JSON body")
		return
	}

	// Validate request
	if req.Name == "" {
		response.WriteError(w, http.StatusBadRequest, "validation_error", "Name is required")
		return
	}

	if len(req.Name) < 2 || len(req.Name) > 255 {
		response.WriteError(w, http.StatusBadRequest, "validation_error", "Name must be between 2 and 255 characters")
		return
	}

	// Create organization
	org, err := h.service.Create(r.Context(), &CreateOrganizationInput{
		Name: req.Name,
	})
	if err != nil {
		h.logger.Error("failed to create organization", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to create organization")
		return
	}

	response.WriteJSON(w, http.StatusCreated, org.ToResponse())
}

// Get handles GET /organizations/{id} - returns an organization by ID.
//
// Path parameters:
//   - id: UUID of the organization
//
// Response: OrganizationResponse
func (h *Handler) Get(w http.ResponseWriter, r *http.Request) {
	// Parse organization ID from URL path
	idStr := chi.URLParam(r, "id")
	id, err := uuid.Parse(idStr)
	if err != nil {
		response.WriteError(w, http.StatusBadRequest, "invalid_id", "Invalid organization ID format")
		return
	}

	org, err := h.service.GetByID(r.Context(), id)
	if err != nil {
		if errors.Is(err, ErrOrganizationNotFound) {
			response.WriteError(w, http.StatusNotFound, "not_found", "Organization not found")
			return
		}
		h.logger.Error("failed to get organization", zap.Error(err), zap.String("org_id", id.String()))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to get organization")
		return
	}

	response.WriteJSON(w, http.StatusOK, org.ToResponse())
}
