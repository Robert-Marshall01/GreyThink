// Package projects provides handler implementations for project endpoints.
package projects

import (
	"encoding/json"
	"errors"
	"net/http"
	"strconv"

	"github.com/go-chi/chi/v5"
	"github.com/google/uuid"
	"github.com/grey-platform/grey-core-api/internal/http/middleware"
	"github.com/grey-platform/grey-core-api/internal/response"
	"go.uber.org/zap"
)

// Handler handles HTTP requests for project operations.
type Handler struct {
	service *Service
	logger  *zap.Logger
}

// NewHandler creates a new project handler.
func NewHandler(service *Service, logger *zap.Logger) *Handler {
	return &Handler{
		service: service,
		logger:  logger,
	}
}

// Create handles POST /projects - creates a new project.
//
// Request body:
//   - name: string (required)
//   - description: string (optional)
//
// Response: ProjectResponse
func (h *Handler) Create(w http.ResponseWriter, r *http.Request) {
	var req CreateProjectRequest
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

	// Get organization ID from context
	orgID, ok := middleware.GetOrganizationID(r.Context())
	if !ok {
		response.WriteError(w, http.StatusUnauthorized, "unauthorized", "Organization context not found")
		return
	}

	// Create project
	project, err := h.service.Create(r.Context(), &CreateProjectInput{
		OrganizationID: orgID,
		Name:           req.Name,
		Description:    req.Description,
	})
	if err != nil {
		h.logger.Error("failed to create project", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to create project")
		return
	}

	response.WriteJSON(w, http.StatusCreated, project.ToResponse())
}

// List handles GET /projects - lists projects for the organization.
//
// Query parameters:
//   - page: int (optional, default: 1)
//   - page_size: int (optional, default: 20)
//
// Response: ProjectListResponse (with pagination)
func (h *Handler) List(w http.ResponseWriter, r *http.Request) {
	// Get organization ID from context
	orgID, ok := middleware.GetOrganizationID(r.Context())
	if !ok {
		response.WriteError(w, http.StatusUnauthorized, "unauthorized", "Organization context not found")
		return
	}

	// Parse pagination parameters
	page := 1
	pageSize := 20

	if pageStr := r.URL.Query().Get("page"); pageStr != "" {
		if p, err := strconv.Atoi(pageStr); err == nil && p > 0 {
			page = p
		}
	}

	if pageSizeStr := r.URL.Query().Get("page_size"); pageSizeStr != "" {
		if ps, err := strconv.Atoi(pageSizeStr); err == nil && ps > 0 && ps <= 100 {
			pageSize = ps
		}
	}

	offset := (page - 1) * pageSize

	// List projects
	projects, total, err := h.service.ListByOrganization(r.Context(), orgID, pageSize, offset)
	if err != nil {
		h.logger.Error("failed to list projects", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to list projects")
		return
	}

	// Convert to response
	data := make([]*ProjectResponse, len(projects))
	for i, p := range projects {
		data[i] = p.ToResponse()
	}

	response.WritePaginated(w, http.StatusOK, data, page, pageSize, total)
}

// Get handles GET /projects/{id} - returns a project by ID.
//
// Path parameters:
//   - id: UUID of the project
//
// Response: ProjectResponse
func (h *Handler) Get(w http.ResponseWriter, r *http.Request) {
	// Parse project ID from URL path
	idStr := chi.URLParam(r, "id")
	id, err := uuid.Parse(idStr)
	if err != nil {
		response.WriteError(w, http.StatusBadRequest, "invalid_id", "Invalid project ID format")
		return
	}

	// Get organization ID from context
	orgID, ok := middleware.GetOrganizationID(r.Context())
	if !ok {
		response.WriteError(w, http.StatusUnauthorized, "unauthorized", "Organization context not found")
		return
	}

	// Get project with org validation (multi-tenant isolation)
	project, err := h.service.GetByIDAndOrg(r.Context(), id, orgID)
	if err != nil {
		if errors.Is(err, ErrProjectNotFound) {
			response.WriteError(w, http.StatusNotFound, "not_found", "Project not found")
			return
		}
		h.logger.Error("failed to get project", zap.Error(err), zap.String("project_id", id.String()))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to get project")
		return
	}

	response.WriteJSON(w, http.StatusOK, project.ToResponse())
}
