// Package auth provides handler implementations for authentication endpoints.
package auth

import (
	"encoding/json"
	"errors"
	"net/http"

	"github.com/google/uuid"
	"github.com/grey-platform/grey-core-api/internal/response"
	"github.com/grey-platform/grey-core-api/internal/users"
	"go.uber.org/zap"
)

// Handler handles HTTP requests for authentication operations.
type Handler struct {
	authService *Service
	userService *users.Service
	logger      *zap.Logger
}

// NewHandler creates a new auth handler.
func NewHandler(authService *Service, userService *users.Service, logger *zap.Logger) *Handler {
	return &Handler{
		authService: authService,
		userService: userService,
		logger:      logger,
	}
}

// Login handles POST /auth/login - authenticates a user.
//
// Request body:
//   - email: string (required)
//   - password: string (required)
//
// Response: AuthSessionResponse with access and refresh tokens
func (h *Handler) Login(w http.ResponseWriter, r *http.Request) {
	var req LoginRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		response.WriteError(w, http.StatusBadRequest, "invalid_request", "Invalid JSON body")
		return
	}

	// Validate request
	if req.Email == "" || req.Password == "" {
		response.WriteError(w, http.StatusBadRequest, "validation_error", "Email and password are required")
		return
	}

	// Authenticate user
	user, err := h.userService.Authenticate(r.Context(), req.Email, req.Password)
	if err != nil {
		if errors.Is(err, users.ErrInvalidCredentials) || errors.Is(err, users.ErrUserNotFound) {
			response.WriteError(w, http.StatusUnauthorized, "invalid_credentials", "Invalid email or password")
			return
		}
		h.logger.Error("authentication failed", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Authentication failed")
		return
	}

	// Generate token pair
	tokens, err := h.authService.GenerateTokenPair(r.Context(), user.ID, user.OrganizationID, user.Email, "member")
	if err != nil {
		h.logger.Error("failed to generate tokens", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to generate session")
		return
	}

	// Calculate expires_in in seconds
	expiresIn := int(h.authService.cfg.AccessTokenExpiry.Seconds())

	response.WriteJSON(w, http.StatusOK, &AuthSessionResponse{
		AccessToken:  tokens.AccessToken,
		RefreshToken: tokens.RefreshToken,
		ExpiresIn:    expiresIn,
	})
}

// Refresh handles POST /auth/refresh - refreshes an access token.
//
// Request body:
//   - refresh_token: string (required)
//
// Response: AuthSessionResponse with new access and refresh tokens
func (h *Handler) Refresh(w http.ResponseWriter, r *http.Request) {
	var req RefreshTokenRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		response.WriteError(w, http.StatusBadRequest, "invalid_request", "Invalid JSON body")
		return
	}

	// Validate request
	if req.RefreshToken == "" {
		response.WriteError(w, http.StatusBadRequest, "validation_error", "Refresh token is required")
		return
	}

	// Refresh tokens
	tokens, err := h.authService.RefreshTokens(r.Context(), req.RefreshToken)
	if err != nil {
		if errors.Is(err, ErrInvalidToken) {
			response.WriteError(w, http.StatusUnauthorized, "invalid_token", "Invalid or expired refresh token")
			return
		}
		h.logger.Error("failed to refresh tokens", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to refresh session")
		return
	}

	// Calculate expires_in in seconds
	expiresIn := int(h.authService.cfg.AccessTokenExpiry.Seconds())

	response.WriteJSON(w, http.StatusOK, &AuthSessionResponse{
		AccessToken:  tokens.AccessToken,
		RefreshToken: tokens.RefreshToken,
		ExpiresIn:    expiresIn,
	})
}

// Register handles POST /auth/register - creates a new user account.
//
// Request body:
//   - email: string (required)
//   - password: string (required, min 6 chars)
//   - name: string (required)
//
// Response: AuthSessionResponse with access and refresh tokens
func (h *Handler) Register(w http.ResponseWriter, r *http.Request) {
	var req RegisterRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		response.WriteError(w, http.StatusBadRequest, "invalid_request", "Invalid JSON body")
		return
	}

	// Validate request
	if req.Email == "" || req.Password == "" || req.Name == "" {
		response.WriteError(w, http.StatusBadRequest, "validation_error", "Email, password, and name are required")
		return
	}

	if len(req.Password) < 6 {
		response.WriteError(w, http.StatusBadRequest, "validation_error", "Password must be at least 6 characters")
		return
	}

	// Check if user already exists
	existing, _ := h.userService.GetByEmailAnyOrg(r.Context(), req.Email)
	if existing != nil {
		response.WriteError(w, http.StatusConflict, "user_exists", "A user with this email already exists")
		return
	}

	// Use a default organization ID for new registrations
	// In a real system, you might create a new org or use a default one
	orgID := uuid.MustParse("00000000-0000-0000-0000-000000000001")

	// Create user
	user, err := h.userService.CreateWithPassword(r.Context(), orgID, req.Email, req.Name, req.Password)
	if err != nil {
		if errors.Is(err, users.ErrUserEmailExists) {
			response.WriteError(w, http.StatusConflict, "user_exists", "A user with this email already exists")
			return
		}
		h.logger.Error("failed to create user", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to create user")
		return
	}

	// Generate token pair
	tokens, err := h.authService.GenerateTokenPair(r.Context(), user.ID, user.OrganizationID, user.Email, "member")
	if err != nil {
		h.logger.Error("failed to generate tokens", zap.Error(err))
		response.WriteError(w, http.StatusInternalServerError, "internal_error", "Failed to generate session")
		return
	}

	// Calculate expires_in in seconds
	expiresIn := int(h.authService.cfg.AccessTokenExpiry.Seconds())

	response.WriteJSON(w, http.StatusCreated, &AuthSessionResponse{
		AccessToken:  tokens.AccessToken,
		RefreshToken: tokens.RefreshToken,
		ExpiresIn:    expiresIn,
	})
}
