// Package middleware provides HTTP middleware for the API.
package middleware

import (
	"context"
	"net/http"
	"strings"
	"time"

	"github.com/go-chi/chi/v5/middleware"
	"github.com/golang-jwt/jwt/v5"
	"github.com/google/uuid"
	"github.com/grey-platform/grey-core-api/internal/config"

	"go.uber.org/zap"
)

// Context keys for request values.
type contextKey string

const (
	// RequestIDKey is the context key for the request ID.
	RequestIDKey contextKey = "request_id"
	// UserIDKey is the context key for the authenticated user ID.
	UserIDKey contextKey = "user_id"
	// OrganizationIDKey is the context key for the organization ID.
	OrganizationIDKey contextKey = "organization_id"
	// UserEmailKey is the context key for the user email.
	UserEmailKey contextKey = "user_email"
	// UserRoleKey is the context key for the user role.
	UserRoleKey contextKey = "user_role"
)

// RequestID middleware generates a unique request ID for each request.
func RequestID(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requestID := r.Header.Get("X-Request-ID")
		if requestID == "" {
			requestID = uuid.New().String()
		}

		ctx := context.WithValue(r.Context(), RequestIDKey, requestID)
		w.Header().Set("X-Request-ID", requestID)
		next.ServeHTTP(w, r.WithContext(ctx))
	})
}

// Logger middleware logs HTTP requests using zap.
func Logger(logger *zap.Logger) func(next http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			start := time.Now()
			ww := middleware.NewWrapResponseWriter(w, r.ProtoMajor)

			defer func() {
				requestID, _ := r.Context().Value(RequestIDKey).(string)
				logger.Info("http request",
					zap.String("request_id", requestID),
					zap.String("method", r.Method),
					zap.String("path", r.URL.Path),
					zap.Int("status", ww.Status()),
					zap.Int("bytes", ww.BytesWritten()),
					zap.Duration("duration", time.Since(start)),
					zap.String("remote_addr", r.RemoteAddr),
					zap.String("user_agent", r.UserAgent()),
				)
			}()

			next.ServeHTTP(ww, r)
		})
	}
}

// JWTAuth middleware validates JWT tokens and extracts claims.
func JWTAuth(cfg config.JWTConfig) func(next http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			authHeader := r.Header.Get("Authorization")
			if authHeader == "" {
				writeUnauthorized(w, "missing authorization header")
				return
			}

			// Extract token from "Bearer <token>"
			parts := strings.SplitN(authHeader, " ", 2)
			if len(parts) != 2 || strings.ToLower(parts[0]) != "bearer" {
				writeUnauthorized(w, "invalid authorization header format")
				return
			}

			tokenString := parts[1]

			// Parse and validate token
			token, err := jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
				if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
					return nil, jwt.ErrSignatureInvalid
				}
				return []byte(cfg.Secret), nil
			})

			if err != nil || !token.Valid {
				writeUnauthorized(w, "invalid or expired token")
				return
			}

			claims, ok := token.Claims.(jwt.MapClaims)
			if !ok {
				writeUnauthorized(w, "invalid token claims")
				return
			}

			// Extract user info from claims
			userIDStr, _ := claims["user_id"].(string)
			orgIDStr, _ := claims["organization_id"].(string)
			email, _ := claims["email"].(string)
			role, _ := claims["role"].(string)

			userID, err := uuid.Parse(userIDStr)
			if err != nil {
				writeUnauthorized(w, "invalid user ID in token")
				return
			}

			orgID, err := uuid.Parse(orgIDStr)
			if err != nil {
				writeUnauthorized(w, "invalid organization ID in token")
				return
			}

			// Add claims to context
			ctx := r.Context()
			ctx = context.WithValue(ctx, UserIDKey, userID)
			ctx = context.WithValue(ctx, OrganizationIDKey, orgID)
			ctx = context.WithValue(ctx, UserEmailKey, email)
			ctx = context.WithValue(ctx, UserRoleKey, role)

			next.ServeHTTP(w, r.WithContext(ctx))
		})
	}
}

// GetUserID extracts the user ID from the request context.
func GetUserID(ctx context.Context) (uuid.UUID, bool) {
	id, ok := ctx.Value(UserIDKey).(uuid.UUID)
	return id, ok
}

// GetOrganizationID extracts the organization ID from the request context.
func GetOrganizationID(ctx context.Context) (uuid.UUID, bool) {
	id, ok := ctx.Value(OrganizationIDKey).(uuid.UUID)
	return id, ok
}

// GetUserEmail extracts the user email from the request context.
func GetUserEmail(ctx context.Context) (string, bool) {
	email, ok := ctx.Value(UserEmailKey).(string)
	return email, ok
}

// GetUserRole extracts the user role from the request context.
func GetUserRole(ctx context.Context) (string, bool) {
	role, ok := ctx.Value(UserRoleKey).(string)
	return role, ok
}

// GetRequestID extracts the request ID from the request context.
func GetRequestID(ctx context.Context) string {
	id, _ := ctx.Value(RequestIDKey).(string)
	return id
}

// writeUnauthorized writes a 401 Unauthorized response matching OpenAPI ErrorResponse schema.
func writeUnauthorized(w http.ResponseWriter, message string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusUnauthorized)
	w.Write([]byte(`{"error":"unauthorized","message":"` + message + `"}`))
}

// RequireRole middleware ensures the user has one of the specified roles.
func RequireRole(roles ...string) func(next http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			userRole, ok := GetUserRole(r.Context())
			if !ok {
				writeUnauthorized(w, "user role not found")
				return
			}

			for _, role := range roles {
				if userRole == role {
					next.ServeHTTP(w, r)
					return
				}
			}

			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusForbidden)
			w.Write([]byte(`{"error":"forbidden","message":"insufficient permissions"}`))
		})
	}
}

// OrganizationScope middleware extracts organization from X-Organization-ID header
// or falls back to the organization ID from the JWT token.
// This allows users who belong to multiple organizations to switch contexts.
func OrganizationScope(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		ctx := r.Context()

		// Check for explicit organization header
		orgHeader := r.Header.Get("X-Organization-ID")
		if orgHeader != "" {
			orgID, err := uuid.Parse(orgHeader)
			if err != nil {
				w.Header().Set("Content-Type", "application/json")
				w.WriteHeader(http.StatusBadRequest)
				w.Write([]byte(`{"error":"invalid_organization","message":"Invalid X-Organization-ID header format"}`))
				return
			}

			// TODO: Validate the user has access to this organization
			// For now, we trust the header if it's a valid UUID
			// In production, verify membership via database lookup

			ctx = context.WithValue(ctx, OrganizationIDKey, orgID)
		}

		// If no header and no org in context, check if JWT provided one
		if _, ok := GetOrganizationID(ctx); !ok {
			writeUnauthorized(w, "organization context required")
			return
		}

		next.ServeHTTP(w, r.WithContext(ctx))
	})
}

// RequireOrganization middleware ensures an organization context is present.
// Use this on routes that require organization scoping.
func RequireOrganization(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		_, ok := GetOrganizationID(r.Context())
		if !ok {
			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte(`{"error":"missing_organization","message":"Organization context is required for this operation"}`))
			return
		}
		next.ServeHTTP(w, r)
	})
}
