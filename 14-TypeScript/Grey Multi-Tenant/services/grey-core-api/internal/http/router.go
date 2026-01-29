// Package http provides the HTTP server and routing for the API.
package http

import (
	"fmt"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/go-chi/chi/v5/middleware"
	"github.com/go-chi/cors"
	"github.com/grey-platform/grey-core-api/internal/auth"
	"github.com/grey-platform/grey-core-api/internal/config"
	"github.com/grey-platform/grey-core-api/internal/db"
	httpMiddleware "github.com/grey-platform/grey-core-api/internal/http/middleware"
	"github.com/grey-platform/grey-core-api/internal/organizations"
	"github.com/grey-platform/grey-core-api/internal/projects"
	"github.com/grey-platform/grey-core-api/internal/users"

	"go.uber.org/zap"
)

// NewServer creates and configures a new HTTP server.
func NewServer(cfg *config.Config, database *db.Database, logger *zap.Logger) *http.Server {
	router := NewRouter(cfg, database, logger)

	return &http.Server{
		Addr:         fmt.Sprintf("%s:%d", cfg.Server.Host, cfg.Server.Port),
		Handler:      router,
		ReadTimeout:  cfg.Server.ReadTimeout,
		WriteTimeout: cfg.Server.WriteTimeout,
	}
}

// NewRouter creates the main router with all routes and middleware.
func NewRouter(cfg *config.Config, database *db.Database, logger *zap.Logger) *chi.Mux {
	r := chi.NewRouter()

	// Global middleware
	r.Use(middleware.RealIP)
	r.Use(httpMiddleware.RequestID)
	r.Use(httpMiddleware.Logger(logger))
	r.Use(middleware.Recoverer)
	r.Use(middleware.Timeout(30 * time.Second))

	// CORS configuration
	r.Use(cors.Handler(cors.Options{
		AllowedOrigins:   cfg.CORS.AllowedOrigins,
		AllowedMethods:   cfg.CORS.AllowedMethods,
		AllowedHeaders:   cfg.CORS.AllowedHeaders,
		ExposedHeaders:   []string{"Link", "X-Request-ID"},
		AllowCredentials: true,
		MaxAge:           300,
	}))

	// Initialize repositories
	userRepo := users.NewPostgresRepository(database.Pool)
	orgRepo := organizations.NewPostgresRepository(database.Pool)
	projectRepo := projects.NewPostgresRepository(database.Pool)
	authRepo := auth.NewPostgresRepository(database.Pool)

	// Initialize services
	userService := users.NewService(userRepo)
	orgService := organizations.NewService(orgRepo)
	projectService := projects.NewService(projectRepo)
	authService := auth.NewService(cfg.JWT, authRepo)

	// Initialize handlers
	authHandler := auth.NewHandler(authService, userService, logger)
	userHandler := users.NewHandler(userService, logger)
	orgHandler := organizations.NewHandler(orgService, logger)
	projectHandler := projects.NewHandler(projectService, logger)

	// Health check endpoint
	r.Get("/health", healthHandler(database))

	// API v1 routes
	r.Route("/api/v1", func(r chi.Router) {
		// Public routes (no auth required)
		r.Group(func(r chi.Router) {
			r.Post("/auth/login", authHandler.Login)
			r.Post("/auth/register", authHandler.Register)
			r.Post("/auth/refresh", authHandler.Refresh)
		})

		// Protected routes (auth required)
		r.Group(func(r chi.Router) {
			r.Use(httpMiddleware.JWTAuth(cfg.JWT))

			// Current user shorthand
			r.Get("/user", userHandler.GetCurrent)

			// User routes
			r.Route("/users", func(r chi.Router) {
				r.Post("/", userHandler.Create)
				r.Get("/me", userHandler.GetCurrent)
			})

			// Organization routes
			r.Route("/organizations", func(r chi.Router) {
				r.Post("/", orgHandler.Create)
				r.Get("/{id}", orgHandler.Get)
			})

			// Project routes
			r.Route("/projects", func(r chi.Router) {
				r.Post("/", projectHandler.Create)
				r.Get("/", projectHandler.List)
				r.Get("/{id}", projectHandler.Get)
			})
		})
	})

	return r
}

// healthHandler returns the health check handler.
func healthHandler(database *db.Database) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if err := database.HealthCheck(r.Context()); err != nil {
			WriteError(w, http.StatusServiceUnavailable, "unhealthy", "Database connection failed")
			return
		}
		WriteJSON(w, http.StatusOK, map[string]string{
			"status": "healthy",
			"time":   time.Now().UTC().Format(time.RFC3339),
		})
	}
}
