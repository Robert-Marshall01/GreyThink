// Package main is the entry point for the Grey Core API service.
// Grey Multi-Tenant: A multi-tenant operational platform.
package main

import (
	"context"
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/grey-platform/grey-core-api/internal/config"
	"github.com/grey-platform/grey-core-api/internal/db"
	httpserver "github.com/grey-platform/grey-core-api/internal/http"

	"go.uber.org/zap"
)

func main() {
	// Initialize logger
	logger, err := initLogger()
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to initialize logger: %v\n", err)
		os.Exit(1)
	}
	defer logger.Sync()

	// Load configuration
	cfg, err := config.Load()
	if err != nil {
		logger.Fatal("failed to load configuration", zap.Error(err))
	}

	logger.Info("starting Grey Core API",
		zap.String("environment", cfg.Environment),
		zap.String("host", cfg.Server.Host),
		zap.Int("port", cfg.Server.Port),
	)

	// Initialize database connection with retries
	var database *db.Database
	var dbErr error
	maxRetries := 30
	retryDelay := 2 * time.Second

	logger.Info("connecting to database...",
		zap.String("url", maskDatabaseURL(cfg.Database.URL)),
	)

	for i := 0; i < maxRetries; i++ {
		database, dbErr = db.New(context.Background(), cfg.Database)
		if dbErr == nil {
			break
		}
		if i < maxRetries-1 {
			logger.Warn("database connection failed, retrying...",
				zap.Int("attempt", i+1),
				zap.Int("maxRetries", maxRetries),
				zap.Duration("retryIn", retryDelay),
				zap.Error(dbErr),
			)
			time.Sleep(retryDelay)
		}
	}
	if dbErr != nil {
		logger.Fatal("failed to connect to database after retries",
			zap.Int("attempts", maxRetries),
			zap.Error(dbErr),
		)
	}
	defer database.Close()

	logger.Info("database connection established")

	// Create HTTP server
	server := httpserver.NewServer(cfg, database, logger)

	// Start server in goroutine
	go func() {
		addr := fmt.Sprintf("%s:%d", cfg.Server.Host, cfg.Server.Port)
		logger.Info("HTTP server listening", zap.String("address", addr))
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			logger.Fatal("HTTP server error", zap.Error(err))
		}
	}()

	// Graceful shutdown
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	<-quit

	logger.Info("shutting down server...")

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	if err := server.Shutdown(ctx); err != nil {
		logger.Fatal("server forced to shutdown", zap.Error(err))
	}

	logger.Info("server exited gracefully")
}

// initLogger creates a production or development logger based on environment.
func initLogger() (*zap.Logger, error) {
	env := os.Getenv("ENVIRONMENT")
	if env == "production" {
		return zap.NewProduction()
	}
	return zap.NewDevelopment()
}

// maskDatabaseURL masks the password in a database URL for safe logging.
func maskDatabaseURL(url string) string {
	// Simple masking: replace password portion
	// Format: postgres://user:password@host:port/db
	if len(url) == 0 {
		return "<not configured>"
	}
	// Find :// and @ to mask password
	start := 0
	for i := 0; i < len(url)-2; i++ {
		if url[i:i+3] == "://" {
			start = i + 3
			break
		}
	}
	atPos := -1
	for i := start; i < len(url); i++ {
		if url[i] == '@' {
			atPos = i
			break
		}
	}
	if atPos == -1 {
		return url
	}
	colonPos := -1
	for i := start; i < atPos; i++ {
		if url[i] == ':' {
			colonPos = i
			break
		}
	}
	if colonPos == -1 {
		return url
	}
	return url[:colonPos+1] + "****" + url[atPos:]
}
