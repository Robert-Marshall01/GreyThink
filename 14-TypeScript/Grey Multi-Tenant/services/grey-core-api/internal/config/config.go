// Package config handles application configuration loading from environment variables.
package config

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"time"

	"github.com/joho/godotenv"
	"github.com/kelseyhightower/envconfig"
)

// Config holds all configuration for the application.
type Config struct {
	Environment string `envconfig:"ENVIRONMENT" default:"development"`
	Server      ServerConfig
	Database    DatabaseConfig
	JWT         JWTConfig
	CORS        CORSConfig
	Log         LogConfig
}

// ServerConfig holds HTTP server configuration.
type ServerConfig struct {
	Host         string        `envconfig:"SERVER_HOST" default:"0.0.0.0"`
	Port         int           `envconfig:"SERVER_PORT" default:"8080"`
	ReadTimeout  time.Duration `envconfig:"SERVER_READ_TIMEOUT" default:"30s"`
	WriteTimeout time.Duration `envconfig:"SERVER_WRITE_TIMEOUT" default:"30s"`
}

// DatabaseConfig holds database configuration.
type DatabaseConfig struct {
	URL            string `envconfig:"DATABASE_URL" required:"true"`
	MaxConnections int32  `envconfig:"DATABASE_MAX_CONNECTIONS" default:"25"`
	MinConnections int32  `envconfig:"DATABASE_MIN_CONNECTIONS" default:"5"`
}

// JWTConfig holds JWT authentication configuration.
type JWTConfig struct {
	Secret             string        `envconfig:"JWT_SECRET" required:"true"`
	AccessTokenExpiry  time.Duration `envconfig:"JWT_ACCESS_TOKEN_EXPIRY" default:"15m"`
	RefreshTokenExpiry time.Duration `envconfig:"JWT_REFRESH_TOKEN_EXPIRY" default:"168h"` // 7 days
	Issuer             string        `envconfig:"JWT_ISSUER" default:"grey-multi-tenant"`
}

// CORSConfig holds CORS configuration.
type CORSConfig struct {
	AllowedOrigins []string `envconfig:"CORS_ALLOWED_ORIGINS" default:"http://localhost:3000"`
	AllowedMethods []string `envconfig:"CORS_ALLOWED_METHODS" default:"GET,POST,PUT,PATCH,DELETE,OPTIONS"`
	AllowedHeaders []string `envconfig:"CORS_ALLOWED_HEADERS" default:"Authorization,Content-Type,X-Request-ID"`
}

// LogConfig holds logging configuration.
type LogConfig struct {
	Level  string `envconfig:"LOG_LEVEL" default:"info"`
	Format string `envconfig:"LOG_FORMAT" default:"json"`
}

// Load reads configuration from environment variables.
// It automatically loads .env files from standard locations before processing.
func Load() (*Config, error) {
	// Load .env files from multiple locations (in order of priority, later overrides earlier)
	loadEnvFiles()

	var cfg Config
	if err := envconfig.Process("", &cfg); err != nil {
		return nil, fmt.Errorf("loading config: %w", err)
	}
	return &cfg, nil
}

// loadEnvFiles attempts to load .env files from standard installation locations.
// Files are loaded in order; later files override earlier ones.
// Errors are silently ignored (file may not exist in all locations).
func loadEnvFiles() {
	var envPaths []string

	// Get executable directory
	execPath, err := os.Executable()
	if err == nil {
		execDir := filepath.Dir(execPath)
		// Check sibling config directory (Windows installed: bin\..\config\.env)
		envPaths = append(envPaths, filepath.Clean(filepath.Join(execDir, "..", "config", ".env")))
		// Check parent directory (development: bin\..\..env)
		envPaths = append(envPaths, filepath.Clean(filepath.Join(execDir, "..", ".env")))
		// Check same directory as executable
		envPaths = append(envPaths, filepath.Join(execDir, ".env"))
	}

	// Platform-specific paths
	switch runtime.GOOS {
	case "windows":
		// Windows: C:\ProgramData\GreyMultiTenant\.env
		programData := os.Getenv("ProgramData")
		if programData != "" {
			envPaths = append(envPaths, filepath.Join(programData, "GreyMultiTenant", ".env"))
		}
		// Windows install config: C:\Program Files\GreyMultiTenant\config\.env
		programFiles := os.Getenv("ProgramFiles")
		if programFiles != "" {
			envPaths = append(envPaths, filepath.Join(programFiles, "GreyMultiTenant", "config", ".env"))
		}
	case "darwin":
		// macOS: /etc/grey-multitenant/.env
		envPaths = append(envPaths, "/etc/grey-multitenant/.env")
		// macOS user: ~/.config/grey-multitenant/.env
		home, _ := os.UserHomeDir()
		if home != "" {
			envPaths = append(envPaths, filepath.Join(home, ".config", "grey-multitenant", ".env"))
		}
	case "linux":
		// Linux: /etc/grey-multitenant/.env
		envPaths = append(envPaths, "/etc/grey-multitenant/.env")
		// Linux user: ~/.config/grey-multitenant/.env
		home, _ := os.UserHomeDir()
		if home != "" {
			envPaths = append(envPaths, filepath.Join(home, ".config", "grey-multitenant", ".env"))
		}
	}

	// Current working directory (highest priority for development)
	cwd, err := os.Getwd()
	if err == nil {
		envPaths = append(envPaths, filepath.Join(cwd, ".env"))
	}

	// Custom path from GREY_CONFIG_PATH environment variable (highest priority)
	if customPath := os.Getenv("GREY_CONFIG_PATH"); customPath != "" {
		envPaths = append(envPaths, customPath)
	}

	// Load each .env file (silently ignore missing files)
	for _, path := range envPaths {
		if _, err := os.Stat(path); err == nil {
			_ = godotenv.Load(path)
		}
	}
}
