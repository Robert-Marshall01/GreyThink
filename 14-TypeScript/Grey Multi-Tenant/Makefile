# Grey Multi-Tenant Platform Makefile
# =====================================
#
# Usage:
#   make <target>
#
# Targets:
#   help       - Show this help message
#   install    - Install all dependencies
#   run        - Run the Go API server locally
#   build      - Build all components
#   test       - Run all tests
#   migrate    - Run database migrations
#   generate   - Generate code (OpenAPI types, etc.)
#   docker-up  - Start Docker Compose services
#   docker-down - Stop Docker Compose services
#   clean      - Clean build artifacts

.PHONY: help install run build test migrate generate docker-up docker-down clean lint

# Default target
.DEFAULT_GOAL := help

# Variables
GO_SERVICE_DIR := services/grey-core-api
TS_CLIENT_DIR := packages/grey-core-client
CONTRACTS_DIR := contracts

# Colors for output
CYAN := \033[36m
RESET := \033[0m

help: ## Show this help message
	@echo "Grey Multi-Tenant Platform"
	@echo "=========================="
	@echo ""
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(CYAN)%-15s$(RESET) %s\n", $$1, $$2}'

# ==================
# Dependencies
# ==================

install: install-go install-node ## Install all dependencies
	@echo "All dependencies installed!"

install-go: ## Install Go dependencies
	@echo "Installing Go dependencies..."
	cd $(GO_SERVICE_DIR) && go mod download

install-node: ## Install Node.js dependencies
	@echo "Installing Node.js dependencies..."
	pnpm install

# ==================
# Development
# ==================

run: ## Run the Go API server locally
	@echo "Starting Grey Core API..."
	cd $(GO_SERVICE_DIR) && go run ./cmd/api

run-watch: ## Run the Go API server with hot reload (requires air)
	@echo "Starting Grey Core API with hot reload..."
	cd $(GO_SERVICE_DIR) && air

dev: docker-db run ## Start database and run API server

# ==================
# Building
# ==================

build: build-go build-ts ## Build all components
	@echo "All components built!"

build-go: ## Build the Go API binary
	@echo "Building Go API..."
	cd $(GO_SERVICE_DIR) && go build -o bin/api ./cmd/api

build-ts: ## Build TypeScript packages
	@echo "Building TypeScript packages..."
	pnpm run build

build-docker: ## Build Docker image for Go API
	@echo "Building Docker image..."
	docker build -t grey-core-api:latest -f $(GO_SERVICE_DIR)/Dockerfile $(GO_SERVICE_DIR)

# ==================
# Testing
# ==================

test: test-go test-ts ## Run all tests
	@echo "All tests completed!"

test-go: ## Run Go tests
	@echo "Running Go tests..."
	cd $(GO_SERVICE_DIR) && go test -v ./...

test-go-coverage: ## Run Go tests with coverage
	@echo "Running Go tests with coverage..."
	cd $(GO_SERVICE_DIR) && go test -v -coverprofile=coverage.out ./...
	cd $(GO_SERVICE_DIR) && go tool cover -html=coverage.out -o coverage.html

test-ts: ## Run TypeScript tests
	@echo "Running TypeScript tests..."
	pnpm run test

lint: lint-go lint-ts ## Run all linters

lint-go: ## Run Go linter
	@echo "Linting Go code..."
	cd $(GO_SERVICE_DIR) && golangci-lint run

lint-ts: ## Run TypeScript linter
	@echo "Linting TypeScript code..."
	pnpm run lint

# ==================
# Database
# ==================

migrate: ## Run database migrations (up)
	@echo "Running database migrations..."
	cd $(GO_SERVICE_DIR) && goose -dir internal/db/migrations postgres "$(DATABASE_URL)" up

migrate-down: ## Rollback last database migration
	@echo "Rolling back last migration..."
	cd $(GO_SERVICE_DIR) && goose -dir internal/db/migrations postgres "$(DATABASE_URL)" down

migrate-status: ## Show migration status
	@echo "Migration status..."
	cd $(GO_SERVICE_DIR) && goose -dir internal/db/migrations postgres "$(DATABASE_URL)" status

migrate-create: ## Create a new migration (usage: make migrate-create name=add_feature)
	@echo "Creating new migration: $(name)..."
	cd $(GO_SERVICE_DIR) && goose -dir internal/db/migrations create $(name) sql

# ==================
# Code Generation
# ==================

generate: generate-ts ## Generate all code
	@echo "All code generated!"

generate-ts: ## Generate TypeScript types from OpenAPI
	@echo "Generating TypeScript types..."
	cd $(TS_CLIENT_DIR) && pnpm run generate

# ==================
# Docker
# ==================

docker-up: ## Start all Docker Compose services
	@echo "Starting Docker Compose services..."
	docker compose up -d

docker-down: ## Stop all Docker Compose services
	@echo "Stopping Docker Compose services..."
	docker compose down

docker-db: ## Start only the database service
	@echo "Starting database..."
	docker compose up -d postgres

docker-logs: ## Show Docker Compose logs
	docker compose logs -f

docker-clean: ## Remove all Docker Compose volumes
	@echo "Removing Docker volumes..."
	docker compose down -v

# ==================
# Cleanup
# ==================

clean: clean-go clean-ts clean-docker ## Clean all build artifacts
	@echo "All build artifacts cleaned!"

clean-go: ## Clean Go build artifacts
	@echo "Cleaning Go build artifacts..."
	cd $(GO_SERVICE_DIR) && rm -rf bin/ tmp/ coverage.out coverage.html

clean-ts: ## Clean TypeScript build artifacts
	@echo "Cleaning TypeScript build artifacts..."
	pnpm run clean
	rm -rf node_modules/.cache

clean-docker: ## Clean Docker images and containers
	@echo "Cleaning Docker..."
	docker compose down --rmi local -v

# ==================
# Utilities
# ==================

fmt: ## Format all code
	@echo "Formatting Go code..."
	cd $(GO_SERVICE_DIR) && gofmt -w .
	@echo "Formatting TypeScript code..."
	pnpm run lint --fix

check: lint test ## Run all checks (lint + test)

.PHONY: all
all: install build test ## Install, build, and test everything
