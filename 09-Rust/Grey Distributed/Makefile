# Grey Distributed Makefile
# =============================================================================
#
# This Makefile provides common development and build operations.
#
# Usage:
#   make build        - Build the binary
#   make test         - Run all tests
#   make test-race    - Run tests with race detector
#   make lint         - Run linters
#   make dev          - Start development environment
#   make clean        - Clean build artifacts
#

.PHONY: all build test test-race test-cover lint fmt vet dev cluster clean help

# Variables
BINARY := greyd
VERSION := $(shell git describe --tags --always --dirty 2>/dev/null || echo "dev")
COMMIT := $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
BUILD_TIME := $(shell date -u +%Y-%m-%dT%H:%M:%SZ)

# Go settings
GOOS ?= $(shell go env GOOS)
GOARCH ?= $(shell go env GOARCH)
GO := go
GOFLAGS := -trimpath
LDFLAGS := -s -w \
	-X main.Version=$(VERSION) \
	-X main.Commit=$(COMMIT) \
	-X main.BuildTime=$(BUILD_TIME)

# Directories
BIN_DIR := ./bin
CMD_DIR := ./cmd/greyd
DATA_DIR := ./data
COVERAGE_DIR := ./coverage

# Default target
all: lint test build

# Build the binary
build:
	@echo "Building $(BINARY) $(VERSION)..."
	@mkdir -p $(BIN_DIR)
	$(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" -o $(BIN_DIR)/$(BINARY) $(CMD_DIR)
	@echo "Built $(BIN_DIR)/$(BINARY)"

# Build for multiple platforms
build-all: build-linux build-darwin build-windows

build-linux:
	GOOS=linux GOARCH=amd64 $(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" -o $(BIN_DIR)/$(BINARY)-linux-amd64 $(CMD_DIR)
	GOOS=linux GOARCH=arm64 $(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" -o $(BIN_DIR)/$(BINARY)-linux-arm64 $(CMD_DIR)

build-darwin:
	GOOS=darwin GOARCH=amd64 $(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" -o $(BIN_DIR)/$(BINARY)-darwin-amd64 $(CMD_DIR)
	GOOS=darwin GOARCH=arm64 $(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" -o $(BIN_DIR)/$(BINARY)-darwin-arm64 $(CMD_DIR)

build-windows:
	GOOS=windows GOARCH=amd64 $(GO) build $(GOFLAGS) -ldflags "$(LDFLAGS)" -o $(BIN_DIR)/$(BINARY)-windows-amd64.exe $(CMD_DIR)

# Run tests
test:
	@echo "Running tests..."
	$(GO) test -v ./...

# Run tests with race detector
test-race:
	@echo "Running tests with race detector..."
	$(GO) test -race -v ./...

# Run tests with coverage
test-cover:
	@echo "Running tests with coverage..."
	@mkdir -p $(COVERAGE_DIR)
	$(GO) test -coverprofile=$(COVERAGE_DIR)/coverage.out ./...
	$(GO) tool cover -html=$(COVERAGE_DIR)/coverage.out -o $(COVERAGE_DIR)/coverage.html
	@echo "Coverage report: $(COVERAGE_DIR)/coverage.html"

# Run benchmarks
bench:
	@echo "Running benchmarks..."
	$(GO) test -bench=. -benchmem ./...

# Lint the codebase
lint: fmt vet
	@echo "Running golangci-lint..."
	@which golangci-lint > /dev/null || (echo "Installing golangci-lint..." && go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest)
	golangci-lint run ./...

# Format code
fmt:
	@echo "Formatting code..."
	$(GO) fmt ./...
	@which goimports > /dev/null || go install golang.org/x/tools/cmd/goimports@latest
	goimports -w .

# Vet code
vet:
	@echo "Vetting code..."
	$(GO) vet ./...

# Start development environment
dev:
	@echo "Starting development environment..."
	@chmod +x ./deploy/dev.sh
	./deploy/dev.sh

# Start local cluster
cluster:
	@echo "Starting local cluster..."
	@chmod +x ./deploy/cluster.sh
	./deploy/cluster.sh start 3

# Stop local cluster
cluster-stop:
	@chmod +x ./deploy/cluster.sh
	./deploy/cluster.sh stop

# Generate code (mocks, protos, etc.)
generate:
	@echo "Generating code..."
	$(GO) generate ./...

# Generate protocol buffers (if using gRPC)
proto:
	@echo "Generating protobuf files..."
	@which protoc > /dev/null || (echo "protoc not found. Install: https://grpc.io/docs/protoc-installation/"; exit 1)
	protoc --go_out=. --go-grpc_out=. ./api/proto/*.proto

# Clean build artifacts
clean:
	@echo "Cleaning..."
	rm -rf $(BIN_DIR)
	rm -rf $(DATA_DIR)
	rm -rf $(COVERAGE_DIR)
	$(GO) clean -cache -testcache

# Deep clean including module cache
clean-all: clean
	$(GO) clean -modcache

# Download dependencies
deps:
	@echo "Downloading dependencies..."
	$(GO) mod download
	$(GO) mod verify

# Tidy dependencies
tidy:
	@echo "Tidying dependencies..."
	$(GO) mod tidy

# Security scan
security:
	@echo "Running security scan..."
	@which gosec > /dev/null || go install github.com/securego/gosec/v2/cmd/gosec@latest
	gosec ./...

# Check for vulnerabilities
vuln:
	@echo "Checking for vulnerabilities..."
	@which govulncheck > /dev/null || go install golang.org/x/vuln/cmd/govulncheck@latest
	govulncheck ./...

# Docker build
docker-build:
	@echo "Building Docker image..."
	docker build -t grey-distributed:$(VERSION) .

# Docker build CLI only
docker-build-cli:
	@echo "Building CLI Docker image..."
	docker build -f Dockerfile.cli -t greyctl:$(VERSION) .

# Docker run (single node)
docker-run:
	docker run -p 8080:8080 -p 9090:9090 grey-distributed:$(VERSION)

# Docker Compose - start local cluster
docker-up:
	@echo "Starting Grey cluster with Docker Compose..."
	docker-compose up -d

# Docker Compose - start with build
docker-up-build:
	@echo "Building and starting Grey cluster..."
	docker-compose up -d --build

# Docker Compose - stop cluster
docker-down:
	@echo "Stopping Grey cluster..."
	docker-compose down

# Docker Compose - stop and remove volumes
docker-clean:
	@echo "Stopping Grey cluster and removing volumes..."
	docker-compose down -v

# Docker Compose - view logs
docker-logs:
	docker-compose logs -f

# Docker Compose - scale workers
docker-scale-workers:
	@echo "Scaling workers to $(WORKERS)..."
	docker-compose up -d --scale grey-worker=$(WORKERS)

# Kubernetes - deploy to cluster
k8s-deploy:
	@echo "Deploying to Kubernetes..."
	kubectl apply -f deploy/kubernetes/namespace.yaml
	kubectl apply -f deploy/kubernetes/configmap.yaml
	kubectl apply -f deploy/kubernetes/secret.yaml
	kubectl apply -f deploy/kubernetes/networkpolicy.yaml
	kubectl apply -f deploy/kubernetes/service.yaml
	kubectl apply -f deploy/kubernetes/deployment.yaml

# Kubernetes - delete deployment
k8s-delete:
	@echo "Deleting Kubernetes deployment..."
	kubectl delete -f deploy/kubernetes/ --ignore-not-found

# Helm package
helm-package:
	@echo "Packaging Helm chart..."
	helm package ./charts/grey -d ./dist

# Show help
help:
	@echo "Grey Distributed - Build System"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Build Targets:"
	@echo "  build         Build the binary"
	@echo "  build-all     Build for all platforms"
	@echo ""
	@echo "Test Targets:"
	@echo "  test          Run tests"
	@echo "  test-race     Run tests with race detector"
	@echo "  test-cover    Run tests with coverage"
	@echo "  bench         Run benchmarks"
	@echo ""
	@echo "Code Quality:"
	@echo "  lint          Run linters"
	@echo "  fmt           Format code"
	@echo "  vet           Vet code"
	@echo "  security      Run security scan"
	@echo "  vuln          Check for vulnerabilities"
	@echo ""
	@echo "Development:"
	@echo "  dev           Start development environment"
	@echo "  cluster       Start local 3-node cluster"
	@echo ""
	@echo "Docker Targets:"
	@echo "  docker-build       Build Docker image"
	@echo "  docker-build-cli   Build CLI-only Docker image"
	@echo "  docker-up          Start cluster with Docker Compose"
	@echo "  docker-up-build    Build and start cluster"
	@echo "  docker-down        Stop cluster"
	@echo "  docker-clean       Stop cluster and remove volumes"
	@echo "  docker-logs        View cluster logs"
	@echo "  docker-scale-workers WORKERS=N  Scale workers"
	@echo ""
	@echo "Kubernetes Targets:"
	@echo "  k8s-deploy    Deploy to Kubernetes cluster"
	@echo "  k8s-delete    Delete Kubernetes deployment"
	@echo ""
	@echo "Misc:"
	@echo "  deps          Download dependencies"
	@echo "  tidy          Tidy dependencies"
	@echo "  clean         Clean build artifacts"
	@echo "  help          Show this help"
