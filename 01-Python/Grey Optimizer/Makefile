# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Main Makefile
# ═══════════════════════════════════════════════════════════════════════════════
#
# Usage:
#   make build              Build all components
#   make install            Install (simulation mode by default)
#   make install-live       Install in live mode (requires confirmation)
#   make uninstall          Uninstall (dry run by default)
#   make uninstall-live     Actually uninstall
#   make test               Run all tests
#   make clean              Clean build artifacts
#   make help               Show this help
#
# ═══════════════════════════════════════════════════════════════════════════════

.PHONY: all build clean install install-live uninstall uninstall-live test \
        test-unit test-integration lint format venv help c-build c-clean \
        daemon-start daemon-stop daemon-status simulate check-deps \
        frontend backend enforcement setup

# ═══════════════════════════════════════════════════════════════════════════════
# Configuration
# ═══════════════════════════════════════════════════════════════════════════════

# Detect OS
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    OS := linux
    INSTALLER := scripts/install.sh
    UNINSTALLER := scripts/uninstall.sh
endif
ifeq ($(UNAME_S),Darwin)
    OS := macos
    INSTALLER := scripts/install_macos.sh
    UNINSTALLER := scripts/uninstall_macos.sh
endif
ifneq ($(findstring MINGW,$(UNAME_S)),)
    OS := windows
    INSTALLER := scripts/install.ps1
    UNINSTALLER := scripts/uninstall.ps1
endif
ifneq ($(findstring MSYS,$(UNAME_S)),)
    OS := windows
    INSTALLER := scripts/install.ps1
    UNINSTALLER := scripts/uninstall.ps1
endif

# Python settings
PYTHON := python3
VENV := .venv
VENV_BIN := $(VENV)/bin
VENV_PYTHON := $(VENV_BIN)/python
VENV_PIP := $(VENV_BIN)/pip
VENV_ACTIVATE := . $(VENV_BIN)/activate

# Project settings
SRC_DIR := src
TEST_DIR := tests
BUILD_DIR := build
C_DIR := c

# Installation mode (simulation|live)
MODE ?= simulation
CONFIRM_LIVE ?= false

# Colors
BLUE := \033[0;34m
GREEN := \033[0;32m
YELLOW := \033[1;33m
RED := \033[0;31m
NC := \033[0m

# ═══════════════════════════════════════════════════════════════════════════════
# Default target
# ═══════════════════════════════════════════════════════════════════════════════

all: build

# ═══════════════════════════════════════════════════════════════════════════════
# Full Setup
# ═══════════════════════════════════════════════════════════════════════════════

setup:
	@echo "$(BLUE)=== Grey Optimizer Setup ===$(NC)"
	./scripts/setup.sh

setup-venv:
	@echo "$(BLUE)=== Setting up Python venv ===$(NC)"
	./scripts/setup_venv.sh

# ═══════════════════════════════════════════════════════════════════════════════
# Build
# ═══════════════════════════════════════════════════════════════════════════════

build: check-deps venv enforcement backend frontend c-build
	@echo "$(GREEN)✓ Build complete$(NC)"

build-fast:
	@echo "$(BLUE)▸ Quick build (skip venv check)$(NC)"
	$(MAKE) c-build

check-deps:
	@echo "$(BLUE)▸ Checking dependencies...$(NC)"
	@command -v $(PYTHON) >/dev/null 2>&1 || { echo "$(RED)Error: Python 3 required$(NC)"; exit 1; }
	@command -v pip3 >/dev/null 2>&1 || { echo "$(RED)Error: pip3 required$(NC)"; exit 1; }
	@echo "$(GREEN)  ✓ Python: $$($(PYTHON) --version)$(NC)"

venv: $(VENV)/touchfile

$(VENV)/touchfile: requirements.txt
	@echo "$(BLUE)▸ Creating virtual environment...$(NC)"
	@test -d $(VENV) || $(PYTHON) -m venv $(VENV)
	@echo "$(BLUE)▸ Installing Python dependencies...$(NC)"
	@$(VENV_PIP) install --upgrade pip >/dev/null
	@$(VENV_PIP) install -r requirements.txt >/dev/null
	@touch $(VENV)/touchfile
	@echo "$(GREEN)  ✓ Virtual environment ready$(NC)"

venv-force:
	@rm -rf $(VENV)
	$(MAKE) venv

# Build C enforcement modules (supports both enforcement/ and c/ directories)
enforcement:
	@echo "$(BLUE)=== Building C Enforcement Modules ===$(NC)"
	@if [ -d "enforcement" ] && [ -f "enforcement/Makefile" ]; then \
		$(MAKE) -C enforcement; \
	fi

# Setup Python backend
backend:
	@echo "$(BLUE)=== Setting up Python Backend ===$(NC)"
	@if [ -d "backend" ]; then \
		cd backend && pip install -e . 2>/dev/null || true; \
	fi

# Build React frontend
frontend:
	@echo "$(BLUE)=== Building React Frontend ===$(NC)"
	@if [ -d "frontend" ] && [ -f "frontend/package.json" ]; then \
		cd frontend && npm install && npm run build; \
	fi

# ═══════════════════════════════════════════════════════════════════════════════
# C Modules (additional c/ directory)
# ═══════════════════════════════════════════════════════════════════════════════

c-build:
	@if [ -d "$(C_DIR)" ] && [ -f "$(C_DIR)/Makefile" ]; then \
		echo "$(BLUE)▸ Building C modules (c/)...$(NC)"; \
		$(MAKE) -C $(C_DIR); \
		echo "$(GREEN)  ✓ C modules built$(NC)"; \
	fi

c-clean:
	@if [ -d "$(C_DIR)" ] && [ -f "$(C_DIR)/Makefile" ]; then \
		$(MAKE) -C $(C_DIR) clean; \
	fi

# ═══════════════════════════════════════════════════════════════════════════════
# Installation
# ═══════════════════════════════════════════════════════════════════════════════

install: build
	@echo "$(BLUE)▸ Installing Grey Optimizer (mode: $(MODE))...$(NC)"
ifeq ($(OS),windows)
	powershell.exe -ExecutionPolicy Bypass -File $(INSTALLER) -Mode $(MODE)
else
	@bash $(INSTALLER) --mode=$(MODE)
endif
	@echo "$(GREEN)✓ Installation complete$(NC)"

install-live: build
	@echo "$(YELLOW)▸ Installing Grey Optimizer in LIVE mode...$(NC)"
	@echo "$(RED)WARNING: This will apply real system changes!$(NC)"
	@echo ""
ifeq ($(OS),windows)
	powershell.exe -ExecutionPolicy Bypass -File $(INSTALLER) -Mode live -ConfirmLive
else
	@bash $(INSTALLER) --mode=live --confirm-live
endif
	@echo "$(GREEN)✓ Live installation complete$(NC)"

install-user:
	@echo "$(BLUE)▸ Installing Grey Optimizer for current user...$(NC)"
ifeq ($(OS),macos)
	@bash $(INSTALLER) --mode=$(MODE) --user-install
else ifeq ($(OS),linux)
	@bash $(INSTALLER) --mode=$(MODE) --user
else
	@echo "$(YELLOW)User installation not supported on $(OS)$(NC)"
endif

# Install systemd service (legacy compatibility)
install-service:
	@echo "$(YELLOW)Note: Use 'make install-live' for full installation$(NC)"
	sudo ./systemd/install.sh 2>/dev/null || bash $(INSTALLER) --mode=live --confirm-live

# ═══════════════════════════════════════════════════════════════════════════════
# Uninstallation
# ═══════════════════════════════════════════════════════════════════════════════

uninstall:
	@echo "$(BLUE)▸ Uninstalling Grey Optimizer (dry run)...$(NC)"
	@echo "$(YELLOW)This is a DRY RUN. Use 'make uninstall-live' to actually remove.$(NC)"
ifeq ($(OS),windows)
	powershell.exe -ExecutionPolicy Bypass -File $(UNINSTALLER)
else
	@bash $(UNINSTALLER)
endif

uninstall-live:
	@echo "$(RED)▸ Uninstalling Grey Optimizer...$(NC)"
ifeq ($(OS),windows)
	powershell.exe -ExecutionPolicy Bypass -File $(UNINSTALLER) -ConfirmUninstall
else
	@bash $(UNINSTALLER) --confirm-uninstall
endif
	@echo "$(GREEN)✓ Uninstallation complete$(NC)"

uninstall-preserve-logs:
	@echo "$(BLUE)▸ Uninstalling Grey Optimizer (preserving logs)...$(NC)"
ifeq ($(OS),windows)
	powershell.exe -ExecutionPolicy Bypass -File $(UNINSTALLER) -ConfirmUninstall -PreserveLogs
else
	@bash $(UNINSTALLER) --confirm-uninstall --preserve-logs
endif

# Uninstall systemd service (legacy compatibility)
uninstall-service:
	@echo "$(YELLOW)Note: Use 'make uninstall-live' for full uninstallation$(NC)"
	sudo ./systemd/uninstall.sh 2>/dev/null || bash $(UNINSTALLER) --confirm-uninstall

# ═══════════════════════════════════════════════════════════════════════════════
# Daemon Control
# ═══════════════════════════════════════════════════════════════════════════════

daemon-start:
	@echo "$(BLUE)▸ Starting daemon...$(NC)"
	@$(VENV_PYTHON) greyctl start

daemon-stop:
	@echo "$(BLUE)▸ Stopping daemon...$(NC)"
	@$(VENV_PYTHON) greyctl stop

daemon-status:
	@$(VENV_PYTHON) greyctl status

daemon-restart: daemon-stop daemon-start
	@echo "$(GREEN)✓ Daemon restarted$(NC)"

# Run in simulation mode
run-simulation:
	./scripts/run_simulation.sh

# Run with full enforcement (requires root)
run-daemon:
	sudo ./scripts/run_daemon.sh

simulate:
	@echo "$(BLUE)▸ Running simulation...$(NC)"
	@$(VENV_PYTHON) greyctl simulate --profile=balanced --duration=60

health:
	@$(VENV_PYTHON) greyctl health

# ═══════════════════════════════════════════════════════════════════════════════
# Testing
# ═══════════════════════════════════════════════════════════════════════════════

test: venv
	@echo "$(BLUE)=== Running All Tests ===$(NC)"
	@if [ -f "scripts/run_tests.sh" ]; then \
		./scripts/run_tests.sh; \
	else \
		$(VENV_ACTIVATE) && pytest $(TEST_DIR) -v --tb=short; \
	fi
	@echo "$(GREEN)✓ All tests passed$(NC)"

test-unit: venv
	@echo "$(BLUE)=== Running Unit Tests ===$(NC)"
	@if [ -f "scripts/run_tests.sh" ]; then \
		./scripts/run_tests.sh --unit; \
	else \
		$(VENV_ACTIVATE) && pytest $(TEST_DIR)/unit -v --tb=short; \
	fi

test-integration: venv
	@echo "$(BLUE)=== Running Integration Tests ===$(NC)"
	@if [ -f "scripts/run_integration_test.sh" ]; then \
		bash scripts/run_integration_test.sh; \
	elif [ -f "scripts/run_tests.sh" ]; then \
		./scripts/run_tests.sh --integration; \
	else \
		$(VENV_ACTIVATE) && pytest $(TEST_DIR)/integration -v; \
	fi

test-coverage: venv
	@echo "$(BLUE)▸ Running tests with coverage...$(NC)"
	@$(VENV_ACTIVATE) && pytest $(TEST_DIR) -v --cov=$(SRC_DIR) --cov-report=html --cov-report=term 2>/dev/null || \
		pytest tests -v --cov=. --cov-report=html
	@echo "$(GREEN)✓ Coverage report: htmlcov/index.html$(NC)"

test-quick:
	@echo "$(BLUE)▸ Running quick tests (no coverage)...$(NC)"
	@$(VENV_ACTIVATE) && pytest $(TEST_DIR)/unit -v --tb=line -q 2>/dev/null || pytest tests -v --tb=line -q

# ═══════════════════════════════════════════════════════════════════════════════
# Code Quality
# ═══════════════════════════════════════════════════════════════════════════════

lint: venv
	@echo "$(BLUE)▸ Running linters...$(NC)"
	@$(VENV_ACTIVATE) && flake8 $(SRC_DIR) greyctl --max-line-length=120 2>/dev/null || \
		flake8 . --max-line-length=120 --exclude=.venv,node_modules
	@echo "$(GREEN)✓ Lint complete$(NC)"

format: venv
	@echo "$(BLUE)▸ Formatting code...$(NC)"
	@$(VENV_ACTIVATE) && black $(SRC_DIR) greyctl tests 2>/dev/null || black . --exclude='.venv|node_modules'
	@echo "$(GREEN)✓ Formatting complete$(NC)"

format-check: venv
	@echo "$(BLUE)▸ Checking code format...$(NC)"
	@$(VENV_ACTIVATE) && black --check $(SRC_DIR) greyctl tests 2>/dev/null || black --check . --exclude='.venv|node_modules'

# ═══════════════════════════════════════════════════════════════════════════════
# Clean
# ═══════════════════════════════════════════════════════════════════════════════

clean:
	@echo "$(BLUE)=== Cleaning Build Artifacts ===$(NC)"
	@if [ -d "enforcement" ] && [ -f "enforcement/Makefile" ]; then \
		$(MAKE) -C enforcement clean 2>/dev/null || true; \
	fi
	$(MAKE) c-clean
	@rm -rf $(BUILD_DIR)
	@rm -rf $(VENV)
	@rm -rf .pytest_cache
	@rm -rf htmlcov
	@rm -rf .coverage
	@rm -rf *.egg-info
	@rm -rf dist
	@rm -rf backend/*.egg-info backend/build 2>/dev/null || true
	@rm -rf frontend/dist 2>/dev/null || true
	@rm -rf proofs/*.json data/*.db 2>/dev/null || true
	@find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	@find . -type f -name "*.pyc" -delete 2>/dev/null || true
	@find . -type f -name "*.pyo" -delete 2>/dev/null || true
	@echo "$(GREEN)✓ Clean complete$(NC)"

clean-all: clean
	@echo "$(BLUE)▸ Deep cleaning...$(NC)"
	@rm -rf node_modules 2>/dev/null || true
	@rm -rf frontend/node_modules 2>/dev/null || true
	@rm -rf .mypy_cache 2>/dev/null || true
	@echo "$(GREEN)✓ Deep clean complete$(NC)"

# ═══════════════════════════════════════════════════════════════════════════════
# Development
# ═══════════════════════════════════════════════════════════════════════════════

dev: venv
	@echo "$(BLUE)Starting development mode...$(NC)"
	@echo "Backend: http://localhost:8080"
	@echo "Frontend: http://localhost:5173"
	@trap 'kill 0' SIGINT; \
	(cd backend && python -m grey_optimizer.daemon --simulation 2>/dev/null || \
	 $(VENV_PYTHON) -m flask --app src.api.app run --debug --port 5000) & \
	(cd frontend && npm run dev 2>/dev/null || true) & \
	wait

dev-daemon: venv
	@echo "$(BLUE)▸ Starting daemon in development mode...$(NC)"
	@$(VENV_ACTIVATE) && python -m src.daemon.orchestrator --mode=simulation 2>/dev/null || \
		cd backend && python -m grey_optimizer.daemon --simulation

watch: venv
	@echo "$(BLUE)▸ Starting file watcher...$(NC)"
	@$(VENV_ACTIVATE) && watchmedo auto-restart -d src -p '*.py' -- python -m src.daemon.orchestrator --mode=simulation

# ═══════════════════════════════════════════════════════════════════════════════
# Packaging
# ═══════════════════════════════════════════════════════════════════════════════

package: build
	@echo "$(BLUE)▸ Creating package...$(NC)"
	@mkdir -p $(BUILD_DIR)/grey-optimizer
	@cp -r src $(BUILD_DIR)/grey-optimizer/ 2>/dev/null || true
	@cp -r backend $(BUILD_DIR)/grey-optimizer/ 2>/dev/null || true
	@cp -r scripts $(BUILD_DIR)/grey-optimizer/
	@cp -r c $(BUILD_DIR)/grey-optimizer/ 2>/dev/null || true
	@cp -r enforcement $(BUILD_DIR)/grey-optimizer/ 2>/dev/null || true
	@cp greyctl $(BUILD_DIR)/grey-optimizer/
	@cp requirements.txt $(BUILD_DIR)/grey-optimizer/
	@cp README.md $(BUILD_DIR)/grey-optimizer/
	@cp Makefile $(BUILD_DIR)/grey-optimizer/
	@cd $(BUILD_DIR) && tar -czf grey-optimizer.tar.gz grey-optimizer
	@echo "$(GREEN)✓ Package: $(BUILD_DIR)/grey-optimizer.tar.gz$(NC)"

# ═══════════════════════════════════════════════════════════════════════════════
# Documentation
# ═══════════════════════════════════════════════════════════════════════════════

docs: venv
	@echo "$(BLUE)=== Generating Documentation ===$(NC)"
	@if [ -d "backend" ]; then \
		cd backend && python -m pydoc -w grey_optimizer; \
	else \
		$(VENV_ACTIVATE) && pdoc --html $(SRC_DIR) -o docs/api --force 2>/dev/null || true; \
	fi
	@echo "$(GREEN)✓ Documentation generated$(NC)"

# ═══════════════════════════════════════════════════════════════════════════════
# Help
# ═══════════════════════════════════════════════════════════════════════════════

help:
	@echo ""
	@echo "$(BLUE)Grey Optimizer - Build System$(NC)"
	@echo ""
	@echo "$(YELLOW)Setup:$(NC)"
	@echo "  make setup            Full setup (venv, deps, build)"
	@echo "  make setup-venv       Create Python virtual environment only"
	@echo ""
	@echo "$(YELLOW)Build:$(NC)"
	@echo "  make build            Build all components"
	@echo "  make build-fast       Quick build (skip venv check)"
	@echo "  make enforcement      Build C enforcement modules"
	@echo "  make c-build          Build additional C modules"
	@echo "  make backend          Setup Python backend"
	@echo "  make frontend         Build React frontend"
	@echo "  make venv             Create/update virtual environment"
	@echo "  make package          Create distribution package"
	@echo ""
	@echo "$(YELLOW)Installation:$(NC)"
	@echo "  make install          Install in simulation mode (default)"
	@echo "  make install-live     Install in live mode (CAUTION!)"
	@echo "  make install-user     Install for current user only"
	@echo "  make install-service  Install systemd service (legacy)"
	@echo "  make uninstall        Dry-run uninstall (shows what would happen)"
	@echo "  make uninstall-live   Actually uninstall"
	@echo "  make uninstall-service Uninstall systemd service (legacy)"
	@echo ""
	@echo "$(YELLOW)Daemon Control:$(NC)"
	@echo "  make daemon-start     Start the daemon"
	@echo "  make daemon-stop      Stop the daemon"
	@echo "  make daemon-status    Show daemon status"
	@echo "  make daemon-restart   Restart the daemon"
	@echo "  make run-simulation   Run in simulation mode"
	@echo "  make run-daemon       Run with full enforcement (root)"
	@echo "  make simulate         Run simulation via greyctl"
	@echo "  make health           Run health check"
	@echo ""
	@echo "$(YELLOW)Testing:$(NC)"
	@echo "  make test             Run all tests"
	@echo "  make test-unit        Run unit tests only"
	@echo "  make test-integration Run integration tests"
	@echo "  make test-coverage    Run tests with coverage report"
	@echo "  make test-quick       Quick tests (no coverage)"
	@echo ""
	@echo "$(YELLOW)Code Quality:$(NC)"
	@echo "  make lint             Run linters"
	@echo "  make format           Format code with black"
	@echo "  make format-check     Check code formatting"
	@echo ""
	@echo "$(YELLOW)Development:$(NC)"
	@echo "  make dev              Start dev mode (backend + frontend)"
	@echo "  make dev-daemon       Start daemon in dev mode"
	@echo "  make watch            Start file watcher with auto-restart"
	@echo "  make docs             Generate documentation"
	@echo ""
	@echo "$(YELLOW)Cleanup:$(NC)"
	@echo "  make clean            Remove build artifacts"
	@echo "  make clean-all        Deep clean (includes node_modules)"
	@echo ""
	@echo "$(YELLOW)Options:$(NC)"
	@echo "  MODE=simulation|live  Set installation mode"
	@echo ""
	@echo "$(YELLOW)Examples:$(NC)"
	@echo "  make build && make install"
	@echo "  make install MODE=live"
	@echo "  make test-coverage"
	@echo ""
