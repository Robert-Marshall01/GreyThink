# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - C Wrappers Makefile
# ═══════════════════════════════════════════════════════════════════════════════
#
# Builds minimal C wrappers for platform-specific enforcement operations.
#
# Usage:
#   make              Build all modules
#   make test         Run tests
#   make clean        Clean build artifacts
#   make install      Install shared libraries
#
# ═══════════════════════════════════════════════════════════════════════════════

# Detect OS
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    OS := linux
    SHARED_EXT := .so
    CFLAGS_OS := -D_GNU_SOURCE
    LDFLAGS_OS := -lrt
endif
ifeq ($(UNAME_S),Darwin)
    OS := macos
    SHARED_EXT := .dylib
    CFLAGS_OS := -D_DARWIN_C_SOURCE
    LDFLAGS_OS :=
endif

# Compiler settings
CC := gcc
CFLAGS := -Wall -Wextra -Werror -fPIC -O2 $(CFLAGS_OS)
CFLAGS_DEBUG := -Wall -Wextra -g -fPIC -DDEBUG $(CFLAGS_OS)
LDFLAGS := -shared $(LDFLAGS_OS)
LDFLAGS_BIN := $(LDFLAGS_OS)

# Directories
SRC_DIR := .
BUILD_DIR := build
LIB_DIR := lib
BIN_DIR := bin
INSTALL_DIR := /usr/local/lib/grey-optimizer
INSTALL_BIN := /usr/local/bin

# Source files for shared library
LIB_SOURCES := enforce.c cgroup_wrapper.c sysctl_wrapper.c
LIB_OBJECTS := $(patsubst %.c,$(BUILD_DIR)/%.o,$(LIB_SOURCES))
LIBRARIES := $(LIB_DIR)/libgrey_enforce$(SHARED_EXT)

# CLI helper binaries
CLI_SOURCES := madvise_helper.c cgroup_helper.c
CLI_BINARIES := $(BIN_DIR)/madvise_helper $(BIN_DIR)/cgroup_helper

# Platform-specific sources
ifeq ($(OS),linux)
    SOURCES += $(wildcard $(SRC_DIR)/linux/*.c)
endif
ifeq ($(OS),macos)
    SOURCES += $(wildcard $(SRC_DIR)/macos/*.c)
endif

# Colors
BLUE := \033[0;34m
GREEN := \033[0;32m
YELLOW := \033[1;33m
RED := \033[0;31m
NC := \033[0m

# ═══════════════════════════════════════════════════════════════════════════════
# Default target
# ═══════════════════════════════════════════════════════════════════════════════

.PHONY: all
all: directories $(LIBRARIES) $(CLI_BINARIES)
	@echo "$(GREEN)✓ C modules and CLI helpers built$(NC)"

# ═══════════════════════════════════════════════════════════════════════════════
# Build rules
# ═══════════════════════════════════════════════════════════════════════════════

.PHONY: directories
directories:
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(LIB_DIR)
	@mkdir -p $(BIN_DIR)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	@echo "$(BLUE)▸ Compiling $<$(NC)"
	@$(CC) $(CFLAGS) -c $< -o $@

$(LIB_DIR)/libgrey_enforce$(SHARED_EXT): $(LIB_OBJECTS)
	@echo "$(BLUE)▸ Linking $@$(NC)"
	@$(CC) $(LDFLAGS) $^ -o $@

# CLI helper binaries
$(BIN_DIR)/madvise_helper: $(SRC_DIR)/madvise_helper.c
	@echo "$(BLUE)▸ Building madvise_helper$(NC)"
	@$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS_BIN)

$(BIN_DIR)/cgroup_helper: $(SRC_DIR)/cgroup_helper.c
	@echo "$(BLUE)▸ Building cgroup_helper$(NC)"
	@$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS_BIN)

# ═══════════════════════════════════════════════════════════════════════════════
# Debug build
# ═══════════════════════════════════════════════════════════════════════════════

.PHONY: debug
debug: CFLAGS := $(CFLAGS_DEBUG)
debug: clean all
	@echo "$(YELLOW)Debug build complete$(NC)"

# ═══════════════════════════════════════════════════════════════════════════════
# Test
# ═══════════════════════════════════════════════════════════════════════════════

.PHONY: test
test: all
	@echo "$(BLUE)▸ Running C tests$(NC)"
	@if [ -f "test_enforce.c" ]; then \
		$(CC) $(CFLAGS_DEBUG) test_enforce.c -o $(BUILD_DIR)/test_enforce -L$(LIB_DIR) -lgrey_enforce -Wl,-rpath,$(LIB_DIR); \
		$(BUILD_DIR)/test_enforce; \
	else \
		echo "$(YELLOW)No test file found$(NC)"; \
	fi

# ═══════════════════════════════════════════════════════════════════════════════
# Install
# ═══════════════════════════════════════════════════════════════════════════════

.PHONY: install
install: all
	@echo "$(BLUE)▸ Installing C libraries and binaries$(NC)"
	@mkdir -p $(INSTALL_DIR)
	@cp $(LIB_DIR)/*$(SHARED_EXT) $(INSTALL_DIR)/
	@mkdir -p $(INSTALL_BIN)
	@cp $(BIN_DIR)/madvise_helper $(INSTALL_BIN)/grey-madvise-helper 2>/dev/null || true
	@cp $(BIN_DIR)/cgroup_helper $(INSTALL_BIN)/grey-cgroup-helper 2>/dev/null || true
	@if [ "$(OS)" = "linux" ]; then \
		ldconfig 2>/dev/null || true; \
	fi
	@echo "$(GREEN)✓ Installed to $(INSTALL_DIR) and $(INSTALL_BIN)$(NC)"

.PHONY: uninstall
uninstall:
	@echo "$(BLUE)▸ Uninstalling C libraries and binaries$(NC)"
	@rm -rf $(INSTALL_DIR)
	@rm -f $(INSTALL_BIN)/grey-madvise-helper
	@rm -f $(INSTALL_BIN)/grey-cgroup-helper
	@echo "$(GREEN)✓ Uninstalled$(NC)"

# ═══════════════════════════════════════════════════════════════════════════════
# Clean
# ═══════════════════════════════════════════════════════════════════════════════

.PHONY: clean
clean:
	@echo "$(BLUE)▸ Cleaning$(NC)"
	@rm -rf $(BUILD_DIR)
	@rm -rf $(LIB_DIR)
	@rm -rf $(BIN_DIR)
	@echo "$(GREEN)✓ Clean$(NC)"

# ═══════════════════════════════════════════════════════════════════════════════
# Help
# ═══════════════════════════════════════════════════════════════════════════════

.PHONY: help
help:
	@echo ""
	@echo "$(BLUE)Grey Optimizer - C Wrappers$(NC)"
	@echo ""
	@echo "$(YELLOW)Targets:$(NC)"
	@echo "  make              Build all C modules"
	@echo "  make debug        Build with debug symbols"
	@echo "  make test         Run tests"
	@echo "  make install      Install to $(INSTALL_DIR)"
	@echo "  make uninstall    Remove installed libraries"
	@echo "  make clean        Remove build artifacts"
	@echo ""
	@echo "$(YELLOW)Current OS:$(NC) $(OS)"
	@echo ""
