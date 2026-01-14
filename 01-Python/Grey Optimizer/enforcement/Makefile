# Grey Optimizer - C Enforcement Modules
# Makefile for building shared libraries

CC ?= gcc
CFLAGS = -Wall -Wextra -O2 -fPIC -std=c11
LDFLAGS = -shared

# Debug build
ifdef DEBUG
    CFLAGS += -g -DGREY_DEBUG
endif

# Source directory
SRC_DIR = src

# Output directory
BUILD_DIR = build
LIB_DIR = lib

# Source files
SOURCES = \
    $(SRC_DIR)/cgroup_controller.c \
    $(SRC_DIR)/scheduler.c \
    $(SRC_DIR)/ksm_controller.c \
    $(SRC_DIR)/cache_ops.c \
    $(SRC_DIR)/disk_ops.c \
    $(SRC_DIR)/memory_ops.c

# Object files
OBJECTS = $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SOURCES))

# Output libraries
LIBS = \
    $(LIB_DIR)/libgrey_cgroup.so \
    $(LIB_DIR)/libgrey_scheduler.so \
    $(LIB_DIR)/libgrey_ksm.so \
    $(LIB_DIR)/libgrey_cache.so \
    $(LIB_DIR)/libgrey_disk.so \
    $(LIB_DIR)/libgrey_memory.so

# Combined library
COMBINED_LIB = $(LIB_DIR)/libgrey_optimizer.so

.PHONY: all clean debug install test

# Default target
all: dirs $(LIBS) $(COMBINED_LIB)

# Debug target
debug: CFLAGS += -g -DGREY_DEBUG
debug: all

# Create directories
dirs:
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(LIB_DIR)

# Compile object files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c $(SRC_DIR)/common.h
	@echo "Compiling $<..."
	$(CC) $(CFLAGS) -c $< -o $@

# Build individual libraries
$(LIB_DIR)/libgrey_cgroup.so: $(BUILD_DIR)/cgroup_controller.o
	@echo "Building $@..."
	$(CC) $(LDFLAGS) $< -o $@

$(LIB_DIR)/libgrey_scheduler.so: $(BUILD_DIR)/scheduler.o
	@echo "Building $@..."
	$(CC) $(LDFLAGS) $< -o $@

$(LIB_DIR)/libgrey_ksm.so: $(BUILD_DIR)/ksm_controller.o
	@echo "Building $@..."
	$(CC) $(LDFLAGS) $< -o $@

$(LIB_DIR)/libgrey_cache.so: $(BUILD_DIR)/cache_ops.o
	@echo "Building $@..."
	$(CC) $(LDFLAGS) $< -o $@

$(LIB_DIR)/libgrey_disk.so: $(BUILD_DIR)/disk_ops.o
	@echo "Building $@..."
	$(CC) $(LDFLAGS) $< -o $@

$(LIB_DIR)/libgrey_memory.so: $(BUILD_DIR)/memory_ops.o
	@echo "Building $@..."
	$(CC) $(LDFLAGS) $< -o $@

# Build combined library
$(COMBINED_LIB): $(OBJECTS)
	@echo "Building combined library..."
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

# Clean build artifacts
clean:
	@echo "Cleaning..."
	rm -rf $(BUILD_DIR)
	rm -rf $(LIB_DIR)

# Install libraries
install: all
	@echo "Installing to /usr/local/lib/grey-optimizer..."
	install -d /usr/local/lib/grey-optimizer
	install -m 644 $(LIB_DIR)/*.so /usr/local/lib/grey-optimizer/
	ldconfig

# Uninstall
uninstall:
	@echo "Uninstalling..."
	rm -rf /usr/local/lib/grey-optimizer
	ldconfig

# Run basic tests
test: all
	@echo "Running basic tests..."
	@echo "Testing cgroup module..."
	@python3 -c "import ctypes; lib = ctypes.CDLL('./$(LIB_DIR)/libgrey_cgroup.so'); print('  cgroup: OK')"
	@echo "Testing scheduler module..."
	@python3 -c "import ctypes; lib = ctypes.CDLL('./$(LIB_DIR)/libgrey_scheduler.so'); print('  scheduler: OK')"
	@echo "Testing KSM module..."
	@python3 -c "import ctypes; lib = ctypes.CDLL('./$(LIB_DIR)/libgrey_ksm.so'); print('  ksm: OK')"
	@echo "Testing cache module..."
	@python3 -c "import ctypes; lib = ctypes.CDLL('./$(LIB_DIR)/libgrey_cache.so'); print('  cache: OK')"
	@echo "Testing disk module..."
	@python3 -c "import ctypes; lib = ctypes.CDLL('./$(LIB_DIR)/libgrey_disk.so'); print('  disk: OK')"
	@echo "Testing combined library..."
	@python3 -c "import ctypes; lib = ctypes.CDLL('./$(LIB_DIR)/libgrey_optimizer.so'); print('  combined: OK')"
	@echo "All tests passed!"

# Help
help:
	@echo "Grey Optimizer C Modules Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build all libraries (default)"
	@echo "  debug     - Build with debug symbols and logging"
	@echo "  clean     - Remove build artifacts"
	@echo "  install   - Install libraries to /usr/local/lib"
	@echo "  uninstall - Remove installed libraries"
	@echo "  test      - Run basic ctypes loading tests"
	@echo "  help      - Show this help"
	@echo ""
	@echo "Environment:"
	@echo "  CC        - C compiler (default: gcc)"
	@echo "  DEBUG     - Set to enable debug build"
