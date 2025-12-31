# SPDX-License-Identifier: MIT
# Cross-platform Makefile for Msh
# Supports Linux, macOS, and Windows (MinGW/MSYS2)

# Detect platform
ifeq ($(OS),Windows_NT)
    PLATFORM := windows
    EXE := .exe
    RM := del /Q
    RMDIR := rmdir /S /Q
    PLATFORM_SRC := platform_win32.c
    PLATFORM_DEFINE := -DMSH_PLATFORM_WINDOWS=1
else
    UNAME_S := $(shell uname -s)
    EXE :=
    RM := rm -f
    RMDIR := rm -rf
    PLATFORM_SRC := platform_posix.c
    PLATFORM_DEFINE := -DMSH_PLATFORM_POSIX=1
    ifeq ($(UNAME_S),Linux)
        PLATFORM := linux
    else ifeq ($(UNAME_S),Darwin)
        PLATFORM := macos
    else
        PLATFORM := posix
    endif
endif

# Compilers
CC ?= gcc
CXX ?= g++

# Target
TARGET := Msh$(EXE)

# Compiler flags
CFLAGS := -Wall -Wextra -std=c99 $(PLATFORM_DEFINE)
CXXFLAGS := -Wall -Wextra -std=c++11 $(PLATFORM_DEFINE)
LDFLAGS :=

# Build type (debug or release)
BUILD ?= release

ifeq ($(BUILD),debug)
    CFLAGS += -g -O0 -DDEBUG
    CXXFLAGS += -g -O0 -DDEBUG
else
    CFLAGS += -O2 -DNDEBUG
    CXXFLAGS += -O2 -DNDEBUG
endif

# Static linking option
ifeq ($(STATIC),1)
    LDFLAGS += -static
endif

# Source files
C_SOURCES := Msh.c run_builtin_command.c jobs.c $(PLATFORM_SRC)
CXX_SOURCES := regex_wrapper.cpp

# Object files
C_OBJECTS := $(C_SOURCES:.c=.o)
CXX_OBJECTS := $(CXX_SOURCES:.cpp=.o)
OBJECTS := $(C_OBJECTS) $(CXX_OBJECTS)

# Header dependencies
HEADERS := platform.h run_builtin_command.h jobs.h regex_wrapper.h

# Phony targets
.PHONY: all clean debug release info help

# Default target
all: $(TARGET)

# Link
$(TARGET): $(OBJECTS)
	@echo "Linking $(TARGET)..."
	$(CXX) $(LDFLAGS) $(OBJECTS) -o $(TARGET)
	@echo "Build complete: $(TARGET)"

# Compile C files
%.o: %.c $(HEADERS)
	@echo "Compiling $<..."
	$(CC) $(CFLAGS) -c $< -o $@

# Compile C++ files
%.o: %.cpp $(HEADERS)
	@echo "Compiling $<..."
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Debug build
debug:
	$(MAKE) BUILD=debug all

# Release build
release:
	$(MAKE) BUILD=release all

# Static build
static:
	$(MAKE) STATIC=1 all

# Clean build artifacts
clean:
ifeq ($(OS),Windows_NT)
	-$(RM) *.o $(TARGET) 2>nul
else
	$(RM) $(OBJECTS) $(TARGET)
endif

# Show build info
info:
	@echo "Platform: $(PLATFORM)"
	@echo "Target: $(TARGET)"
	@echo "C Compiler: $(CC)"
	@echo "C++ Compiler: $(CXX)"
	@echo "C Sources: $(C_SOURCES)"
	@echo "C++ Sources: $(CXX_SOURCES)"
	@echo "Build type: $(BUILD)"

# Help
help:
	@echo "Msh Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build the project (default)"
	@echo "  debug     - Build with debug symbols"
	@echo "  release   - Build optimized release"
	@echo "  static    - Build statically linked binary"
	@echo "  clean     - Remove build artifacts"
	@echo "  info      - Show build configuration"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Variables:"
	@echo "  BUILD=debug|release  - Set build type (default: release)"
	@echo "  STATIC=1             - Enable static linking"
	@echo "  CC=<compiler>        - Set C compiler (default: gcc)"
	@echo "  CXX=<compiler>       - Set C++ compiler (default: g++)"
