#!/bin/bash
# SPDX-License-Identifier: MIT
# Cross-platform build script for Msh (Linux/macOS)
# Builds the Msh shell with platform abstraction layer

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== Msh Build Script (POSIX) ===${NC}"

# Configuration
CC="${CC:-gcc}"
CXX="${CXX:-g++}"
OUTPUT="Msh"
CFLAGS="-Wall -Wextra -std=c99 -O2 -DMSH_PLATFORM_POSIX=1"
CXXFLAGS="-Wall -Wextra -std=c++11 -O2 -DMSH_PLATFORM_POSIX=1"
LDFLAGS=""

# Parse arguments
BUILD_TYPE="release"
STATIC_LINK=0
VERBOSE=0

for arg in "$@"; do
    case $arg in
        --debug)
            BUILD_TYPE="debug"
            CFLAGS="-Wall -Wextra -std=c99 -g -O0 -DDEBUG -DMSH_PLATFORM_POSIX=1"
            CXXFLAGS="-Wall -Wextra -std=c++11 -g -O0 -DDEBUG -DMSH_PLATFORM_POSIX=1"
            ;;
        --static)
            STATIC_LINK=1
            LDFLAGS="$LDFLAGS -static"
            ;;
        --verbose|-v)
            VERBOSE=1
            ;;
        --clean)
            echo -e "${YELLOW}Cleaning build artifacts...${NC}"
            rm -f *.o "$OUTPUT" "$OUTPUT.exe" 2>/dev/null || true
            echo -e "${GREEN}Clean complete.${NC}"
            exit 0
            ;;
        --help|-h)
            echo "Usage: $0 [options]"
            echo "Options:"
            echo "  --debug     Build with debug symbols"
            echo "  --static    Build statically linked binary"
            echo "  --verbose   Show compilation commands"
            echo "  --clean     Remove build artifacts"
            echo "  --help      Show this help message"
            exit 0
            ;;
    esac
done

echo -e "${YELLOW}Build type: ${BUILD_TYPE}${NC}"
if [ $STATIC_LINK -eq 1 ]; then
    echo -e "${YELLOW}Static linking: enabled${NC}"
fi

# Source files
C_SOURCES=(
    "Msh.c"
    "run_builtin_command.c"
    "jobs.c"
    "platform_posix.c"
)

CXX_SOURCES=(
    "regex_wrapper.cpp"
)

# Check for missing files
echo -e "${YELLOW}Checking source files...${NC}"
for src in "${C_SOURCES[@]}"; do
    if [ ! -f "$src" ]; then
        echo -e "${RED}Error: Missing source file: $src${NC}"
        exit 1
    fi
done

for src in "${CXX_SOURCES[@]}"; do
    if [ ! -f "$src" ]; then
        echo -e "${RED}Error: Missing source file: $src${NC}"
        exit 1
    fi
done

# Compile C files
echo -e "${YELLOW}Compiling C sources...${NC}"
for src in "${C_SOURCES[@]}"; do
    obj="${src%.c}.o"
    if [ $VERBOSE -eq 1 ]; then
        echo "$CC $CFLAGS -c $src -o $obj"
    fi
    $CC $CFLAGS -c "$src" -o "$obj"
    echo -e "  ${GREEN}✓${NC} $src"
done

# Compile C++ files
echo -e "${YELLOW}Compiling C++ sources...${NC}"
for src in "${CXX_SOURCES[@]}"; do
    obj="${src%.cpp}.o"
    if [ $VERBOSE -eq 1 ]; then
        echo "$CXX $CXXFLAGS -c $src -o $obj"
    fi
    $CXX $CXXFLAGS -c "$src" -o "$obj"
    echo -e "  ${GREEN}✓${NC} $src"
done

# Link
echo -e "${YELLOW}Linking...${NC}"
OBJECTS=""
for src in "${C_SOURCES[@]}"; do
    OBJECTS="$OBJECTS ${src%.c}.o"
done
for src in "${CXX_SOURCES[@]}"; do
    OBJECTS="$OBJECTS ${src%.cpp}.o"
done

if [ $VERBOSE -eq 1 ]; then
    echo "$CXX $LDFLAGS $OBJECTS -o $OUTPUT"
fi
$CXX $LDFLAGS $OBJECTS -o "$OUTPUT"

# Clean up object files (optional)
# rm -f *.o

echo -e "${GREEN}Build complete: ${OUTPUT}${NC}"

# Show binary info
if command -v file &> /dev/null; then
    echo ""
    file "$OUTPUT"
fi

if command -v ls &> /dev/null; then
    ls -lh "$OUTPUT"
fi

echo -e "${GREEN}Done.${NC}"
