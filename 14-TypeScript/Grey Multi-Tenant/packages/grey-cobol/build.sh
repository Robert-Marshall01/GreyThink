#!/bin/bash
# Build script for Grey SDK COBOL package
# Requires GnuCOBOL (cobc)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$SCRIPT_DIR/src"
COPYBOOK_DIR="$SCRIPT_DIR/copybooks"
TEST_DIR="$SCRIPT_DIR/tests"
BUILD_DIR="$SCRIPT_DIR/build"

# Create build directory
mkdir -p "$BUILD_DIR"

echo "=== Building Grey SDK for COBOL ==="

# Compile all source files into object modules
echo "Compiling source modules..."

cobc -c -I "$COPYBOOK_DIR" -o "$BUILD_DIR/grey_error.o" "$SRC_DIR/grey_error.cbl"
cobc -c -I "$COPYBOOK_DIR" -o "$BUILD_DIR/json_utils.o" "$SRC_DIR/json_utils.cbl"
cobc -c -I "$COPYBOOK_DIR" -o "$BUILD_DIR/http_stubs.o" "$SRC_DIR/http_stubs.cbl"
cobc -c -I "$COPYBOOK_DIR" -o "$BUILD_DIR/http_client.o" "$SRC_DIR/http_client.cbl"
cobc -c -I "$COPYBOOK_DIR" -o "$BUILD_DIR/auth_client.o" "$SRC_DIR/auth_client.cbl"
cobc -c -I "$COPYBOOK_DIR" -o "$BUILD_DIR/user_client.o" "$SRC_DIR/user_client.cbl"
cobc -c -I "$COPYBOOK_DIR" -o "$BUILD_DIR/projects_client.o" "$SRC_DIR/projects_client.cbl"
cobc -c -I "$COPYBOOK_DIR" -o "$BUILD_DIR/query_client.o" "$SRC_DIR/query_client.cbl"
cobc -c -I "$COPYBOOK_DIR" -o "$BUILD_DIR/mutation_client.o" "$SRC_DIR/mutation_client.cbl"
cobc -c -I "$COPYBOOK_DIR" -o "$BUILD_DIR/grey_sdk.o" "$SRC_DIR/grey_sdk.cbl"

echo "Creating static library..."
ar rcs "$BUILD_DIR/libgreysdk.a" "$BUILD_DIR"/*.o

echo "Compiling test program..."
cobc -x -I "$COPYBOOK_DIR" -o "$BUILD_DIR/test_sdk" \
    "$TEST_DIR/test_sdk.cbl" \
    "$BUILD_DIR/libgreysdk.a"

echo ""
echo "=== Build Complete ==="
echo "Library: $BUILD_DIR/libgreysdk.a"
echo "Test:    $BUILD_DIR/test_sdk"
echo ""
echo "Run tests with: $BUILD_DIR/test_sdk"
