# Grey SDK for COBOL - Makefile
# For use with GnuCOBOL on Windows and Unix-like systems

COBC = cobc
COBCFLAGS = -I copybooks

SRC_DIR = src
COPYBOOK_DIR = copybooks
TEST_DIR = tests
BUILD_DIR = build

# Source files
SOURCES = \
	$(SRC_DIR)/grey_error.cbl \
	$(SRC_DIR)/json_utils.cbl \
	$(SRC_DIR)/http_stubs.cbl \
	$(SRC_DIR)/http_client.cbl \
	$(SRC_DIR)/auth_client.cbl \
	$(SRC_DIR)/user_client.cbl \
	$(SRC_DIR)/projects_client.cbl \
	$(SRC_DIR)/query_client.cbl \
	$(SRC_DIR)/mutation_client.cbl \
	$(SRC_DIR)/grey_sdk.cbl

# Object files
OBJECTS = \
	$(BUILD_DIR)/grey_error.o \
	$(BUILD_DIR)/json_utils.o \
	$(BUILD_DIR)/http_stubs.o \
	$(BUILD_DIR)/http_client.o \
	$(BUILD_DIR)/auth_client.o \
	$(BUILD_DIR)/user_client.o \
	$(BUILD_DIR)/projects_client.o \
	$(BUILD_DIR)/query_client.o \
	$(BUILD_DIR)/mutation_client.o \
	$(BUILD_DIR)/grey_sdk.o

# Library
LIBRARY = $(BUILD_DIR)/libgreysdk.a

# Test executable
TEST_EXE = $(BUILD_DIR)/test_sdk

.PHONY: all clean test dirs

all: dirs $(LIBRARY)

dirs:
	@if not exist $(BUILD_DIR) mkdir $(BUILD_DIR)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cbl
	$(COBC) -c $(COBCFLAGS) -o $@ $<

$(LIBRARY): $(OBJECTS)
	ar rcs $@ $(OBJECTS)

$(TEST_EXE): $(LIBRARY) $(TEST_DIR)/test_sdk.cbl
	$(COBC) -x $(COBCFLAGS) -o $@ $(TEST_DIR)/test_sdk.cbl $(LIBRARY)

test: $(TEST_EXE)
	$(TEST_EXE)

clean:
	@if exist $(BUILD_DIR) rmdir /s /q $(BUILD_DIR)
