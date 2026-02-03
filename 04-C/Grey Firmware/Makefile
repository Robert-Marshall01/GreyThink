# ============================================================================
# Grey Firmware - Makefile
# ============================================================================
# A modular firmware demonstration project showcasing embedded systems expertise.
#
# Targets:
#   make            - Build the demo application
#   make clean      - Remove build artifacts
#   make debug      - Build with debug symbols
#   make release    - Build optimized release
#   make test       - Build and run unit tests
#   make test-unit  - Run unit tests only
#   make test-integ - Run integration tests only
#   make test-ai    - Run AI/ML tests only
#   make docs       - Generate documentation (requires Doxygen)
#   make format     - Format source code (requires clang-format)
#   make analyze    - Static analysis (requires cppcheck)
#
# Configuration:
#   CROSS_COMPILE   - Cross-compiler prefix (e.g., arm-none-eabi-)
#   TARGET_ARCH     - Target architecture flags
#   EXTRA_CFLAGS    - Additional compiler flags
# ============================================================================

# Project configuration
PROJECT     := grey_firmware
VERSION     := 1.0.0

# Directory structure
SRC_DIR     := src
INC_DIR     := include
BUILD_DIR   := build
OBJ_DIR     := $(BUILD_DIR)/obj
BIN_DIR     := $(BUILD_DIR)/bin

# Toolchain (cross-compile by setting CROSS_COMPILE)
CROSS_COMPILE ?=
CC          := $(CROSS_COMPILE)gcc
AR          := $(CROSS_COMPILE)ar
SIZE        := $(CROSS_COMPILE)size
OBJCOPY     := $(CROSS_COMPILE)objcopy
OBJDUMP     := $(CROSS_COMPILE)objdump

# Compiler flags
CFLAGS_COMMON := -Wall -Wextra -Wpedantic -Werror
CFLAGS_COMMON += -std=c11
CFLAGS_COMMON += -I$(INC_DIR)
CFLAGS_COMMON += -D_POSIX_C_SOURCE=200809L

# Debug flags
CFLAGS_DEBUG := $(CFLAGS_COMMON) -g3 -O0 -DDEBUG

# Release flags
CFLAGS_RELEASE := $(CFLAGS_COMMON) -O2 -DNDEBUG

# Default to debug
CFLAGS := $(CFLAGS_DEBUG)

# Linker flags
LDFLAGS := -lm

# Target architecture (override for embedded)
TARGET_ARCH ?=

# ============================================================================
# Source Files
# ============================================================================

# Core modules
CORE_SRC := \
    $(SRC_DIR)/core/scheduler.c \
    $(SRC_DIR)/core/driver_registry.c \
    $(SRC_DIR)/core/message_bus.c \
    $(SRC_DIR)/core/error_handler.c \
    $(SRC_DIR)/core/reliability.c \
    $(SRC_DIR)/core/metrics.c \
    $(SRC_DIR)/core/health.c

# IoT modules
IOT_SRC := \
    $(SRC_DIR)/iot/mqtt_client.c \
    $(SRC_DIR)/iot/ota_update.c

# Automotive modules
AUTO_SRC := \
    $(SRC_DIR)/automotive/can_bus.c \
    $(SRC_DIR)/automotive/watchdog.c

# Consumer modules
CONSUMER_SRC := \
    $(SRC_DIR)/consumer/usb_hid.c \
    $(SRC_DIR)/consumer/display_driver.c

# Medical modules
MEDICAL_SRC := \
    $(SRC_DIR)/medical/sensor_acquisition.c \
    $(SRC_DIR)/medical/calibration.c \
    $(SRC_DIR)/medical/medical.c

# Security modules
SECURITY_SRC := \
    $(SRC_DIR)/security/secure_boot.c \
    $(SRC_DIR)/security/firmware_signing.c

# Power management modules (Phase 3 spotlight)
POWER_SRC := \
    $(SRC_DIR)/power/power_state.c

# Audio modules (Phase 5)
AUDIO_SRC := \
    $(SRC_DIR)/audio/audio.c

# HMI modules (Phase 5)
HMI_SRC := \
    $(SRC_DIR)/hmi/hmi.c

# Safety-critical modules (Phase 5)
SAFETY_SRC := \
    $(SRC_DIR)/safety/safety.c

# Timekeeping modules (Phase 5)
TIME_SRC := \
    $(SRC_DIR)/time/timekeeping.c

# Networking modules (Phase 5 spotlight)
NET_SRC := \
    $(SRC_DIR)/net/tcp_ip.c \
    $(SRC_DIR)/net/dns.c \
    $(SRC_DIR)/net/http_client.c

# Aerospace modules (Phase 6)
AEROSPACE_SRC := \
    $(SRC_DIR)/aerospace/aerospace.c

# Industrial IoT modules (Phase 6)
INDUSTRIAL_SRC := \
    $(SRC_DIR)/industrial/industrial.c

# AI/ML modules (Phase 6 spotlight)
AI_SRC := \
    $(SRC_DIR)/ai/ai.c

# Smart Home modules (Phase 7 spotlight)
SMARTHOME_SRC := \
    $(SRC_DIR)/smarthome/smarthome.c \
    $(SRC_DIR)/smarthome/smarthome_spotlight.c

# Wearables modules (Phase 7)
WEARABLES_SRC := \
    $(SRC_DIR)/wearables/wearables.c

# Industrial Safety modules (Phase 7)
INDUSTRIAL_SAFETY_SRC := \
    $(SRC_DIR)/industrial_safety/industrial_safety.c

# Cloud modules (Phase 7)
CLOUD_SRC := \
    $(SRC_DIR)/cloud/cloud.c

# Storage modules (Phase 7)
STORAGE_SRC := \
    $(SRC_DIR)/storage/storage.c

# Energy modules (Phase 8 spotlight)
ENERGY_SRC := \
    $(SRC_DIR)/energy/energy_spotlight.c

# Drone modules (Phase 9 spotlight)
DRONE_SRC := \
    $(SRC_DIR)/drone/drone_spotlight.c

# Vision modules (Phase 10 spotlight)
VISION_SRC := \
    $(SRC_DIR)/vision/vision_spotlight.c

# Marine modules (Phase 11 spotlight)
MARINE_SRC := \
    $(SRC_DIR)/marine/marine_spotlight.c

# Renewable modules (Phase 12 spotlight)
RENEWABLE_SRC := \
    $(SRC_DIR)/renewable/renewable_spotlight.c

# Space Habitat modules (Phase 13 spotlight)
HABITAT_SRC := \
    $(SRC_DIR)/space_habitat/habitat_spotlight.c

# BMS modules (Phase 14 spotlight)
BMS_SRC := \
    $(SRC_DIR)/energy_storage/bms_spotlight.c

# FDR modules (Phase 15 spotlight)
FDR_SRC := \
    $(SRC_DIR)/aviation/fdr_spotlight.c

# Demo and main
DEMO_SRC := \
    $(SRC_DIR)/demo/demo.c \
    $(SRC_DIR)/demo/extended_demo.c \
    $(SRC_DIR)/main.c

# All sources (excluding main for library builds)
LIB_SRCS := $(CORE_SRC) $(IOT_SRC) $(AUTO_SRC) $(CONSUMER_SRC) $(MEDICAL_SRC) \
            $(SECURITY_SRC) $(POWER_SRC) $(AUDIO_SRC) $(HMI_SRC) $(SAFETY_SRC) \
            $(TIME_SRC) $(NET_SRC) $(AEROSPACE_SRC) $(INDUSTRIAL_SRC) $(AI_SRC) \
            $(SMARTHOME_SRC) $(WEARABLES_SRC) $(INDUSTRIAL_SAFETY_SRC) \
            $(CLOUD_SRC) $(STORAGE_SRC) $(ENERGY_SRC) $(DRONE_SRC) $(VISION_SRC) \
            $(MARINE_SRC) $(RENEWABLE_SRC) $(HABITAT_SRC) $(BMS_SRC) $(FDR_SRC) \
            $(SRC_DIR)/demo/demo.c $(SRC_DIR)/demo/extended_demo.c

# All sources for application
SRCS := $(CORE_SRC) $(IOT_SRC) $(AUTO_SRC) $(CONSUMER_SRC) $(MEDICAL_SRC) \
        $(SECURITY_SRC) $(POWER_SRC) $(AUDIO_SRC) $(HMI_SRC) $(SAFETY_SRC) \
        $(TIME_SRC) $(NET_SRC) $(AEROSPACE_SRC) $(INDUSTRIAL_SRC) $(AI_SRC) \
        $(SMARTHOME_SRC) $(WEARABLES_SRC) $(INDUSTRIAL_SAFETY_SRC) \
        $(CLOUD_SRC) $(STORAGE_SRC) $(ENERGY_SRC) $(DRONE_SRC) $(VISION_SRC) \
        $(MARINE_SRC) $(RENEWABLE_SRC) $(HABITAT_SRC) $(BMS_SRC) $(FDR_SRC) \
        $(DEMO_SRC)

# Test sources
TEST_DIR    := tests
TEST_COMMON := $(TEST_DIR)/test_framework.c
UNIT_TEST   := $(TEST_DIR)/unit_tests.c
INTEG_TEST  := $(TEST_DIR)/integration_tests.c
FAULT_TEST  := $(TEST_DIR)/fault_injection_tests.c
NET_TEST    := $(TEST_DIR)/network_tests.c
AI_TEST     := $(TEST_DIR)/ai_tests.c
SMARTHOME_TEST := $(TEST_DIR)/smarthome_tests.c
ENERGY_TEST := $(TEST_DIR)/energy_tests.c
DRONE_TEST  := $(TEST_DIR)/drone_tests.c
VISION_TEST := $(TEST_DIR)/vision_tests.c
MARINE_TEST := $(TEST_DIR)/marine_tests.c
RENEWABLE_TEST := $(TEST_DIR)/renewable_tests.c
HABITAT_TEST := $(TEST_DIR)/habitat_tests.c
BMS_TEST := $(TEST_DIR)/bms_tests.c
FDR_TEST := $(TEST_DIR)/fdr_tests.c
SAT_TEST := $(TEST_DIR)/satellite_tests.c
MICROGRID_TEST := $(TEST_DIR)/microgrid_tests.c
FIREFIGHTING_TEST := $(TEST_DIR)/firefighting_tests.c
PIPELINE_TEST := $(TEST_DIR)/test_pipeline.c
HYDROGEN_TEST := $(TEST_DIR)/test_hydrogen.c

# Object files
OBJS := $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)
LIB_OBJS := $(LIB_SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

# Dependency files
DEPS := $(OBJS:.o=.d)

# Output binary
TARGET := $(BIN_DIR)/$(PROJECT)

# ============================================================================
# Build Rules
# ============================================================================

.PHONY: all clean debug release docs format analyze help test test-unit test-integ test-ai

# Default target
all: $(TARGET)
	@echo ""
	@echo "Build complete: $(TARGET)"
	@$(SIZE) $(TARGET) 2>/dev/null || true
	@echo ""

# Debug build
debug: CFLAGS := $(CFLAGS_DEBUG)
debug: clean all

# Release build
release: CFLAGS := $(CFLAGS_RELEASE)
release: clean all
	@echo "Stripping symbols..."
	@strip $(TARGET) 2>/dev/null || true

# Link
$(TARGET): $(OBJS) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(TARGET_ARCH) $(OBJS) -o $@ $(LDFLAGS)

# Compile
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	@mkdir -p $(dir $@)
	@echo "[CC] $<"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -MMD -MP -c $< -o $@

# Create directories
$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)/core $(OBJ_DIR)/iot $(OBJ_DIR)/automotive
	@mkdir -p $(OBJ_DIR)/consumer $(OBJ_DIR)/medical $(OBJ_DIR)/security
	@mkdir -p $(OBJ_DIR)/power $(OBJ_DIR)/demo
	@mkdir -p $(OBJ_DIR)/audio $(OBJ_DIR)/hmi $(OBJ_DIR)/safety
	@mkdir -p $(OBJ_DIR)/time $(OBJ_DIR)/net
	@mkdir -p $(OBJ_DIR)/aerospace $(OBJ_DIR)/industrial $(OBJ_DIR)/ai
	@mkdir -p $(OBJ_DIR)/smarthome $(OBJ_DIR)/wearables $(OBJ_DIR)/industrial_safety
	@mkdir -p $(OBJ_DIR)/cloud $(OBJ_DIR)/storage $(OBJ_DIR)/energy $(OBJ_DIR)/drone
	@mkdir -p $(OBJ_DIR)/vision $(OBJ_DIR)/marine $(OBJ_DIR)/renewable
	@mkdir -p $(OBJ_DIR)/space_habitat $(OBJ_DIR)/energy_storage $(OBJ_DIR)/aviation $(OBJ_DIR)/space
	@mkdir -p $(OBJ_DIR)/asteroid $(OBJ_DIR)/hyperloop $(OBJ_DIR)/hyperloop

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

# Clean
clean:
	@echo "Cleaning..."
	@rm -rf $(BUILD_DIR)

# ============================================================================
# Test Targets
# ============================================================================

# Test binaries
TEST_BIN_UNIT  := $(BIN_DIR)/unit_tests
TEST_BIN_INTEG := $(BIN_DIR)/integration_tests
TEST_BIN_FAULT := $(BIN_DIR)/fault_injection_tests
TEST_BIN_NET   := $(BIN_DIR)/network_tests
TEST_BIN_AI    := $(BIN_DIR)/ai_tests
TEST_BIN_SMARTHOME := $(BIN_DIR)/smarthome_tests
TEST_BIN_ENERGY := $(BIN_DIR)/energy_tests
TEST_BIN_DRONE  := $(BIN_DIR)/drone_tests
TEST_BIN_VISION := $(BIN_DIR)/vision_tests
TEST_BIN_MARINE := $(BIN_DIR)/marine_tests
TEST_BIN_RENEWABLE := $(BIN_DIR)/renewable_tests
TEST_BIN_HABITAT := $(BIN_DIR)/habitat_tests
TEST_BIN_BMS := $(BIN_DIR)/bms_tests
TEST_BIN_FDR := $(BIN_DIR)/fdr_tests
TEST_BIN_SAT := $(BIN_DIR)/satellite_tests
TEST_BIN_MICROGRID := $(BIN_DIR)/microgrid_tests
TEST_BIN_FIREFIGHTING := $(BIN_DIR)/firefighting_tests
TEST_BIN_PIPELINE := $(BIN_DIR)/pipeline_tests
TEST_BIN_HYDROGEN := $(BIN_DIR)/hydrogen_tests

# Build and run all tests
test: test-unit test-integ test-fault test-net test-ai test-smarthome test-energy test-drone test-vision test-marine test-renewable test-habitat test-bms test-fdr test-satellite test-microgrid test-firefighting test-pipeline test-hydrogen test-rocket test-radiation test-debris test-vessel test-lunar test-orbital test-asteroid test-hyperloop test-hyperloop

# Unit tests
test-unit: $(TEST_BIN_UNIT)
	@echo ""
	@echo "Running unit tests..."
	@echo "================================================================"
	@$(TEST_BIN_UNIT)

# Integration tests
test-integ: $(TEST_BIN_INTEG)
	@echo ""
	@echo "Running integration tests..."
	@echo "================================================================"
	@$(TEST_BIN_INTEG)

# Fault injection tests
test-fault: $(TEST_BIN_FAULT)
	@echo ""
	@echo "Running fault injection tests..."
	@echo "================================================================"
	@$(TEST_BIN_FAULT)

# Network tests
test-net: $(TEST_BIN_NET)
	@echo ""
	@echo "Running network tests..."
	@echo "================================================================"
	@$(TEST_BIN_NET)

# AI tests
test-ai: $(TEST_BIN_AI)
	@echo ""
	@echo "Running AI/ML tests..."
	@echo "================================================================"
	@$(TEST_BIN_AI)

# Build unit test binary
$(TEST_BIN_UNIT): $(LIB_OBJS) $(TEST_COMMON) $(UNIT_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(LIB_OBJS) $(TEST_COMMON) $(UNIT_TEST) \
		-o $@ $(LDFLAGS)

# Build integration test binary
$(TEST_BIN_INTEG): $(LIB_OBJS) $(TEST_COMMON) $(INTEG_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(LIB_OBJS) $(TEST_COMMON) $(INTEG_TEST) \
		-o $@ $(LDFLAGS)

# Build fault injection test binary (compile reliability.c with injection enabled)
FAULT_LIB_OBJS := $(filter-out $(OBJ_DIR)/core/reliability.o,$(LIB_OBJS))
$(TEST_BIN_FAULT): $(FAULT_LIB_OBJS) $(FAULT_TEST) $(SRC_DIR)/core/reliability.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) -DGF_ENABLE_FAULT_INJECTION \
		$(FAULT_LIB_OBJS) $(SRC_DIR)/core/reliability.c $(FAULT_TEST) \
		-o $@ $(LDFLAGS)

# Build network test binary
$(TEST_BIN_NET): $(LIB_OBJS) $(NET_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(LIB_OBJS) $(NET_TEST) \
		-o $@ $(LDFLAGS)

# Build AI test binary (standalone)
$(TEST_BIN_AI): $(AI_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(AI_TEST) \
		-o $@ $(LDFLAGS)

# Smart Home tests
test-smarthome: $(TEST_BIN_SMARTHOME)
	@echo ""
	@echo "Running Smart Home tests..."
	@echo "================================================================"
	@$(TEST_BIN_SMARTHOME)

# Build Smart Home test binary (standalone)
$(TEST_BIN_SMARTHOME): $(SMARTHOME_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(SMARTHOME_TEST) \
		-o $@ $(LDFLAGS)

# Energy tests
test-energy: $(TEST_BIN_ENERGY)
	@echo ""
	@echo "Running Energy tests..."
	@echo "================================================================"
	@$(TEST_BIN_ENERGY)

# Build Energy test binary (standalone)
$(TEST_BIN_ENERGY): $(ENERGY_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(ENERGY_TEST) \
		-o $@ $(LDFLAGS)

# Drone tests
test-drone: $(TEST_BIN_DRONE)
	@echo ""
	@echo "Running Drone tests..."
	@echo "================================================================"
	@$(TEST_BIN_DRONE)

# Build Drone test binary (standalone)
$(TEST_BIN_DRONE): $(DRONE_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(DRONE_TEST) \
		-o $@ $(LDFLAGS)

# Vision tests
test-vision: $(TEST_BIN_VISION)
	@echo ""
	@echo "Running Vision tests..."
	@echo "================================================================"
	@$(TEST_BIN_VISION)

# Build Vision test binary (standalone)
$(TEST_BIN_VISION): $(VISION_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(VISION_TEST) \
		-o $@ $(LDFLAGS)

# Marine tests
test-marine: $(TEST_BIN_MARINE)
	@echo ""
	@echo "Running Marine tests..."
	@echo "================================================================"
	@$(TEST_BIN_MARINE)

# Build Marine test binary (standalone)
$(TEST_BIN_MARINE): $(MARINE_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(MARINE_TEST) \
		-o $@ $(LDFLAGS)

# Renewable tests
test-renewable: $(TEST_BIN_RENEWABLE)
	@echo ""
	@echo "Running Renewable tests..."
	@echo "================================================================"
	@$(TEST_BIN_RENEWABLE)

# Build Renewable test binary (standalone)
$(TEST_BIN_RENEWABLE): $(RENEWABLE_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(RENEWABLE_TEST) \
		-o $@ $(LDFLAGS)

# Habitat tests
test-habitat: $(TEST_BIN_HABITAT)
	@echo ""
	@echo "Running Habitat tests..."
	@echo "================================================================"
	@$(TEST_BIN_HABITAT)

# Build Habitat test binary (standalone)
$(TEST_BIN_HABITAT): $(HABITAT_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(HABITAT_TEST) \
		-o $@ $(LDFLAGS)

# BMS tests
test-bms: $(TEST_BIN_BMS)
	@echo ""
	@echo "Running BMS tests..."
	@echo "================================================================"
	@$(TEST_BIN_BMS)

# Build BMS test binary (includes bms_spotlight.c for implementation)
$(TEST_BIN_BMS): $(BMS_TEST) $(SRC_DIR)/energy_storage/bms_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(SRC_DIR)/energy_storage/bms_spotlight.c $(BMS_TEST) \
		-o $@ $(LDFLAGS)

# FDR tests
test-fdr: $(TEST_BIN_FDR)
	@echo ""
	@echo "Running FDR tests..."
	@echo "================================================================"
	@$(TEST_BIN_FDR)

# Build FDR test binary (includes fdr_spotlight.c for implementation)
$(TEST_BIN_FDR): $(FDR_TEST) $(SRC_DIR)/aviation/fdr_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(SRC_DIR)/aviation/fdr_spotlight.c $(FDR_TEST) \
		-o $@ $(LDFLAGS)

# Satellite tests
test-satellite: $(TEST_BIN_SAT)
	@echo ""
	@echo "Running Satellite Communication tests..."
	@echo "================================================================"
	@$(TEST_BIN_SAT)

# Build Satellite test binary (includes sat_comm_spotlight.c for implementation)
$(TEST_BIN_SAT): $(SAT_TEST) $(SRC_DIR)/space/sat_comm_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(SRC_DIR)/space/sat_comm_spotlight.c $(SAT_TEST) \
		-o $@ $(LDFLAGS)

# Microgrid tests
test-microgrid: $(TEST_BIN_MICROGRID)
	@echo ""
	@echo "Running Microgrid Controller tests..."
	@echo "================================================================"
	@$(TEST_BIN_MICROGRID)

# Build Microgrid test binary (standalone, includes implementation directly)
$(TEST_BIN_MICROGRID): $(MICROGRID_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(MICROGRID_TEST) \
		-o $@ $(LDFLAGS)

# Firefighting tests
test-firefighting: $(TEST_BIN_FIREFIGHTING)
	@echo ""
	@echo "Running Firefighting Thermal Imaging & Suppression tests..."
	@echo "================================================================"
	@$(TEST_BIN_FIREFIGHTING)

# Build Firefighting test binary (standalone, includes implementation directly)
$(TEST_BIN_FIREFIGHTING): $(FIREFIGHTING_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(FIREFIGHTING_TEST) \
		-o $@ $(LDFLAGS)

# Pipeline Leak Detection tests
test-pipeline: $(TEST_BIN_PIPELINE)
	@echo ""
	@echo "Running Pipeline Leak Detection tests..."
	@echo "================================================================"
	@$(TEST_BIN_PIPELINE)

# Build Pipeline test binary (standalone, includes implementation directly)
$(TEST_BIN_PIPELINE): $(PIPELINE_TEST) | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(PIPELINE_TEST) \
		-o $@ $(LDFLAGS)

# Hydrogen Fuel Cell tests
test-hydrogen: $(TEST_BIN_HYDROGEN)
	@echo ""
	@echo "Running Hydrogen Fuel Cell tests..."
	@echo "================================================================"
	@$(TEST_BIN_HYDROGEN)

# Build Hydrogen test binary (includes hydrogen_spotlight.c for implementation)
$(TEST_BIN_HYDROGEN): $(HYDROGEN_TEST) $(SRC_DIR)/hydrogen/hydrogen_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(SRC_DIR)/hydrogen/hydrogen_spotlight.c $(HYDROGEN_TEST) \
		-o $@ $(LDFLAGS)

# Rocket Engine Telemetry & Safety variables
ROCKET_TEST = $(TEST_DIR)/test_rocket.c
TEST_BIN_ROCKET = $(BIN_DIR)/test_rocket

# Rocket Engine tests
test-rocket: $(TEST_BIN_ROCKET)
	@echo ""
	@echo "Running Rocket Engine Telemetry & Safety tests..."
	@echo "================================================================"
	@$(TEST_BIN_ROCKET)

# Build Rocket test binary (test file includes spotlight.c directly)
$(TEST_BIN_ROCKET): $(ROCKET_TEST) $(SRC_DIR)/rocket/rocket_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(ROCKET_TEST) \
		-o $@ $(LDFLAGS)

# Radiation Shielding Control variables
RADIATION_TEST = $(TEST_DIR)/test_radiation.c
TEST_BIN_RADIATION = $(BIN_DIR)/test_radiation

# Radiation Shielding tests
test-radiation: $(TEST_BIN_RADIATION)
	@echo ""
	@echo "Running Radiation Shielding Control & Telemetry tests..."
	@echo "================================================================"
	@$(TEST_BIN_RADIATION)

# Build Radiation test binary (test file includes spotlight.c directly)
$(TEST_BIN_RADIATION): $(RADIATION_TEST) $(SRC_DIR)/radiation/radiation_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(RADIATION_TEST) \
		-o $@ $(LDFLAGS)

# Orbital Debris Tracking & Collision Prediction variables
DEBRIS_TEST = $(TEST_DIR)/test_debris.c
TEST_BIN_DEBRIS = $(BIN_DIR)/test_debris

# Debris Tracking tests
test-debris: $(TEST_BIN_DEBRIS)
	@echo ""
	@echo "Running Orbital Debris Tracking & Collision Prediction tests..."
	@echo "================================================================"
	@$(TEST_BIN_DEBRIS)

# Build Debris test binary (test file includes spotlight.c directly)
$(TEST_BIN_DEBRIS): $(DEBRIS_TEST) $(SRC_DIR)/debris/debris_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(DEBRIS_TEST) \
		-o $@ $(LDFLAGS)

# Autonomous Vessel Control & Safety variables
VESSEL_TEST = $(TEST_DIR)/test_vessel.c
TEST_BIN_VESSEL = $(BIN_DIR)/test_vessel

# Vessel Control tests
test-vessel: $(TEST_BIN_VESSEL)
	@echo ""
	@echo "Running Autonomous Vessel Control & Safety tests..."
	@echo "================================================================"
	@$(TEST_BIN_VESSEL)

# Build Vessel test binary (test file includes spotlight.c directly)
$(TEST_BIN_VESSEL): $(VESSEL_TEST) $(SRC_DIR)/vessel/vessel_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) \
		$(VESSEL_TEST) \
		-o $@ $(LDFLAGS)

# Lunar Mining Regolith Extraction variables
LUNAR_TEST = $(TEST_DIR)/test_lunar.c
TEST_BIN_LUNAR = $(BIN_DIR)/test_lunar

# Lunar Mining tests
test-lunar: $(TEST_BIN_LUNAR)
	@echo ""
	@echo "Running Lunar Mining Regolith Extraction tests..."
	@echo "================================================================"
	@$(TEST_BIN_LUNAR)

# Build Lunar test binary (includes lunar_spotlight.c with UNIT_TEST defined)
$(TEST_BIN_LUNAR): $(LUNAR_TEST) $(SRC_DIR)/lunar/lunar_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) -DUNIT_TEST \
		$(SRC_DIR)/lunar/lunar_spotlight.c $(LUNAR_TEST) \
		-o $@ $(LDFLAGS)

# Orbital Manufacturing Zero-Gravity Fabrication variables
ORBITAL_TEST = $(TEST_DIR)/test_orbital.c
TEST_BIN_ORBITAL = $(BIN_DIR)/test_orbital

# Orbital Manufacturing tests
test-orbital: $(TEST_BIN_ORBITAL)
	@echo ""
	@echo "Running Zero-Gravity Fabrication Control tests..."
	@echo "================================================================"
	@$(TEST_BIN_ORBITAL)

# Build Orbital test binary (includes orbital_spotlight.c with UNIT_TEST defined)
$(TEST_BIN_ORBITAL): $(ORBITAL_TEST) $(SRC_DIR)/orbital/orbital_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) -DUNIT_TEST \
		$(SRC_DIR)/orbital/orbital_spotlight.c $(ORBITAL_TEST) \
		-o $@ $(LDFLAGS)

# Asteroid Drill Control & Telemetry variables
ASTEROID_TEST = $(TEST_DIR)/test_asteroid.c
TEST_BIN_ASTEROID = $(BIN_DIR)/test_asteroid

# Asteroid Drill tests
test-asteroid: $(TEST_BIN_ASTEROID)
	@echo ""
	@echo "Running Asteroid Drill Control & Telemetry tests..."
	@echo "================================================================"
	@$(TEST_BIN_ASTEROID)

# Build Asteroid test binary (includes asteroid_spotlight.c with UNIT_TEST defined)
$(TEST_BIN_ASTEROID): $(ASTEROID_TEST) $(SRC_DIR)/asteroid/asteroid_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) -DUNIT_TEST \
		$(SRC_DIR)/asteroid/asteroid_spotlight.c $(ASTEROID_TEST) \
		-o $@ $(LDFLAGS)

# Hyperloop Pod Control & Safety variables
HYPERLOOP_TEST = $(TEST_DIR)/test_hyperloop.c
TEST_BIN_HYPERLOOP = $(BIN_DIR)/test_hyperloop

# Hyperloop tests
test-hyperloop: $(TEST_BIN_HYPERLOOP)
	@echo ""
	@echo "Running Hyperloop Pod Control & Safety tests..."
	@echo "================================================================"
	@$(TEST_BIN_HYPERLOOP)

# Build Hyperloop test binary (test file includes hyperloop_spotlight.c directly)
$(TEST_BIN_HYPERLOOP): $(HYPERLOOP_TEST) $(SRC_DIR)/hyperloop/hyperloop_spotlight.c | $(BIN_DIR)
	@echo "[LD] $@"
	@$(CC) $(CFLAGS) $(TARGET_ARCH) -I$(INC_DIR) -DUNIT_TEST \
		$(HYPERLOOP_TEST) \
		-o $@ $(LDFLAGS)

# ============================================================================
# Development Tools
# ============================================================================

# Generate documentation
docs:
	@echo "Generating documentation..."
	@doxygen Doxyfile 2>/dev/null || echo "Doxygen not installed"

# Format source code
format:
	@echo "Formatting source code..."
	@find $(SRC_DIR) $(INC_DIR) -name "*.c" -o -name "*.h" | \
		xargs clang-format -i 2>/dev/null || echo "clang-format not installed"

# Static analysis
analyze:
	@echo "Running static analysis..."
	@cppcheck --enable=all --inconclusive --std=c11 \
		-I$(INC_DIR) $(SRC_DIR) 2>/dev/null || echo "cppcheck not installed"

# Help
help:
	@echo ""
	@echo "Grey Firmware Build System"
	@echo "=========================="
	@echo ""
	@echo "Targets:"
	@echo "  make            Build debug version"
	@echo "  make release    Build optimized release"
	@echo "  make clean      Remove build artifacts"
	@echo "  make test       Run all tests"
	@echo "  make test-unit  Run unit tests only"
	@echo "  make test-integ Run integration tests only"
	@echo "  make test-fault Run fault injection tests only"
	@echo "  make test-net   Run network tests only"
	@echo "  make test-ai    Run AI/ML tests only"
	@echo "  make test-smarthome Run Smart Home tests only"
	@echo "  make test-energy Run Energy tests only"
	@echo "  make test-drone  Run Drone tests only"
	@echo "  make test-vision Run Vision tests only"
	@echo "  make test-marine Run Marine tests only"
	@echo "  make test-renewable Run Renewable tests only"
	@echo "  make test-habitat Run Habitat tests only"
	@echo "  make test-bms    Run BMS tests only"
	@echo "  make test-fdr    Run FDR tests only"
	@echo "  make test-satellite Run Satellite tests only"
	@echo "  make test-microgrid Run Microgrid tests only"
	@echo "  make test-firefighting Run Firefighting tests only"
	@echo "  make test-pipeline Run Pipeline tests only"
	@echo "  make test-hydrogen Run Hydrogen Fuel Cell tests only"
	@echo "  make test-rocket Run Rocket Engine tests only"
	@echo "  make test-radiation Run Radiation Shielding tests only"
	@echo "  make test-debris Run Debris Tracking tests only"
	@echo "  make test-vessel Run Vessel Control tests only"
	@echo "  make test-lunar  Run Lunar Mining tests only"
	@echo "  make test-orbital Run Orbital Manufacturing tests only"
	@echo "  make test-asteroid Run Asteroid Drill tests only"
	@echo "  make test-hyperloop Run Hyperloop Pod tests only"
	@echo "  make docs       Generate documentation"
	@echo "  make format     Format source code"
	@echo "  make analyze    Run static analysis"
	@echo ""
	@echo "Cross-compilation:"
	@echo "  make CROSS_COMPILE=arm-none-eabi-"
	@echo ""
	@echo "Directory structure:"
	@echo "  $(INC_DIR)/    Header files"
	@echo "  $(SRC_DIR)/    Source files"
	@echo "  $(TEST_DIR)/   Test files"
	@echo "  $(BUILD_DIR)/  Build output"
	@echo ""

# Include dependency files
-include $(DEPS)
