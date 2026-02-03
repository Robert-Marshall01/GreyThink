# ============================================================================
# Grey Firmware - CMake Build Configuration
# ============================================================================
# Cross-platform build support with CMake 3.13+
#
# Build instructions:
#   mkdir build && cd build
#   cmake ..
#   cmake --build .
#
# Debug/Release:
#   cmake -DCMAKE_BUILD_TYPE=Release ..
#
# Cross-compile:
#   cmake -DCMAKE_TOOLCHAIN_FILE=arm-toolchain.cmake ..
# ============================================================================

cmake_minimum_required(VERSION 3.13)

project(grey_firmware
    VERSION 1.0.0
    DESCRIPTION "Modular Firmware Framework Demonstration"
    LANGUAGES C
)

# C standard
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Export compile commands for IDE support
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# ============================================================================
# Compiler Flags
# ============================================================================

# Common warnings
add_compile_options(
    -Wall
    -Wextra
    -Wpedantic
    $<$<CONFIG:Debug>:-g3>
    $<$<CONFIG:Debug>:-O0>
    $<$<CONFIG:Release>:-O2>
)

# POSIX compatibility
add_definitions(-D_POSIX_C_SOURCE=200809L)

# Debug definition
add_compile_definitions($<$<CONFIG:Debug>:DEBUG>)

# ============================================================================
# Include Directories
# ============================================================================

include_directories(${CMAKE_SOURCE_DIR}/include)

# ============================================================================
# Source Files
# ============================================================================

# Core modules
set(CORE_SOURCES
    src/core/scheduler.c
    src/core/driver_registry.c
    src/core/message_bus.c
    src/core/error_handler.c
)

# IoT modules
set(IOT_SOURCES
    src/iot/mqtt_client.c
    src/iot/ota_update.c
)

# Automotive modules
set(AUTO_SOURCES
    src/automotive/can_bus.c
    src/automotive/watchdog.c
)

# Consumer modules
set(CONSUMER_SOURCES
    src/consumer/usb_hid.c
    src/consumer/display_driver.c
)

# Medical modules
set(MEDICAL_SOURCES
    src/medical/sensor_acquisition.c
    src/medical/calibration.c
)

# Security modules
set(SECURITY_SOURCES
    src/security/secure_boot.c
    src/security/firmware_signing.c
)

# Demo
set(DEMO_SOURCES
    src/demo/demo.c
)

# ============================================================================
# Library Target
# ============================================================================

add_library(grey_firmware_lib STATIC
    ${CORE_SOURCES}
    ${IOT_SOURCES}
    ${AUTO_SOURCES}
    ${CONSUMER_SOURCES}
    ${MEDICAL_SOURCES}
    ${SECURITY_SOURCES}
    ${DEMO_SOURCES}
)

target_include_directories(grey_firmware_lib PUBLIC
    ${CMAKE_SOURCE_DIR}/include
)

# ============================================================================
# Executable Target
# ============================================================================

add_executable(grey_firmware
    src/main.c
)

target_link_libraries(grey_firmware
    grey_firmware_lib
    m  # Math library
)

# ============================================================================
# Installation
# ============================================================================

install(TARGETS grey_firmware
    RUNTIME DESTINATION bin
)

install(DIRECTORY include/
    DESTINATION include/grey_firmware
    FILES_MATCHING PATTERN "*.h"
)

# ============================================================================
# Custom Targets
# ============================================================================

# Documentation (Doxygen)
find_package(Doxygen)
if(DOXYGEN_FOUND)
    add_custom_target(docs
        COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_SOURCE_DIR}/Doxyfile
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Generating documentation with Doxygen"
    )
endif()

# Format (clang-format)
find_program(CLANG_FORMAT clang-format)
if(CLANG_FORMAT)
    file(GLOB_RECURSE ALL_SOURCE_FILES
        ${CMAKE_SOURCE_DIR}/src/*.c
        ${CMAKE_SOURCE_DIR}/include/*.h
    )
    add_custom_target(format
        COMMAND ${CLANG_FORMAT} -i ${ALL_SOURCE_FILES}
        COMMENT "Formatting source code with clang-format"
    )
endif()

# Static analysis (cppcheck)
find_program(CPPCHECK cppcheck)
if(CPPCHECK)
    add_custom_target(analyze
        COMMAND ${CPPCHECK}
            --enable=all
            --inconclusive
            --std=c11
            -I${CMAKE_SOURCE_DIR}/include
            ${CMAKE_SOURCE_DIR}/src
        COMMENT "Running static analysis with cppcheck"
    )
endif()

# ============================================================================
# Status
# ============================================================================

message(STATUS "")
message(STATUS "Grey Firmware v${PROJECT_VERSION}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C Compiler: ${CMAKE_C_COMPILER}")
message(STATUS "  Install prefix: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "")
