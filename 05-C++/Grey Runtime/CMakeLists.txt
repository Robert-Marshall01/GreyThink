cmake_minimum_required(VERSION 3.20)
project(GreyRuntime VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Options
option(GREY_ENABLE_JIT "Enable JIT compiler" ON)
option(GREY_ENABLE_SANDBOX "Enable sandboxing layer" ON)
option(GREY_ENABLE_DEBUG "Enable debugging/introspection" ON)
option(GREY_ENABLE_CRYPTO "Enable crypto library" ON)
option(GREY_ENABLE_NETWORKING "Enable networking library" ON)

# Platform detection
if(WIN32)
    add_definitions(-DGREY_PLATFORM_WINDOWS)
    set(PLATFORM_LIBS ws2_32 bcrypt)
elseif(APPLE)
    add_definitions(-DGREY_PLATFORM_MACOS)
    set(PLATFORM_LIBS pthread dl)
elseif(UNIX)
    add_definitions(-DGREY_PLATFORM_LINUX)
    set(PLATFORM_LIBS pthread dl)
endif()

# Source files (exclude main.cpp from library)
file(GLOB_RECURSE GREY_SOURCES
    "src/*.cpp"
)
list(FILTER GREY_SOURCES EXCLUDE REGEX "src/main\\.cpp$")

file(GLOB_RECURSE GREY_HEADERS
    "include/*.h"
    "include/*.hpp"
)

# Main library
add_library(grey_runtime STATIC ${GREY_SOURCES} ${GREY_HEADERS})
target_include_directories(grey_runtime PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)
target_link_libraries(grey_runtime PUBLIC ${PLATFORM_LIBS})

if(GREY_ENABLE_JIT)
    target_compile_definitions(grey_runtime PUBLIC GREY_ENABLE_JIT=1)
endif()
if(GREY_ENABLE_SANDBOX)
    target_compile_definitions(grey_runtime PUBLIC GREY_ENABLE_SANDBOX=1)
endif()
if(GREY_ENABLE_DEBUG)
    target_compile_definitions(grey_runtime PUBLIC GREY_ENABLE_DEBUG=1)
endif()
if(GREY_ENABLE_CRYPTO)
    target_compile_definitions(grey_runtime PUBLIC GREY_ENABLE_CRYPTO=1)
endif()
if(GREY_ENABLE_NETWORKING)
    target_compile_definitions(grey_runtime PUBLIC GREY_ENABLE_NETWORKING=1)
endif()

# CLI executable
add_executable(grey src/main.cpp)
target_link_libraries(grey PRIVATE grey_runtime)

# Tests
enable_testing()
file(GLOB TEST_SOURCES "tests/*.cpp")
if(TEST_SOURCES)
    add_executable(grey_tests ${TEST_SOURCES})
    target_link_libraries(grey_tests PRIVATE grey_runtime)
    add_test(NAME GreyRuntimeTests COMMAND grey_tests)
endif()

# Example programs
file(GLOB EXAMPLE_SOURCES "examples/*.cpp")
foreach(EXAMPLE_SRC ${EXAMPLE_SOURCES})
    get_filename_component(EXAMPLE_NAME ${EXAMPLE_SRC} NAME_WE)
    add_executable(${EXAMPLE_NAME} ${EXAMPLE_SRC})
    target_link_libraries(${EXAMPLE_NAME} PRIVATE grey_runtime)
endforeach()
