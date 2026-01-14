/**
 * Grey Optimizer - Platform Detection Header
 * 
 * This header provides platform detection macros and cross-platform
 * type definitions for the enforcement modules.
 */

#ifndef GREY_PLATFORM_H
#define GREY_PLATFORM_H

/* Platform detection */
#if defined(_WIN32) || defined(_WIN64)
    #define GREY_PLATFORM_WINDOWS 1
    #define GREY_PLATFORM_NAME "Windows"
#elif defined(__APPLE__) && defined(__MACH__)
    #define GREY_PLATFORM_MACOS 1
    #define GREY_PLATFORM_NAME "macOS"
#elif defined(__linux__)
    #define GREY_PLATFORM_LINUX 1
    #define GREY_PLATFORM_NAME "Linux"
#else
    #error "Unsupported platform"
#endif

/* Export macros for shared libraries */
#ifdef GREY_PLATFORM_WINDOWS
    #ifdef GREY_BUILDING_DLL
        #define GREY_API __declspec(dllexport)
    #else
        #define GREY_API __declspec(dllimport)
    #endif
#else
    #define GREY_API __attribute__((visibility("default")))
#endif

/* Standard includes */
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* Platform-specific includes */
#ifdef GREY_PLATFORM_LINUX
    #include <unistd.h>
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <fcntl.h>
    #include <errno.h>
    #include <sched.h>
    #include <sys/resource.h>
#endif

#ifdef GREY_PLATFORM_MACOS
    #include <unistd.h>
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <fcntl.h>
    #include <errno.h>
    #include <sys/resource.h>
#endif

#ifdef GREY_PLATFORM_WINDOWS
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
    #include <processthreadsapi.h>
    #include <winbase.h>
#endif

/* Common error codes */
typedef enum {
    GREY_OK = 0,
    GREY_ERROR_INVALID_PARAM = -1,
    GREY_ERROR_PERMISSION = -2,
    GREY_ERROR_NOT_FOUND = -3,
    GREY_ERROR_IO = -4,
    GREY_ERROR_NOT_SUPPORTED = -5,
    GREY_ERROR_SYSTEM = -6,
    GREY_ERROR_SIMULATION = -7,  /* Not an error, just simulation mode */
} grey_error_t;

/* Result structure for enforcement operations */
typedef struct {
    grey_error_t code;
    char message[256];
    bool simulated;
} grey_result_t;

/* Initialize result with success */
static inline grey_result_t grey_result_ok(bool simulated) {
    grey_result_t r = {0};
    r.code = GREY_OK;
    r.simulated = simulated;
    return r;
}

/* Initialize result with error */
static inline grey_result_t grey_result_error(grey_error_t code, const char* msg) {
    grey_result_t r = {0};
    r.code = code;
    r.simulated = false;
    if (msg) {
        size_t len = strlen(msg);
        if (len >= sizeof(r.message)) len = sizeof(r.message) - 1;
        memcpy(r.message, msg, len);
        r.message[len] = '\0';
    }
    return r;
}

/* Global simulation mode flag */
extern bool grey_simulation_mode;

/**
 * Set simulation mode globally.
 * 
 * When simulation mode is enabled, all enforcement operations
 * will log their intent but not actually modify the system.
 */
GREY_API void grey_set_simulation_mode(bool enabled);

/**
 * Get current simulation mode.
 */
GREY_API bool grey_get_simulation_mode(void);

/**
 * Get platform name.
 */
GREY_API const char* grey_get_platform(void);

/**
 * Initialize the enforcement library.
 * 
 * Must be called before any enforcement operations.
 * Returns 0 on success, negative on error.
 */
GREY_API int grey_init(void);

/**
 * Cleanup the enforcement library.
 */
GREY_API void grey_cleanup(void);

#endif /* GREY_PLATFORM_H */
