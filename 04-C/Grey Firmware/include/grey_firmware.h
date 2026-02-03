/**
 * @file grey_firmware.h
 * @brief Grey Firmware - Master Header
 * 
 * WHAT: Unified header for the Grey Firmware demonstration platform.
 * 
 * WHY: In production firmware, a clean namespace and modular architecture
 *      enable teams to work independently while maintaining system coherence.
 *      This header establishes the grey_firmware namespace and provides
 *      access to all subsystems.
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Single entry point for all firmware functionality
 *      - Clear module separation with defined interfaces
 *      - Type definitions that ensure cross-platform compatibility
 * 
 * @author Grey Firmware Team
 * @version 1.0.0
 */

#ifndef GREY_FIRMWARE_H
#define GREY_FIRMWARE_H

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Standard Type Definitions                                                  */
/*===========================================================================*/

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* Firmware version information */
#define GF_VERSION_MAJOR    1
#define GF_VERSION_MINOR    0
#define GF_VERSION_PATCH    0

/* Result codes used throughout Grey Firmware */
typedef enum {
    GF_OK = 0,              /* Operation successful */
    GF_ERR_GENERIC,         /* Generic error */
    GF_ERR_INVALID_PARAM,   /* Invalid parameter passed */
    GF_ERR_TIMEOUT,         /* Operation timed out */
    GF_ERR_BUSY,            /* Resource busy */
    GF_ERR_NO_MEMORY,       /* Memory allocation failed */
    GF_ERR_NOT_INIT,        /* Module not initialized */
    GF_ERR_ALREADY_INIT,    /* Module already initialized */
    GF_ERR_HW_FAULT,        /* Hardware fault detected */
    GF_ERR_CRYPTO,          /* Cryptographic operation failed */
    GF_ERR_AUTH_FAIL,       /* Authentication failed */
    GF_ERR_CRC,             /* CRC check failed */
    GF_ERR_OVERFLOW,        /* Buffer overflow */
    GF_ERR_UNDERFLOW,       /* Buffer underflow */
} gf_result_t;

/* Priority levels for scheduler */
typedef enum {
    GF_PRIORITY_IDLE = 0,
    GF_PRIORITY_LOW,
    GF_PRIORITY_NORMAL,
    GF_PRIORITY_HIGH,
    GF_PRIORITY_CRITICAL,
    GF_PRIORITY_COUNT
} gf_priority_t;

/* Forward declarations */
typedef struct gf_task gf_task_t;
typedef struct gf_driver gf_driver_t;
typedef struct gf_message gf_message_t;

/*===========================================================================*/
/* Core Framework Headers                                                     */
/*===========================================================================*/

#include "core/scheduler.h"
#include "core/driver_registry.h"
#include "core/message_bus.h"
#include "core/error_handler.h"

/*===========================================================================*/
/* Domain Module Headers                                                      */
/*===========================================================================*/

/* IoT Domain */
#include "iot/mqtt_client.h"
#include "iot/ota_update.h"

/* Automotive Domain */
#include "automotive/can_bus.h"
#include "automotive/watchdog.h"

/* Consumer Domain */
#include "consumer/usb_hid.h"
#include "consumer/display_driver.h"

/* Medical Domain */
#include "medical/sensor_acquisition.h"
#include "medical/calibration.h"

/* Security Domain */
#include "security/secure_boot.h"
#include "security/firmware_signing.h"

/*===========================================================================*/
/* System Initialization                                                      */
/*===========================================================================*/

/**
 * @brief Initialize the Grey Firmware system
 * @return GF_OK on success, error code otherwise
 */
gf_result_t gf_system_init(void);

/**
 * @brief Start the Grey Firmware scheduler (does not return)
 */
void gf_system_run(void);

/**
 * @brief Get system uptime in milliseconds
 */
uint32_t gf_get_uptime_ms(void);

#ifdef __cplusplus
}
#endif

#endif /* GREY_FIRMWARE_H */
