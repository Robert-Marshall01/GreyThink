/**
 * @file nurse_call.h
 * @brief Nurse Call System Interface for Smart Healthcare Facilities
 * 
 * @details
 * This module implements the embedded interface for hospital nurse call systems
 * including patient call buttons, staff locating, and integration with clinical
 * workflows and electronic health records.
 * 
 * INDUSTRY RELEVANCE:
 * - Hospital nurse call systems (Ascom, Rauland, Jeron)
 * - Clinical workflow management
 * - Patient room automation
 * - Staff safety/duress systems
 * - Real-time locating systems (RTLS)
 * - EHR/EMR integration
 * 
 * STANDARDS COMPLIANCE:
 * - UL 1069 (Hospital signaling equipment)
 * - IEC 60601-1 (Medical electrical equipment)
 * - HL7 integration protocols
 * - HIPAA privacy requirements
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_NURSE_CALL_H
#define GF_NURSE_CALL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_NC_OK = 0,
    GF_NC_ERROR_NOT_INITIALIZED,
    GF_NC_ERROR_NULL_PTR,
    GF_NC_ERROR_INVALID_ROOM,
    GF_NC_ERROR_QUEUE_FULL,
    GF_NC_ERROR_COMMS,
    GF_NC_WARN_CALL_PENDING
} gf_nc_status_t;

typedef enum {
    GF_NC_CALL_NORMAL,            /**< Standard patient call */
    GF_NC_CALL_BATH,              /**< Bathroom assistance */
    GF_NC_CALL_STAFF_EMERGENCY,   /**< Staff emergency/duress */
    GF_NC_CALL_CODE_BLUE,         /**< Cardiac arrest/emergency */
    GF_NC_CALL_REMINDER,          /**< Scheduled reminder */
    GF_NC_CALL_FALL_DETECT        /**< Automatic fall detection */
} gf_nc_call_type_t;

typedef enum {
    GF_NC_PRIORITY_LOW,
    GF_NC_PRIORITY_NORMAL,
    GF_NC_PRIORITY_URGENT,
    GF_NC_PRIORITY_EMERGENCY
} gf_nc_priority_t;

typedef enum {
    GF_NC_STATE_IDLE,             /**< No active call */
    GF_NC_STATE_PENDING,          /**< Call placed, waiting */
    GF_NC_STATE_ASSIGNED,         /**< Staff assigned */
    GF_NC_STATE_IN_PROGRESS,      /**< Staff responding */
    GF_NC_STATE_COMPLETED,        /**< Call resolved */
    GF_NC_STATE_CANCELLED         /**< Call cancelled */
} gf_nc_call_state_t;

typedef struct {
    uint16_t room_id;             /**< Room identifier */
    uint8_t bed_id;               /**< Bed in room */
    uint32_t patient_id;          /**< Patient ID if available */
    gf_nc_call_type_t type;       /**< Call type */
    gf_nc_priority_t priority;    /**< Call priority */
    gf_nc_call_state_t state;     /**< Current state */
    uint64_t call_time_ms;        /**< Time call placed */
    uint64_t response_time_ms;    /**< Time staff responded */
    uint16_t assigned_staff_id;   /**< Responding staff ID */
} gf_nc_call_t;

typedef struct {
    uint16_t staff_id;            /**< Staff identifier */
    char name[32];                /**< Staff name */
    uint8_t role;                 /**< Role (RN, CNA, etc.) */
    uint16_t assigned_zone;       /**< Assignment zone */
    bool available;               /**< Currently available */
    uint16_t location_room;       /**< Current room from RTLS */
} gf_nc_staff_t;

typedef void (*gf_nc_call_cb_t)(const gf_nc_call_t* call, void* user_data);
typedef void (*gf_nc_escalation_cb_t)(const gf_nc_call_t* call, 
                                       uint16_t wait_time_sec, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_nc_status_t gf_nc_init(void);
void gf_nc_shutdown(void);
gf_nc_status_t gf_nc_place_call(uint16_t room_id, uint8_t bed_id, 
                                 gf_nc_call_type_t type, gf_nc_priority_t priority);
gf_nc_status_t gf_nc_cancel_call(uint16_t room_id, uint8_t bed_id);
gf_nc_status_t gf_nc_assign_call(uint16_t room_id, uint16_t staff_id);
gf_nc_status_t gf_nc_complete_call(uint16_t room_id, uint8_t bed_id);
gf_nc_status_t gf_nc_get_active_calls(gf_nc_call_t* calls, uint8_t max_calls, 
                                       uint8_t* count);
gf_nc_status_t gf_nc_register_staff(const gf_nc_staff_t* staff);
gf_nc_status_t gf_nc_update_staff_location(uint16_t staff_id, uint16_t room_id);
gf_nc_status_t gf_nc_register_call_callback(gf_nc_call_cb_t callback, void* user_data);
gf_nc_status_t gf_nc_register_escalation_callback(gf_nc_escalation_cb_t callback,
                                                   uint16_t escalation_sec, void* user_data);
gf_nc_status_t gf_nc_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_NURSE_CALL_H */
