/**
 * @file emergency_telemetry.h
 * @brief Emergency Telemetry for Disaster Shelters
 *
 * INDUSTRY RELEVANCE:
 * During disasters, communication is often degraded. This module provides
 * low-bandwidth telemetry for shelter status reporting via satellite,
 * mesh radio, or cellular backup - essential for FEMA coordination and
 * first responder operations.
 *
 * Key capabilities demonstrated:
 * - Multi-path communication failover
 * - Compressed status packets (<100 bytes)
 * - Mesh network relay capability
 * - Distress beacon integration
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_SHELTER_EMERGENCY_TLM_H
#define GF_SHELTER_EMERGENCY_TLM_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Communication path */
typedef enum {
    GF_COMM_CELLULAR,
    GF_COMM_SATELLITE,
    GF_COMM_MESH_RADIO,
    GF_COMM_LORA
} gf_emergency_comm_t;

/** Shelter status code */
typedef enum {
    GF_SHELTER_STATUS_OK,
    GF_SHELTER_STATUS_DEGRADED,
    GF_SHELTER_STATUS_EMERGENCY,
    GF_SHELTER_STATUS_EVACUATE
} gf_shelter_code_t;

/** Compact telemetry packet */
typedef struct {
    uint32_t shelter_id;
    gf_shelter_code_t status;
    uint8_t occupancy;
    uint8_t occupancy_max;
    float battery_pct;
    float air_quality_index;
    int32_t latitude_e7;
    int32_t longitude_e7;
    uint32_t timestamp;
} gf_emergency_packet_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_emergency_tlm_init(uint32_t shelter_id);
int gf_emergency_send_status(const gf_emergency_packet_t* packet);
int gf_emergency_activate_beacon(void);
gf_emergency_comm_t gf_emergency_get_active_path(void);
void gf_emergency_tlm_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SHELTER_EMERGENCY_TLM_H */
