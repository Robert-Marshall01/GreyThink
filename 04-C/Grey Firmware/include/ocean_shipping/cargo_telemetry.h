/**
 * @file cargo_telemetry.h
 * @brief Smart Shipping Cargo Telemetry Module
 * 
 * INDUSTRY RELEVANCE:
 * Real-time cargo monitoring is essential for:
 * - Stability management (prevent capsizing from cargo shift)
 * - Cold chain compliance for perishables ($35B annual losses)
 * - Dangerous goods monitoring (IMDG compliance)
 * - Insurance and liability documentation
 * - Port operations optimization
 * 
 * IoT-enabled containers projected at 6.5M units by 2027.
 * 
 * STANDARDS: ISO 17712, IMDG Code, SOLAS Verified Gross Mass
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CARGO_TELEMETRY_H
#define GF_CARGO_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_CARGO_HOLDS 12
#define MAX_CONTAINERS 1000

/* Cargo types */
typedef enum {
    CARGO_GENERAL,
    CARGO_CONTAINER,
    CARGO_BULK_DRY,
    CARGO_BULK_LIQUID,
    CARGO_REEFER,
    CARGO_DANGEROUS,
    CARGO_OVERSIZED
} cargo_type_t;

/* Hold status */
typedef enum {
    HOLD_EMPTY,
    HOLD_LOADING,
    HOLD_LOADED,
    HOLD_UNLOADING,
    HOLD_SECURED
} hold_status_t;

/* Cargo hold data */
typedef struct {
    uint8_t hold_id;
    cargo_type_t cargo_type;
    hold_status_t status;
    float weight_tonnes;
    float max_capacity_tonnes;
    float utilization_pct;
    float temperature_c;
    float humidity_pct;
    float co2_ppm;
    bool ventilation_active;
    bool reefer_active;
} hold_telemetry_t;

/* Vessel stability */
typedef struct {
    float draft_fore_m;
    float draft_aft_m;
    float draft_mid_m;
    float trim_m;
    float heel_deg;
    float gm_metacentric_m;    /**< Stability indicator */
    float displacement_tonnes;
    float deadweight_tonnes;
    bool stability_ok;
} stability_data_t;

/* Container status */
typedef struct {
    char container_id[12];
    uint16_t bay_row_tier;     /**< Packed position */
    cargo_type_t type;
    float weight_kg;
    float temp_setpoint_c;
    float temp_actual_c;
    bool door_sensor;
    bool shock_detected;
    uint8_t battery_pct;
} container_status_t;

/**
 * @brief Initialize cargo telemetry
 * @param num_holds Number of cargo holds
 * @return 0 on success
 */
int cargo_telem_init(uint8_t num_holds);

/**
 * @brief Update hold telemetry
 * @param hold_id Hold identifier
 * @param telem Hold telemetry data
 * @return 0 on success
 */
int cargo_telem_update_hold(uint8_t hold_id, const hold_telemetry_t *telem);

/**
 * @brief Get current stability data
 * @param stability Output stability structure
 * @return 0 on success
 */
int cargo_telem_get_stability(stability_data_t *stability);

/**
 * @brief Check for stability alerts
 * @return true if stability is compromised
 */
bool cargo_telem_stability_alert(void);

/**
 * @brief Register container
 * @param container Container data
 * @return 0 on success
 */
int cargo_telem_register_container(const container_status_t *container);

/**
 * @brief Get container status
 * @param container_id Container identifier
 * @param status Output status structure
 * @return 0 on success, negative if not found
 */
int cargo_telem_get_container(const char *container_id, container_status_t *status);

/**
 * @brief Generate cargo manifest telemetry
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written
 */
int cargo_telem_generate(uint8_t *buffer, size_t max_len);

/**
 * @brief Get total cargo weight
 * @return Total weight in tonnes
 */
float cargo_telem_get_total_weight(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CARGO_TELEMETRY_H */
