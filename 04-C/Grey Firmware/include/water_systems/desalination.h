/**
 * @file desalination.h
 * @brief Desalination Plant Control System Interface
 * 
 * @details
 * This module provides control interfaces for desalination systems including
 * reverse osmosis (RO), multi-stage flash (MSF), and electrodialysis (ED)
 * plants used for producing fresh water from seawater or brackish water.
 * 
 * INDUSTRY RELEVANCE:
 * - Municipal water treatment (Saudi Arabia, UAE, Israel)
 * - Industrial process water
 * - Offshore platform water supply
 * - Cruise ship freshwater systems
 * - Agricultural irrigation
 * - Disaster relief water production
 * 
 * KEY PROCESS TYPES:
 * - Reverse Osmosis (RO) - most energy efficient
 * - Multi-Stage Flash (MSF) - thermal desalination
 * - Multi-Effect Distillation (MED)
 * - Electrodialysis (ED) - brackish water
 * - Forward Osmosis (FO) - emerging
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_DESALINATION_H
#define GF_DESALINATION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_DESAL_OK = 0,
    GF_DESAL_ERROR_NOT_INITIALIZED,
    GF_DESAL_ERROR_NULL_PTR,
    GF_DESAL_ERROR_PRESSURE,
    GF_DESAL_ERROR_MEMBRANE,
    GF_DESAL_ERROR_QUALITY,
    GF_DESAL_ERROR_PUMP_FAULT,
    GF_DESAL_ERROR_VALVE_FAULT,
    GF_DESAL_WARN_MEMBRANE_SCALING,
    GF_DESAL_WARN_HIGH_TDS
} gf_desal_status_t;

typedef enum {
    GF_DESAL_TYPE_RO,             /**< Reverse Osmosis */
    GF_DESAL_TYPE_BWRO,           /**< Brackish Water RO */
    GF_DESAL_TYPE_MSF,            /**< Multi-Stage Flash */
    GF_DESAL_TYPE_MED,            /**< Multi-Effect Distillation */
    GF_DESAL_TYPE_ED,             /**< Electrodialysis */
    GF_DESAL_TYPE_FO              /**< Forward Osmosis */
} gf_desal_type_t;

typedef enum {
    GF_DESAL_STATE_IDLE,
    GF_DESAL_STATE_STARTUP,
    GF_DESAL_STATE_RUNNING,
    GF_DESAL_STATE_FLUSHING,
    GF_DESAL_STATE_CIP,           /**< Clean-In-Place */
    GF_DESAL_STATE_STANDBY,
    GF_DESAL_STATE_SHUTDOWN,
    GF_DESAL_STATE_FAULT
} gf_desal_state_t;

typedef struct {
    gf_desal_type_t type;         /**< Process type */
    float design_capacity_m3h;    /**< Design capacity (m³/h) */
    float feed_tds_ppm;           /**< Feed water TDS (ppm) */
    float target_tds_ppm;         /**< Target product TDS */
    float recovery_pct;           /**< Recovery ratio (%) */
    uint8_t num_stages;           /**< Number of stages/passes */
    float operating_pressure_bar; /**< Operating pressure */
} gf_desal_config_t;

typedef struct {
    float feed_flow_m3h;          /**< Feed water flow */
    float permeate_flow_m3h;      /**< Product water flow */
    float reject_flow_m3h;        /**< Brine reject flow */
    float feed_pressure_bar;      /**< Feed pressure */
    float membrane_dp_bar;        /**< Membrane differential pressure */
    float feed_tds_ppm;           /**< Feed TDS */
    float product_tds_ppm;        /**< Product TDS */
    float reject_tds_ppm;         /**< Reject TDS */
    float recovery_pct;           /**< Current recovery */
    float power_kw;               /**< Power consumption */
    float sec_kwh_m3;             /**< Specific energy consumption */
    uint64_t timestamp_ms;        /**< Reading timestamp */
} gf_desal_readings_t;

typedef struct {
    uint32_t runtime_hours;       /**< Total runtime */
    float total_produced_m3;      /**< Total water produced */
    float avg_recovery_pct;       /**< Average recovery */
    float avg_sec_kwh_m3;         /**< Average SEC */
    uint32_t cip_cycles;          /**< CIP cycles performed */
    uint32_t membrane_age_days;   /**< Membrane age */
} gf_desal_stats_t;

typedef void (*gf_desal_quality_cb_t)(float tds_ppm, bool acceptable, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_desal_status_t gf_desal_init(const gf_desal_config_t* config);
void gf_desal_shutdown(void);
gf_desal_status_t gf_desal_start(void);
gf_desal_status_t gf_desal_stop(void);
gf_desal_status_t gf_desal_get_state(gf_desal_state_t* state);
gf_desal_status_t gf_desal_get_readings(gf_desal_readings_t* readings);
gf_desal_status_t gf_desal_set_capacity(float target_m3h);
gf_desal_status_t gf_desal_set_recovery(float recovery_pct);
gf_desal_status_t gf_desal_start_cip(void);
gf_desal_status_t gf_desal_flush(uint32_t duration_sec);
gf_desal_status_t gf_desal_register_quality_callback(gf_desal_quality_cb_t callback,
                                                      void* user_data);
gf_desal_status_t gf_desal_get_stats(gf_desal_stats_t* stats);
gf_desal_status_t gf_desal_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_DESALINATION_H */
