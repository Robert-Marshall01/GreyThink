/**
 * @file water_recycling.h
 * @brief Water Recycling and Reuse System Telemetry
 * 
 * @details
 * This module provides telemetry and control interfaces for water recycling
 * systems including greywater treatment, blackwater processing, and
 * potable water reuse applications.
 * 
 * INDUSTRY RELEVANCE:
 * - Municipal water reuse (California, Singapore, Israel)
 * - Industrial wastewater recycling
 * - Building greywater systems
 * - Agricultural water reuse
 * - Space station water recovery (ISS WRS)
 * - Off-grid sustainable buildings
 * 
 * KEY PROCESSES:
 * - Membrane bioreactor (MBR)
 * - Ultrafiltration/Nanofiltration
 * - UV disinfection
 * - Advanced oxidation
 * - Ion exchange
 * - Activated carbon
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_WATER_RECYCLING_H
#define GF_WATER_RECYCLING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_WRC_OK = 0,
    GF_WRC_ERROR_NOT_INITIALIZED,
    GF_WRC_ERROR_NULL_PTR,
    GF_WRC_ERROR_QUALITY,
    GF_WRC_ERROR_OVERFLOW,
    GF_WRC_ERROR_EQUIPMENT,
    GF_WRC_WARN_CAPACITY_HIGH,
    GF_WRC_WARN_MAINTENANCE
} gf_wrc_status_t;

typedef enum {
    GF_WRC_SOURCE_GREYWATER,      /**< Sinks, showers, laundry */
    GF_WRC_SOURCE_BLACKWATER,     /**< Toilet waste */
    GF_WRC_SOURCE_STORMWATER,     /**< Rain/storm runoff */
    GF_WRC_SOURCE_INDUSTRIAL,     /**< Industrial process water */
    GF_WRC_SOURCE_CONDENSATE      /**< HVAC condensate */
} gf_wrc_source_t;

typedef enum {
    GF_WRC_USE_IRRIGATION,        /**< Landscape irrigation */
    GF_WRC_USE_TOILET_FLUSH,      /**< Toilet flushing */
    GF_WRC_USE_COOLING,           /**< Cooling tower makeup */
    GF_WRC_USE_POTABLE,           /**< Direct potable reuse */
    GF_WRC_USE_INDUSTRIAL         /**< Industrial process */
} gf_wrc_use_t;

typedef enum {
    GF_WRC_STAGE_COLLECTION,      /**< Collection tank */
    GF_WRC_STAGE_PRETREAT,        /**< Pretreatment */
    GF_WRC_STAGE_PRIMARY,         /**< Primary treatment */
    GF_WRC_STAGE_SECONDARY,       /**< Secondary treatment */
    GF_WRC_STAGE_TERTIARY,        /**< Tertiary treatment */
    GF_WRC_STAGE_DISINFECTION,    /**< Disinfection */
    GF_WRC_STAGE_STORAGE          /**< Treated storage */
} gf_wrc_stage_t;

typedef struct {
    float ph;                     /**< pH level */
    float turbidity_ntu;          /**< Turbidity (NTU) */
    float tss_mgl;                /**< Total suspended solids */
    float bod_mgl;                /**< Biochemical oxygen demand */
    float cod_mgl;                /**< Chemical oxygen demand */
    float toc_mgl;                /**< Total organic carbon */
    float ammonia_mgl;            /**< Ammonia nitrogen */
    float nitrate_mgl;            /**< Nitrate nitrogen */
    float phosphate_mgl;          /**< Phosphate */
    float chlorine_mgl;           /**< Free chlorine */
    float ecoli_cfu100ml;         /**< E. coli count */
    uint64_t timestamp_ms;
} gf_wrc_quality_t;

typedef struct {
    gf_wrc_stage_t stage;         /**< Treatment stage */
    float inlet_flow_lpm;         /**< Inlet flow rate */
    float outlet_flow_lpm;        /**< Outlet flow rate */
    float tank_level_pct;         /**< Tank level (%) */
    float removal_efficiency_pct; /**< Removal efficiency */
    uint8_t equipment_status;     /**< Equipment status bitmap */
    gf_wrc_quality_t inlet_quality;   /**< Inlet quality */
    gf_wrc_quality_t outlet_quality;  /**< Outlet quality */
} gf_wrc_stage_data_t;

typedef struct {
    float total_recycled_m3;      /**< Total water recycled */
    float avg_recovery_pct;       /**< Average recovery rate */
    float energy_kwh_m3;          /**< Energy per m³ */
    uint32_t runtime_hours;       /**< Total runtime */
    uint32_t quality_violations;  /**< Quality violations */
} gf_wrc_stats_t;

typedef void (*gf_wrc_quality_cb_t)(gf_wrc_stage_t stage, const gf_wrc_quality_t* quality,
                                     bool passed, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_wrc_status_t gf_wrc_init(gf_wrc_source_t source, gf_wrc_use_t target_use);
void gf_wrc_shutdown(void);
gf_wrc_status_t gf_wrc_start(void);
gf_wrc_status_t gf_wrc_stop(void);
gf_wrc_status_t gf_wrc_get_stage_data(gf_wrc_stage_t stage, gf_wrc_stage_data_t* data);
gf_wrc_status_t gf_wrc_get_final_quality(gf_wrc_quality_t* quality);
gf_wrc_status_t gf_wrc_set_flow_setpoint(float flow_lpm);
gf_wrc_status_t gf_wrc_register_quality_callback(gf_wrc_quality_cb_t callback, void* user_data);
gf_wrc_status_t gf_wrc_get_stats(gf_wrc_stats_t* stats);
gf_wrc_status_t gf_wrc_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_WATER_RECYCLING_H */
