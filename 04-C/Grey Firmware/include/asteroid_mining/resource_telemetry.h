/**
 * @file resource_telemetry.h
 * @brief Asteroid Resource Telemetry Collector Stub
 * 
 * Industry Relevance:
 * Real-time resource telemetry is critical for space mining economics.
 * This module tracks extracted materials, yield rates, and quality metrics
 * for mission planning and investor reporting. Demonstrates:
 * - CCSDS-compatible telemetry packet generation
 * - Resource classification and grading algorithms
 * - Yield prediction based on sensor fusion
 * - Long-duration mission data aggregation
 * 
 * Applications: Mining yield optimization, mission economics, sample analysis
 * 
 * @author Grey Firmware Project
 */

#ifndef RESOURCE_TELEMETRY_H
#define RESOURCE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Resource type classification */
typedef enum {
    RESOURCE_WATER_ICE,      /**< H2O ice for propellant */
    RESOURCE_PLATINUM_GROUP, /**< Platinum group metals */
    RESOURCE_IRON_NICKEL,    /**< Structural metals */
    RESOURCE_RARE_EARTH,     /**< Rare earth elements */
    RESOURCE_CARBON,         /**< Carbon compounds */
    RESOURCE_SILICATES       /**< Silicate minerals */
} resource_type_t;

/** Resource extraction record */
typedef struct {
    uint32_t timestamp;      /**< Extraction time (Unix epoch) */
    resource_type_t type;    /**< Resource classification */
    float mass_kg;           /**< Extracted mass (kg) */
    float purity_pct;        /**< Purity percentage */
    float depth_m;           /**< Extraction depth */
    uint16_t sample_id;      /**< Unique sample identifier */
} resource_record_t;

/** Yield statistics */
typedef struct {
    float total_mass_kg;     /**< Total extracted mass */
    float avg_purity_pct;    /**< Average purity */
    uint32_t sample_count;   /**< Number of samples */
    float extraction_rate;   /**< kg/hour rate */
    float mission_efficiency;/**< Efficiency percentage */
} yield_statistics_t;

/** Telemetry packet (CCSDS-compatible) */
typedef struct {
    uint16_t apid;           /**< Application Process ID */
    uint16_t sequence;       /**< Packet sequence count */
    uint32_t timestamp;      /**< Packet timestamp */
    resource_record_t current; /**< Current extraction data */
    yield_statistics_t stats;/**< Mission statistics */
    uint8_t checksum;        /**< Packet checksum */
} resource_telemetry_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize resource telemetry system
 * @return 0 on success, negative on error
 */
int resource_telemetry_init(void);

/**
 * @brief Record a resource extraction event
 * @param record Extraction data
 * @return 0 on success, negative on error
 */
int resource_telemetry_record(const resource_record_t *record);

/**
 * @brief Get current yield statistics
 * @param stats Output statistics structure
 * @return 0 on success, negative on error
 */
int resource_telemetry_get_stats(yield_statistics_t *stats);

/**
 * @brief Generate telemetry packet for downlink
 * @param packet Output telemetry packet
 * @return 0 on success, negative on error
 */
int resource_telemetry_generate(resource_telemetry_t *packet);

/**
 * @brief Reset statistics for new mission phase
 * @return 0 on success, negative on error
 */
int resource_telemetry_reset(void);

/**
 * @brief Shutdown telemetry system
 */
void resource_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* RESOURCE_TELEMETRY_H */
