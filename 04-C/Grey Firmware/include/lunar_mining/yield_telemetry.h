/**
 * @file yield_telemetry.h
 * @brief Regolith Yield Telemetry Collector Interface
 * 
 * INDUSTRY RELEVANCE:
 * Lunar mining economics depend on accurate yield measurement. ISRU systems
 * must extract oxygen, water, and metals efficiently from regolith. Real-time
 * telemetry enables:
 * - Resource utilization optimization
 * - Mining site selection validation
 * - System performance trending
 * - Mission operations planning
 * 
 * Companies like OffWorld, Moon Express, and Blue Origin are developing
 * autonomous mining systems requiring embedded telemetry expertise for:
 * - Low-bandwidth data compression (Deep Space Network constraints)
 * - Onboard analytics for autonomous decision-making
 * - Long-duration data archival (multi-year missions)
 * 
 * STANDARDS:
 * - CCSDS 133.0-B-2 (Space Packet Protocol)
 * - CCSDS 122.0-B-2 (Image Data Compression)
 * - NASA-STD-3001 (Human-System Standard for telemetry)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_YIELD_TELEMETRY_H
#define GF_YIELD_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define YIELD_MAX_SESSIONS          100   /**< Maximum extraction sessions */
#define YIELD_HISTORY_HOURS         720   /**< 30-day rolling history */
#define YIELD_PACKET_SIZE           256   /**< Telemetry packet size */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Resource type */
typedef enum {
    RESOURCE_OXYGEN,             /**< O2 from ilmenite reduction */
    RESOURCE_WATER,              /**< H2O from polar ice */
    RESOURCE_IRON,               /**< Fe from regolith */
    RESOURCE_TITANIUM,           /**< Ti from ilmenite */
    RESOURCE_ALUMINUM,           /**< Al from anorthosite */
    RESOURCE_HELIUM3,            /**< He-3 for fusion (rare) */
    RESOURCE_REGOLITH_RAW        /**< Unprocessed regolith */
} resource_type_t;

/** Extraction session summary */
typedef struct {
    uint32_t session_id;
    uint32_t start_time;
    uint32_t end_time;
    float regolith_mass_kg;      /**< Total regolith processed */
    float yield_kg[8];           /**< Yield by resource type */
    float energy_consumed_kwh;
    float drill_runtime_hours;
    uint8_t fault_count;
    bool completed;
} extraction_session_t;

/** Real-time yield metrics */
typedef struct {
    float current_rate_kg_hr;    /**< Current extraction rate */
    float cumulative_yield_kg;   /**< Total since session start */
    float efficiency_percent;    /**< Yield vs theoretical max */
    float energy_per_kg_kwh;     /**< Energy efficiency */
    uint32_t timestamp;
} yield_metrics_t;

/** Telemetry packet header */
typedef struct {
    uint16_t sync_word;
    uint16_t packet_type;
    uint8_t version;
    uint8_t flags;
    uint16_t length;
    uint32_t timestamp;
    uint16_t sequence;
    uint16_t crc16;
} yield_packet_header_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize yield telemetry subsystem
 * @return 0 on success, negative error code on failure
 */
int yield_telemetry_init(void);

/**
 * @brief Start a new extraction session
 * @param session_id Pointer to receive new session ID
 * @return 0 on success, negative error code on failure
 */
int yield_session_start(uint32_t *session_id);

/**
 * @brief Update yield data for current session
 * @param resource Resource type being extracted
 * @param mass_kg Mass extracted in kg
 * @return 0 on success, negative error code on failure
 */
int yield_update(resource_type_t resource, float mass_kg);

/**
 * @brief End current extraction session
 * @param session Pointer to receive session summary
 * @return 0 on success, negative error code on failure
 */
int yield_session_end(extraction_session_t *session);

/**
 * @brief Get current yield metrics
 * @param metrics Pointer to store current metrics
 * @return 0 on success, negative error code on failure
 */
int yield_get_metrics(yield_metrics_t *metrics);

/**
 * @brief Generate telemetry packet for downlink
 * @param buffer Buffer to write packet
 * @param max_len Maximum buffer length
 * @return Bytes written, or negative error code
 */
int yield_generate_packet(uint8_t *buffer, size_t max_len);

/**
 * @brief Get historical session data
 * @param session_id Session ID to retrieve
 * @param session Pointer to store session data
 * @return 0 on success, negative error code on failure
 */
int yield_get_session(uint32_t session_id, extraction_session_t *session);

/**
 * @brief Shutdown telemetry subsystem
 */
void yield_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_YIELD_TELEMETRY_H */
