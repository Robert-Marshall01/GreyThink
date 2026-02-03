/**
 * @file flight_data_recorder.h
 * @brief Flight Data Recorder (FDR) Interface - Black Box System
 * 
 * @details
 * This module provides the interface for a flight data recorder system,
 * commonly known as a "black box." It captures critical flight parameters
 * for accident investigation and flight safety analysis.
 * 
 * INDUSTRY RELEVANCE:
 * - Commercial Aviation: FAA/EASA mandated FDR for all transport aircraft
 *   (Boeing 737, Airbus A320) capturing 88+ parameters
 * - Business Aviation: Gulfstream, Bombardier aircraft data recording
 * - Helicopter Operations: HEMS (air ambulance) flight monitoring
 * - Military: Mission data recording for debriefing and analysis
 * - Space: Launch vehicle telemetry recording (SpaceX, NASA)
 * 
 * COMPLIANCE:
 * - TSO-C124b: Flight Data Recorder Systems
 * - ED-112A: MOPS for Crash Protected Airborne Recorder Systems
 * - ICAO Annex 6: Flight Recorders
 * - 14 CFR 121.344: Digital Flight Data Recorders
 * 
 * RECORDING REQUIREMENTS (per ED-112A):
 * - Minimum 25 hours of recording
 * - Crash survivability: 3400g impact, 1100°C fire, 20000ft seawater
 * - Underwater locator beacon (30-day battery)
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_FLIGHT_DATA_RECORDER_H
#define GF_FLIGHT_DATA_RECORDER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum number of recorded parameters */
#define GF_FDR_MAX_PARAMETERS           128

/** Recording sample rate (Hz) */
#define GF_FDR_SAMPLE_RATE_HZ           8

/** Minimum recording duration (hours) */
#define GF_FDR_MIN_DURATION_HOURS       25

/** Frame sync word */
#define GF_FDR_SYNC_WORD                0x247

/** Subframe count per frame */
#define GF_FDR_SUBFRAMES_PER_FRAME      4

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief FDR parameter categories (per TSO-C124b)
 */
typedef enum {
    GF_FDR_CAT_TIME,              /**< Time reference */
    GF_FDR_CAT_ALTITUDE,          /**< Altitude parameters */
    GF_FDR_CAT_AIRSPEED,          /**< Airspeed parameters */
    GF_FDR_CAT_HEADING,           /**< Heading/track */
    GF_FDR_CAT_ATTITUDE,          /**< Attitude (roll/pitch) */
    GF_FDR_CAT_ACCELERATION,      /**< Normal/lateral acceleration */
    GF_FDR_CAT_ENGINE,            /**< Engine parameters */
    GF_FDR_CAT_CONTROL,           /**< Control surface positions */
    GF_FDR_CAT_CONFIGURATION,     /**< Aircraft configuration */
    GF_FDR_CAT_AUTOPILOT,         /**< Autopilot status */
    GF_FDR_CAT_WARNING,           /**< Warning/caution status */
    GF_FDR_CAT_NAVIGATION,        /**< Nav source/mode */
    GF_FDR_CAT_FUEL,              /**< Fuel quantity */
    GF_FDR_CAT_CUSTOM             /**< Operator-defined */
} gf_fdr_category_t;

/**
 * @brief FDR operating state
 */
typedef enum {
    GF_FDR_STATE_OFF,             /**< Not recording */
    GF_FDR_STATE_INIT,            /**< Initializing */
    GF_FDR_STATE_RECORDING,       /**< Normal recording */
    GF_FDR_STATE_TRIGGERED,       /**< Event triggered */
    GF_FDR_STATE_FAULT,           /**< Recording fault */
    GF_FDR_STATE_READOUT          /**< Readout mode */
} gf_fdr_state_t;

/**
 * @brief Recording event triggers
 */
typedef enum {
    GF_FDR_TRIGGER_NONE,          /**< Normal recording */
    GF_FDR_TRIGGER_TAKEOFF,       /**< Takeoff detected */
    GF_FDR_TRIGGER_LANDING,       /**< Landing detected */
    GF_FDR_TRIGGER_EXCEEDANCE,    /**< Parameter exceedance */
    GF_FDR_TRIGGER_WARNING,       /**< Warning activated */
    GF_FDR_TRIGGER_MANUAL,        /**< Manual trigger */
    GF_FDR_TRIGGER_IMPACT         /**< Impact detected */
} gf_fdr_trigger_t;

/**
 * @brief Parameter definition
 */
typedef struct {
    uint8_t id;                   /**< Parameter ID */
    gf_fdr_category_t category;   /**< Parameter category */
    const char* name;             /**< Parameter name */
    float resolution;             /**< Recording resolution */
    float min_value;              /**< Minimum value */
    float max_value;              /**< Maximum value */
    uint8_t bits;                 /**< Bits per sample */
    uint8_t rate_hz;              /**< Sample rate (1-8 Hz) */
} gf_fdr_param_def_t;

/**
 * @brief Single parameter sample
 */
typedef struct {
    uint8_t param_id;             /**< Parameter ID */
    float value;                  /**< Sampled value */
    uint32_t timestamp_ms;        /**< Sample timestamp */
    bool valid;                   /**< Validity flag */
} gf_fdr_sample_t;

/**
 * @brief FDR frame (1-second recording block)
 */
typedef struct {
    uint16_t sync_word;           /**< Frame sync (0x247) */
    uint32_t frame_number;        /**< Frame sequence number */
    uint32_t timestamp_s;         /**< UTC timestamp (seconds) */
    uint16_t subframe_index;      /**< Current subframe */
    uint16_t crc;                 /**< Frame CRC */
} gf_fdr_frame_header_t;

/**
 * @brief FDR status structure
 */
typedef struct {
    gf_fdr_state_t state;         /**< Operating state */
    uint32_t frames_recorded;     /**< Total frames recorded */
    uint32_t recording_hours;     /**< Recording duration (hours) */
    uint32_t memory_used_pct;     /**< Memory usage (%) */
    bool memory_full;             /**< Memory full (wrap active) */
    uint32_t fault_count;         /**< Fault counter */
    uint32_t last_trigger;        /**< Last event trigger type */
    uint32_t trigger_count;       /**< Event trigger count */
} gf_fdr_status_t;

/**
 * @brief FDR configuration
 */
typedef struct {
    uint8_t param_count;          /**< Number of parameters */
    const gf_fdr_param_def_t* params; /**< Parameter definitions */
    uint32_t memory_size_kb;      /**< Recording memory size */
    bool enable_wrap;             /**< Enable wraparound recording */
    bool enable_ulb;              /**< Enable underwater locator beacon */
} gf_fdr_config_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize flight data recorder
 * @param config FDR configuration
 * @return 0 on success
 */
int gf_fdr_init(const gf_fdr_config_t* config);

/**
 * @brief Shutdown flight data recorder
 */
void gf_fdr_shutdown(void);

/**
 * @brief Start recording
 * @return 0 on success
 */
int gf_fdr_start_recording(void);

/**
 * @brief Stop recording
 */
void gf_fdr_stop_recording(void);

/**
 * @brief Record a parameter sample
 * @param sample Parameter sample
 * @return 0 on success
 */
int gf_fdr_record_sample(const gf_fdr_sample_t* sample);

/**
 * @brief Record batch of samples (frame)
 * @param samples Sample array
 * @param count Number of samples
 * @return 0 on success
 */
int gf_fdr_record_frame(const gf_fdr_sample_t* samples, uint8_t count);

/**
 * @brief Trigger event recording
 * @param trigger Event trigger type
 * @param data Associated data
 */
void gf_fdr_trigger_event(gf_fdr_trigger_t trigger, uint32_t data);

/**
 * @brief Get FDR status
 * @param status Output status
 * @return 0 on success
 */
int gf_fdr_get_status(gf_fdr_status_t* status);

/**
 * @brief Process FDR (call periodically)
 * @param delta_ms Time since last call
 */
void gf_fdr_process(uint32_t delta_ms);

/**
 * @brief Read back recorded data
 * @param frame_start Starting frame number
 * @param frame_count Number of frames to read
 * @param buffer Output buffer
 * @param buffer_size Buffer size
 * @return Bytes read
 */
int gf_fdr_readback(uint32_t frame_start, uint32_t frame_count,
                     uint8_t* buffer, uint32_t buffer_size);

#ifdef __cplusplus
}
#endif

#endif /* GF_FLIGHT_DATA_RECORDER_H */
