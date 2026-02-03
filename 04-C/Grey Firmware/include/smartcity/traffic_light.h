/**
 * @file traffic_light.h
 * @brief Traffic Light Controller Interface
 * 
 * INDUSTRY RELEVANCE:
 * Smart traffic management is central to modern urban infrastructure:
 * - Intelligent Transportation Systems (ITS) for traffic optimization
 * - Emergency vehicle preemption for faster response times
 * - Pedestrian and cyclist safety at intersections
 * - Connected vehicle (V2I) communication for signal timing
 * - Traffic data collection for urban planning and analytics
 * - Energy-efficient LED signal control with fault detection
 * 
 * This controller demonstrates expertise in:
 * - Real-time state machine design for safety-critical systems
 * - Conflict monitoring and fail-safe operation
 * - NTCIP and ITE standards compliance
 * - Adaptive timing based on traffic flow sensors
 * - Communication with central traffic management systems
 * 
 * Safety features:
 * - Hardware conflict monitor for incompatible signals
 * - Fail-safe to flash mode on fault detection
 * - Lamp monitoring for burned-out detection
 * - Voltage monitoring for power anomalies
 */

#ifndef GF_TRAFFIC_LIGHT_H
#define GF_TRAFFIC_LIGHT_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ===== Configuration ===== */

#define GF_TL_MAX_PHASES            16      /* Maximum signal phases */
#define GF_TL_MAX_RINGS             4       /* Maximum concurrent rings */
#define GF_TL_MAX_CHANNELS          32      /* Maximum output channels */
#define GF_TL_MIN_GREEN_MS          4000    /* Minimum green time */
#define GF_TL_YELLOW_DEFAULT_MS     3500    /* Default yellow interval */
#define GF_TL_ALL_RED_DEFAULT_MS    2000    /* Default all-red clearance */

/* ===== Status Codes ===== */

typedef enum {
    GF_TL_OK = 0,
    GF_TL_ERROR_NOT_INITIALIZED,
    GF_TL_ERROR_INVALID_PHASE,
    GF_TL_ERROR_CONFLICT_DETECTED,
    GF_TL_ERROR_LAMP_FAULT,
    GF_TL_ERROR_POWER_FAULT,
    GF_TL_ERROR_TIMING_VIOLATION,
    GF_TL_WARN_PREEMPTION_ACTIVE
} gf_tl_status_t;

/* ===== Signal States ===== */

typedef enum {
    GF_TL_STATE_OFF = 0,
    GF_TL_STATE_RED,
    GF_TL_STATE_YELLOW,
    GF_TL_STATE_GREEN,
    GF_TL_STATE_FLASH_RED,
    GF_TL_STATE_FLASH_YELLOW,
    GF_TL_STATE_PROTECTED_LEFT,     /* Green arrow */
    GF_TL_STATE_PERMITTED_LEFT      /* Flashing yellow arrow */
} gf_tl_state_t;

typedef enum {
    GF_TL_MODE_NORMAL,              /* Fixed-time or actuated */
    GF_TL_MODE_FLASH,               /* Flash mode */
    GF_TL_MODE_PREEMPTION,          /* Emergency preemption */
    GF_TL_MODE_COORDINATION,        /* Coordinated with corridor */
    GF_TL_MODE_ADAPTIVE             /* AI-based adaptive timing */
} gf_tl_mode_t;

/* ===== Data Structures ===== */

/**
 * @brief Phase timing configuration
 */
typedef struct {
    uint8_t phase_id;
    uint16_t min_green_ms;
    uint16_t max_green_ms;
    uint16_t extension_ms;          /* Per-vehicle extension */
    uint16_t yellow_ms;
    uint16_t all_red_ms;
    bool pedestrian_enabled;
    uint16_t walk_ms;
    uint16_t flash_dont_walk_ms;
} gf_tl_phase_timing_t;

/**
 * @brief Channel output configuration
 */
typedef struct {
    uint8_t channel_id;
    uint8_t phase_id;               /* Associated phase */
    gf_tl_state_t current_state;
    bool lamp_ok;                   /* Lamp status */
    uint8_t brightness;             /* LED brightness (0-100) */
} gf_tl_channel_t;

/**
 * @brief Intersection configuration
 */
typedef struct {
    uint8_t phase_count;
    uint8_t ring_count;
    gf_tl_phase_timing_t phases[GF_TL_MAX_PHASES];
    uint8_t conflict_matrix[GF_TL_MAX_PHASES];  /* Bit flags */
    gf_tl_mode_t mode;
    uint16_t cycle_length_ms;
    bool coordination_enabled;
    uint16_t offset_ms;             /* Coordination offset */
} gf_tl_config_t;

/**
 * @brief Detection input
 */
typedef struct {
    uint8_t detector_id;
    uint8_t phase_id;
    bool vehicle_present;
    uint16_t occupancy_pct;         /* Occupancy percentage */
    uint32_t count;                 /* Vehicle count */
    uint32_t timestamp_ms;
} gf_tl_detector_t;

/* ===== API Functions ===== */

gf_tl_status_t gf_tl_init(const gf_tl_config_t* config);
void gf_tl_shutdown(void);
gf_tl_status_t gf_tl_run_cycle(void);
gf_tl_status_t gf_tl_set_mode(gf_tl_mode_t mode);
gf_tl_status_t gf_tl_advance_phase(void);
gf_tl_status_t gf_tl_preemption_start(uint8_t direction);
gf_tl_status_t gf_tl_preemption_end(void);
gf_tl_status_t gf_tl_update_detection(const gf_tl_detector_t* detector);
gf_tl_status_t gf_tl_get_channel_state(uint8_t channel, gf_tl_channel_t* state);
gf_tl_status_t gf_tl_get_statistics(uint32_t* cycles, uint32_t* vehicles, float* avg_delay);
bool gf_tl_check_conflicts(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_TRAFFIC_LIGHT_H */
