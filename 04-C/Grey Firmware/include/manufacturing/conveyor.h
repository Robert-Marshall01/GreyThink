/**
 * @file conveyor.h
 * @brief Conveyor Belt Sensor and Control Interface
 *
 * INDUSTRY RELEVANCE:
 * Conveyor systems are fundamental to material handling in manufacturing,
 * logistics, and distribution centers. This module demonstrates:
 * - Proximity/photoelectric sensor integration
 * - Speed control and synchronization
 * - Zone-based tracking for product identification
 * - Integration with barcode/RFID readers
 *
 * These skills apply to material handling companies (Dematic, Honeywell
 * Intelligrated), e-commerce fulfillment (Amazon Robotics), and industrial
 * automation integrators building high-throughput systems.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires safety guarding integration.
 */

#ifndef GF_CONVEYOR_H
#define GF_CONVEYOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_CONVEYOR_MAX_ZONES       32      /**< Maximum tracking zones */
#define GF_CONVEYOR_MAX_SENSORS     64      /**< Maximum sensors per conveyor */
#define GF_CONVEYOR_MAX_ITEMS       256     /**< Maximum tracked items */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Conveyor type
 */
typedef enum {
    GF_CONVEYOR_BELT,           /**< Belt conveyor */
    GF_CONVEYOR_ROLLER,         /**< Roller conveyor */
    GF_CONVEYOR_CHAIN,          /**< Chain conveyor */
    GF_CONVEYOR_ACCUMULATING    /**< Zero-pressure accumulation */
} gf_conveyor_type_t;

/**
 * @brief Sensor type
 */
typedef enum {
    GF_SENSOR_PHOTOELECTRIC,    /**< Photoelectric through-beam */
    GF_SENSOR_PROXIMITY,        /**< Proximity sensor */
    GF_SENSOR_ENCODER,          /**< Rotary encoder */
    GF_SENSOR_BARCODE,          /**< Barcode scanner */
    GF_SENSOR_RFID,             /**< RFID reader */
    GF_SENSOR_WEIGHT_SCALE      /**< Weight scale */
} gf_conveyor_sensor_type_t;

/**
 * @brief Conveyor state
 */
typedef enum {
    GF_CONVEYOR_STOPPED,        /**< Stopped */
    GF_CONVEYOR_RUNNING,        /**< Running at speed */
    GF_CONVEYOR_SLOWING,        /**< Decelerating */
    GF_CONVEYOR_ACCELERATING,   /**< Accelerating */
    GF_CONVEYOR_FAULTED,        /**< Fault condition */
    GF_CONVEYOR_ESTOP           /**< Emergency stop */
} gf_conveyor_state_t;

/**
 * @brief Sensor configuration
 */
typedef struct {
    uint8_t id;                         /**< Sensor ID */
    gf_conveyor_sensor_type_t type;     /**< Sensor type */
    float position_mm;                   /**< Position along conveyor */
    uint8_t zone;                        /**< Associated zone */
    bool active_high;                    /**< Active high/low */
} gf_conveyor_sensor_config_t;

/**
 * @brief Zone configuration
 */
typedef struct {
    uint8_t zone_id;                    /**< Zone identifier */
    float start_mm;                      /**< Zone start position */
    float end_mm;                        /**< Zone end position */
    bool accumulation;                   /**< Enable accumulation mode */
    float min_spacing_mm;                /**< Minimum item spacing */
} gf_conveyor_zone_t;

/**
 * @brief Tracked item
 */
typedef struct {
    uint32_t item_id;                   /**< Unique item identifier */
    char barcode[64];                   /**< Barcode data */
    uint8_t rfid[12];                   /**< RFID tag data */
    float position_mm;                   /**< Current position */
    uint8_t current_zone;               /**< Current zone */
    float length_mm;                     /**< Item length */
    float weight_kg;                     /**< Item weight */
    uint64_t entry_time_ms;             /**< Entry timestamp */
} gf_conveyor_item_t;

/**
 * @brief Conveyor configuration
 */
typedef struct {
    gf_conveyor_type_t type;            /**< Conveyor type */
    float length_mm;                     /**< Total length */
    float width_mm;                      /**< Belt width */
    float max_speed_mm_s;               /**< Maximum speed */
    float acceleration_mm_s2;           /**< Acceleration rate */
    float encoder_resolution_mm;        /**< Encoder resolution */
    uint8_t num_zones;                  /**< Number of zones */
    gf_conveyor_zone_t zones[GF_CONVEYOR_MAX_ZONES]; /**< Zone configs */
} gf_conveyor_config_t;

/**
 * @brief Conveyor status
 */
typedef struct {
    gf_conveyor_state_t state;          /**< Current state */
    float current_speed_mm_s;           /**< Current speed */
    float target_speed_mm_s;            /**< Target speed */
    float motor_current_a;              /**< Motor current */
    float motor_temp_c;                 /**< Motor temperature */
    uint32_t total_distance_m;          /**< Total distance traveled */
    uint32_t item_count;                /**< Items processed today */
    uint16_t fault_code;                /**< Active fault code */
    bool vfd_online;                    /**< VFD communication OK */
} gf_conveyor_status_t;

/**
 * @brief Sensor event
 */
typedef struct {
    uint8_t sensor_id;                  /**< Sensor ID */
    gf_conveyor_sensor_type_t type;     /**< Sensor type */
    bool triggered;                     /**< True = leading edge, False = trailing */
    uint32_t timestamp_ms;              /**< Event timestamp */
    union {
        char barcode[64];               /**< Barcode data (if barcode sensor) */
        uint8_t rfid[12];               /**< RFID data (if RFID reader) */
        float weight_kg;                /**< Weight data (if scale) */
    } data;
} gf_conveyor_sensor_event_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * Callbacks
 * ───────────────────────────────────────────────────────────────────────────── */

typedef void (*gf_conveyor_sensor_cb_t)(const gf_conveyor_sensor_event_t* event, void* user_data);
typedef void (*gf_conveyor_zone_cb_t)(uint8_t zone, uint32_t item_id, bool entered, void* user_data);

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize conveyor subsystem
 * @param config Conveyor configuration
 * @return 0 on success, negative error code on failure
 */
int gf_conveyor_init(const gf_conveyor_config_t* config);

/**
 * @brief Shutdown conveyor subsystem
 */
void gf_conveyor_deinit(void);

/**
 * @brief Add sensor to conveyor
 * @param sensor Sensor configuration
 * @return 0 on success
 */
int gf_conveyor_add_sensor(const gf_conveyor_sensor_config_t* sensor);

/**
 * @brief Start conveyor
 * @param speed Target speed (mm/s)
 * @return 0 on success
 */
int gf_conveyor_start(float speed);

/**
 * @brief Stop conveyor
 * @param deceleration Deceleration rate (mm/s²), 0 = default
 */
void gf_conveyor_stop(float deceleration);

/**
 * @brief Emergency stop
 */
void gf_conveyor_estop(void);

/**
 * @brief Set target speed
 * @param speed Target speed (mm/s)
 */
void gf_conveyor_set_speed(float speed);

/**
 * @brief Jog conveyor
 * @param distance Distance to jog (mm), negative = reverse
 */
void gf_conveyor_jog(float distance);

/**
 * @brief Get current status
 * @param[out] status Output status
 */
void gf_conveyor_get_status(gf_conveyor_status_t* status);

/**
 * @brief Get tracked item
 * @param item_id Item identifier
 * @param[out] item Output item data
 * @return 0 on success, -1 if not found
 */
int gf_conveyor_get_item(uint32_t item_id, gf_conveyor_item_t* item);

/**
 * @brief Get all items in zone
 * @param zone Zone ID
 * @param[out] items Output item array
 * @param max_items Array capacity
 * @return Number of items in zone
 */
int gf_conveyor_get_zone_items(uint8_t zone, gf_conveyor_item_t* items, uint8_t max_items);

/**
 * @brief Register sensor callback
 * @param callback Sensor event callback
 * @param user_data User context
 */
void gf_conveyor_set_sensor_callback(gf_conveyor_sensor_cb_t callback, void* user_data);

/**
 * @brief Register zone transition callback
 * @param callback Zone transition callback
 * @param user_data User context
 */
void gf_conveyor_set_zone_callback(gf_conveyor_zone_cb_t callback, void* user_data);

/**
 * @brief Clear fault and reset
 * @return 0 on success
 */
int gf_conveyor_reset(void);

/**
 * @brief Process conveyor updates (call periodically)
 * @param dt Time step (seconds)
 */
void gf_conveyor_update(float dt);

#ifdef __cplusplus
}
#endif

#endif /* GF_CONVEYOR_H */
