/**
 * @file zerog_fab_sensor.h
 * @brief Zero-Gravity Fabrication Sensor Interface
 *
 * INDUSTRY RELEVANCE:
 * Space-based manufacturing (ISS, Axiom, Orbital Reef) enables production of
 * materials impossible to create under Earth gravity. Fiber optics, pharmaceuticals,
 * and metal alloys benefit from microgravity processing. This driver interfaces
 * with sensors monitoring fabrication chamber conditions.
 *
 * MARKET CONTEXT:
 * - NASA ISRU and in-space manufacturing initiatives
 * - Commercial space stations requiring on-orbit fabrication
 * - Pharmaceutical crystallization in microgravity
 * - Advanced fiber optic production (ZBLAN)
 * - 3D printing in space (Made In Space, Redwire)
 *
 * TECHNICAL APPROACH:
 * - Multi-axis accelerometer for microgravity verification (< 10^-6 g)
 * - Thermal gradient sensors for solidification front tracking
 * - Optical sensors for material flow and bubble detection
 * - Vibration isolation monitoring
 * - Chamber pressure and atmosphere composition sensors
 *
 * @author Grey Firmware Project
 */

#ifndef GF_ZEROG_FAB_SENSOR_H
#define GF_ZEROG_FAB_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Microgravity quality levels
 */
typedef enum {
    MICROG_QUALITY_EXCELLENT,    /**< < 10^-6 g residual */
    MICROG_QUALITY_GOOD,         /**< < 10^-5 g residual */
    MICROG_QUALITY_MARGINAL,     /**< < 10^-4 g residual */
    MICROG_QUALITY_POOR,         /**< > 10^-4 g residual */
    MICROG_QUALITY_INVALID       /**< Sensor fault */
} gf_microg_quality_t;

/**
 * @brief Fabrication sensor types
 */
typedef enum {
    FAB_SENSOR_ACCELEROMETER,    /**< 3-axis residual gravity */
    FAB_SENSOR_THERMAL,          /**< Thermal gradient array */
    FAB_SENSOR_OPTICAL,          /**< Material flow camera */
    FAB_SENSOR_PRESSURE,         /**< Chamber pressure */
    FAB_SENSOR_VIBRATION,        /**< Isolation platform status */
    FAB_SENSOR_ATMOSPHERE        /**< Gas composition */
} gf_fab_sensor_type_t;

/**
 * @brief Fabrication sensor reading
 */
typedef struct {
    gf_fab_sensor_type_t type;
    float value;
    float min_threshold;
    float max_threshold;
    uint32_t timestamp_ms;
    bool valid;
    bool alarm;
} gf_fab_sensor_reading_t;

/**
 * @brief Accelerometer data for microgravity verification
 */
typedef struct {
    float accel_x_ug;            /**< X-axis micro-g */
    float accel_y_ug;            /**< Y-axis micro-g */
    float accel_z_ug;            /**< Z-axis micro-g */
    float magnitude_ug;          /**< Total residual acceleration */
    gf_microg_quality_t quality;
    uint32_t timestamp_ms;
} gf_microg_data_t;

/**
 * @brief Thermal gradient data
 */
typedef struct {
    float temps_c[16];           /**< 16-zone thermal array */
    float gradient_k_per_mm;     /**< Thermal gradient magnitude */
    float gradient_angle_deg;    /**< Gradient direction */
    bool uniform;                /**< Gradient uniformity status */
} gf_thermal_gradient_t;

/* Function prototypes */
int gf_zerog_sensor_init(void);
int gf_zerog_sensor_read(gf_fab_sensor_type_t type, gf_fab_sensor_reading_t *reading);
int gf_zerog_get_microg_data(gf_microg_data_t *data);
int gf_zerog_get_thermal_gradient(gf_thermal_gradient_t *gradient);
int gf_zerog_sensor_calibrate(gf_fab_sensor_type_t type);
bool gf_zerog_sensor_healthy(gf_fab_sensor_type_t type);
void gf_zerog_sensor_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ZEROG_FAB_SENSOR_H */
