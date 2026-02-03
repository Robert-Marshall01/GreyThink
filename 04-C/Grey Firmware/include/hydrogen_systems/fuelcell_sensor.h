/**
 * @file fuelcell_sensor.h
 * @brief Hydrogen Fuel Cell Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Hydrogen fuel cells are central to the clean energy transition, powering
 * everything from backup power systems and forklifts to buses, trucks, and
 * stationary power plants. Firmware engineers with fuel cell expertise are
 * in demand at Plug Power, Bloom Energy, Ballard Power, Toyota, Hyundai,
 * Nikola, and NEL Hydrogen.
 * 
 * This module provides sensor interfaces for monitoring PEM (Proton Exchange
 * Membrane) fuel cell stacks including voltage, current, temperature, and
 * hydrogen flow measurements critical for safe operation.
 * 
 * KEY CAPABILITIES:
 * - Cell voltage monitoring (individual cell and stack)
 * - Current measurement (DC output)
 * - Temperature sensing (stack, coolant, inlet/outlet)
 * - Hydrogen flow rate measurement
 * - Oxygen/air flow monitoring
 * - Humidity sensing (membrane health)
 * - Pressure transducer interfaces
 * 
 * STANDARDS COMPLIANCE:
 * - IEC 62282-3 (Fuel cell technologies - Safety)
 * - SAE J2615 (PEM fuel cell systems)
 * - ISO 14687 (Hydrogen fuel quality)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_FUELCELL_SENSOR_H
#define GF_FUELCELL_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define FC_MAX_CELLS           120   /**< Max cells in stack */
#define FC_MAX_TEMP_SENSORS    16    /**< Temperature sensor count */
#define FC_VOLTAGE_MIN_MV      600   /**< Min cell voltage (degraded) */
#define FC_VOLTAGE_NOM_MV      700   /**< Nominal cell voltage */
#define FC_VOLTAGE_MAX_MV      1000  /**< Max open-circuit voltage */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Fuel cell sensor type */
typedef enum {
    FC_SENSOR_CELL_VOLTAGE,    /**< Individual cell voltage */
    FC_SENSOR_STACK_VOLTAGE,   /**< Total stack voltage */
    FC_SENSOR_STACK_CURRENT,   /**< Stack output current */
    FC_SENSOR_TEMPERATURE,     /**< Temperature (various locations) */
    FC_SENSOR_H2_FLOW,         /**< Hydrogen flow rate */
    FC_SENSOR_AIR_FLOW,        /**< Air/oxygen flow rate */
    FC_SENSOR_H2_PRESSURE,     /**< Hydrogen inlet pressure */
    FC_SENSOR_COOLANT_TEMP,    /**< Coolant temperature */
    FC_SENSOR_HUMIDITY         /**< Membrane humidity */
} fc_sensor_type_t;

/** Temperature sensor location */
typedef enum {
    FC_TEMP_STACK_INLET,
    FC_TEMP_STACK_OUTLET,
    FC_TEMP_COOLANT_IN,
    FC_TEMP_COOLANT_OUT,
    FC_TEMP_AMBIENT,
    FC_TEMP_MEMBRANE
} fc_temp_location_t;

/** Sensor reading */
typedef struct {
    fc_sensor_type_t type;
    uint8_t channel;           /**< Sensor channel/index */
    float value;               /**< Measured value */
    float quality;             /**< 0.0-1.0 quality indicator */
    uint32_t timestamp_ms;
    bool valid;
} fc_sensor_reading_t;

/** Stack health summary */
typedef struct {
    float min_cell_voltage_mv;
    float max_cell_voltage_mv;
    float avg_cell_voltage_mv;
    float cell_voltage_spread_mv;  /**< Max-Min spread */
    float stack_power_w;
    float efficiency_pct;
    uint8_t degraded_cells;        /**< Cells below threshold */
} fc_stack_health_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize fuel cell sensor subsystem
 * @param cell_count Number of cells in stack
 * @return 0 on success, negative on error
 */
int fc_sensor_init(uint8_t cell_count);

/**
 * @brief Read individual cell voltage
 * @param cell_index Cell number (0 to cell_count-1)
 * @param voltage_mv Output voltage in millivolts
 * @return 0 on success
 */
int fc_read_cell_voltage(uint8_t cell_index, float* voltage_mv);

/**
 * @brief Read stack voltage and current
 * @param voltage_v Output voltage in volts
 * @param current_a Output current in amps
 * @return 0 on success
 */
int fc_read_stack_power(float* voltage_v, float* current_a);

/**
 * @brief Read temperature sensor
 * @param location Temperature sensor location
 * @param temp_c Output temperature in Celsius
 * @return 0 on success
 */
int fc_read_temperature(fc_temp_location_t location, float* temp_c);

/**
 * @brief Read hydrogen flow rate
 * @param flow_slpm Output flow in standard liters per minute
 * @return 0 on success
 */
int fc_read_h2_flow(float* flow_slpm);

/**
 * @brief Get stack health summary
 * @param health Output health structure
 * @return 0 on success
 */
int fc_get_stack_health(fc_stack_health_t* health);

/**
 * @brief Shutdown fuel cell sensors
 */
void fc_sensor_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_FUELCELL_SENSOR_H */
