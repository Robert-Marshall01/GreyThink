/**
 * @file tube_pressure_sensor.h
 * @brief Tube Pressure Sensor Driver for Hyperloop Systems
 * 
 * INDUSTRY RELEVANCE:
 * Hyperloop transportation systems operate in near-vacuum tubes requiring
 * continuous pressure monitoring for pod safety and propulsion efficiency.
 * This driver interfaces with high-precision vacuum gauges measuring tube
 * pressure, detecting leaks, and coordinating with propulsion systems.
 * 
 * Applications:
 * - Virgin Hyperloop / Hyperloop One systems
 * - High-speed maglev transportation
 * - Vacuum tube freight systems
 * - Pneumatic tube networks
 * - Particle accelerator vacuum systems
 * 
 * Standards: ISO 3529 (Vacuum Technology), ASME B31.3 (Process Piping)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_TUBE_PRESSURE_SENSOR_H
#define GF_TUBE_PRESSURE_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define PRESSURE_MAX_SENSORS        32      /* Maximum pressure sensors */
#define PRESSURE_SAMPLE_RATE_HZ     100     /* High-speed sampling */

/* Operating pressure ranges (Pa) */
#define PRESSURE_ATMOSPHERIC_PA     101325  /* Atmospheric pressure */
#define PRESSURE_NOMINAL_PA         100     /* Nominal tube vacuum (0.1 kPa) */
#define PRESSURE_WARNING_PA         500     /* Warning threshold */
#define PRESSURE_CRITICAL_PA        1000    /* Emergency threshold */
#define PRESSURE_BREACH_PA          10000   /* Tube breach detected */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Pressure sensor technology */
typedef enum {
    PRESSURE_PIRANI,            /* Pirani thermal conductivity */
    PRESSURE_COLD_CATHODE,      /* Cold cathode ionization */
    PRESSURE_CAPACITANCE,       /* Capacitance diaphragm */
    PRESSURE_PIEZO,             /* Piezoresistive */
    PRESSURE_CONVECTION         /* Convection-enhanced Pirani */
} pressure_sensor_type_t;

/** Sensor location */
typedef enum {
    LOCATION_TUBE_SECTION,      /* Main tube section */
    LOCATION_AIRLOCK,           /* Station airlock */
    LOCATION_POD_BAY,           /* Pod docking area */
    LOCATION_PUMP_STATION,      /* Vacuum pump station */
    LOCATION_EMERGENCY_VALVE    /* Emergency vent location */
} sensor_location_t;

/** Pressure status */
typedef enum {
    PRESSURE_STATUS_NOMINAL,    /* Operating pressure achieved */
    PRESSURE_STATUS_PUMPING,    /* Still evacuating */
    PRESSURE_STATUS_ELEVATED,   /* Pressure rising */
    PRESSURE_STATUS_LEAK,       /* Slow leak detected */
    PRESSURE_STATUS_BREACH,     /* Major breach detected */
    PRESSURE_STATUS_OFFLINE     /* Sensor offline */
} pressure_status_t;

/** Pressure reading */
typedef struct {
    uint8_t sensor_id;
    pressure_sensor_type_t type;
    sensor_location_t location;
    uint32_t tube_section;
    float pressure_pa;
    float rate_of_change_pa_s;
    pressure_status_t status;
    uint32_t timestamp_ms;
    bool valid;
} pressure_reading_t;

/** Tube section state */
typedef struct {
    uint32_t section_id;
    float length_km;
    float avg_pressure_pa;
    pressure_status_t status;
    uint8_t sensor_count;
    bool isolation_active;
} tube_section_t;

/** Module configuration */
typedef struct {
    uint8_t num_sensors;
    uint32_t num_sections;
    float target_pressure_pa;
    float leak_threshold_pa_s;
    bool auto_isolation;
} pressure_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int tube_pressure_init(const pressure_config_t *config);
void tube_pressure_shutdown(void);

int tube_pressure_read(uint8_t sensor_id, pressure_reading_t *reading);
int tube_pressure_read_section(uint32_t section_id, tube_section_t *state);
float tube_pressure_get_average(void);

bool tube_pressure_is_safe(void);
bool tube_pressure_detect_leak(uint32_t section_id);
int tube_pressure_isolate_section(uint32_t section_id);

void tube_pressure_update(uint32_t elapsed_ms);

#endif /* GF_TUBE_PRESSURE_SENSOR_H */
