/**
 * @file nutrient_sensor.h
 * @brief Advanced Soil Nutrient Sensor for Edge Agriculture Biotech
 * 
 * INDUSTRY RELEVANCE:
 * Precision agriculture market exceeds $8 billion, with soil sensing as a
 * critical enabler. Advanced nutrient monitoring enables:
 * - Real-time NPK (Nitrogen, Phosphorus, Potassium) measurement
 * - Micronutrient analysis (Fe, Mn, Zn, Cu, B)
 * - Soil organic matter and microbial activity assessment
 * - Variable-rate fertilizer application optimization
 * 
 * Target applications: Precision farming, greenhouse automation, vertical
 * farms, soil research, regenerative agriculture, carbon sequestration.
 * 
 * Standards: ISO 11464 (soil sampling), ISO 10390 (soil pH)
 */

#ifndef GF_NUTRIENT_SENSOR_H
#define GF_NUTRIENT_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Nutrient Sensor Types                                                      */
/*===========================================================================*/

typedef enum {
    NUTRIENT_N,             /* Nitrogen (total, nitrate, ammonium) */
    NUTRIENT_P,             /* Phosphorus */
    NUTRIENT_K,             /* Potassium */
    NUTRIENT_CA,            /* Calcium */
    NUTRIENT_MG,            /* Magnesium */
    NUTRIENT_S,             /* Sulfur */
    NUTRIENT_FE,            /* Iron */
    NUTRIENT_MN,            /* Manganese */
    NUTRIENT_ZN,            /* Zinc */
    NUTRIENT_CU,            /* Copper */
    NUTRIENT_B,             /* Boron */
    NUTRIENT_COUNT
} nutrient_type_t;

typedef enum {
    SOIL_TYPE_CLAY,
    SOIL_TYPE_LOAM,
    SOIL_TYPE_SAND,
    SOIL_TYPE_SILT,
    SOIL_TYPE_PEAT,
    SOIL_TYPE_ORGANIC
} soil_type_t;

typedef struct {
    float concentration_ppm;    /* Concentration in ppm */
    float availability_pct;     /* Plant-available percentage */
    bool deficient;             /* Below optimal range */
    bool excessive;             /* Above optimal range */
} nutrient_reading_t;

typedef struct {
    float ph;                   /* Soil pH (0-14) */
    float ec_dsm;               /* Electrical conductivity (dS/m) */
    float cec_meq;              /* Cation exchange capacity (meq/100g) */
    float organic_matter_pct;   /* Organic matter percentage */
    float moisture_pct;         /* Volumetric water content */
    float temperature_c;        /* Soil temperature */
    soil_type_t soil_type;      /* Detected soil type */
    nutrient_reading_t nutrients[NUTRIENT_COUNT];
    uint32_t timestamp;
} soil_analysis_t;

typedef struct {
    uint32_t sample_depth_mm;   /* Sampling depth */
    uint32_t analysis_interval; /* Interval between analyses (s) */
    bool enable_micronutrients; /* Enable micronutrient analysis */
    float temp_compensation;    /* Temperature compensation factor */
} nutrient_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int nutrient_sensor_init(const nutrient_config_t* config);
int nutrient_sensor_shutdown(void);

int nutrient_start_analysis(void);
int nutrient_get_analysis(soil_analysis_t* analysis);
int nutrient_get_single(nutrient_type_t nutrient, nutrient_reading_t* reading);

int nutrient_calibrate_ph(float ref_low, float ref_high);
int nutrient_calibrate_ec(float ref_value);

int nutrient_set_optimal_range(nutrient_type_t nutrient, float min, float max);
int nutrient_generate_prescription(uint8_t* prescription_data, uint16_t max_len);

#endif /* GF_NUTRIENT_SENSOR_H */
