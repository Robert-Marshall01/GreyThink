/**
 * @file soil_analyzer.h
 * @brief Planetary Soil Analysis Sensor Driver
 * 
 * INDUSTRY RELEVANCE:
 * In-situ resource utilization (ISRU) and astrobiology missions require complex
 * soil analysis capabilities. This driver interfaces with spectrometers, gas
 * chromatographs, and other analytical instruments for extraterrestrial soil
 * characterization. Critical for Mars Sample Return and lunar ISRU programs.
 * 
 * Key applications:
 * - Mars Sample Return mission instrument control
 * - Lunar water ice detection (VIPER, Artemis)
 * - Asteroid mining feasibility analysis
 * - Biosignature detection for astrobiology
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_SOIL_ANALYZER_H
#define GF_SOIL_ANALYZER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define SOIL_MAX_ELEMENTS           32      /**< Max elements to detect */
#define SOIL_MAX_COMPOUNDS          64      /**< Max compounds to identify */
#define SOIL_SPECTRUM_BINS          2048    /**< Spectrometer bins */
#define SOIL_GC_CHANNELS            8       /**< Gas chromatograph channels */
#define SOIL_SAMPLE_TIMEOUT_MS      30000   /**< Analysis timeout */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Analysis mode
 */
typedef enum {
    SOIL_MODE_IDLE = 0,
    SOIL_MODE_STANDBY,
    SOIL_MODE_XRAY_FLUORESCENCE,    /**< XRF elemental analysis */
    SOIL_MODE_MASS_SPECTROMETRY,    /**< MS compound identification */
    SOIL_MODE_RAMAN_SPECTROSCOPY,   /**< Raman mineralogy */
    SOIL_MODE_GAS_CHROMATOGRAPHY,   /**< GC volatile detection */
    SOIL_MODE_THERMAL_ANALYSIS,     /**< TGA/DSC thermal properties */
    SOIL_MODE_CALIBRATING
} soil_analysis_mode_t;

/**
 * @brief Element detection result
 */
typedef struct {
    uint8_t atomic_number;          /**< Element atomic number */
    float concentration_ppm;        /**< Concentration in ppm */
    float confidence;               /**< Detection confidence (0-1) */
} soil_element_t;

/**
 * @brief Compound identification result
 */
typedef struct {
    char name[32];                  /**< Compound name */
    float molecular_weight;         /**< Molecular weight (g/mol) */
    float abundance_percent;        /**< Relative abundance (%) */
    float confidence;               /**< ID confidence (0-1) */
    bool organic;                   /**< Organic compound flag */
} soil_compound_t;

/**
 * @brief Complete analysis result
 */
typedef struct {
    uint32_t sample_id;
    uint32_t analysis_time_ms;
    
    /* Elemental composition */
    uint8_t element_count;
    soil_element_t elements[SOIL_MAX_ELEMENTS];
    
    /* Compound identification */
    uint8_t compound_count;
    soil_compound_t compounds[SOIL_MAX_COMPOUNDS];
    
    /* Physical properties */
    float water_content_percent;
    float organic_carbon_percent;
    float ph_estimate;
    float density_kg_m3;
    
    /* Quality metrics */
    float signal_to_noise;
    bool calibration_valid;
} soil_analysis_result_t;

/**
 * @brief Sensor configuration
 */
typedef struct {
    bool enable_xrf;
    bool enable_ms;
    bool enable_raman;
    bool enable_gc;
    bool enable_thermal;
    uint16_t integration_time_ms;
    uint8_t averaging_count;
    bool auto_calibration;
} soil_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize soil analyzer
 * @param config Sensor configuration
 * @return 0 on success, negative on error
 */
int soil_analyzer_init(const soil_config_t* config);

/**
 * @brief Shutdown analyzer
 * @return 0 on success, negative on error
 */
int soil_analyzer_shutdown(void);

/**
 * @brief Start sample analysis
 * @param mode Analysis mode to use
 * @return Sample ID on success, negative on error
 */
int soil_analyzer_start(soil_analysis_mode_t mode);

/**
 * @brief Check if analysis is complete
 * @return true if complete, false if in progress
 */
bool soil_analyzer_complete(void);

/**
 * @brief Get analysis results
 * @param result Output result structure
 * @return 0 on success, negative on error
 */
int soil_analyzer_get_result(soil_analysis_result_t* result);

/**
 * @brief Run calibration sequence
 * @return 0 on success, negative on error
 */
int soil_analyzer_calibrate(void);

/**
 * @brief Get raw spectrum data
 * @param mode Spectrum type
 * @param buffer Output buffer
 * @param buffer_size Buffer size
 * @return Bytes written, negative on error
 */
int soil_analyzer_get_spectrum(soil_analysis_mode_t mode,
                                uint8_t* buffer, size_t buffer_size);

#ifdef __cplusplus
}
#endif

#endif /* GF_SOIL_ANALYZER_H */
