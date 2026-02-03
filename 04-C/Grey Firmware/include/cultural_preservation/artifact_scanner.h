/**
 * @file artifact_scanner.h
 * @brief Cultural Artifact Scanning System
 *
 * INDUSTRY RELEVANCE:
 * High-resolution 3D scanning of cultural artifacts enables digital
 * preservation, research access, and virtual museum exhibitions.
 * Non-invasive imaging protects irreplaceable items.
 *
 * MARKET CONTEXT:
 * - Smithsonian, Google Arts & Culture digitization
 * - UNESCO World Heritage documentation
 * - Insurance and provenance verification
 * - Virtual and augmented reality exhibits
 * - Academic research access platforms
 *
 * TECHNICAL APPROACH:
 * - Multi-spectral imaging (visible, IR, UV)
 * - Photogrammetry and structured light 3D
 * - Material composition analysis
 * - Damage and degradation detection
 * - IIIF-compatible output formats
 *
 * @author Grey Firmware Project
 */

#ifndef GF_ARTIFACT_SCANNER_H
#define GF_ARTIFACT_SCANNER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Scanning mode
 */
typedef enum {
    SCAN_MODE_PHOTO,           /**< High-res photography */
    SCAN_MODE_3D_STRUCTURED,   /**< Structured light 3D */
    SCAN_MODE_MULTISPECTRAL,   /**< IR/UV/visible bands */
    SCAN_MODE_XRAY,            /**< Radiography */
    SCAN_MODE_LIDAR            /**< Laser scanning */
} gf_scan_mode_t;

/**
 * @brief Artifact material type
 */
typedef enum {
    MATERIAL_UNKNOWN,
    MATERIAL_STONE,
    MATERIAL_METAL,
    MATERIAL_CERAMIC,
    MATERIAL_TEXTILE,
    MATERIAL_PAPER,
    MATERIAL_WOOD,
    MATERIAL_ORGANIC
} gf_artifact_material_t;

/**
 * @brief Scan configuration
 */
typedef struct {
    gf_scan_mode_t mode;
    float resolution_mm;       /**< Spatial resolution */
    uint8_t color_depth_bits;  /**< 8, 16, or 32 */
    bool capture_raw;
    float lighting_intensity;  /**< 0.0-1.0, minimize for sensitive items */
} gf_scan_config_t;

/**
 * @brief Scan result metadata
 */
typedef struct {
    uint32_t scan_id;
    char artifact_id[32];
    gf_scan_mode_t mode;
    uint32_t data_size_bytes;
    float actual_resolution_mm;
    uint32_t timestamp;
    float scan_duration_sec;
    char operator_id[16];
} gf_scan_result_t;

/**
 * @brief Artifact condition assessment
 */
typedef struct {
    float overall_condition;   /**< 0.0-1.0 */
    bool cracks_detected;
    bool discoloration_detected;
    bool structural_damage;
    char notes[256];
} gf_condition_t;

/* Function prototypes */
int gf_artifact_scanner_init(void);
int gf_artifact_configure(const gf_scan_config_t *config);
int gf_artifact_start_scan(const char *artifact_id);
int gf_artifact_get_scan_result(gf_scan_result_t *result);
int gf_artifact_assess_condition(gf_condition_t *condition);
int gf_artifact_detect_material(gf_artifact_material_t *material);
int gf_artifact_export_iiif(uint8_t *buffer, size_t max_len);
void gf_artifact_scanner_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ARTIFACT_SCANNER_H */
