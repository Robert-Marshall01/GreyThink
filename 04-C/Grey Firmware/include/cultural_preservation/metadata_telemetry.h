/**
 * @file metadata_telemetry.h
 * @brief Cultural Artifact Metadata Telemetry
 *
 * INDUSTRY RELEVANCE:
 * Comprehensive metadata is essential for cultural heritage research,
 * provenance tracking, and collection management. Standards like
 * Dublin Core and CIDOC-CRM enable interoperability.
 *
 * MARKET CONTEXT:
 * - Museum collection management systems
 * - Art market provenance verification
 * - International loan and exhibition tracking
 * - Insurance documentation
 * - Repatriation and ownership research
 *
 * TECHNICAL APPROACH:
 * - Dublin Core metadata standard compliance
 * - CIDOC-CRM ontology mapping
 * - Linked Open Data integration
 * - Provenance chain documentation
 * - Multi-language support for descriptions
 *
 * @author Grey Firmware Project
 */

#ifndef GF_METADATA_TELEMETRY_H
#define GF_METADATA_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Provenance event type
 */
typedef enum {
    PROV_CREATION,
    PROV_ACQUISITION,
    PROV_TRANSFER,
    PROV_LOAN,
    PROV_RESTORATION,
    PROV_EXHIBITION,
    PROV_DEACCESSION
} gf_provenance_type_t;

/**
 * @brief Conservation status
 */
typedef enum {
    CONSERV_EXCELLENT,
    CONSERV_GOOD,
    CONSERV_FAIR,
    CONSERV_POOR,
    CONSERV_CRITICAL
} gf_conservation_status_t;

/**
 * @brief Artifact metadata record
 */
typedef struct {
    char artifact_id[32];
    char title[128];
    char creator[64];
    char date_created[32];
    char culture_origin[64];
    char medium[64];
    char dimensions[64];
    char location[64];
    gf_conservation_status_t status;
} gf_artifact_metadata_t;

/**
 * @brief Provenance event record
 */
typedef struct {
    gf_provenance_type_t type;
    char date[32];
    char from_entity[64];
    char to_entity[64];
    char location[64];
    char documentation[128];
    bool verified;
} gf_provenance_event_t;

/**
 * @brief Telemetry report structure
 */
typedef struct {
    char artifact_id[32];
    uint32_t report_time;
    uint8_t provenance_count;
    gf_conservation_status_t current_status;
    uint32_t last_inspection;
    float environmental_risk;    /**< 0.0-1.0 */
} gf_metadata_report_t;

/* Function prototypes */
int gf_metadata_telemetry_init(void);
int gf_metadata_set_artifact(const gf_artifact_metadata_t *meta);
int gf_metadata_get_artifact(const char *artifact_id, gf_artifact_metadata_t *meta);
int gf_metadata_add_provenance(const char *artifact_id, const gf_provenance_event_t *event);
int gf_metadata_get_provenance(const char *artifact_id, gf_provenance_event_t *events, 
                                uint8_t max_count, uint8_t *found);
int gf_metadata_generate_report(const char *artifact_id, gf_metadata_report_t *report);
int gf_metadata_export_dublin_core(const char *artifact_id, uint8_t *buffer, size_t max_len);
void gf_metadata_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_METADATA_TELEMETRY_H */
