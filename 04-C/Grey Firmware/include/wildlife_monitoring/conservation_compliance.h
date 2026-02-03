/**
 * @file conservation_compliance.h
 * @brief Conservation Standards Compliance Logging
 * 
 * INDUSTRY RELEVANCE:
 * Wildlife monitoring must comply with conservation regulations, research
 * ethics, and data sharing standards. CITES, IUCN, and national wildlife
 * agencies require detailed tracking of endangered species. ESG reporting
 * and biodiversity credits also drive demand for auditable conservation data.
 * 
 * This module provides compliance logging, data validation, and audit trail
 * generation for wildlife monitoring systems.
 * 
 * KEY CAPABILITIES:
 * - CITES tracking compliance
 * - Research permit validation
 * - Animal welfare monitoring
 * - Data provenance logging
 * - Access control logging
 * - Export/import documentation
 * - Biodiversity credit tracking
 * 
 * STANDARDS COMPLIANCE:
 * - CITES (Convention on International Trade)
 * - IUCN Red List standards
 * - Movebank data ethics
 * - GBIF biodiversity standards
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CONSERVATION_COMPLIANCE_H
#define GF_CONSERVATION_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define CC_MAX_LOG_ENTRIES     1024  /**< Compliance log size */
#define CC_PERMIT_ID_LEN       32    /**< Permit ID length */
#define CC_SPECIES_NAME_LEN    64    /**< Species name length */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Conservation status */
typedef enum {
    CC_STATUS_LEAST_CONCERN,
    CC_STATUS_NEAR_THREATENED,
    CC_STATUS_VULNERABLE,
    CC_STATUS_ENDANGERED,
    CC_STATUS_CRITICALLY_ENDANGERED,
    CC_STATUS_EXTINCT_WILD,
    CC_STATUS_EXTINCT
} cc_status_t;

/** CITES appendix */
typedef enum {
    CC_CITES_NONE,
    CC_CITES_APPENDIX_I,    /**< Most endangered */
    CC_CITES_APPENDIX_II,   /**< Trade controlled */
    CC_CITES_APPENDIX_III   /**< Country-specific */
} cc_cites_t;

/** Compliance event */
typedef enum {
    CC_EVENT_COLLAR_ATTACHED,
    CC_EVENT_COLLAR_REMOVED,
    CC_EVENT_DATA_EXPORT,
    CC_EVENT_PERMIT_VERIFIED,
    CC_EVENT_WELFARE_CHECK,
    CC_EVENT_MORTALITY_DETECTED,
    CC_EVENT_ESCAPE_ATTEMPT,
    CC_EVENT_LOCATION_SHARED
} cc_event_t;

/** Species record */
typedef struct {
    char species_name[CC_SPECIES_NAME_LEN];
    char common_name[CC_SPECIES_NAME_LEN];
    cc_status_t iucn_status;
    cc_cites_t cites_appendix;
    bool protected_species;
} cc_species_t;

/** Compliance log entry */
typedef struct {
    uint32_t timestamp;
    uint32_t animal_id;
    cc_event_t event;
    char permit_id[CC_PERMIT_ID_LEN];
    char researcher_id[CC_PERMIT_ID_LEN];
    double latitude;
    double longitude;
    uint8_t hash[32];      /**< SHA-256 for integrity */
} cc_log_entry_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize compliance logging
 * @param permit_id Research permit identifier
 * @return 0 on success
 */
int cc_init(const char* permit_id);

/**
 * @brief Register species for monitoring
 * @param species Species information
 * @return 0 on success
 */
int cc_register_species(const cc_species_t* species);

/**
 * @brief Log compliance event
 * @param animal_id Animal identifier
 * @param event Event type
 * @param lat Latitude (or 0 if N/A)
 * @param lon Longitude (or 0 if N/A)
 * @return 0 on success
 */
int cc_log_event(uint32_t animal_id, cc_event_t event,
                 double lat, double lon);

/**
 * @brief Verify permit validity
 * @param permit_id Permit to verify
 * @param valid Output validity flag
 * @return 0 on success
 */
int cc_verify_permit(const char* permit_id, bool* valid);

/**
 * @brief Generate audit report
 * @param entries Output log entries
 * @param max_entries Maximum entries
 * @param count Output entry count
 * @return 0 on success
 */
int cc_get_audit_log(cc_log_entry_t* entries, 
                     uint16_t max_entries, uint16_t* count);

/**
 * @brief Shutdown compliance logging
 */
void cc_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CONSERVATION_COMPLIANCE_H */
