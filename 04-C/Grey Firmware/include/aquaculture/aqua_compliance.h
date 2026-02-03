/**
 * @file aqua_compliance.h
 * @brief Food Safety & Compliance Telemetry for Aquaculture
 *
 * INDUSTRY RELEVANCE:
 * Seafood traceability is mandated by FDA, EU, and global food safety
 * regulations (FSMA, EU Reg 178/2002). This module provides audit-ready
 * logging and batch tracking essential for farm-to-fork transparency
 * in commercial aquaculture operations.
 *
 * Key capabilities demonstrated:
 * - HACCP critical control point logging
 * - Batch/lot traceability records
 * - Environmental condition archiving
 * - Tamper-evident audit trail
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_AQUA_COMPLIANCE_H
#define GF_AQUA_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Compliance record types */
typedef enum {
    GF_AQUA_RECORD_WATER_QUALITY,   /**< WQ measurement record */
    GF_AQUA_RECORD_FEEDING,         /**< Feeding event record */
    GF_AQUA_RECORD_TREATMENT,       /**< Medical/chemical treatment */
    GF_AQUA_RECORD_HARVEST,         /**< Harvest batch record */
    GF_AQUA_RECORD_MORTALITY        /**< Mortality event */
} gf_aqua_record_type_t;

/** Compliance log entry */
typedef struct {
    uint32_t record_id;
    gf_aqua_record_type_t type;
    uint64_t timestamp;
    char batch_id[32];
    char operator_id[16];
    uint8_t data[128];
    uint16_t data_len;
    uint32_t checksum;
} gf_aqua_compliance_record_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_aqua_compliance_init(void);
int gf_aqua_log_record(const gf_aqua_compliance_record_t* record);
int gf_aqua_export_batch(const char* batch_id, void* buffer, size_t* len);
int gf_aqua_verify_chain(uint32_t start_id, uint32_t end_id);
void gf_aqua_compliance_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_AQUA_COMPLIANCE_H */
