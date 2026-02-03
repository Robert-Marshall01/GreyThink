/**
 * @file model_loader.h
 * @brief Model Loader with Checksum Verification
 *
 * INDUSTRY RELEVANCE:
 * Reliable model loading is critical for edge AI deployment:
 * - Integrity verification (CRC32, SHA-256)
 * - Version compatibility checking
 * - Secure model storage and loading
 * - Flash/SD card model management
 * - OTA model updates
 *
 * This module provides secure model loading with verification.
 */

#ifndef GF_MODEL_LOADER_H
#define GF_MODEL_LOADER_H

#include "ai/inference.h"
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Model Storage Definitions                                                 */
/*===========================================================================*/

/**
 * @brief Model storage locations
 */
typedef enum {
    GF_MODEL_FROM_MEMORY,       /**< Model in RAM */
    GF_MODEL_FROM_FLASH,        /**< Model in internal flash */
    GF_MODEL_FROM_EXTERNAL,     /**< Model in external flash/SD */
    GF_MODEL_FROM_STREAM        /**< Streaming load */
} gf_model_source_t;

/**
 * @brief Model verification methods
 */
typedef enum {
    GF_VERIFY_NONE,             /**< No verification */
    GF_VERIFY_CRC32,            /**< CRC32 checksum */
    GF_VERIFY_SHA256,           /**< SHA-256 hash */
    GF_VERIFY_SIGNATURE         /**< ECDSA signature */
} gf_verify_method_t;

/**
 * @brief Model load status
 */
typedef enum {
    GF_LOAD_OK = 0,
    GF_LOAD_ERROR_NOT_FOUND = -1,
    GF_LOAD_ERROR_READ = -2,
    GF_LOAD_ERROR_CHECKSUM = -3,
    GF_LOAD_ERROR_SIGNATURE = -4,
    GF_LOAD_ERROR_VERSION = -5,
    GF_LOAD_ERROR_SIZE = -6,
    GF_LOAD_ERROR_FORMAT = -7,
    GF_LOAD_ERROR_MEMORY = -8
} gf_load_status_t;

/*===========================================================================*/
/* Model Catalog                                                             */
/*===========================================================================*/

/**
 * @brief Maximum models in catalog
 */
#define GF_MODEL_MAX_CATALOG    8

/**
 * @brief Model catalog entry
 */
typedef struct {
    char name[32];              /**< Model name */
    char version[16];           /**< Version string */
    gf_model_source_t source;
    uint32_t address;           /**< Flash address or file offset */
    uint32_t size;              /**< Model size bytes */
    uint32_t checksum;          /**< Verification checksum */
    gf_verify_method_t verify;
    gf_model_format_t format;
    gf_tensor_shape_t input_shape;
    gf_tensor_shape_t output_shape;
    uint32_t required_arena;
    bool is_quantized;
    uint64_t timestamp;         /**< Last update timestamp */
} gf_model_entry_t;

/**
 * @brief Model catalog
 */
typedef struct {
    gf_model_entry_t entries[GF_MODEL_MAX_CATALOG];
    uint8_t count;
    uint8_t active_index;       /**< Currently loaded model */
} gf_model_catalog_t;

/*===========================================================================*/
/* Model Loader Configuration                                                */
/*===========================================================================*/

/**
 * @brief Loader configuration
 */
typedef struct {
    gf_verify_method_t default_verify;
    bool allow_unsigned;            /**< Allow unverified models */
    uint32_t max_model_size;
    
    /* Flash read function (for flash-based models) */
    int (*flash_read)(uint32_t addr, void* buf, size_t len);
    
    /* External storage read (for SD/external flash) */
    int (*ext_read)(const char* path, void* buf, size_t len);
    
    /* Public key for signature verification */
    const uint8_t* public_key;
    size_t public_key_len;
} gf_loader_config_t;

/**
 * @brief Default loader configuration
 */
#define GF_LOADER_CONFIG_DEFAULT {    \
    .default_verify = GF_VERIFY_CRC32, \
    .allow_unsigned = false,          \
    .max_model_size = GF_AI_MAX_MODEL_SIZE, \
    .flash_read = NULL,               \
    .ext_read = NULL,                 \
    .public_key = NULL,               \
    .public_key_len = 0               \
}

/**
 * @brief Model loader handle
 */
typedef struct gf_model_loader* gf_loader_t;

/*===========================================================================*/
/* Model Loader API                                                          */
/*===========================================================================*/

/**
 * @brief Initialize model loader
 * @param config Configuration
 * @param loader Output handle
 * @return GF_LOAD_OK on success
 */
gf_load_status_t gf_loader_init(const gf_loader_config_t* config,
                                 gf_loader_t* loader);

/**
 * @brief Scan for available models
 * @param loader Loader handle
 * @param catalog Output catalog
 * @return Number of models found
 */
int gf_loader_scan(gf_loader_t loader, gf_model_catalog_t* catalog);

/**
 * @brief Register a model in catalog
 * @param loader Loader handle
 * @param entry Model entry
 * @return Model index on success, negative on error
 */
int gf_loader_register(gf_loader_t loader, const gf_model_entry_t* entry);

/**
 * @brief Load model from catalog by name
 * @param loader Loader handle
 * @param name Model name
 * @param engine Inference engine to load into
 * @return GF_LOAD_OK on success
 */
gf_load_status_t gf_loader_load_by_name(gf_loader_t loader,
                                         const char* name,
                                         gf_inference_t engine);

/**
 * @brief Load model from catalog by index
 * @param loader Loader handle
 * @param index Catalog index
 * @param engine Inference engine
 * @return GF_LOAD_OK on success
 */
gf_load_status_t gf_loader_load_by_index(gf_loader_t loader,
                                          uint8_t index,
                                          gf_inference_t engine);

/**
 * @brief Load model from memory with verification
 * @param loader Loader handle
 * @param data Model data
 * @param size Data size
 * @param expected_checksum Expected checksum (0 to skip)
 * @param engine Inference engine
 * @return GF_LOAD_OK on success
 */
gf_load_status_t gf_loader_load_from_memory(gf_loader_t loader,
                                             const void* data,
                                             size_t size,
                                             uint32_t expected_checksum,
                                             gf_inference_t engine);

/**
 * @brief Verify model integrity without loading
 * @param loader Loader handle
 * @param data Model data
 * @param size Data size
 * @param method Verification method
 * @param expected Eexpected checksum/hash
 * @return GF_LOAD_OK if verification passes
 */
gf_load_status_t gf_loader_verify(gf_loader_t loader,
                                   const void* data,
                                   size_t size,
                                   gf_verify_method_t method,
                                   const void* expected);

/**
 * @brief Get catalog information
 * @param loader Loader handle
 * @param catalog Output catalog
 * @return GF_LOAD_OK on success
 */
gf_load_status_t gf_loader_get_catalog(gf_loader_t loader,
                                        gf_model_catalog_t* catalog);

/**
 * @brief Get model entry by name
 * @param loader Loader handle
 * @param name Model name
 * @param entry Output entry
 * @return GF_LOAD_OK on success
 */
gf_load_status_t gf_loader_get_entry(gf_loader_t loader,
                                      const char* name,
                                      gf_model_entry_t* entry);

/**
 * @brief Deinitialize loader
 * @param loader Loader handle
 */
void gf_loader_deinit(gf_loader_t loader);

/*===========================================================================*/
/* Checksum/Hash Utilities                                                   */
/*===========================================================================*/

/**
 * @brief Calculate CRC32 of data
 * @param data Data buffer
 * @param len Data length
 * @return CRC32 value
 */
uint32_t gf_model_crc32(const void* data, size_t len);

/**
 * @brief Calculate SHA-256 of data
 * @param data Data buffer
 * @param len Data length
 * @param hash Output hash (32 bytes)
 */
void gf_model_sha256(const void* data, size_t len, uint8_t hash[32]);

/**
 * @brief Verify ECDSA signature
 * @param data Data buffer
 * @param len Data length
 * @param signature Signature bytes
 * @param sig_len Signature length
 * @param public_key Public key
 * @param key_len Key length
 * @return true if signature valid
 */
bool gf_model_verify_signature(const void* data,
                                size_t len,
                                const uint8_t* signature,
                                size_t sig_len,
                                const uint8_t* public_key,
                                size_t key_len);

#ifdef __cplusplus
}
#endif

#endif /* GF_MODEL_LOADER_H */
