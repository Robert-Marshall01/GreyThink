/**
 * @file error_correction.h
 * @brief Error Correction Coding Module - Edge Space Communications
 * 
 * @details Industry Relevance:
 * Satellite and deep-space communications operate at very low SNR with
 * significant channel impairments. Forward Error Correction (FEC) is essential:
 * - Reed-Solomon codes for burst error correction
 * - Convolutional/Viterbi for random errors
 * - LDPC/Turbo codes for near-Shannon performance
 * - Interleaving to distribute burst errors
 * 
 * CCSDS (Consultative Committee for Space Data Systems) defines standard
 * coding schemes used by NASA, ESA, JAXA for all spacecraft communications.
 * 
 * Trade-offs: Coding gain vs. latency vs. computational complexity.
 * Typical satellite links use concatenated coding (RS outer + Conv inner).
 * 
 * Standards: CCSDS 131.0-B-3, DVB-S2X, IESS-308 (Intelsat)
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_SPACE_ERROR_CORRECTION_H
#define GF_SPACE_ERROR_CORRECTION_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Reed-Solomon block size (255,223) standard */
#define GF_FEC_RS_BLOCK_SIZE            255
#define GF_FEC_RS_DATA_SIZE             223
#define GF_FEC_RS_PARITY_SIZE           32

/** Maximum correctable symbols per block */
#define GF_FEC_RS_MAX_ERRORS            16

/** Convolutional code constraint length */
#define GF_FEC_CONV_K                   7

/** Convolutional code rate (1/2) */
#define GF_FEC_CONV_RATE_INV            2

/** Interleaver depth */
#define GF_FEC_INTERLEAVE_DEPTH         4

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief FEC coding schemes
 */
typedef enum {
    GF_FEC_NONE,                    /**< No FEC (testing only) */
    GF_FEC_RS_255_223,              /**< Reed-Solomon (255,223) */
    GF_FEC_RS_255_239,              /**< Reed-Solomon (255,239) */
    GF_FEC_CONV_1_2,                /**< Convolutional rate 1/2 */
    GF_FEC_CONCAT_RS_CONV,          /**< Concatenated RS + Conv */
    GF_FEC_LDPC_1_2,                /**< LDPC rate 1/2 */
    GF_FEC_LDPC_3_4,                /**< LDPC rate 3/4 */
    GF_FEC_TURBO                    /**< Turbo code */
} gf_fec_scheme_t;

/**
 * @brief Interleaving modes
 */
typedef enum {
    GF_INTERLEAVE_NONE,             /**< No interleaving */
    GF_INTERLEAVE_BLOCK,            /**< Block interleaving */
    GF_INTERLEAVE_CONVOL,           /**< Convolutional interleaving */
    GF_INTERLEAVE_RANDOM            /**< Pseudo-random interleaving */
} gf_interleave_mode_t;

/**
 * @brief Channel quality indicator
 */
typedef struct {
    float snr_db;                   /**< Signal-to-noise ratio */
    float ber_raw;                  /**< Raw bit error rate */
    float ber_decoded;              /**< Post-FEC bit error rate */
    uint32_t errors_corrected;      /**< Errors corrected */
    uint32_t errors_detected;       /**< Errors detected (uncorrectable) */
    bool sync_lock;                 /**< Decoder sync status */
} gf_fec_quality_t;

/**
 * @brief FEC encoder/decoder context
 */
typedef struct {
    gf_fec_scheme_t scheme;         /**< Active coding scheme */
    gf_interleave_mode_t interleave;/**< Interleaving mode */
    uint16_t block_size;            /**< Current block size */
    uint16_t data_size;             /**< Data portion size */
    uint32_t frames_encoded;        /**< Total frames encoded */
    uint32_t frames_decoded;        /**< Total frames decoded */
    uint32_t decode_failures;       /**< Uncorrectable frames */
    gf_fec_quality_t quality;       /**< Current quality metrics */
} gf_fec_context_t;

/**
 * @brief Reed-Solomon codec parameters
 */
typedef struct {
    uint8_t n;                      /**< Block length */
    uint8_t k;                      /**< Data length */
    uint8_t t;                      /**< Error correction capability */
    uint8_t generator_poly;         /**< Generator polynomial */
    uint8_t primitive_element;      /**< Primitive element */
    uint8_t first_root;             /**< First root of generator */
} gf_rs_params_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize FEC subsystem
 * @return 0 on success, negative error code on failure
 */
int gf_fec_init(void);

/**
 * @brief Shutdown FEC subsystem
 * @return 0 on success
 */
int gf_fec_shutdown(void);

/**
 * @brief Set active coding scheme
 * @param scheme FEC scheme to use
 * @param interleave Interleaving mode
 * @return 0 on success
 */
int gf_fec_set_scheme(gf_fec_scheme_t scheme, gf_interleave_mode_t interleave);

/**
 * @brief Encode data block
 * @param data Input data
 * @param data_len Data length
 * @param encoded Output buffer (must be sized for parity)
 * @param encoded_len Output: total encoded length
 * @return 0 on success
 */
int gf_fec_encode(const uint8_t* data, uint16_t data_len,
                  uint8_t* encoded, uint16_t* encoded_len);

/**
 * @brief Decode data block
 * @param encoded Input encoded data
 * @param encoded_len Encoded length
 * @param decoded Output buffer
 * @param decoded_len Output: decoded data length
 * @param errors_corrected Output: number of errors fixed
 * @return 0 on success, -1 if uncorrectable
 */
int gf_fec_decode(const uint8_t* encoded, uint16_t encoded_len,
                  uint8_t* decoded, uint16_t* decoded_len,
                  uint16_t* errors_corrected);

/**
 * @brief Interleave data block
 * @param data Input data
 * @param len Data length
 * @param interleaved Output buffer
 * @return 0 on success
 */
int gf_fec_interleave(const uint8_t* data, uint16_t len, uint8_t* interleaved);

/**
 * @brief Deinterleave data block
 * @param interleaved Input interleaved data
 * @param len Data length
 * @param data Output buffer
 * @return 0 on success
 */
int gf_fec_deinterleave(const uint8_t* interleaved, uint16_t len, uint8_t* data);

/**
 * @brief Get current FEC context
 * @param ctx Output context structure
 * @return 0 on success
 */
int gf_fec_get_context(gf_fec_context_t* ctx);

/**
 * @brief Get channel quality metrics
 * @param quality Output quality structure
 * @return 0 on success
 */
int gf_fec_get_quality(gf_fec_quality_t* quality);

/**
 * @brief Reed-Solomon encode block
 * @param params RS parameters
 * @param data Input data (k bytes)
 * @param codeword Output codeword (n bytes)
 * @return 0 on success
 */
int gf_rs_encode(const gf_rs_params_t* params,
                 const uint8_t* data, uint8_t* codeword);

/**
 * @brief Reed-Solomon decode block
 * @param params RS parameters
 * @param codeword Input codeword (n bytes)
 * @param data Output data (k bytes)
 * @param errors_fixed Output: errors corrected
 * @return 0 on success, -1 if uncorrectable
 */
int gf_rs_decode(const gf_rs_params_t* params,
                 const uint8_t* codeword, uint8_t* data,
                 uint8_t* errors_fixed);

#ifdef __cplusplus
}
#endif

#endif /* GF_SPACE_ERROR_CORRECTION_H */
