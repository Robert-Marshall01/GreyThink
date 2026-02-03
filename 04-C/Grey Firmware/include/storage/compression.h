/**
 * @file compression.h
 * @brief Data Compression Module for Edge Storage
 * 
 * Provides lightweight compression algorithms suitable for embedded
 * systems, optimizing storage efficiency and transmission bandwidth.
 * 
 * Key Features:
 * - Multiple compression algorithms (LZ4, RLE, Huffman)
 * - Streaming compression for large data
 * - Delta compression for time-series data
 * - Configurable speed/ratio tradeoffs
 * - In-place decompression support
 * 
 * Industry Relevance:
 * - Telemetry data compression for bandwidth savings
 * - Flash storage optimization
 * - OTA update payload compression
 * - Sensor data logging with minimal storage
 */

#ifndef GF_COMPRESSION_H
#define GF_COMPRESSION_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define GF_COMP_MAX_BLOCK_SIZE      (64 * 1024)  /* 64KB max block */
#define GF_COMP_HEADER_SIZE         8
#define GF_COMP_DICTIONARY_SIZE     4096

/*===========================================================================*/
/* Enumerations                                                               */
/*===========================================================================*/

/**
 * @brief Compression algorithms
 */
typedef enum {
    GF_COMP_ALG_NONE         = 0x00,  /** No compression (passthrough) */
    GF_COMP_ALG_RLE          = 0x01,  /** Run-Length Encoding */
    GF_COMP_ALG_LZ4          = 0x02,  /** LZ4 fast compression */
    GF_COMP_ALG_HUFFMAN      = 0x03,  /** Huffman encoding */
    GF_COMP_ALG_DELTA        = 0x04,  /** Delta encoding for time-series */
    GF_COMP_ALG_ZSTD_LITE    = 0x05,  /** Lightweight ZSTD variant */
    GF_COMP_ALG_CUSTOM       = 0xFF,  /** Custom algorithm via callback */
} gf_comp_algorithm_t;

/**
 * @brief Compression levels (speed vs ratio tradeoff)
 */
typedef enum {
    GF_COMP_LEVEL_FAST       = 0,     /** Fastest, lower ratio */
    GF_COMP_LEVEL_DEFAULT    = 1,     /** Balanced */
    GF_COMP_LEVEL_BEST       = 2,     /** Best ratio, slower */
} gf_comp_level_t;

/**
 * @brief Compression result codes
 */
typedef enum {
    GF_COMP_OK               = 0,
    GF_COMP_ERR_INVALID      = -1,
    GF_COMP_ERR_NOMEM        = -2,
    GF_COMP_ERR_OVERFLOW     = -3,
    GF_COMP_ERR_CORRUPT      = -4,
    GF_COMP_ERR_UNSUPPORTED  = -5,
} gf_comp_result_t;

/**
 * @brief Stream state for streaming compression
 */
typedef enum {
    GF_COMP_STREAM_INIT      = 0,
    GF_COMP_STREAM_RUNNING   = 1,
    GF_COMP_STREAM_FLUSHING  = 2,
    GF_COMP_STREAM_DONE      = 3,
    GF_COMP_STREAM_ERROR     = 4,
} gf_comp_stream_state_t;

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Compression configuration
 */
typedef struct {
    gf_comp_algorithm_t algorithm;    /** Compression algorithm */
    gf_comp_level_t     level;        /** Compression level */
    bool                use_dict;     /** Enable dictionary compression */
    uint8_t            *dictionary;   /** Optional pre-built dictionary */
    size_t              dict_size;    /** Dictionary size */
    uint32_t            block_size;   /** Block size for streaming */
    bool                checksum;     /** Add integrity checksum */
} gf_comp_config_t;

/**
 * @brief Compressed block header
 */
typedef struct {
    uint8_t             magic[2];     /** Compression magic bytes */
    uint8_t             algorithm;    /** Algorithm identifier */
    uint8_t             flags;        /** Compression flags */
    uint16_t            orig_size;    /** Original data size */
    uint16_t            comp_size;    /** Compressed data size */
} gf_comp_header_t;

/**
 * @brief Compression statistics
 */
typedef struct {
    uint64_t            bytes_in;     /** Total input bytes */
    uint64_t            bytes_out;    /** Total output bytes */
    float               ratio;        /** Compression ratio */
    uint32_t            blocks;       /** Blocks processed */
    uint32_t            errors;       /** Compression errors */
    uint32_t            time_us;      /** Processing time in microseconds */
} gf_comp_stats_t;

/**
 * @brief Streaming compression context
 */
typedef struct {
    gf_comp_config_t        config;
    gf_comp_stream_state_t  state;
    uint8_t                *input_buf;
    size_t                  input_pos;
    size_t                  input_len;
    uint8_t                *output_buf;
    size_t                  output_pos;
    size_t                  output_size;
    uint8_t                 dict[GF_COMP_DICTIONARY_SIZE];
    size_t                  dict_pos;
    gf_comp_stats_t         stats;
    uint32_t                checksum;
} gf_comp_stream_t;

/**
 * @brief Delta compression context for time-series
 */
typedef struct {
    int64_t             last_value;
    uint64_t            last_timestamp;
    uint8_t             value_bits;    /** Bits per delta value */
    uint8_t             time_bits;     /** Bits per delta timestamp */
    bool                initialized;
} gf_delta_ctx_t;

/**
 * @brief Custom compression callbacks
 */
typedef int (*gf_comp_encode_cb_t)(const void *input, size_t input_size,
                                   void *output, size_t *output_size, void *ctx);
typedef int (*gf_comp_decode_cb_t)(const void *input, size_t input_size,
                                   void *output, size_t output_size, void *ctx);

/*===========================================================================*/
/* API Functions - Basic Compression                                          */
/*===========================================================================*/

/**
 * @brief Initialize compression module
 */
int gf_comp_init(void);

/**
 * @brief Set default compression configuration
 */
int gf_comp_set_config(const gf_comp_config_t *config);

/**
 * @brief Compress data in one shot
 */
int gf_comp_compress(gf_comp_algorithm_t alg, const void *input,
                     size_t input_size, void *output, size_t *output_size);

/**
 * @brief Decompress data in one shot
 */
int gf_comp_decompress(const void *input, size_t input_size,
                       void *output, size_t output_size, size_t *actual_size);

/**
 * @brief Get maximum compressed size for buffer allocation
 */
size_t gf_comp_bound(gf_comp_algorithm_t alg, size_t input_size);

/**
 * @brief Detect compression algorithm from header
 */
gf_comp_algorithm_t gf_comp_detect(const void *data, size_t size);

/**
 * @brief Get original size from compressed data header
 */
int gf_comp_get_orig_size(const void *data, size_t size, size_t *orig_size);

/*===========================================================================*/
/* API Functions - Streaming Compression                                      */
/*===========================================================================*/

/**
 * @brief Initialize streaming compression context
 */
int gf_comp_stream_init(gf_comp_stream_t *stream, const gf_comp_config_t *config,
                        bool compress);

/**
 * @brief Feed input data to stream
 */
int gf_comp_stream_input(gf_comp_stream_t *stream, const void *data, size_t size);

/**
 * @brief Process stream and get output
 */
int gf_comp_stream_process(gf_comp_stream_t *stream, void *output,
                           size_t output_size, size_t *produced);

/**
 * @brief Flush remaining data and finalize
 */
int gf_comp_stream_finish(gf_comp_stream_t *stream, void *output,
                          size_t output_size, size_t *produced);

/**
 * @brief Reset stream for reuse
 */
int gf_comp_stream_reset(gf_comp_stream_t *stream);

/**
 * @brief Free stream resources
 */
int gf_comp_stream_free(gf_comp_stream_t *stream);

/*===========================================================================*/
/* API Functions - Delta Compression                                          */
/*===========================================================================*/

/**
 * @brief Initialize delta compression for time-series
 */
int gf_delta_init(gf_delta_ctx_t *ctx);

/**
 * @brief Encode a timestamped value using delta compression
 */
int gf_delta_encode(gf_delta_ctx_t *ctx, uint64_t timestamp, int64_t value,
                    void *output, size_t *output_size);

/**
 * @brief Decode delta-compressed timestamp/value
 */
int gf_delta_decode(gf_delta_ctx_t *ctx, const void *input, size_t input_size,
                    uint64_t *timestamp, int64_t *value);

/**
 * @brief Reset delta context (after discontinuity)
 */
int gf_delta_reset(gf_delta_ctx_t *ctx);

/*===========================================================================*/
/* API Functions - Dictionary                                                 */
/*===========================================================================*/

/**
 * @brief Train dictionary from sample data
 */
int gf_comp_train_dict(const void *samples, size_t sample_size,
                       void *dict, size_t *dict_size);

/**
 * @brief Set pre-built dictionary
 */
int gf_comp_set_dict(const void *dict, size_t dict_size);

/*===========================================================================*/
/* API Functions - Custom Algorithm                                           */
/*===========================================================================*/

/**
 * @brief Register custom compression algorithm
 */
int gf_comp_register_custom(gf_comp_encode_cb_t encode,
                            gf_comp_decode_cb_t decode, void *ctx);

/*===========================================================================*/
/* API Functions - Statistics                                                 */
/*===========================================================================*/

/**
 * @brief Get compression statistics
 */
int gf_comp_get_stats(gf_comp_stats_t *stats);

/**
 * @brief Reset compression statistics
 */
int gf_comp_reset_stats(void);

#endif /* GF_COMPRESSION_H */
