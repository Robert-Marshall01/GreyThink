/**
 * @file inference.h
 * @brief Edge AI Inference Engine (Spotlight Subsystem)
 *
 * INDUSTRY RELEVANCE:
 * Edge AI/ML enables intelligent devices without cloud dependency:
 * - On-device inference for privacy and low latency
 * - TinyML for microcontrollers (ARM Cortex-M, RISC-V)
 * - Quantized models (INT8/INT4) for efficiency
 * - Sensor preprocessing and feature extraction
 * - Real-time classification and anomaly detection
 *
 * Used in: Predictive maintenance, voice control, gesture recognition,
 *          anomaly detection, quality inspection, smart sensors
 *
 * This is a SPOTLIGHT SUBSYSTEM with full implementation.
 */

#ifndef GF_INFERENCE_H
#define GF_INFERENCE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Inference Engine Configuration                                            */
/*===========================================================================*/

/**
 * @brief Maximum supported layers in a model
 */
#define GF_AI_MAX_LAYERS        32

/**
 * @brief Maximum input/output tensor dimensions
 */
#define GF_AI_MAX_DIMS          4

/**
 * @brief Maximum model size (bytes)
 */
#define GF_AI_MAX_MODEL_SIZE    (128 * 1024)

/**
 * @brief Maximum tensor arena size (bytes)
 */
#define GF_AI_MAX_ARENA_SIZE    (64 * 1024)

/*===========================================================================*/
/* Data Type Definitions                                                     */
/*===========================================================================*/

/**
 * @brief Supported tensor data types
 */
typedef enum {
    GF_DTYPE_FLOAT32,           /**< 32-bit float */
    GF_DTYPE_INT32,             /**< 32-bit integer */
    GF_DTYPE_INT16,             /**< 16-bit integer */
    GF_DTYPE_INT8,              /**< 8-bit quantized */
    GF_DTYPE_UINT8,             /**< 8-bit unsigned quantized */
    GF_DTYPE_INT4,              /**< 4-bit quantized (packed) */
    GF_DTYPE_BOOL               /**< Boolean */
} gf_dtype_t;

/**
 * @brief Layer types supported by the engine
 */
typedef enum {
    GF_LAYER_INPUT,             /**< Input layer */
    GF_LAYER_DENSE,             /**< Fully connected */
    GF_LAYER_CONV2D,            /**< 2D convolution */
    GF_LAYER_DEPTHWISE_CONV2D,  /**< Depthwise separable conv */
    GF_LAYER_MAXPOOL2D,         /**< Max pooling */
    GF_LAYER_AVGPOOL2D,         /**< Average pooling */
    GF_LAYER_FLATTEN,           /**< Flatten to 1D */
    GF_LAYER_RESHAPE,           /**< Reshape tensor */
    GF_LAYER_RELU,              /**< ReLU activation */
    GF_LAYER_RELU6,             /**< ReLU6 activation */
    GF_LAYER_SIGMOID,           /**< Sigmoid activation */
    GF_LAYER_TANH,              /**< Tanh activation */
    GF_LAYER_SOFTMAX,           /**< Softmax activation */
    GF_LAYER_BATCH_NORM,        /**< Batch normalization */
    GF_LAYER_DROPOUT,           /**< Dropout (inference: passthrough) */
    GF_LAYER_ADD,               /**< Element-wise add */
    GF_LAYER_CONCAT,            /**< Concatenation */
    GF_LAYER_OUTPUT             /**< Output layer */
} gf_layer_type_t;

/**
 * @brief Model format/origin
 */
typedef enum {
    GF_MODEL_NATIVE,            /**< Grey Firmware native format */
    GF_MODEL_TFLITE,            /**< TensorFlow Lite (subset) */
    GF_MODEL_ONNX,              /**< ONNX (subset) */
    GF_MODEL_CUSTOM             /**< Custom format */
} gf_model_format_t;

/**
 * @brief Inference status codes
 */
typedef enum {
    GF_AI_OK = 0,
    GF_AI_ERROR_INVALID_MODEL = -1,
    GF_AI_ERROR_CHECKSUM = -2,
    GF_AI_ERROR_VERSION = -3,
    GF_AI_ERROR_OOM = -4,           /**< Out of memory */
    GF_AI_ERROR_SHAPE = -5,         /**< Shape mismatch */
    GF_AI_ERROR_UNSUPPORTED = -6,   /**< Unsupported operation */
    GF_AI_ERROR_INVOKE = -7,        /**< Inference failed */
    GF_AI_ERROR_NOT_READY = -8
} gf_ai_status_t;

/*===========================================================================*/
/* Tensor Structures                                                         */
/*===========================================================================*/

/**
 * @brief Tensor shape
 */
typedef struct {
    uint32_t dims[GF_AI_MAX_DIMS];  /**< Dimension sizes */
    uint8_t num_dims;               /**< Number of dimensions */
} gf_tensor_shape_t;

/**
 * @brief Quantization parameters
 */
typedef struct {
    float scale;                /**< Quantization scale */
    int32_t zero_point;         /**< Quantization zero point */
} gf_quant_params_t;

/**
 * @brief Tensor descriptor
 */
typedef struct {
    const char* name;           /**< Tensor name (optional) */
    gf_tensor_shape_t shape;
    gf_dtype_t dtype;
    gf_quant_params_t quant;
    void* data;                 /**< Pointer to tensor data */
    size_t bytes;               /**< Total bytes */
    bool is_variable;           /**< Can change between invocations */
} gf_tensor_t;

/*===========================================================================*/
/* Model Structures                                                          */
/*===========================================================================*/

/**
 * @brief Layer parameters (union for all layer types)
 */
typedef struct {
    gf_layer_type_t type;
    
    /* Common */
    uint8_t input_indices[4];   /**< Input tensor indices */
    uint8_t num_inputs;
    uint8_t output_index;       /**< Output tensor index */
    
    /* Conv2D / DepthwiseConv2D */
    struct {
        uint8_t kernel_h;
        uint8_t kernel_w;
        uint8_t stride_h;
        uint8_t stride_w;
        uint8_t padding;        /**< 0=valid, 1=same */
        uint8_t dilation_h;
        uint8_t dilation_w;
    } conv;
    
    /* Pooling */
    struct {
        uint8_t pool_h;
        uint8_t pool_w;
        uint8_t stride_h;
        uint8_t stride_w;
    } pool;
    
    /* Dense */
    struct {
        uint16_t input_size;
        uint16_t output_size;
        bool has_bias;
    } dense;
    
    /* Reshape */
    gf_tensor_shape_t target_shape;
    
    /* Weight tensor indices */
    int16_t weights_index;      /**< -1 if none */
    int16_t bias_index;         /**< -1 if none */
} gf_layer_t;

/**
 * @brief Model header (stored at start of model file)
 */
typedef struct {
    uint32_t magic;             /**< 'GFAI' = 0x47464149 */
    uint16_t version_major;
    uint16_t version_minor;
    gf_model_format_t format;
    uint32_t model_size;        /**< Total model size */
    uint32_t checksum;          /**< CRC32 of model data */
    
    /* Model structure */
    uint8_t num_inputs;
    uint8_t num_outputs;
    uint8_t num_layers;
    uint8_t num_tensors;
    
    /* Input/output info */
    gf_tensor_shape_t input_shape;
    gf_tensor_shape_t output_shape;
    gf_dtype_t input_dtype;
    gf_dtype_t output_dtype;
    
    /* Quantization info */
    bool is_quantized;
    gf_quant_params_t input_quant;
    gf_quant_params_t output_quant;
    
    /* Metadata */
    char model_name[32];
    uint32_t num_parameters;    /**< Total trainable parameters */
    uint32_t required_arena;    /**< Required tensor arena size */
} gf_model_header_t;

/**
 * @brief Runtime model representation
 */
typedef struct {
    gf_model_header_t header;
    const gf_layer_t* layers;
    gf_tensor_t* tensors;
    void* tensor_arena;         /**< Working memory */
    size_t arena_size;
    bool is_loaded;
} gf_model_t;

/*===========================================================================*/
/* Inference Configuration                                                   */
/*===========================================================================*/

/**
 * @brief Inference engine configuration
 */
typedef struct {
    void* tensor_arena;         /**< Pre-allocated arena (optional) */
    size_t arena_size;          /**< Arena size (bytes) */
    bool enable_profiling;      /**< Enable layer timing */
    uint32_t invoke_timeout_ms; /**< Timeout for inference */
} gf_inference_config_t;

/**
 * @brief Default inference configuration
 */
#define GF_INFERENCE_CONFIG_DEFAULT { \
    .tensor_arena = NULL,             \
    .arena_size = GF_AI_MAX_ARENA_SIZE, \
    .enable_profiling = false,        \
    .invoke_timeout_ms = 1000         \
}

/**
 * @brief Inference result
 */
typedef struct {
    gf_tensor_t output;         /**< Output tensor */
    int32_t top_class;          /**< Top classification (if classifier) */
    float top_confidence;       /**< Confidence of top class */
    float* class_scores;        /**< All class scores (if softmax output) */
    uint8_t num_classes;
    uint32_t inference_time_us; /**< Inference duration */
} gf_inference_result_t;

/**
 * @brief Profiling data for a single layer
 */
typedef struct {
    gf_layer_type_t type;
    uint32_t execution_time_us;
    uint32_t memory_used;
} gf_layer_profile_t;

/**
 * @brief Complete model profile
 */
typedef struct {
    gf_layer_profile_t layers[GF_AI_MAX_LAYERS];
    uint8_t num_layers;
    uint32_t total_time_us;
    uint32_t peak_memory;
    uint32_t invocations;
} gf_model_profile_t;

/**
 * @brief Inference engine handle
 */
typedef struct gf_inference_engine* gf_inference_t;

/*===========================================================================*/
/* Inference Engine API                                                      */
/*===========================================================================*/

/**
 * @brief Initialize inference engine
 * @param config Configuration
 * @param engine Output handle
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_init(const gf_inference_config_t* config,
                                  gf_inference_t* engine);

/**
 * @brief Load model from memory
 * @param engine Inference engine
 * @param model_data Pointer to model data
 * @param model_size Size of model data
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_load_model(gf_inference_t engine,
                                        const void* model_data,
                                        size_t model_size);

/**
 * @brief Get model information
 * @param engine Inference engine
 * @param header Output header info
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_get_model_info(gf_inference_t engine,
                                            gf_model_header_t* header);

/**
 * @brief Set input tensor data
 * @param engine Inference engine
 * @param input_index Input tensor index (usually 0)
 * @param data Input data
 * @param size Data size in bytes
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_set_input(gf_inference_t engine,
                                       uint8_t input_index,
                                       const void* data,
                                       size_t size);

/**
 * @brief Run inference
 * @param engine Inference engine
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_invoke(gf_inference_t engine);

/**
 * @brief Get inference result
 * @param engine Inference engine
 * @param result Output result
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_get_result(gf_inference_t engine,
                                        gf_inference_result_t* result);

/**
 * @brief Get output tensor directly
 * @param engine Inference engine
 * @param output_index Output tensor index
 * @param tensor Output tensor descriptor
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_get_output(gf_inference_t engine,
                                        uint8_t output_index,
                                        gf_tensor_t* tensor);

/**
 * @brief Get profiling data
 * @param engine Inference engine
 * @param profile Output profile
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_get_profile(gf_inference_t engine,
                                         gf_model_profile_t* profile);

/**
 * @brief Unload current model
 * @param engine Inference engine
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_unload_model(gf_inference_t engine);

/**
 * @brief Deinitialize inference engine
 * @param engine Inference engine
 */
void gf_inference_deinit(gf_inference_t engine);

/*===========================================================================*/
/* Convenience Functions                                                     */
/*===========================================================================*/

/**
 * @brief Run inference with float input/output
 * @param engine Inference engine
 * @param input Float input array
 * @param input_size Input size (floats)
 * @param output Float output array
 * @param output_size Output buffer size (floats)
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_run_float(gf_inference_t engine,
                                       const float* input,
                                       size_t input_size,
                                       float* output,
                                       size_t output_size);

/**
 * @brief Get top-K classification results
 * @param engine Inference engine
 * @param k Number of top results
 * @param classes Output class indices
 * @param scores Output confidence scores
 * @return GF_AI_OK on success
 */
gf_ai_status_t gf_inference_get_top_k(gf_inference_t engine,
                                       uint8_t k,
                                       int32_t* classes,
                                       float* scores);

/**
 * @brief Check if model is loaded
 * @param engine Inference engine
 * @return true if model is loaded
 */
bool gf_inference_is_ready(gf_inference_t engine);

#ifdef __cplusplus
}
#endif

#endif /* GF_INFERENCE_H */
