/**
 * @file ai_tests.c
 * @brief Edge AI/ML Inference Engine Tests
 *
 * Tests for:
 * - Tensor operations
 * - Dense layer inference
 * - Feature extraction pipeline
 * - Model loading with checksum
 * - Classification simulation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

/* Forward declarations for testing */
typedef struct gf_tensor* gf_tensor_t;
typedef struct gf_inference_engine* gf_inference_t;
typedef struct { int dims[4]; } gf_tensor_shape_t;
typedef enum { GF_DTYPE_FLOAT32 = 0, GF_DTYPE_INT16, GF_DTYPE_INT8 } gf_data_type_t;
typedef enum { GF_AI_OK = 0, GF_AI_ERROR_PARAM = -1 } gf_ai_status_t;

/*===========================================================================*/
/* Test Infrastructure                                                       */
/*===========================================================================*/

static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST_ASSERT(cond, msg) do { \
    if (!(cond)) { \
        printf("  FAIL: %s\n", msg); \
        tests_failed++; \
        return -1; \
    } \
} while(0)

#define TEST_ASSERT_FLOAT_EQ(a, b, tol, msg) do { \
    if (fabsf((a) - (b)) > (tol)) { \
        printf("  FAIL: %s (expected %.6f, got %.6f)\n", msg, (b), (a)); \
        tests_failed++; \
        return -1; \
    } \
} while(0)

#define RUN_TEST(fn) do { \
    tests_run++; \
    printf("Running %s... ", #fn); \
    if (fn() == 0) { \
        printf("PASS\n"); \
        tests_passed++; \
    } \
} while(0)

/*===========================================================================*/
/* Mock/Simulation Implementations for Standalone Testing                    */
/*===========================================================================*/

/* Simulated tensor structure */
typedef struct gf_tensor_mock {
    gf_tensor_shape_t shape;
    gf_data_type_t dtype;
    float* data;
    size_t size;
} gf_tensor_mock_t;

static gf_ai_status_t mock_tensor_create(const gf_tensor_shape_t* shape,
                                          gf_data_type_t dtype,
                                          gf_tensor_mock_t** tensor)
{
    gf_tensor_mock_t* t = malloc(sizeof(gf_tensor_mock_t));
    if (!t) return GF_AI_ERROR_PARAM;
    
    t->shape = *shape;
    t->dtype = dtype;
    
    size_t elements = 1;
    for (int i = 0; i < 4; i++) {
        if (shape->dims[i] > 0) elements *= shape->dims[i];
    }
    
    t->size = elements;
    t->data = calloc(elements, sizeof(float));
    if (!t->data) {
        free(t);
        return GF_AI_ERROR_PARAM;
    }
    
    *tensor = t;
    return GF_AI_OK;
}

static void mock_tensor_destroy(gf_tensor_mock_t* t)
{
    if (t) {
        free(t->data);
        free(t);
    }
}

/* Simulated dense layer forward */
static void mock_dense_forward(const float* input, size_t in_size,
                               const float* weights, const float* bias,
                               float* output, size_t out_size)
{
    for (size_t o = 0; o < out_size; o++) {
        float sum = 0.0f;
        for (size_t i = 0; i < in_size; i++) {
            sum += weights[o * in_size + i] * input[i];
        }
        if (bias) sum += bias[o];
        output[o] = sum;
    }
}

/* Simulated ReLU activation */
static void mock_relu(float* data, size_t len)
{
    for (size_t i = 0; i < len; i++) {
        if (data[i] < 0.0f) data[i] = 0.0f;
    }
}

/* Simulated softmax activation */
static void mock_softmax(float* data, size_t len)
{
    float max_val = data[0];
    for (size_t i = 1; i < len; i++) {
        if (data[i] > max_val) max_val = data[i];
    }
    
    float sum = 0.0f;
    for (size_t i = 0; i < len; i++) {
        data[i] = expf(data[i] - max_val);
        sum += data[i];
    }
    
    for (size_t i = 0; i < len; i++) {
        data[i] /= sum;
    }
}

/* Simulated CRC32 */
static uint32_t mock_crc32(const void* data, size_t len)
{
    const uint8_t* bytes = data;
    uint32_t crc = 0xFFFFFFFF;
    
    for (size_t i = 0; i < len; i++) {
        crc ^= bytes[i];
        for (int j = 0; j < 8; j++) {
            crc = (crc >> 1) ^ (0xEDB88320 & -(crc & 1));
        }
    }
    
    return ~crc;
}

/* Simulated statistical functions */
static float mock_mean(const float* data, size_t len)
{
    float sum = 0.0f;
    for (size_t i = 0; i < len; i++) sum += data[i];
    return sum / len;
}

static float mock_variance(const float* data, size_t len)
{
    float mean = mock_mean(data, len);
    float sum_sq = 0.0f;
    for (size_t i = 0; i < len; i++) {
        float diff = data[i] - mean;
        sum_sq += diff * diff;
    }
    return sum_sq / (len - 1);
}

static float mock_rms(const float* data, size_t len)
{
    float sum_sq = 0.0f;
    for (size_t i = 0; i < len; i++) {
        sum_sq += data[i] * data[i];
    }
    return sqrtf(sum_sq / len);
}

/*===========================================================================*/
/* Unit Tests                                                                */
/*===========================================================================*/

static int test_tensor_create_destroy(void)
{
    gf_tensor_shape_t shape = {{4, 8, 0, 0}};
    gf_tensor_mock_t* tensor = NULL;
    
    gf_ai_status_t status = mock_tensor_create(&shape, GF_DTYPE_FLOAT32, &tensor);
    TEST_ASSERT(status == GF_AI_OK, "Tensor create should succeed");
    TEST_ASSERT(tensor != NULL, "Tensor pointer should be valid");
    TEST_ASSERT(tensor->size == 32, "Tensor size should be 32 elements");
    
    mock_tensor_destroy(tensor);
    return 0;
}

static int test_tensor_shape(void)
{
    gf_tensor_shape_t shape = {{3, 4, 5, 0}};
    gf_tensor_mock_t* tensor = NULL;
    
    mock_tensor_create(&shape, GF_DTYPE_FLOAT32, &tensor);
    
    TEST_ASSERT(tensor->shape.dims[0] == 3, "Dim 0 should be 3");
    TEST_ASSERT(tensor->shape.dims[1] == 4, "Dim 1 should be 4");
    TEST_ASSERT(tensor->shape.dims[2] == 5, "Dim 2 should be 5");
    TEST_ASSERT(tensor->size == 60, "Total size should be 60");
    
    mock_tensor_destroy(tensor);
    return 0;
}

static int test_tensor_set_get_element(void)
{
    gf_tensor_shape_t shape = {{10, 0, 0, 0}};
    gf_tensor_mock_t* tensor = NULL;
    
    mock_tensor_create(&shape, GF_DTYPE_FLOAT32, &tensor);
    
    /* Set some values */
    tensor->data[0] = 1.5f;
    tensor->data[5] = -2.5f;
    tensor->data[9] = 3.14159f;
    
    TEST_ASSERT_FLOAT_EQ(tensor->data[0], 1.5f, 0.001f, "Element 0");
    TEST_ASSERT_FLOAT_EQ(tensor->data[5], -2.5f, 0.001f, "Element 5");
    TEST_ASSERT_FLOAT_EQ(tensor->data[9], 3.14159f, 0.001f, "Element 9");
    
    mock_tensor_destroy(tensor);
    return 0;
}

static int test_dense_layer_basic(void)
{
    /* 3-input, 2-output dense layer */
    float input[3] = {1.0f, 2.0f, 3.0f};
    float weights[6] = {
        0.1f, 0.2f, 0.3f,  /* Weights for output 0 */
        0.4f, 0.5f, 0.6f   /* Weights for output 1 */
    };
    float bias[2] = {0.1f, 0.2f};
    float output[2];
    
    mock_dense_forward(input, 3, weights, bias, output, 2);
    
    /* Expected: out[0] = 0.1*1 + 0.2*2 + 0.3*3 + 0.1 = 1.5 */
    /* Expected: out[1] = 0.4*1 + 0.5*2 + 0.6*3 + 0.2 = 3.4 */
    TEST_ASSERT_FLOAT_EQ(output[0], 1.5f, 0.001f, "Dense output 0");
    TEST_ASSERT_FLOAT_EQ(output[1], 3.4f, 0.001f, "Dense output 1");
    
    return 0;
}

static int test_dense_layer_no_bias(void)
{
    float input[2] = {1.0f, 2.0f};
    float weights[4] = {
        1.0f, 0.5f,
        0.5f, 1.0f
    };
    float output[2];
    
    mock_dense_forward(input, 2, weights, NULL, output, 2);
    
    /* Expected: out[0] = 1*1 + 0.5*2 = 2.0 */
    /* Expected: out[1] = 0.5*1 + 1*2 = 2.5 */
    TEST_ASSERT_FLOAT_EQ(output[0], 2.0f, 0.001f, "Dense no-bias output 0");
    TEST_ASSERT_FLOAT_EQ(output[1], 2.5f, 0.001f, "Dense no-bias output 1");
    
    return 0;
}

static int test_relu_activation(void)
{
    float data[5] = {-2.0f, -1.0f, 0.0f, 1.0f, 2.0f};
    
    mock_relu(data, 5);
    
    TEST_ASSERT_FLOAT_EQ(data[0], 0.0f, 0.001f, "ReLU of -2");
    TEST_ASSERT_FLOAT_EQ(data[1], 0.0f, 0.001f, "ReLU of -1");
    TEST_ASSERT_FLOAT_EQ(data[2], 0.0f, 0.001f, "ReLU of 0");
    TEST_ASSERT_FLOAT_EQ(data[3], 1.0f, 0.001f, "ReLU of 1");
    TEST_ASSERT_FLOAT_EQ(data[4], 2.0f, 0.001f, "ReLU of 2");
    
    return 0;
}

static int test_softmax_activation(void)
{
    float data[3] = {1.0f, 2.0f, 3.0f};
    
    mock_softmax(data, 3);
    
    /* Verify sum to 1 */
    float sum = data[0] + data[1] + data[2];
    TEST_ASSERT_FLOAT_EQ(sum, 1.0f, 0.001f, "Softmax sum to 1");
    
    /* Verify ordering preserved */
    TEST_ASSERT(data[0] < data[1], "Softmax ordering 0 < 1");
    TEST_ASSERT(data[1] < data[2], "Softmax ordering 1 < 2");
    
    /* Verify approximate values */
    /* softmax([1,2,3]) ≈ [0.0900, 0.2447, 0.6652] */
    TEST_ASSERT_FLOAT_EQ(data[0], 0.0900f, 0.01f, "Softmax[0]");
    TEST_ASSERT_FLOAT_EQ(data[1], 0.2447f, 0.01f, "Softmax[1]");
    TEST_ASSERT_FLOAT_EQ(data[2], 0.6652f, 0.01f, "Softmax[2]");
    
    return 0;
}

static int test_crc32_checksum(void)
{
    const char* test_data = "Hello, Grey Firmware!";
    uint32_t crc = mock_crc32(test_data, strlen(test_data));
    
    /* Verify CRC is non-zero and consistent */
    TEST_ASSERT(crc != 0, "CRC32 should be non-zero");
    
    /* Verify same input produces same CRC */
    uint32_t crc2 = mock_crc32(test_data, strlen(test_data));
    TEST_ASSERT(crc == crc2, "CRC32 should be deterministic");
    
    /* Verify different input produces different CRC */
    uint32_t crc3 = mock_crc32("Different data", 14);
    TEST_ASSERT(crc != crc3, "Different data should have different CRC");
    
    return 0;
}

static int test_feature_mean(void)
{
    float data[5] = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f};
    
    float mean = mock_mean(data, 5);
    
    TEST_ASSERT_FLOAT_EQ(mean, 3.0f, 0.001f, "Mean of [1,2,3,4,5]");
    
    return 0;
}

static int test_feature_variance(void)
{
    float data[5] = {2.0f, 4.0f, 4.0f, 4.0f, 6.0f};
    
    float var = mock_variance(data, 5);
    
    /* Variance = 2.0 (population variance n) or 2.5 (sample variance n-1) */
    /* Our mock uses population variance */
    TEST_ASSERT_FLOAT_EQ(var, 2.0f, 0.001f, "Variance");
    
    return 0;
}

static int test_feature_rms(void)
{
    float data[4] = {1.0f, 2.0f, 3.0f, 4.0f};
    
    float rms = mock_rms(data, 4);
    
    /* RMS = sqrt((1+4+9+16)/4) = sqrt(7.5) ≈ 2.7386 */
    TEST_ASSERT_FLOAT_EQ(rms, 2.7386f, 0.01f, "RMS");
    
    return 0;
}

/*===========================================================================*/
/* Integration Tests                                                         */
/*===========================================================================*/

static int test_simple_classifier(void)
{
    /*
     * Simple 2-class classifier simulation:
     * Input: 4 features
     * Hidden: 8 neurons (ReLU)
     * Output: 2 classes (softmax)
     */
    
    /* Random-ish weights for demonstration */
    float input[4] = {0.5f, 0.8f, 0.2f, 0.9f};
    
    /* Layer 1: 4 -> 8 */
    float w1[32] = {
        0.1f, 0.2f, 0.3f, 0.4f,
        -0.1f, 0.2f, -0.3f, 0.4f,
        0.2f, -0.2f, 0.2f, -0.2f,
        0.3f, 0.3f, 0.3f, 0.3f,
        -0.2f, 0.1f, 0.4f, -0.3f,
        0.4f, -0.1f, 0.1f, 0.2f,
        0.1f, 0.1f, 0.1f, 0.1f,
        -0.3f, -0.2f, 0.5f, 0.1f
    };
    float b1[8] = {0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f};
    
    /* Layer 2: 8 -> 2 */
    float w2[16] = {
        0.2f, -0.1f, 0.3f, 0.2f, -0.2f, 0.1f, 0.3f, -0.1f,
        -0.2f, 0.1f, -0.3f, -0.2f, 0.2f, -0.1f, -0.3f, 0.1f
    };
    float b2[2] = {0.0f, 0.0f};
    
    float hidden[8];
    float output[2];
    
    /* Forward pass */
    mock_dense_forward(input, 4, w1, b1, hidden, 8);
    mock_relu(hidden, 8);
    mock_dense_forward(hidden, 8, w2, b2, output, 2);
    mock_softmax(output, 2);
    
    /* Verify valid probability distribution */
    float sum = output[0] + output[1];
    TEST_ASSERT_FLOAT_EQ(sum, 1.0f, 0.001f, "Classifier outputs sum to 1");
    TEST_ASSERT(output[0] >= 0.0f && output[0] <= 1.0f, "Class 0 probability valid");
    TEST_ASSERT(output[1] >= 0.0f && output[1] <= 1.0f, "Class 1 probability valid");
    
    return 0;
}

static int test_feature_extraction_pipeline(void)
{
    /* Simulate feature extraction: normalize then stats */
    float raw_signal[100];
    
    /* Generate test signal */
    for (int i = 0; i < 100; i++) {
        raw_signal[i] = sinf(i * 0.1f) * 50.0f + 100.0f;  /* 50-150 range */
    }
    
    /* Stage 1: Normalize to [0,1] */
    float normalized[100];
    float min_val = raw_signal[0], max_val = raw_signal[0];
    for (int i = 1; i < 100; i++) {
        if (raw_signal[i] < min_val) min_val = raw_signal[i];
        if (raw_signal[i] > max_val) max_val = raw_signal[i];
    }
    float range = max_val - min_val;
    for (int i = 0; i < 100; i++) {
        normalized[i] = (raw_signal[i] - min_val) / range;
    }
    
    /* Verify normalization */
    float norm_min = normalized[0], norm_max = normalized[0];
    for (int i = 1; i < 100; i++) {
        if (normalized[i] < norm_min) norm_min = normalized[i];
        if (normalized[i] > norm_max) norm_max = normalized[i];
    }
    TEST_ASSERT_FLOAT_EQ(norm_min, 0.0f, 0.001f, "Normalized min");
    TEST_ASSERT_FLOAT_EQ(norm_max, 1.0f, 0.001f, "Normalized max");
    
    /* Stage 2: Extract statistics */
    float mean = mock_mean(normalized, 100);
    float var = mock_variance(normalized, 100);
    float rms_val = mock_rms(normalized, 100);
    
    /* Verify stats are reasonable */
    TEST_ASSERT(mean > 0.4f && mean < 0.6f, "Mean should be near 0.5");
    TEST_ASSERT(var > 0.0f && var < 0.5f, "Variance should be reasonable");
    TEST_ASSERT(rms_val > 0.0f && rms_val < 1.0f, "RMS should be in range");
    
    return 0;
}

static int test_model_checksum_verification(void)
{
    /* Simulate model data */
    uint8_t model_data[256];
    for (int i = 0; i < 256; i++) {
        model_data[i] = (uint8_t)(i * 7 + 13);  /* Pseudo-random */
    }
    
    /* Calculate checksum */
    uint32_t checksum = mock_crc32(model_data, 256);
    
    /* Verify checksum matches */
    uint32_t verify = mock_crc32(model_data, 256);
    TEST_ASSERT(checksum == verify, "Checksum verification pass");
    
    /* Corrupt one byte */
    model_data[100] ^= 0xFF;
    uint32_t corrupted = mock_crc32(model_data, 256);
    TEST_ASSERT(checksum != corrupted, "Corrupted data detected");
    
    return 0;
}

static int test_argmax_classification(void)
{
    /* Simulate classification output and find predicted class */
    float probabilities[5] = {0.05f, 0.10f, 0.60f, 0.15f, 0.10f};
    
    int predicted_class = 0;
    float max_prob = probabilities[0];
    
    for (int i = 1; i < 5; i++) {
        if (probabilities[i] > max_prob) {
            max_prob = probabilities[i];
            predicted_class = i;
        }
    }
    
    TEST_ASSERT(predicted_class == 2, "Argmax should find class 2");
    TEST_ASSERT_FLOAT_EQ(max_prob, 0.60f, 0.001f, "Max probability");
    
    return 0;
}

static int test_batch_inference(void)
{
    /* Simulate batch of 4 inputs through simple network */
    float inputs[4][3] = {
        {1.0f, 0.0f, 0.0f},
        {0.0f, 1.0f, 0.0f},
        {0.0f, 0.0f, 1.0f},
        {0.5f, 0.5f, 0.5f}
    };
    
    float weights[6] = {
        1.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 1.0f
    };
    
    float outputs[4][2];
    
    for (int b = 0; b < 4; b++) {
        mock_dense_forward(inputs[b], 3, weights, NULL, outputs[b], 2);
    }
    
    /* Verify each batch output */
    TEST_ASSERT_FLOAT_EQ(outputs[0][0], 1.0f, 0.001f, "Batch 0, out 0");
    TEST_ASSERT_FLOAT_EQ(outputs[0][1], 0.0f, 0.001f, "Batch 0, out 1");
    TEST_ASSERT_FLOAT_EQ(outputs[1][0], 0.0f, 0.001f, "Batch 1, out 0");
    TEST_ASSERT_FLOAT_EQ(outputs[1][1], 1.0f, 0.001f, "Batch 1, out 1");
    TEST_ASSERT_FLOAT_EQ(outputs[2][0], 0.0f, 0.001f, "Batch 2, out 0");
    TEST_ASSERT_FLOAT_EQ(outputs[2][1], 1.0f, 0.001f, "Batch 2, out 1");
    TEST_ASSERT_FLOAT_EQ(outputs[3][0], 0.5f, 0.001f, "Batch 3, out 0");
    TEST_ASSERT_FLOAT_EQ(outputs[3][1], 1.0f, 0.001f, "Batch 3, out 1");
    
    return 0;
}

/*===========================================================================*/
/* Quantization Tests                                                        */
/*===========================================================================*/

static int test_int8_quantization(void)
{
    /* Simulate INT8 quantization */
    float scale = 0.02f;  /* Maps [-2.55, 2.55] to [-127, 127] */
    
    float float_vals[5] = {-1.0f, -0.5f, 0.0f, 0.5f, 1.0f};
    int8_t quant_vals[5];
    float dequant_vals[5];
    
    /* Quantize */
    for (int i = 0; i < 5; i++) {
        int32_t q = (int32_t)roundf(float_vals[i] / scale);
        if (q > 127) q = 127;
        if (q < -127) q = -127;
        quant_vals[i] = (int8_t)q;
    }
    
    /* Dequantize */
    for (int i = 0; i < 5; i++) {
        dequant_vals[i] = quant_vals[i] * scale;
    }
    
    /* Verify quantization error is small */
    for (int i = 0; i < 5; i++) {
        float error = fabsf(dequant_vals[i] - float_vals[i]);
        TEST_ASSERT(error < scale, "Quantization error within one step");
    }
    
    return 0;
}

/*===========================================================================*/
/* Stress Tests                                                              */
/*===========================================================================*/

static int test_large_tensor(void)
{
    /* Test with larger tensor (simulated) */
    gf_tensor_shape_t shape = {{64, 64, 3, 0}};  /* 12288 elements */
    gf_tensor_mock_t* tensor = NULL;
    
    gf_ai_status_t status = mock_tensor_create(&shape, GF_DTYPE_FLOAT32, &tensor);
    TEST_ASSERT(status == GF_AI_OK, "Large tensor creation");
    TEST_ASSERT(tensor->size == 12288, "Large tensor size");
    
    /* Fill with data */
    for (size_t i = 0; i < tensor->size; i++) {
        tensor->data[i] = (float)i / 12288.0f;
    }
    
    /* Verify */
    TEST_ASSERT_FLOAT_EQ(tensor->data[0], 0.0f, 0.001f, "First element");
    TEST_ASSERT_FLOAT_EQ(tensor->data[12287], 0.9999f, 0.001f, "Last element");
    
    mock_tensor_destroy(tensor);
    return 0;
}

static int test_deep_network_simulation(void)
{
    /* Simulate 4-layer network */
    float x[16];
    float temp1[32], temp2[16], temp3[8], output[4];
    
    /* Initialize input */
    for (int i = 0; i < 16; i++) {
        x[i] = sinf(i * 0.5f);
    }
    
    /* Layer 1: 16 -> 32 (expansion) */
    float w1[512];  /* 16*32 */
    for (int i = 0; i < 512; i++) w1[i] = 0.01f * (i % 10 - 5);
    mock_dense_forward(x, 16, w1, NULL, temp1, 32);
    mock_relu(temp1, 32);
    
    /* Layer 2: 32 -> 16 */
    float w2[512];
    for (int i = 0; i < 512; i++) w2[i] = 0.01f * ((i+3) % 10 - 5);
    mock_dense_forward(temp1, 32, w2, NULL, temp2, 16);
    mock_relu(temp2, 16);
    
    /* Layer 3: 16 -> 8 */
    float w3[128];
    for (int i = 0; i < 128; i++) w3[i] = 0.01f * ((i+7) % 10 - 5);
    mock_dense_forward(temp2, 16, w3, NULL, temp3, 8);
    mock_relu(temp3, 8);
    
    /* Layer 4: 8 -> 4 (output) */
    float w4[32];
    for (int i = 0; i < 32; i++) w4[i] = 0.01f * ((i+11) % 10 - 5);
    mock_dense_forward(temp3, 8, w4, NULL, output, 4);
    mock_softmax(output, 4);
    
    /* Verify sum = 1 */
    float sum = output[0] + output[1] + output[2] + output[3];
    TEST_ASSERT_FLOAT_EQ(sum, 1.0f, 0.01f, "Deep network output sums to 1");
    
    return 0;
}

/*===========================================================================*/
/* Test Main                                                                 */
/*===========================================================================*/

int main(void)
{
    printf("\n=== Grey Firmware Edge AI Test Suite ===\n\n");
    
    printf("--- Tensor Unit Tests ---\n");
    RUN_TEST(test_tensor_create_destroy);
    RUN_TEST(test_tensor_shape);
    RUN_TEST(test_tensor_set_get_element);
    
    printf("\n--- Layer Unit Tests ---\n");
    RUN_TEST(test_dense_layer_basic);
    RUN_TEST(test_dense_layer_no_bias);
    RUN_TEST(test_relu_activation);
    RUN_TEST(test_softmax_activation);
    
    printf("\n--- Utility Tests ---\n");
    RUN_TEST(test_crc32_checksum);
    RUN_TEST(test_feature_mean);
    RUN_TEST(test_feature_variance);
    RUN_TEST(test_feature_rms);
    
    printf("\n--- Integration Tests ---\n");
    RUN_TEST(test_simple_classifier);
    RUN_TEST(test_feature_extraction_pipeline);
    RUN_TEST(test_model_checksum_verification);
    RUN_TEST(test_argmax_classification);
    RUN_TEST(test_batch_inference);
    
    printf("\n--- Quantization Tests ---\n");
    RUN_TEST(test_int8_quantization);
    
    printf("\n--- Stress Tests ---\n");
    RUN_TEST(test_large_tensor);
    RUN_TEST(test_deep_network_simulation);
    
    printf("\n=== AI Test Results ===\n");
    printf("Total:  %d\n", tests_run);
    printf("Passed: %d\n", tests_passed);
    printf("Failed: %d\n", tests_failed);
    printf("========================\n\n");
    
    return tests_failed > 0 ? 1 : 0;
}
