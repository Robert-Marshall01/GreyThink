/**
 * @file ai_intrusion_detection.h
 * @brief AI-Powered Intrusion Detection System
 * 
 * INDUSTRY RELEVANCE:
 * Traditional signature-based IDS cannot detect zero-day attacks. AI/ML-based
 * intrusion detection uses behavioral analysis and anomaly detection to identify
 * novel threats. This module demonstrates edge-deployed AI security for IoT
 * devices, industrial control systems, and critical infrastructure.
 * 
 * Key applications:
 * - Industrial control system (ICS/SCADA) security
 * - IoT device fleet protection
 * - Automotive network intrusion detection
 * - Medical device security (FDA cybersecurity)
 * - Critical infrastructure protection
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_AI_INTRUSION_DETECTION_H
#define GF_AI_INTRUSION_DETECTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define IDS_MAX_FEATURES            64      /**< Max input features */
#define IDS_MAX_RULES               128     /**< Max detection rules */
#define IDS_HISTORY_DEPTH           1024    /**< Event history */
#define IDS_MODEL_SIZE_MAX          65536   /**< Max model size (bytes) */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Detection method
 */
typedef enum {
    IDS_METHOD_SIGNATURE = 0,       /**< Signature-based */
    IDS_METHOD_ANOMALY,             /**< Statistical anomaly */
    IDS_METHOD_BEHAVIORAL,          /**< Behavioral ML */
    IDS_METHOD_ENSEMBLE             /**< Combined methods */
} ids_method_t;

/**
 * @brief Threat severity
 */
typedef enum {
    IDS_SEVERITY_INFO = 0,
    IDS_SEVERITY_LOW,
    IDS_SEVERITY_MEDIUM,
    IDS_SEVERITY_HIGH,
    IDS_SEVERITY_CRITICAL
} ids_severity_t;

/**
 * @brief Attack category
 */
typedef enum {
    IDS_ATTACK_NONE = 0,
    IDS_ATTACK_SCANNING,            /**< Port/network scanning */
    IDS_ATTACK_DOS,                 /**< Denial of service */
    IDS_ATTACK_INJECTION,           /**< Command/data injection */
    IDS_ATTACK_REPLAY,              /**< Replay attack */
    IDS_ATTACK_SPOOFING,            /**< Identity spoofing */
    IDS_ATTACK_EXFILTRATION,        /**< Data exfiltration */
    IDS_ATTACK_MALWARE,             /**< Malware behavior */
    IDS_ATTACK_UNKNOWN              /**< Unknown anomaly */
} ids_attack_t;

/**
 * @brief Detection event
 */
typedef struct {
    uint32_t event_id;
    uint32_t timestamp;
    ids_method_t method;
    ids_attack_t category;
    ids_severity_t severity;
    float confidence;               /**< Detection confidence (0-1) */
    uint32_t source_ip;
    uint16_t source_port;
    uint32_t dest_ip;
    uint16_t dest_port;
    char description[128];
    uint8_t raw_data[64];
    uint16_t raw_data_len;
} ids_event_t;

/**
 * @brief Model performance metrics
 */
typedef struct {
    float true_positive_rate;
    float false_positive_rate;
    float precision;
    float f1_score;
    uint32_t events_analyzed;
    uint32_t detections;
    uint32_t model_version;
    uint32_t last_update;
} ids_metrics_t;

/**
 * @brief Configuration
 */
typedef struct {
    ids_method_t primary_method;
    float anomaly_threshold;        /**< Anomaly detection threshold */
    float confidence_threshold;     /**< Min confidence to alert */
    bool enable_learning;           /**< Online learning enabled */
    bool enable_signature;
    bool enable_ml;
    uint16_t analysis_window_ms;
} ids_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize IDS
 * @param config IDS configuration
 * @return 0 on success, negative on error
 */
int ids_init(const ids_config_t* config);

/**
 * @brief Shutdown IDS
 * @return 0 on success, negative on error
 */
int ids_shutdown(void);

/**
 * @brief Load ML model
 * @param model_data Model binary data
 * @param model_size Model size
 * @return 0 on success, negative on error
 */
int ids_load_model(const uint8_t* model_data, uint32_t model_size);

/**
 * @brief Analyze packet/data
 * @param data Data to analyze
 * @param len Data length
 * @param event Output detection event (if detected)
 * @return 1 if threat detected, 0 if clean, negative on error
 */
int ids_analyze(const uint8_t* data, uint16_t len, ids_event_t* event);

/**
 * @brief Analyze network flow
 * @param features Flow feature vector
 * @param feature_count Number of features
 * @param event Output detection event
 * @return 1 if threat detected, 0 if clean, negative on error
 */
int ids_analyze_flow(const float* features, uint8_t feature_count,
                     ids_event_t* event);

/**
 * @brief Get recent events
 * @param events Output event array
 * @param max_events Array size
 * @return Number of events returned
 */
int ids_get_events(ids_event_t* events, uint16_t max_events);

/**
 * @brief Get performance metrics
 * @param metrics Output metrics
 * @return 0 on success, negative on error
 */
int ids_get_metrics(ids_metrics_t* metrics);

/**
 * @brief Update baseline (for anomaly detection)
 * @return 0 on success, negative on error
 */
int ids_update_baseline(void);

/**
 * @brief Add signature rule
 * @param pattern Pattern to match
 * @param pattern_len Pattern length
 * @param attack_type Attack category
 * @param severity Threat severity
 * @return Rule ID on success, negative on error
 */
int ids_add_signature(const uint8_t* pattern, uint16_t pattern_len,
                      ids_attack_t attack_type, ids_severity_t severity);

#ifdef __cplusplus
}
#endif

#endif /* GF_AI_INTRUSION_DETECTION_H */
