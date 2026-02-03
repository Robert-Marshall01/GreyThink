/**
 * @file intrusion_detection.h
 * @brief Edge Intrusion Detection System (IDS) Interface
 *
 * INDUSTRY RELEVANCE:
 * Edge security is critical as IoT devices become attack vectors.
 * Intrusion detection at the edge provides real-time threat response.
 * This module demonstrates:
 * - Anomaly detection using ML on resource-constrained devices
 * - Network traffic analysis (deep packet inspection)
 * - Behavioral analysis for compromised device detection
 * - Integration with SIEM (Security Information and Event Management)
 *
 * These skills apply to IoT security companies (Armis, Claroty, Nozomi),
 * network security vendors (Palo Alto, CrowdStrike), and critical
 * infrastructure protection for industrial control systems.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires validated threat signatures.
 */

#ifndef GF_INTRUSION_DETECTION_H
#define GF_INTRUSION_DETECTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_IDS_MAX_RULES            256     /**< Maximum detection rules */
#define GF_IDS_MAX_ALERTS           64      /**< Maximum queued alerts */
#define GF_IDS_MAX_SIGNATURE_LEN    512     /**< Maximum signature length */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Detection method
 */
typedef enum {
    GF_IDS_SIGNATURE,           /**< Signature-based detection */
    GF_IDS_ANOMALY,             /**< Anomaly-based detection */
    GF_IDS_BEHAVIORAL,          /**< Behavioral analysis */
    GF_IDS_HEURISTIC            /**< Heuristic detection */
} gf_ids_method_t;

/**
 * @brief Alert severity
 */
typedef enum {
    GF_IDS_SEVERITY_LOW,        /**< Low priority */
    GF_IDS_SEVERITY_MEDIUM,     /**< Medium priority */
    GF_IDS_SEVERITY_HIGH,       /**< High priority */
    GF_IDS_SEVERITY_CRITICAL    /**< Critical - immediate action */
} gf_ids_severity_t;

/**
 * @brief Attack category (MITRE ATT&CK aligned)
 */
typedef enum {
    GF_IDS_RECON,               /**< Reconnaissance */
    GF_IDS_INITIAL_ACCESS,      /**< Initial access attempt */
    GF_IDS_EXECUTION,           /**< Code execution */
    GF_IDS_PERSISTENCE,         /**< Persistence mechanism */
    GF_IDS_PRIVILEGE_ESCALATION,/**< Privilege escalation */
    GF_IDS_DEFENSE_EVASION,     /**< Defense evasion */
    GF_IDS_CREDENTIAL_ACCESS,   /**< Credential access */
    GF_IDS_LATERAL_MOVEMENT,    /**< Lateral movement */
    GF_IDS_COLLECTION,          /**< Data collection */
    GF_IDS_EXFILTRATION,        /**< Data exfiltration */
    GF_IDS_IMPACT               /**< Impact/disruption */
} gf_ids_category_t;

/**
 * @brief Response action
 */
typedef enum {
    GF_IDS_ACTION_LOG,          /**< Log only */
    GF_IDS_ACTION_ALERT,        /**< Generate alert */
    GF_IDS_ACTION_BLOCK,        /**< Block traffic */
    GF_IDS_ACTION_QUARANTINE,   /**< Quarantine device */
    GF_IDS_ACTION_DISCONNECT    /**< Disconnect from network */
} gf_ids_action_t;

/**
 * @brief Detection rule
 */
typedef struct {
    uint32_t rule_id;                   /**< Rule identifier */
    char name[64];                      /**< Rule name */
    gf_ids_method_t method;             /**< Detection method */
    gf_ids_category_t category;         /**< Attack category */
    gf_ids_severity_t severity;         /**< Alert severity */
    gf_ids_action_t action;             /**< Response action */
    uint8_t signature[GF_IDS_MAX_SIGNATURE_LEN]; /**< Detection signature */
    uint16_t signature_len;             /**< Signature length */
    bool enabled;                       /**< Rule enabled */
} gf_ids_rule_t;

/**
 * @brief Network packet metadata
 */
typedef struct {
    uint8_t src_ip[16];                 /**< Source IP (v4 or v6) */
    uint8_t dst_ip[16];                 /**< Destination IP */
    uint16_t src_port;                  /**< Source port */
    uint16_t dst_port;                  /**< Destination port */
    uint8_t protocol;                   /**< IP protocol */
    uint16_t payload_len;               /**< Payload length */
    uint32_t flags;                     /**< TCP flags, etc. */
    bool is_ipv6;                       /**< IPv6 flag */
} gf_ids_packet_t;

/**
 * @brief Security alert
 */
typedef struct {
    uint32_t alert_id;                  /**< Alert identifier */
    uint32_t rule_id;                   /**< Triggered rule ID */
    gf_ids_severity_t severity;         /**< Alert severity */
    gf_ids_category_t category;         /**< Attack category */
    char description[128];              /**< Alert description */
    gf_ids_packet_t packet;             /**< Triggering packet (if network) */
    uint64_t timestamp_ms;              /**< Detection timestamp */
    bool acknowledged;                  /**< Alert acknowledged */
    gf_ids_action_t action_taken;       /**< Action taken */
} gf_ids_alert_t;

/**
 * @brief Behavioral baseline
 */
typedef struct {
    float avg_cpu_usage;                /**< Average CPU usage */
    float avg_memory_usage;             /**< Average memory usage */
    float avg_network_rx_kbps;          /**< Average network receive */
    float avg_network_tx_kbps;          /**< Average network transmit */
    uint32_t avg_syscalls_per_sec;      /**< Average syscall rate */
    float std_dev_factor;               /**< Standard deviation threshold */
} gf_ids_baseline_t;

/**
 * @brief IDS statistics
 */
typedef struct {
    uint64_t packets_inspected;         /**< Total packets inspected */
    uint64_t alerts_generated;          /**< Total alerts generated */
    uint64_t attacks_blocked;           /**< Attacks blocked */
    uint32_t rules_triggered;           /**< Rules triggered today */
    uint32_t false_positives;           /**< Marked false positives */
    float avg_inspection_us;            /**< Average inspection time */
    uint64_t bytes_inspected;           /**< Total bytes inspected */
} gf_ids_stats_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * Callbacks
 * ───────────────────────────────────────────────────────────────────────────── */

typedef void (*gf_ids_alert_cb_t)(const gf_ids_alert_t* alert, void* user_data);

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize intrusion detection system
 * @return 0 on success, negative error code on failure
 */
int gf_ids_init(void);

/**
 * @brief Shutdown IDS
 */
void gf_ids_deinit(void);

/**
 * @brief Load detection rules
 * @param rules Rule array
 * @param count Number of rules
 * @return 0 on success
 */
int gf_ids_load_rules(const gf_ids_rule_t* rules, uint16_t count);

/**
 * @brief Add single rule
 * @param rule Rule to add
 * @return 0 on success
 */
int gf_ids_add_rule(const gf_ids_rule_t* rule);

/**
 * @brief Enable/disable rule
 * @param rule_id Rule identifier
 * @param enable True to enable
 */
void gf_ids_enable_rule(uint32_t rule_id, bool enable);

/**
 * @brief Inspect network packet
 * @param packet Packet metadata
 * @param payload Packet payload
 * @param payload_len Payload length
 * @return Alert ID if threat detected, 0 otherwise
 */
uint32_t gf_ids_inspect_packet(const gf_ids_packet_t* packet,
                                const uint8_t* payload,
                                uint16_t payload_len);

/**
 * @brief Check system behavior against baseline
 * @return Alert ID if anomaly detected, 0 otherwise
 */
uint32_t gf_ids_check_behavior(void);

/**
 * @brief Update behavioral baseline
 * @param baseline New baseline
 */
void gf_ids_set_baseline(const gf_ids_baseline_t* baseline);

/**
 * @brief Learn baseline from current behavior
 * @param learning_period_s Learning period in seconds
 */
void gf_ids_learn_baseline(uint32_t learning_period_s);

/**
 * @brief Get pending alert
 * @param[out] alert Alert output
 * @return 0 on success, -1 if no alerts
 */
int gf_ids_get_alert(gf_ids_alert_t* alert);

/**
 * @brief Acknowledge alert
 * @param alert_id Alert identifier
 */
void gf_ids_ack_alert(uint32_t alert_id);

/**
 * @brief Mark alert as false positive
 * @param alert_id Alert identifier
 */
void gf_ids_false_positive(uint32_t alert_id);

/**
 * @brief Set alert callback
 * @param callback Alert callback
 * @param user_data User context
 */
void gf_ids_set_alert_callback(gf_ids_alert_cb_t callback, void* user_data);

/**
 * @brief Get IDS statistics
 * @param[out] stats Statistics output
 */
void gf_ids_get_stats(gf_ids_stats_t* stats);

/**
 * @brief Reset statistics
 */
void gf_ids_reset_stats(void);

/**
 * @brief Export alerts for SIEM
 * @param output Buffer for export
 * @param output_len Buffer length
 * @return Bytes written
 */
int gf_ids_export_alerts(char* output, size_t output_len);

#ifdef __cplusplus
}
#endif

#endif /* GF_INTRUSION_DETECTION_H */
