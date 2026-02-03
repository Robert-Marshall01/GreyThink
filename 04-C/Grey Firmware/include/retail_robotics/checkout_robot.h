/**
 * @file checkout_robot.h
 * @brief Autonomous Checkout Robot Control
 *
 * INDUSTRY RELEVANCE:
 * Autonomous checkout robots reduce labor costs and provide 24/7
 * checkout capability. Computer vision and sensor fusion enable
 * accurate item identification and customer interaction.
 *
 * MARKET CONTEXT:
 * - Amazon Just Walk Out technology competitors
 * - Walmart, Target, Kroger automation initiatives
 * - Labor shortage solutions
 * - Contactless shopping experience
 * - Loss prevention and shrinkage reduction
 *
 * TECHNICAL APPROACH:
 * - Multi-modal item recognition (CV, RFID, barcode)
 * - Weight verification for accuracy
 * - Payment terminal integration
 * - Queue management and customer flow
 * - Anti-theft detection
 *
 * @author Grey Firmware Project
 */

#ifndef GF_CHECKOUT_ROBOT_H
#define GF_CHECKOUT_ROBOT_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Checkout robot operational state
 */
typedef enum {
    CHECKOUT_IDLE,
    CHECKOUT_SCANNING,
    CHECKOUT_WEIGHING,
    CHECKOUT_PAYMENT,
    CHECKOUT_BAGGING,
    CHECKOUT_ERROR,
    CHECKOUT_MAINTENANCE
} gf_checkout_state_t;

/**
 * @brief Item identification method
 */
typedef enum {
    ITEM_ID_BARCODE,
    ITEM_ID_RFID,
    ITEM_ID_VISION,
    ITEM_ID_MANUAL,
    ITEM_ID_WEIGHT_LOOKUP
} gf_item_id_method_t;

/**
 * @brief Scanned item record
 */
typedef struct {
    char sku[16];
    char description[64];
    uint32_t price_cents;
    float weight_kg;
    gf_item_id_method_t method;
    float confidence;            /**< 0.0-1.0 for vision ID */
    bool age_restricted;
    uint32_t scan_time;
} gf_scanned_item_t;

/**
 * @brief Transaction record
 */
typedef struct {
    uint32_t transaction_id;
    gf_scanned_item_t items[64];
    uint8_t item_count;
    uint32_t subtotal_cents;
    uint32_t tax_cents;
    uint32_t total_cents;
    gf_checkout_state_t state;
    uint32_t start_time;
} gf_transaction_t;

/**
 * @brief Weight verification status
 */
typedef struct {
    float expected_kg;
    float actual_kg;
    float tolerance_kg;
    bool verified;
    char discrepancy_reason[64];
} gf_weight_verify_t;

/* Function prototypes */
int gf_checkout_init(void);
int gf_checkout_start_transaction(uint32_t *transaction_id);
int gf_checkout_scan_item(gf_scanned_item_t *item);
int gf_checkout_verify_weight(gf_weight_verify_t *verify);
int gf_checkout_void_item(const char *sku);
int gf_checkout_get_transaction(gf_transaction_t *txn);
int gf_checkout_process_payment(const char *payment_token);
int gf_checkout_complete_transaction(void);
int gf_checkout_abort_transaction(void);
void gf_checkout_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CHECKOUT_ROBOT_H */
