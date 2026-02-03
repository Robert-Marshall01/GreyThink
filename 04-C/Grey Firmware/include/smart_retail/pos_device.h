/**
 * @file pos_device.h
 * @brief Point-of-Sale Device Interface for Smart Retail
 * 
 * INDUSTRY RELEVANCE:
 * POS systems are embedded devices handling:
 * - Barcode/QR scanning
 * - Payment processing (EMV chip cards)
 * - Receipt printing
 * - Cash drawer control
 * - Customer displays
 * 
 * Compliance: PCI-DSS for payment security
 * Companies: NCR, Verifone, Ingenico, Square
 */

#ifndef GF_POS_DEVICE_H
#define GF_POS_DEVICE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_POS_MAX_LINE_ITEMS       100
#define GF_POS_MAX_RECEIPT_LINES    50
#define GF_POS_BARCODE_MAX_LEN      48
#define GF_POS_PRODUCT_NAME_LEN     40

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_POS_OK = 0,
    GF_POS_ERROR_NULL_PTR,
    GF_POS_ERROR_NOT_INITIALIZED,
    GF_POS_ERROR_SCANNER_FAULT,
    GF_POS_ERROR_PRINTER_FAULT,
    GF_POS_ERROR_PAYMENT_DECLINED,
    GF_POS_ERROR_PAYMENT_TIMEOUT,
    GF_POS_ERROR_DRAWER_JAMMED,
    GF_POS_ERROR_NO_PAPER,
    GF_POS_ERROR_COMMS_FAIL,
    GF_POS_WARN_LOW_PAPER
} gf_pos_status_t;

typedef enum {
    GF_BARCODE_EAN13,
    GF_BARCODE_EAN8,
    GF_BARCODE_UPC_A,
    GF_BARCODE_UPC_E,
    GF_BARCODE_CODE128,
    GF_BARCODE_CODE39,
    GF_BARCODE_QR,
    GF_BARCODE_DATAMATRIX,
    GF_BARCODE_PDF417
} gf_barcode_type_t;

typedef enum {
    GF_PAYMENT_CASH,
    GF_PAYMENT_CARD_SWIPE,
    GF_PAYMENT_CARD_CHIP,
    GF_PAYMENT_CARD_CONTACTLESS,
    GF_PAYMENT_MOBILE_NFC,
    GF_PAYMENT_QR_CODE,
    GF_PAYMENT_GIFT_CARD,
    GF_PAYMENT_STORE_CREDIT
} gf_payment_method_t;

typedef enum {
    GF_TXN_STATE_IDLE,
    GF_TXN_STATE_SCANNING,
    GF_TXN_STATE_SUBTOTAL,
    GF_TXN_STATE_PAYMENT,
    GF_TXN_STATE_COMPLETE,
    GF_TXN_STATE_VOIDED,
    GF_TXN_STATE_SUSPENDED
} gf_pos_txn_state_t;

/**
 * @brief Device configuration
 */
typedef struct {
    uint16_t store_id;
    uint16_t terminal_id;
    char store_name[32];
    char address_line1[48];
    char address_line2[48];
    bool emv_enabled;
    bool nfc_enabled;
    uint16_t payment_timeout_sec;
} gf_pos_config_t;

/**
 * @brief Scanned barcode
 */
typedef struct {
    gf_barcode_type_t type;
    char data[GF_POS_BARCODE_MAX_LEN];
    uint8_t data_len;
    uint64_t scan_time_ms;
} gf_barcode_scan_t;

/**
 * @brief Line item
 */
typedef struct {
    char sku[20];
    char description[GF_POS_PRODUCT_NAME_LEN];
    uint16_t quantity;
    float unit_price;
    float discount_pct;
    float line_total;
    bool taxable;
    float tax_rate_pct;
} gf_pos_line_item_t;

/**
 * @brief Transaction totals
 */
typedef struct {
    float subtotal;
    float discount_total;
    float tax_total;
    float grand_total;
    uint16_t item_count;
    uint16_t line_count;
} gf_pos_totals_t;

/**
 * @brief Payment info
 */
typedef struct {
    gf_payment_method_t method;
    float amount;
    char card_last_four[5];
    char auth_code[16];
    char transaction_id[32];
    bool approved;
    uint64_t timestamp_ms;
} gf_pos_payment_t;

/**
 * @brief Receipt line
 */
typedef struct {
    char text[48];
    bool bold;
    bool centered;
    bool double_height;
} gf_receipt_line_t;

/**
 * @brief Customer display
 */
typedef struct {
    char line1[20];
    char line2[20];
    float amount;
    bool show_amount;
} gf_customer_display_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_barcode_cb_t)(const gf_barcode_scan_t* scan, void* user_data);

typedef void (*gf_payment_cb_t)(const gf_pos_payment_t* payment, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_pos_status_t gf_pos_init(const gf_pos_config_t* config);
void gf_pos_shutdown(void);

/* Transaction control */
gf_pos_status_t gf_pos_start_transaction(void);
gf_pos_status_t gf_pos_void_transaction(void);
gf_pos_status_t gf_pos_suspend_transaction(void);
gf_pos_status_t gf_pos_resume_transaction(uint32_t txn_id);
gf_pos_txn_state_t gf_pos_get_state(void);

/* Scanning */
gf_pos_status_t gf_pos_enable_scanner(bool enable);
gf_pos_status_t gf_pos_manual_entry(const char* sku);

/* Line items */
gf_pos_status_t gf_pos_add_item(const gf_pos_line_item_t* item);
gf_pos_status_t gf_pos_remove_item(uint16_t line_number);
gf_pos_status_t gf_pos_void_item(uint16_t line_number);
gf_pos_status_t gf_pos_apply_discount(uint16_t line_number, float discount_pct);
gf_pos_status_t gf_pos_get_totals(gf_pos_totals_t* totals);

/* Payment */
gf_pos_status_t gf_pos_start_payment(gf_payment_method_t method, float amount);
gf_pos_status_t gf_pos_cancel_payment(void);
gf_pos_status_t gf_pos_process_cash(float tendered, float* change);

/* Peripherals */
gf_pos_status_t gf_pos_open_drawer(void);
gf_pos_status_t gf_pos_print_receipt(const gf_receipt_line_t* lines, uint8_t line_count);
gf_pos_status_t gf_pos_cut_paper(void);
gf_pos_status_t gf_pos_update_display(const gf_customer_display_t* display);

/* Callbacks */
gf_pos_status_t gf_pos_register_barcode_callback(gf_barcode_cb_t cb, void* user_data);
gf_pos_status_t gf_pos_register_payment_callback(gf_payment_cb_t cb, void* user_data);

/* Periodic processing */
gf_pos_status_t gf_pos_process(void);

#endif /* GF_POS_DEVICE_H */
