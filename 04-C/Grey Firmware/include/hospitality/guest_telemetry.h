/**
 * @file guest_telemetry.h
 * @brief Guest Telemetry Collector for Smart Hospitality Systems
 * 
 * INDUSTRY RELEVANCE:
 * Guest experience analytics drive loyalty and revenue. Privacy-compliant
 * telemetry enables:
 * - Preference learning for personalization
 * - Predictive service (room service, maintenance)
 * - Operational efficiency (housekeeping routing)
 * - Loyalty program integration
 * 
 * Target applications: Hotel chains, cruise lines, casinos, theme parks,
 * airports, convention centers.
 * 
 * Standards: GDPR (privacy), CCPA (California privacy), PCI DSS (payment),
 *            HTNG (hospitality tech), ISO 27001 (information security)
 */

#ifndef GF_GUEST_TELEMETRY_H
#define GF_GUEST_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Guest Telemetry Types                                                      */
/*===========================================================================*/

typedef enum {
    GUEST_EVENT_CHECKIN,
    GUEST_EVENT_CHECKOUT,
    GUEST_EVENT_ROOM_ENTRY,
    GUEST_EVENT_ROOM_EXIT,
    GUEST_EVENT_AMENITY_USE,
    GUEST_EVENT_SERVICE_REQUEST,
    GUEST_EVENT_DINING,
    GUEST_EVENT_FEEDBACK
} guest_event_t;

typedef enum {
    AMENITY_SPA,
    AMENITY_POOL,
    AMENITY_GYM,
    AMENITY_RESTAURANT,
    AMENITY_BAR,
    AMENITY_CASINO,
    AMENITY_CONCIERGE,
    AMENITY_PARKING
} amenity_t;

typedef enum {
    PREF_TEMP_COOL,
    PREF_TEMP_WARM,
    PREF_LIGHTING_BRIGHT,
    PREF_LIGHTING_DIM,
    PREF_PILLOW_SOFT,
    PREF_PILLOW_FIRM,
    PREF_FLOOR_HIGH,
    PREF_FLOOR_LOW,
    PREF_QUIET_ROOM,
    PREF_VIEW_REQUIRED
} preference_t;

typedef struct {
    char guest_id[16];
    char reservation_id[16];
    char room_id[12];
    uint32_t checkin_time;
    uint32_t checkout_time;
    uint8_t loyalty_tier;       /* 0=none, 1-5 tier levels */
    uint32_t stay_count;        /* Total stays */
    uint32_t preferences;       /* Bitmap of preferences */
} guest_profile_t;

typedef struct {
    guest_event_t event;
    uint32_t timestamp;
    char guest_id[16];
    char location[24];
    char details[48];
    float spend_amount;         /* If applicable */
    uint8_t satisfaction;       /* 1-5 rating if provided */
} guest_event_record_t;

typedef struct {
    char guest_id[16];
    float temp_preference_c;
    uint8_t lighting_preference;
    uint16_t color_temp_k;
    char favorite_drink[32];
    char dietary_restrictions[48];
    uint32_t preferred_wakeup;  /* Preferred wake time */
    bool late_checkout_usual;
    bool extra_towels;
} learned_preferences_t;

typedef struct {
    bool anonymize_data;        /* Anonymize for analytics */
    bool enable_location;       /* Track location */
    bool enable_spend;          /* Track spending */
    uint32_t retention_days;    /* Data retention period */
    bool gdpr_consent;          /* GDPR consent obtained */
} telemetry_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int guest_telemetry_init(const telemetry_config_t* config);
int guest_telemetry_shutdown(void);

int guest_register(const guest_profile_t* profile);
int guest_update_profile(const char* guest_id, const guest_profile_t* profile);
int guest_get_profile(const char* guest_id, guest_profile_t* profile);

int guest_log_event(const guest_event_record_t* event);
int guest_get_events(const char* guest_id, guest_event_record_t* events,
                    uint16_t max, uint16_t* count);

int guest_learn_preferences(const char* guest_id, 
                           learned_preferences_t* prefs);
int guest_set_preference(const char* guest_id, preference_t pref, bool value);

int guest_get_analytics(const char* metric, uint32_t start, uint32_t end,
                       float* value);

int guest_request_data_export(const char* guest_id, uint8_t* data,
                             uint32_t max_len, uint32_t* len);
int guest_request_data_deletion(const char* guest_id);

#endif /* GF_GUEST_TELEMETRY_H */
