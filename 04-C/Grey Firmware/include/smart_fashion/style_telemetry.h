/**
 * @file style_telemetry.h
 * @brief Fashion Style Telemetry Collector Interface
 * 
 * INDUSTRY RELEVANCE:
 * Fashion tech companies use wearable telemetry to understand garment usage
 * patterns, enabling:
 * - Personalized styling recommendations
 * - Garment lifecycle tracking
 * - Sustainability metrics (wear count, repair alerts)
 * - Interactive fashion experiences
 * 
 * Applications include rental fashion services (Rent the Runway), athletic
 * wear analytics, and luxury brand engagement. Embedded expertise needed:
 * - Efficient data collection with minimal user impact
 * - Privacy-preserving analytics
 * - Cloud sync with offline buffering
 * - Low-energy BLE telemetry
 * 
 * STANDARDS:
 * - GDPR (Data Privacy)
 * - Bluetooth SIG (BLE specifications)
 * - ISO 14001 (Environmental Management)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_STYLE_TELEMETRY_H
#define GF_STYLE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Wear event type */
typedef enum {
    WEAR_EVENT_PUT_ON,
    WEAR_EVENT_TAKE_OFF,
    WEAR_EVENT_ACTIVITY_CHANGE,
    WEAR_EVENT_WASH,
    WEAR_EVENT_REPAIR
} wear_event_t;

/** Activity classification */
typedef enum {
    ACTIVITY_IDLE,
    ACTIVITY_WALKING,
    ACTIVITY_RUNNING,
    ACTIVITY_SITTING,
    ACTIVITY_WORKING,
    ACTIVITY_EXERCISING
} activity_t;

/** Wear session data */
typedef struct {
    uint32_t session_id;
    uint32_t start_time;
    uint32_t end_time;
    uint32_t duration_minutes;
    activity_t primary_activity;
    float avg_temperature_c;
    float avg_humidity_pct;
    uint16_t step_count;
    bool washed_after;
} wear_session_t;

/** Garment lifecycle metrics */
typedef struct {
    uint32_t total_wear_count;
    uint32_t total_wash_count;
    uint32_t total_hours_worn;
    float condition_score;       /**< 0.0 (worn) to 1.0 (new) */
    uint32_t first_worn_date;
    uint32_t last_worn_date;
} garment_lifecycle_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int style_telemetry_init(void);
int style_record_event(wear_event_t event);
int style_get_current_session(wear_session_t *session);
int style_get_lifecycle(garment_lifecycle_t *lifecycle);
int style_sync_to_cloud(void);
void style_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_STYLE_TELEMETRY_H */
