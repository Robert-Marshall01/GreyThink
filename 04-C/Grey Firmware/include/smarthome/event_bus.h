/**
 * @file event_bus.h
 * @brief Home Automation Event Bus
 * 
 * INDUSTRY RELEVANCE:
 * Home automation systems require real-time event distribution between sensors,
 * controllers, and actuators. Unlike generic message buses, home automation
 * event buses support room/zone context, scene triggers, timers, and
 * automation rules. Products like Home Assistant, openHAB, and SmartThings
 * rely on sophisticated event routing.
 * 
 * WHY THIS MATTERS:
 * - Room/zone-aware event routing
 * - Automation rule triggers (if-this-then-that)
 * - Scene coordination across devices
 * - Timer and schedule integration
 * - State machine for device lifecycles
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Context-aware event routing
 * - Rule engine integration hooks
 * - Scene coordination patterns
 * - Event persistence for replay
 */

#ifndef GF_HOME_EVENT_BUS_H
#define GF_HOME_EVENT_BUS_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_HOME_MAX_ROOMS           16      /* Maximum rooms/zones */
#define GF_HOME_MAX_DEVICES         64      /* Maximum devices */
#define GF_HOME_MAX_RULES           32      /* Maximum automation rules */
#define GF_HOME_MAX_SCENES          16      /* Maximum scenes */
#define GF_HOME_EVENT_QUEUE_SIZE    32      /* Event queue depth */
#define GF_HOME_DEVICE_NAME_LEN     24      /* Device name length */
#define GF_HOME_ROOM_NAME_LEN       16      /* Room name length */

/*===========================================================================*/
/* Event Types                                                                */
/*===========================================================================*/

/* Device categories */
typedef enum {
    GF_HOME_CAT_LIGHT = 0,          /* Lighting devices */
    GF_HOME_CAT_SWITCH,             /* Switches and outlets */
    GF_HOME_CAT_SENSOR,             /* Sensors (motion, temp, etc.) */
    GF_HOME_CAT_COVER,              /* Blinds, shades, garage doors */
    GF_HOME_CAT_CLIMATE,            /* HVAC, thermostats, fans */
    GF_HOME_CAT_LOCK,               /* Door locks */
    GF_HOME_CAT_CAMERA,             /* Security cameras */
    GF_HOME_CAT_MEDIA,              /* Media players, TVs */
    GF_HOME_CAT_APPLIANCE,          /* Smart appliances */
    GF_HOME_CAT_ALARM,              /* Security system */
    GF_HOME_CAT_SYSTEM              /* System events */
} gf_home_category_t;

/* Event type identifiers */
typedef enum {
    /* State change events */
    GF_HOME_EVT_STATE_CHANGE = 0x00,    /* Device state changed */
    GF_HOME_EVT_AVAILABILITY,           /* Device online/offline */
    
    /* Sensor events */
    GF_HOME_EVT_MOTION_START = 0x10,    /* Motion detected */
    GF_HOME_EVT_MOTION_STOP,            /* Motion ended */
    GF_HOME_EVT_DOOR_OPEN,              /* Door/window opened */
    GF_HOME_EVT_DOOR_CLOSE,             /* Door/window closed */
    GF_HOME_EVT_TEMPERATURE,            /* Temperature reading */
    GF_HOME_EVT_HUMIDITY,               /* Humidity reading */
    GF_HOME_EVT_LIGHT_LEVEL,            /* Ambient light reading */
    GF_HOME_EVT_SMOKE_ALARM,            /* Smoke detected */
    GF_HOME_EVT_WATER_LEAK,             /* Water leak detected */
    GF_HOME_EVT_CO_ALARM,               /* Carbon monoxide */
    
    /* Control events */
    GF_HOME_EVT_COMMAND = 0x20,         /* User command */
    GF_HOME_EVT_SCENE_ACTIVATE,         /* Scene activated */
    GF_HOME_EVT_RULE_TRIGGER,           /* Automation rule triggered */
    GF_HOME_EVT_SCHEDULE,               /* Scheduled event */
    
    /* System events */
    GF_HOME_EVT_DEVICE_JOIN = 0x30,     /* New device joined */
    GF_HOME_EVT_DEVICE_LEAVE,           /* Device removed */
    GF_HOME_EVT_MODE_CHANGE,            /* Home/away/night mode */
    GF_HOME_EVT_ALARM_ARM,              /* Security armed */
    GF_HOME_EVT_ALARM_DISARM,           /* Security disarmed */
    GF_HOME_EVT_ALARM_TRIGGER,          /* Security breach */
    
    /* Error events */
    GF_HOME_EVT_ERROR = 0xF0,           /* Error occurred */
    GF_HOME_EVT_LOW_BATTERY,            /* Low battery warning */
    GF_HOME_EVT_OFFLINE                 /* Device went offline */
} gf_home_event_type_t;

/* Home modes */
typedef enum {
    GF_HOME_MODE_HOME = 0,
    GF_HOME_MODE_AWAY,
    GF_HOME_MODE_NIGHT,
    GF_HOME_MODE_VACATION
} gf_home_mode_t;

/* Alarm states */
typedef enum {
    GF_HOME_ALARM_DISARMED = 0,
    GF_HOME_ALARM_ARMED_HOME,
    GF_HOME_ALARM_ARMED_AWAY,
    GF_HOME_ALARM_TRIGGERED
} gf_home_alarm_state_t;

/*===========================================================================*/
/* Data Structures                                                            */
/*===========================================================================*/

/* Device identifier */
typedef struct {
    uint16_t    id;                     /* Unique device ID */
    uint8_t     category;               /* gf_home_category_t */
    uint8_t     room_id;                /* Room assignment */
    char        name[GF_HOME_DEVICE_NAME_LEN];
} gf_home_device_t;

/* Room definition */
typedef struct {
    uint8_t     id;
    char        name[GF_HOME_ROOM_NAME_LEN];
    uint8_t     floor;                  /* Floor level */
    bool        occupied;               /* Occupancy state */
    uint16_t    device_count;           /* Devices in room */
} gf_home_room_t;

/* Home automation event */
typedef struct {
    uint32_t                timestamp;      /* Event timestamp */
    gf_home_event_type_t    type;           /* Event type */
    uint16_t                device_id;      /* Source device ID */
    uint8_t                 room_id;        /* Room context */
    uint8_t                 category;       /* Device category */
    union {
        /* State change payload */
        struct {
            uint8_t     attribute_id;
            uint8_t     old_value[8];
            uint8_t     new_value[8];
            uint8_t     value_len;
        } state;
        
        /* Sensor payload */
        struct {
            int32_t     value;              /* Scaled integer value */
            uint8_t     scale;              /* Decimal places */
            uint8_t     unit;               /* Unit type */
        } sensor;
        
        /* Command payload */
        struct {
            uint8_t     command_id;
            uint8_t     params[8];
            uint8_t     param_len;
        } command;
        
        /* Scene payload */
        struct {
            uint8_t     scene_id;
            uint8_t     transition_time;    /* In seconds */
        } scene;
        
        /* Rule trigger payload */
        struct {
            uint8_t     rule_id;
            uint8_t     trigger_device_id;
        } rule;
        
        /* Error payload */
        struct {
            uint16_t    error_code;
            char        message[16];
        } error;
        
        /* Raw data */
        uint8_t raw[16];
    } data;
} gf_home_event_t;

/* Automation rule definition */
typedef struct {
    uint8_t     id;
    char        name[24];
    bool        enabled;
    
    /* Trigger condition */
    struct {
        gf_home_event_type_t type;
        uint16_t device_id;             /* 0 = any device */
        uint8_t  room_id;               /* 0 = any room */
        uint8_t  condition;             /* Comparison operator */
        int32_t  threshold;             /* Value threshold */
    } trigger;
    
    /* Action to execute */
    struct {
        uint16_t target_device_id;      /* 0 = scene */
        uint8_t  command_id;
        uint8_t  params[8];
        uint8_t  param_len;
        uint8_t  scene_id;              /* If target is scene */
        uint16_t delay_ms;              /* Delay before action */
    } action;
} gf_home_rule_t;

/* Scene definition */
typedef struct {
    uint8_t     id;
    char        name[24];
    uint8_t     room_id;                /* 0 = whole home */
    
    /* Scene actions (simplified - device states) */
    struct {
        uint16_t device_id;
        uint8_t  state[8];
        uint8_t  state_len;
    } actions[8];
    uint8_t     action_count;
    
    uint16_t    transition_time_ms;
} gf_home_scene_t;

/* Event callback signature */
typedef void (*gf_home_event_cb_t)(const gf_home_event_t *event, void *ctx);

/* Rule trigger callback (called when rule fires) */
typedef void (*gf_home_rule_cb_t)(const gf_home_rule_t *rule,
                                   const gf_home_event_t *trigger_event,
                                   void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize home automation event bus
 */
int gf_home_init(void);

/**
 * @brief Register a room
 */
int gf_home_register_room(const gf_home_room_t *room);

/**
 * @brief Register a device
 */
int gf_home_register_device(const gf_home_device_t *device);

/**
 * @brief Unregister a device
 */
int gf_home_unregister_device(uint16_t device_id);

/**
 * @brief Publish an event to the bus
 */
int gf_home_publish_event(const gf_home_event_t *event);

/**
 * @brief Subscribe to events (filter by type, device, room)
 * @param type Event type filter (0xFF = all types)
 * @param device_id Device filter (0 = all devices)
 * @param room_id Room filter (0 = all rooms)
 * @param callback Event handler
 * @param ctx User context
 * @return Subscription handle or error
 */
int gf_home_subscribe(gf_home_event_type_t type, uint16_t device_id,
                       uint8_t room_id, gf_home_event_cb_t callback, void *ctx);

/**
 * @brief Unsubscribe from events
 */
int gf_home_unsubscribe(int handle);

/**
 * @brief Create automation rule
 */
int gf_home_create_rule(const gf_home_rule_t *rule);

/**
 * @brief Enable/disable automation rule
 */
int gf_home_enable_rule(uint8_t rule_id, bool enable);

/**
 * @brief Delete automation rule
 */
int gf_home_delete_rule(uint8_t rule_id);

/**
 * @brief Register rule trigger callback
 */
int gf_home_register_rule_cb(gf_home_rule_cb_t callback, void *ctx);

/**
 * @brief Create a scene
 */
int gf_home_create_scene(const gf_home_scene_t *scene);

/**
 * @brief Activate a scene
 */
int gf_home_activate_scene(uint8_t scene_id);

/**
 * @brief Set home mode (home/away/night/vacation)
 */
int gf_home_set_mode(gf_home_mode_t mode);

/**
 * @brief Get current home mode
 */
gf_home_mode_t gf_home_get_mode(void);

/**
 * @brief Set alarm state
 */
int gf_home_set_alarm_state(gf_home_alarm_state_t state, const char *pin);

/**
 * @brief Get alarm state
 */
gf_home_alarm_state_t gf_home_get_alarm_state(void);

/**
 * @brief Process event queue (call from main loop)
 * @param max_events Maximum events to process (0 = all)
 * @return Number of events processed
 */
int gf_home_process(int max_events);

/**
 * @brief Get room by ID
 */
int gf_home_get_room(uint8_t room_id, gf_home_room_t *room);

/**
 * @brief Get device by ID
 */
int gf_home_get_device(uint16_t device_id, gf_home_device_t *device);

/**
 * @brief Get devices in room
 * @param room_id Room to query
 * @param devices Output array
 * @param max_devices Array size
 * @return Number of devices
 */
int gf_home_get_room_devices(uint8_t room_id, gf_home_device_t *devices,
                              int max_devices);

/**
 * @brief Get event history
 * @param events Output array
 * @param max_events Array size
 * @param since_timestamp Get events after this time (0 = all)
 * @return Number of events
 */
int gf_home_get_history(gf_home_event_t *events, int max_events,
                         uint32_t since_timestamp);

#endif /* GF_HOME_EVENT_BUS_H */
