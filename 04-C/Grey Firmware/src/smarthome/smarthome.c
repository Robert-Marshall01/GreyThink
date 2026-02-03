/**
 * @file smarthome.c
 * @brief Smart Home / Consumer IoT Implementation Stubs
 * 
 * This module provides stub implementations for smart home protocols
 * and consumer IoT functionality. The stubs demonstrate API usage
 * while production implementations would integrate with actual
 * radio hardware (802.15.4, WiFi, BLE).
 */

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#include "smarthome/zigbee.h"
#include "smarthome/matter.h"
#include "smarthome/lighting.h"
#include "smarthome/event_bus.h"

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_OK                   0
#define GF_ERR_INVALID_PARAM   -1
#define GF_ERR_NO_RESOURCE     -2
#define GF_ERR_NOT_READY       -3
#define GF_ERR_TIMEOUT         -4

/*===========================================================================*/
/* Zigbee Stack Stub State                                                    */
/*===========================================================================*/

static struct {
    bool                    initialized;
    gf_zigbee_state_t       state;
    gf_zigbee_device_type_t type;
    gf_zigbee_network_t     network;
    gf_zigbee_device_t      devices[GF_ZIGBEE_MAX_DEVICES];
    int                     device_count;
    gf_zigbee_rx_cb_t       rx_callback;
    void                   *rx_ctx;
    gf_zigbee_pair_cb_t     pair_callback;
    void                   *pair_ctx;
    uint8_t                 seq_num;
} g_zigbee;

/*===========================================================================*/
/* Zigbee API Implementation                                                  */
/*===========================================================================*/

int gf_zigbee_init(gf_zigbee_device_type_t type)
{
    memset(&g_zigbee, 0, sizeof(g_zigbee));
    g_zigbee.type = type;
    g_zigbee.state = GF_ZIGBEE_STATE_OFFLINE;
    g_zigbee.initialized = true;
    return GF_OK;
}

int gf_zigbee_start(const gf_zigbee_network_t *network)
{
    if (!g_zigbee.initialized) {
        return GF_ERR_NOT_READY;
    }
    
    if (network) {
        memcpy(&g_zigbee.network, network, sizeof(gf_zigbee_network_t));
    } else {
        /* Auto-configure defaults */
        g_zigbee.network.pan_id = 0x1234;
        g_zigbee.network.channel = 15;
        g_zigbee.network.security_level = 5;
    }
    
    g_zigbee.state = GF_ZIGBEE_STATE_ONLINE;
    return GF_OK;
}

int gf_zigbee_stop(void)
{
    g_zigbee.state = GF_ZIGBEE_STATE_OFFLINE;
    return GF_OK;
}

gf_zigbee_state_t gf_zigbee_get_state(void)
{
    return g_zigbee.state;
}

int gf_zigbee_permit_join(uint8_t duration_sec)
{
    (void)duration_sec;
    g_zigbee.network.permit_joining = (duration_sec > 0);
    return GF_OK;
}

const gf_zigbee_device_t *gf_zigbee_get_device(uint16_t short_addr)
{
    for (int i = 0; i < g_zigbee.device_count; i++) {
        if (g_zigbee.devices[i].short_addr == short_addr) {
            return &g_zigbee.devices[i];
        }
    }
    return NULL;
}

const gf_zigbee_device_t *gf_zigbee_get_device_by_ieee(
    const gf_zigbee_ieee_addr_t *ieee)
{
    for (int i = 0; i < g_zigbee.device_count; i++) {
        if (memcmp(&g_zigbee.devices[i].ieee_addr, ieee, 
                   sizeof(gf_zigbee_ieee_addr_t)) == 0) {
            return &g_zigbee.devices[i];
        }
    }
    return NULL;
}

int gf_zigbee_get_devices(gf_zigbee_device_t *devices, int max_devices)
{
    int count = (g_zigbee.device_count < max_devices) ? 
                 g_zigbee.device_count : max_devices;
    memcpy(devices, g_zigbee.devices, count * sizeof(gf_zigbee_device_t));
    return count;
}

int gf_zigbee_remove_device(uint16_t short_addr, bool force)
{
    (void)force;
    for (int i = 0; i < g_zigbee.device_count; i++) {
        if (g_zigbee.devices[i].short_addr == short_addr) {
            /* Shift remaining devices */
            memmove(&g_zigbee.devices[i], &g_zigbee.devices[i + 1],
                   (g_zigbee.device_count - i - 1) * sizeof(gf_zigbee_device_t));
            g_zigbee.device_count--;
            return GF_OK;
        }
    }
    return GF_ERR_INVALID_PARAM;
}

int gf_zigbee_send(uint16_t dst_addr, const gf_zcl_message_t *msg)
{
    if (!g_zigbee.initialized || g_zigbee.state != GF_ZIGBEE_STATE_ONLINE) {
        return GF_ERR_NOT_READY;
    }
    if (!msg) {
        return GF_ERR_INVALID_PARAM;
    }
    
    /* Stub: would transmit over 802.15.4 radio */
    (void)dst_addr;
    return GF_OK;
}

int gf_zigbee_send_with_response(uint16_t dst_addr, 
                                  const gf_zcl_message_t *msg,
                                  gf_zigbee_rx_cb_t callback, void *ctx,
                                  uint32_t timeout_ms)
{
    (void)timeout_ms;
    (void)callback;
    (void)ctx;
    return gf_zigbee_send(dst_addr, msg);
}

int gf_zigbee_register_rx_callback(gf_zigbee_rx_cb_t callback, void *ctx)
{
    g_zigbee.rx_callback = callback;
    g_zigbee.rx_ctx = ctx;
    return GF_OK;
}

int gf_zigbee_register_pair_callback(gf_zigbee_pair_cb_t callback, void *ctx)
{
    g_zigbee.pair_callback = callback;
    g_zigbee.pair_ctx = ctx;
    return GF_OK;
}

int gf_zigbee_process(uint32_t timeout_ms)
{
    (void)timeout_ms;
    /* Stub: would process incoming radio packets */
    return 0;
}

int gf_zigbee_get_network_key(uint8_t key[16])
{
    memcpy(key, g_zigbee.network.network_key, 16);
    return GF_OK;
}

int gf_zigbee_set_network_key(const uint8_t key[16])
{
    memcpy(g_zigbee.network.network_key, key, 16);
    return GF_OK;
}

/*===========================================================================*/
/* ZCL Helper Implementations                                                 */
/*===========================================================================*/

int gf_zcl_build_on_off(gf_zcl_message_t *msg, uint8_t endpoint, uint8_t cmd)
{
    if (!msg) return GF_ERR_INVALID_PARAM;
    
    memset(msg, 0, sizeof(*msg));
    msg->cluster_id = GF_ZCL_CLUSTER_ON_OFF;
    msg->endpoint = endpoint;
    msg->frame_type = GF_ZCL_FRAME_CLUSTER;
    msg->command_id = cmd;  /* 0=off, 1=on, 2=toggle */
    msg->seq_num = g_zigbee.seq_num++;
    msg->payload_len = 0;
    
    return GF_OK;
}

int gf_zcl_build_level(gf_zcl_message_t *msg, uint8_t endpoint,
                       uint8_t level, uint16_t transition_time)
{
    if (!msg) return GF_ERR_INVALID_PARAM;
    
    memset(msg, 0, sizeof(*msg));
    msg->cluster_id = GF_ZCL_CLUSTER_LEVEL;
    msg->endpoint = endpoint;
    msg->frame_type = GF_ZCL_FRAME_CLUSTER;
    msg->command_id = 0x04; /* Move to level with on/off */
    msg->seq_num = g_zigbee.seq_num++;
    
    msg->payload[0] = level;
    msg->payload[1] = transition_time & 0xFF;
    msg->payload[2] = (transition_time >> 8) & 0xFF;
    msg->payload_len = 3;
    
    return GF_OK;
}

int gf_zcl_build_color_hs(gf_zcl_message_t *msg, uint8_t endpoint,
                          uint8_t hue, uint8_t saturation,
                          uint16_t transition_time)
{
    if (!msg) return GF_ERR_INVALID_PARAM;
    
    memset(msg, 0, sizeof(*msg));
    msg->cluster_id = GF_ZCL_CLUSTER_COLOR;
    msg->endpoint = endpoint;
    msg->frame_type = GF_ZCL_FRAME_CLUSTER;
    msg->command_id = 0x06; /* Move to hue and saturation */
    msg->seq_num = g_zigbee.seq_num++;
    
    msg->payload[0] = hue;
    msg->payload[1] = saturation;
    msg->payload[2] = transition_time & 0xFF;
    msg->payload[3] = (transition_time >> 8) & 0xFF;
    msg->payload_len = 4;
    
    return GF_OK;
}

int gf_zcl_build_color_temp(gf_zcl_message_t *msg, uint8_t endpoint,
                            uint16_t mireds, uint16_t transition_time)
{
    if (!msg) return GF_ERR_INVALID_PARAM;
    
    memset(msg, 0, sizeof(*msg));
    msg->cluster_id = GF_ZCL_CLUSTER_COLOR;
    msg->endpoint = endpoint;
    msg->frame_type = GF_ZCL_FRAME_CLUSTER;
    msg->command_id = 0x0A; /* Move to color temperature */
    msg->seq_num = g_zigbee.seq_num++;
    
    msg->payload[0] = mireds & 0xFF;
    msg->payload[1] = (mireds >> 8) & 0xFF;
    msg->payload[2] = transition_time & 0xFF;
    msg->payload[3] = (transition_time >> 8) & 0xFF;
    msg->payload_len = 4;
    
    return GF_OK;
}

int gf_zcl_build_read_attr(gf_zcl_message_t *msg, uint16_t cluster,
                           uint8_t endpoint, const uint16_t *attr_ids,
                           int attr_count)
{
    if (!msg || !attr_ids || attr_count <= 0) {
        return GF_ERR_INVALID_PARAM;
    }
    
    memset(msg, 0, sizeof(*msg));
    msg->cluster_id = cluster;
    msg->endpoint = endpoint;
    msg->frame_type = GF_ZCL_FRAME_GLOBAL;
    msg->command_id = GF_ZCL_CMD_READ_ATTR;
    msg->seq_num = g_zigbee.seq_num++;
    
    for (int i = 0; i < attr_count && msg->payload_len < GF_ZIGBEE_PAYLOAD_MAX - 2; i++) {
        msg->payload[msg->payload_len++] = attr_ids[i] & 0xFF;
        msg->payload[msg->payload_len++] = (attr_ids[i] >> 8) & 0xFF;
    }
    
    return GF_OK;
}

/*===========================================================================*/
/* Matter Stack Stub State                                                    */
/*===========================================================================*/

static struct {
    bool                    initialized;
    gf_matter_state_t       state;
    gf_matter_transport_t   transport;
    gf_matter_device_info_t device_info;
    gf_matter_fabric_t      fabrics[GF_MATTER_MAX_FABRICS];
    gf_matter_endpoint_t    endpoints[GF_MATTER_MAX_ENDPOINTS];
    int                     endpoint_count;
    gf_matter_commission_cb_t commission_cb;
    void                   *commission_ctx;
    gf_matter_cmd_cb_t      cmd_cb;
    void                   *cmd_ctx;
    gf_matter_attr_cb_t     attr_cb;
    void                   *attr_ctx;
} g_matter;

/*===========================================================================*/
/* Matter API Implementation                                                  */
/*===========================================================================*/

int gf_matter_init(const gf_matter_device_info_t *device_info,
                   const gf_matter_dac_t *dac)
{
    (void)dac;
    memset(&g_matter, 0, sizeof(g_matter));
    if (device_info) {
        memcpy(&g_matter.device_info, device_info, sizeof(gf_matter_device_info_t));
    }
    g_matter.state = GF_MATTER_STATE_UNPAIRED;
    g_matter.initialized = true;
    return GF_OK;
}

int gf_matter_start(gf_matter_transport_t transport)
{
    if (!g_matter.initialized) {
        return GF_ERR_NOT_READY;
    }
    g_matter.transport = transport;
    return GF_OK;
}

int gf_matter_stop(void)
{
    g_matter.state = GF_MATTER_STATE_UNPAIRED;
    return GF_OK;
}

gf_matter_state_t gf_matter_get_state(void)
{
    return g_matter.state;
}

int gf_matter_add_endpoint(const gf_matter_endpoint_t *endpoint)
{
    if (!endpoint || g_matter.endpoint_count >= GF_MATTER_MAX_ENDPOINTS) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(&g_matter.endpoints[g_matter.endpoint_count], endpoint,
           sizeof(gf_matter_endpoint_t));
    g_matter.endpoint_count++;
    return GF_OK;
}

int gf_matter_open_commissioning(const gf_matter_setup_payload_t *setup,
                                  uint16_t timeout_sec)
{
    (void)setup;
    (void)timeout_sec;
    g_matter.state = GF_MATTER_STATE_COMMISSIONING;
    return GF_OK;
}

int gf_matter_close_commissioning(void)
{
    if (g_matter.state == GF_MATTER_STATE_COMMISSIONING) {
        g_matter.state = GF_MATTER_STATE_UNPAIRED;
    }
    return GF_OK;
}

int gf_matter_generate_qr_code(const gf_matter_setup_payload_t *setup,
                                char *qr_buffer, uint16_t buffer_len)
{
    if (!setup || !qr_buffer || buffer_len < 64) {
        return GF_ERR_INVALID_PARAM;
    }
    /* Simplified QR code format */
    snprintf(qr_buffer, buffer_len, "MT:Y.K9042PS00KA0648G00");
    return GF_OK;
}

int gf_matter_generate_manual_code(const gf_matter_setup_payload_t *setup,
                                    char *code_buffer, uint16_t buffer_len)
{
    if (!setup || !code_buffer || buffer_len < 16) {
        return GF_ERR_INVALID_PARAM;
    }
    snprintf(code_buffer, buffer_len, "%04u-%03u-%04u",
             (setup->setup_passcode / 10000) % 10000,
             (setup->discriminator) % 1000,
             setup->setup_passcode % 10000);
    return GF_OK;
}

const gf_matter_fabric_t *gf_matter_get_fabric(uint8_t index)
{
    if (index >= GF_MATTER_MAX_FABRICS) {
        return NULL;
    }
    if (!g_matter.fabrics[index].is_active) {
        return NULL;
    }
    return &g_matter.fabrics[index];
}

int gf_matter_leave_fabric(uint64_t fabric_id)
{
    for (int i = 0; i < GF_MATTER_MAX_FABRICS; i++) {
        if (g_matter.fabrics[i].fabric_id == fabric_id) {
            g_matter.fabrics[i].is_active = false;
            return GF_OK;
        }
    }
    return GF_ERR_INVALID_PARAM;
}

int gf_matter_factory_reset(void)
{
    memset(g_matter.fabrics, 0, sizeof(g_matter.fabrics));
    g_matter.state = GF_MATTER_STATE_UNPAIRED;
    return GF_OK;
}

int gf_matter_register_commission_cb(gf_matter_commission_cb_t callback,
                                      void *ctx)
{
    g_matter.commission_cb = callback;
    g_matter.commission_ctx = ctx;
    return GF_OK;
}

int gf_matter_register_cmd_cb(gf_matter_cmd_cb_t callback, void *ctx)
{
    g_matter.cmd_cb = callback;
    g_matter.cmd_ctx = ctx;
    return GF_OK;
}

int gf_matter_register_attr_cb(gf_matter_attr_cb_t callback, void *ctx)
{
    g_matter.attr_cb = callback;
    g_matter.attr_ctx = ctx;
    return GF_OK;
}

int gf_matter_set_attribute(uint8_t endpoint, uint32_t cluster,
                            uint32_t attr_id, const void *value, uint16_t len)
{
    (void)endpoint;
    (void)cluster;
    (void)attr_id;
    (void)value;
    (void)len;
    /* Stub: would update attribute store */
    return GF_OK;
}

int gf_matter_get_attribute(uint8_t endpoint, uint32_t cluster,
                            uint32_t attr_id, void *value, uint16_t *len)
{
    (void)endpoint;
    (void)cluster;
    (void)attr_id;
    (void)value;
    (void)len;
    return GF_OK;
}

int gf_matter_send_command(uint8_t endpoint, uint32_t cluster,
                           uint8_t command, const void *payload, uint16_t len)
{
    (void)endpoint;
    (void)cluster;
    (void)command;
    (void)payload;
    (void)len;
    return GF_OK;
}

int gf_matter_process(uint32_t timeout_ms)
{
    (void)timeout_ms;
    return 0;
}

int gf_matter_set_on_off(uint8_t endpoint, bool on)
{
    uint8_t v = on ? 1 : 0;
    return gf_matter_set_attribute(endpoint, GF_MATTER_CLUSTER_ON_OFF, 0, &v, 1);
}

int gf_matter_set_level(uint8_t endpoint, uint8_t level)
{
    return gf_matter_set_attribute(endpoint, GF_MATTER_CLUSTER_LEVEL, 0, &level, 1);
}

int gf_matter_set_color_hs(uint8_t endpoint, uint8_t hue, uint8_t saturation)
{
    uint8_t v[2] = {hue, saturation};
    return gf_matter_set_attribute(endpoint, GF_MATTER_CLUSTER_COLOR, 0, v, 2);
}

int gf_matter_set_color_temp(uint8_t endpoint, uint16_t color_temp_mireds)
{
    return gf_matter_set_attribute(endpoint, GF_MATTER_CLUSTER_COLOR, 7, 
                                   &color_temp_mireds, 2);
}

int gf_matter_report_temperature(uint8_t endpoint, int16_t temp_centi_c)
{
    return gf_matter_set_attribute(endpoint, GF_MATTER_CLUSTER_TEMP_MEAS, 0,
                                   &temp_centi_c, 2);
}

int gf_matter_report_humidity(uint8_t endpoint, uint16_t humidity_percent_100)
{
    return gf_matter_set_attribute(endpoint, GF_MATTER_CLUSTER_HUMIDITY_MEAS, 0,
                                   &humidity_percent_100, 2);
}

int gf_matter_report_occupancy(uint8_t endpoint, bool occupied)
{
    uint8_t v = occupied ? 1 : 0;
    return gf_matter_set_attribute(endpoint, GF_MATTER_CLUSTER_OCCUPANCY, 0, &v, 1);
}

/*===========================================================================*/
/* Lighting Subsystem Stub State                                              */
/*===========================================================================*/

static struct {
    bool                    initialized;
    gf_light_fixture_t      fixtures[GF_LIGHT_MAX_FIXTURES];
    int                     fixture_count;
    gf_light_zone_t         zones[GF_LIGHT_MAX_ZONES];
    int                     zone_count;
    gf_light_scene_t        scenes[GF_LIGHT_MAX_SCENES];
    int                     scene_count;
    gf_light_transition_t   transitions[GF_LIGHT_MAX_FIXTURES];
    gf_light_event_cb_t     event_cb;
    void                   *event_ctx;
} g_lighting;

/*===========================================================================*/
/* Lighting API Implementation                                                */
/*===========================================================================*/

int gf_light_init(void)
{
    memset(&g_lighting, 0, sizeof(g_lighting));
    g_lighting.initialized = true;
    return GF_OK;
}

int gf_light_register_fixture(const gf_light_fixture_t *fixture)
{
    if (!fixture || g_lighting.fixture_count >= GF_LIGHT_MAX_FIXTURES) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(&g_lighting.fixtures[g_lighting.fixture_count], fixture,
           sizeof(gf_light_fixture_t));
    return g_lighting.fixture_count++;
}

int gf_light_create_zone(const gf_light_zone_t *zone)
{
    if (!zone || g_lighting.zone_count >= GF_LIGHT_MAX_ZONES) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(&g_lighting.zones[g_lighting.zone_count], zone,
           sizeof(gf_light_zone_t));
    return g_lighting.zone_count++;
}

int gf_light_set_power(uint8_t fixture_id, bool on)
{
    if (fixture_id >= g_lighting.fixture_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_lighting.fixtures[fixture_id].on = on;
    if (g_lighting.event_cb) {
        g_lighting.event_cb(fixture_id, g_lighting.fixtures[fixture_id].mode,
                           on ? g_lighting.fixtures[fixture_id].brightness : 0,
                           g_lighting.event_ctx);
    }
    return GF_OK;
}

int gf_light_set_zone_power(uint8_t zone_id, bool on)
{
    if (zone_id >= g_lighting.zone_count) {
        return GF_ERR_INVALID_PARAM;
    }
    gf_light_zone_t *zone = &g_lighting.zones[zone_id];
    for (int i = 0; i < zone->fixture_count; i++) {
        gf_light_set_power(zone->fixture_ids[i], on);
    }
    return GF_OK;
}

int gf_light_set_brightness(uint8_t fixture_id, uint8_t brightness,
                            uint16_t transition_ms)
{
    if (fixture_id >= g_lighting.fixture_count) {
        return GF_ERR_INVALID_PARAM;
    }
    
    if (transition_ms == 0) {
        g_lighting.fixtures[fixture_id].brightness = brightness;
    } else {
        /* Set up transition */
        gf_light_transition_t *t = &g_lighting.transitions[fixture_id];
        t->fixture_id = fixture_id;
        t->start_brightness = g_lighting.fixtures[fixture_id].brightness;
        t->target_brightness = brightness;
        t->duration_ms = transition_ms;
        t->active = true;
    }
    return GF_OK;
}

int gf_light_set_zone_brightness(uint8_t zone_id, uint8_t brightness,
                                  uint16_t transition_ms)
{
    if (zone_id >= g_lighting.zone_count) {
        return GF_ERR_INVALID_PARAM;
    }
    gf_light_zone_t *zone = &g_lighting.zones[zone_id];
    for (int i = 0; i < zone->fixture_count; i++) {
        gf_light_set_brightness(zone->fixture_ids[i], brightness, transition_ms);
    }
    return GF_OK;
}

int gf_light_set_rgb(uint8_t fixture_id, gf_light_rgb_t color,
                     uint16_t transition_ms)
{
    if (fixture_id >= g_lighting.fixture_count) {
        return GF_ERR_INVALID_PARAM;
    }
    
    gf_light_fixture_t *f = &g_lighting.fixtures[fixture_id];
    if (!(f->capabilities & GF_LIGHT_CAP_RGB)) {
        return GF_ERR_INVALID_PARAM;
    }
    
    if (transition_ms == 0) {
        f->color = color;
        f->mode = GF_LIGHT_MODE_RGB;
    } else {
        gf_light_transition_t *t = &g_lighting.transitions[fixture_id];
        t->start_color = f->color;
        t->target_color = color;
        t->duration_ms = transition_ms;
        t->active = true;
    }
    return GF_OK;
}

int gf_light_set_hsv(uint8_t fixture_id, gf_light_hsv_t color,
                     uint16_t transition_ms)
{
    gf_light_rgb_t rgb = gf_light_hsv_to_rgb(color);
    return gf_light_set_rgb(fixture_id, rgb, transition_ms);
}

int gf_light_set_cct(uint8_t fixture_id, uint16_t mireds,
                     uint16_t transition_ms)
{
    if (fixture_id >= g_lighting.fixture_count) {
        return GF_ERR_INVALID_PARAM;
    }
    
    gf_light_fixture_t *f = &g_lighting.fixtures[fixture_id];
    if (!(f->capabilities & GF_LIGHT_CAP_CCT)) {
        return GF_ERR_INVALID_PARAM;
    }
    
    (void)transition_ms;
    f->cct.mireds = mireds;
    f->mode = GF_LIGHT_MODE_CCT;
    return GF_OK;
}

int gf_light_set_zone_rgb(uint8_t zone_id, gf_light_rgb_t color,
                          uint16_t transition_ms)
{
    if (zone_id >= g_lighting.zone_count) {
        return GF_ERR_INVALID_PARAM;
    }
    gf_light_zone_t *zone = &g_lighting.zones[zone_id];
    for (int i = 0; i < zone->fixture_count; i++) {
        gf_light_set_rgb(zone->fixture_ids[i], color, transition_ms);
    }
    return GF_OK;
}

int gf_light_set_zone_cct(uint8_t zone_id, uint16_t mireds,
                          uint16_t transition_ms)
{
    if (zone_id >= g_lighting.zone_count) {
        return GF_ERR_INVALID_PARAM;
    }
    gf_light_zone_t *zone = &g_lighting.zones[zone_id];
    for (int i = 0; i < zone->fixture_count; i++) {
        gf_light_set_cct(zone->fixture_ids[i], mireds, transition_ms);
    }
    return GF_OK;
}

int gf_light_create_scene(const gf_light_scene_t *scene)
{
    if (!scene || g_lighting.scene_count >= GF_LIGHT_MAX_SCENES) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(&g_lighting.scenes[g_lighting.scene_count], scene,
           sizeof(gf_light_scene_t));
    return g_lighting.scene_count++;
}

int gf_light_activate_scene(uint8_t scene_id)
{
    if (scene_id >= g_lighting.scene_count) {
        return GF_ERR_INVALID_PARAM;
    }
    
    gf_light_scene_t *scene = &g_lighting.scenes[scene_id];
    gf_light_set_zone_brightness(scene->zone_id, scene->brightness,
                                  scene->transition_ms);
    if (scene->mode == GF_LIGHT_MODE_RGB) {
        gf_light_set_zone_rgb(scene->zone_id, scene->color, scene->transition_ms);
    } else if (scene->mode == GF_LIGHT_MODE_CCT) {
        gf_light_set_zone_cct(scene->zone_id, scene->cct.mireds, scene->transition_ms);
    }
    return GF_OK;
}

int gf_light_get_fixture(uint8_t fixture_id, gf_light_fixture_t *fixture)
{
    if (fixture_id >= g_lighting.fixture_count || !fixture) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(fixture, &g_lighting.fixtures[fixture_id], sizeof(gf_light_fixture_t));
    return GF_OK;
}

int gf_light_get_zone(uint8_t zone_id, gf_light_zone_t *zone)
{
    if (zone_id >= g_lighting.zone_count || !zone) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(zone, &g_lighting.zones[zone_id], sizeof(gf_light_zone_t));
    return GF_OK;
}

int gf_light_update_occupancy(uint8_t zone_id, bool occupied)
{
    if (zone_id >= g_lighting.zone_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_lighting.zones[zone_id].occupied = occupied;
    return GF_OK;
}

int gf_light_update_ambient(uint8_t zone_id, uint16_t lux)
{
    if (zone_id >= g_lighting.zone_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_lighting.zones[zone_id].ambient_lux = lux;
    return GF_OK;
}

int gf_light_register_event_cb(gf_light_event_cb_t callback, void *ctx)
{
    g_lighting.event_cb = callback;
    g_lighting.event_ctx = ctx;
    return GF_OK;
}

int gf_light_register_occupancy_cb(gf_light_occupancy_cb_t callback, void *ctx)
{
    (void)callback;
    (void)ctx;
    return GF_OK;
}

int gf_light_process(void)
{
    /* Process active transitions */
    /* Stub: would update PWM outputs based on transitions */
    return 0;
}

/*===========================================================================*/
/* Color Conversion Utilities                                                 */
/*===========================================================================*/

gf_light_rgb_t gf_light_hsv_to_rgb(gf_light_hsv_t hsv)
{
    gf_light_rgb_t rgb = {0, 0, 0};
    
    if (hsv.s == 0) {
        rgb.r = rgb.g = rgb.b = hsv.v;
        return rgb;
    }
    
    uint8_t region = hsv.h / 43;
    uint8_t remainder = (hsv.h - (region * 43)) * 6;
    
    uint8_t p = (hsv.v * (255 - hsv.s)) >> 8;
    uint8_t q = (hsv.v * (255 - ((hsv.s * remainder) >> 8))) >> 8;
    uint8_t t = (hsv.v * (255 - ((hsv.s * (255 - remainder)) >> 8))) >> 8;
    
    switch (region) {
        case 0:  rgb.r = hsv.v; rgb.g = t;     rgb.b = p;     break;
        case 1:  rgb.r = q;     rgb.g = hsv.v; rgb.b = p;     break;
        case 2:  rgb.r = p;     rgb.g = hsv.v; rgb.b = t;     break;
        case 3:  rgb.r = p;     rgb.g = q;     rgb.b = hsv.v; break;
        case 4:  rgb.r = t;     rgb.g = p;     rgb.b = hsv.v; break;
        default: rgb.r = hsv.v; rgb.g = p;     rgb.b = q;     break;
    }
    
    return rgb;
}

gf_light_hsv_t gf_light_rgb_to_hsv(gf_light_rgb_t rgb)
{
    gf_light_hsv_t hsv = {0, 0, 0};
    
    uint8_t min = rgb.r < rgb.g ? (rgb.r < rgb.b ? rgb.r : rgb.b) 
                                : (rgb.g < rgb.b ? rgb.g : rgb.b);
    uint8_t max = rgb.r > rgb.g ? (rgb.r > rgb.b ? rgb.r : rgb.b)
                                : (rgb.g > rgb.b ? rgb.g : rgb.b);
    
    hsv.v = max;
    
    if (max == 0 || max == min) {
        return hsv;
    }
    
    hsv.s = 255 * (max - min) / max;
    
    if (max == rgb.r) {
        hsv.h = 0 + 43 * (rgb.g - rgb.b) / (max - min);
    } else if (max == rgb.g) {
        hsv.h = 85 + 43 * (rgb.b - rgb.r) / (max - min);
    } else {
        hsv.h = 171 + 43 * (rgb.r - rgb.g) / (max - min);
    }
    
    return hsv;
}

uint16_t gf_light_kelvin_to_mireds(uint16_t kelvin)
{
    if (kelvin == 0) return 500;
    return 1000000 / kelvin;
}

uint16_t gf_light_mireds_to_kelvin(uint16_t mireds)
{
    if (mireds == 0) return 6500;
    return 1000000 / mireds;
}

void gf_light_cct_to_ww_cw(uint16_t mireds, uint8_t brightness,
                           uint8_t *warm_pwm, uint8_t *cool_pwm)
{
    /* Linear interpolation between warm (500 mireds) and cool (153 mireds) */
    if (mireds <= 153) {
        *warm_pwm = 0;
        *cool_pwm = brightness;
    } else if (mireds >= 500) {
        *warm_pwm = brightness;
        *cool_pwm = 0;
    } else {
        uint16_t range = 500 - 153;
        uint16_t warm_ratio = mireds - 153;
        *warm_pwm = (brightness * warm_ratio) / range;
        *cool_pwm = brightness - *warm_pwm;
    }
}

uint8_t gf_light_gamma_correct(uint8_t linear)
{
    /* Approximate gamma 2.2 correction using lookup or calculation */
    /* Simplified: gamma^2.2 approximation */
    uint32_t corrected = ((uint32_t)linear * linear * linear) >> 16;
    return corrected > 255 ? 255 : (uint8_t)corrected;
}

uint32_t gf_light_get_total_power_mw(void)
{
    uint32_t total = 0;
    for (int i = 0; i < g_lighting.fixture_count; i++) {
        if (g_lighting.fixtures[i].on) {
            total += g_lighting.fixtures[i].power_mw;
        }
    }
    return total;
}

/*===========================================================================*/
/* Home Event Bus Stub State                                                  */
/*===========================================================================*/

static struct {
    bool                initialized;
    gf_home_room_t      rooms[GF_HOME_MAX_ROOMS];
    int                 room_count;
    gf_home_device_t    devices[GF_HOME_MAX_DEVICES];
    int                 device_count;
    gf_home_rule_t      rules[GF_HOME_MAX_RULES];
    int                 rule_count;
    gf_home_scene_t     home_scenes[GF_HOME_MAX_SCENES];
    int                 home_scene_count;
    gf_home_event_t     event_queue[GF_HOME_EVENT_QUEUE_SIZE];
    int                 event_head;
    int                 event_tail;
    gf_home_mode_t      mode;
    gf_home_alarm_state_t alarm_state;
    
    struct {
        gf_home_event_type_t type;
        uint16_t            device_id;
        uint8_t             room_id;
        gf_home_event_cb_t  callback;
        void               *ctx;
        bool                active;
    } subscriptions[16];
    int                 sub_count;
    
    gf_home_rule_cb_t   rule_cb;
    void               *rule_ctx;
} g_home;

/*===========================================================================*/
/* Home Event Bus API Implementation                                          */
/*===========================================================================*/

int gf_home_init(void)
{
    memset(&g_home, 0, sizeof(g_home));
    g_home.mode = GF_HOME_MODE_HOME;
    g_home.alarm_state = GF_HOME_ALARM_DISARMED;
    g_home.initialized = true;
    return GF_OK;
}

int gf_home_register_room(const gf_home_room_t *room)
{
    if (!room || g_home.room_count >= GF_HOME_MAX_ROOMS) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(&g_home.rooms[g_home.room_count], room, sizeof(gf_home_room_t));
    return g_home.room_count++;
}

int gf_home_register_device(const gf_home_device_t *device)
{
    if (!device || g_home.device_count >= GF_HOME_MAX_DEVICES) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(&g_home.devices[g_home.device_count], device, sizeof(gf_home_device_t));
    return g_home.device_count++;
}

int gf_home_unregister_device(uint16_t device_id)
{
    for (int i = 0; i < g_home.device_count; i++) {
        if (g_home.devices[i].id == device_id) {
            memmove(&g_home.devices[i], &g_home.devices[i + 1],
                   (g_home.device_count - i - 1) * sizeof(gf_home_device_t));
            g_home.device_count--;
            return GF_OK;
        }
    }
    return GF_ERR_INVALID_PARAM;
}

int gf_home_publish_event(const gf_home_event_t *event)
{
    if (!event) {
        return GF_ERR_INVALID_PARAM;
    }
    
    int next = (g_home.event_head + 1) % GF_HOME_EVENT_QUEUE_SIZE;
    if (next == g_home.event_tail) {
        return GF_ERR_NO_RESOURCE; /* Queue full */
    }
    
    memcpy(&g_home.event_queue[g_home.event_head], event, sizeof(gf_home_event_t));
    g_home.event_head = next;
    return GF_OK;
}

int gf_home_subscribe(gf_home_event_type_t type, uint16_t device_id,
                       uint8_t room_id, gf_home_event_cb_t callback, void *ctx)
{
    if (g_home.sub_count >= 16 || !callback) {
        return GF_ERR_NO_RESOURCE;
    }
    
    int handle = g_home.sub_count;
    g_home.subscriptions[handle].type = type;
    g_home.subscriptions[handle].device_id = device_id;
    g_home.subscriptions[handle].room_id = room_id;
    g_home.subscriptions[handle].callback = callback;
    g_home.subscriptions[handle].ctx = ctx;
    g_home.subscriptions[handle].active = true;
    g_home.sub_count++;
    
    return handle;
}

int gf_home_unsubscribe(int handle)
{
    if (handle < 0 || handle >= g_home.sub_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_home.subscriptions[handle].active = false;
    return GF_OK;
}

int gf_home_create_rule(const gf_home_rule_t *rule)
{
    if (!rule || g_home.rule_count >= GF_HOME_MAX_RULES) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(&g_home.rules[g_home.rule_count], rule, sizeof(gf_home_rule_t));
    return g_home.rule_count++;
}

int gf_home_enable_rule(uint8_t rule_id, bool enable)
{
    if (rule_id >= g_home.rule_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_home.rules[rule_id].enabled = enable;
    return GF_OK;
}

int gf_home_delete_rule(uint8_t rule_id)
{
    if (rule_id >= g_home.rule_count) {
        return GF_ERR_INVALID_PARAM;
    }
    memmove(&g_home.rules[rule_id], &g_home.rules[rule_id + 1],
           (g_home.rule_count - rule_id - 1) * sizeof(gf_home_rule_t));
    g_home.rule_count--;
    return GF_OK;
}

int gf_home_register_rule_cb(gf_home_rule_cb_t callback, void *ctx)
{
    g_home.rule_cb = callback;
    g_home.rule_ctx = ctx;
    return GF_OK;
}

int gf_home_create_scene(const gf_home_scene_t *scene)
{
    if (!scene || g_home.home_scene_count >= GF_HOME_MAX_SCENES) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(&g_home.home_scenes[g_home.home_scene_count], scene,
           sizeof(gf_home_scene_t));
    return g_home.home_scene_count++;
}

int gf_home_activate_scene(uint8_t scene_id)
{
    if (scene_id >= g_home.home_scene_count) {
        return GF_ERR_INVALID_PARAM;
    }
    
    /* Publish scene activation event */
    gf_home_event_t evt = {0};
    evt.type = GF_HOME_EVT_SCENE_ACTIVATE;
    evt.data.scene.scene_id = scene_id;
    return gf_home_publish_event(&evt);
}

int gf_home_set_mode(gf_home_mode_t mode)
{
    gf_home_mode_t old_mode = g_home.mode;
    g_home.mode = mode;
    
    if (old_mode != mode) {
        gf_home_event_t evt = {0};
        evt.type = GF_HOME_EVT_MODE_CHANGE;
        evt.data.state.old_value[0] = old_mode;
        evt.data.state.new_value[0] = mode;
        evt.data.state.value_len = 1;
        gf_home_publish_event(&evt);
    }
    return GF_OK;
}

gf_home_mode_t gf_home_get_mode(void)
{
    return g_home.mode;
}

int gf_home_set_alarm_state(gf_home_alarm_state_t state, const char *pin)
{
    (void)pin;
    gf_home_alarm_state_t old = g_home.alarm_state;
    g_home.alarm_state = state;
    
    if (old != state) {
        gf_home_event_t evt = {0};
        evt.type = (state == GF_HOME_ALARM_DISARMED) ? 
                   GF_HOME_EVT_ALARM_DISARM : GF_HOME_EVT_ALARM_ARM;
        gf_home_publish_event(&evt);
    }
    return GF_OK;
}

gf_home_alarm_state_t gf_home_get_alarm_state(void)
{
    return g_home.alarm_state;
}

int gf_home_process(int max_events)
{
    int count = 0;
    
    while (g_home.event_tail != g_home.event_head) {
        if (max_events > 0 && count >= max_events) break;
        
        gf_home_event_t *evt = &g_home.event_queue[g_home.event_tail];
        
        /* Dispatch to subscribers */
        for (int i = 0; i < g_home.sub_count; i++) {
            if (!g_home.subscriptions[i].active) continue;
            
            bool type_match = (g_home.subscriptions[i].type == 0xFF) ||
                             (g_home.subscriptions[i].type == evt->type);
            bool device_match = (g_home.subscriptions[i].device_id == 0) ||
                               (g_home.subscriptions[i].device_id == evt->device_id);
            bool room_match = (g_home.subscriptions[i].room_id == 0) ||
                             (g_home.subscriptions[i].room_id == evt->room_id);
            
            if (type_match && device_match && room_match) {
                g_home.subscriptions[i].callback(evt, g_home.subscriptions[i].ctx);
            }
        }
        
        /* Check automation rules */
        for (int r = 0; r < g_home.rule_count; r++) {
            if (!g_home.rules[r].enabled) continue;
            
            if (g_home.rules[r].trigger.type == evt->type) {
                if (g_home.rule_cb) {
                    g_home.rule_cb(&g_home.rules[r], evt, g_home.rule_ctx);
                }
            }
        }
        
        g_home.event_tail = (g_home.event_tail + 1) % GF_HOME_EVENT_QUEUE_SIZE;
        count++;
    }
    
    return count;
}

int gf_home_get_room(uint8_t room_id, gf_home_room_t *room)
{
    if (room_id >= g_home.room_count || !room) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(room, &g_home.rooms[room_id], sizeof(gf_home_room_t));
    return GF_OK;
}

int gf_home_get_device(uint16_t device_id, gf_home_device_t *device)
{
    for (int i = 0; i < g_home.device_count; i++) {
        if (g_home.devices[i].id == device_id) {
            if (device) {
                memcpy(device, &g_home.devices[i], sizeof(gf_home_device_t));
            }
            return GF_OK;
        }
    }
    return GF_ERR_INVALID_PARAM;
}

int gf_home_get_room_devices(uint8_t room_id, gf_home_device_t *devices,
                              int max_devices)
{
    int count = 0;
    for (int i = 0; i < g_home.device_count && count < max_devices; i++) {
        if (g_home.devices[i].room_id == room_id) {
            if (devices) {
                memcpy(&devices[count], &g_home.devices[i], sizeof(gf_home_device_t));
            }
            count++;
        }
    }
    return count;
}

int gf_home_get_history(gf_home_event_t *events, int max_events,
                         uint32_t since_timestamp)
{
    (void)events;
    (void)max_events;
    (void)since_timestamp;
    /* Stub: would return historical events */
    return 0;
}
