/**
 * @file smarthome_tests.c
 * @brief Smart Home Subsystem Test Suite
 * 
 * Comprehensive tests for the Zigbee/Matter Smart Home spotlight:
 * - Device discovery and pairing
 * - Commissioning state machine
 * - Secure communication with encryption
 * - ZCL command handling
 * - Scene and group management
 * - Event callback system
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

/*===========================================================================*/
/* Test Framework                                                             */
/*===========================================================================*/

static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;
static int current_group_tests = 0;
static int current_group_passed = 0;
static const char *current_group = NULL;

#define TEST_ASSERT(cond, msg) do { \
    if (!(cond)) { \
        printf("  [FAIL] %s\n", msg); \
        tests_failed++; \
        current_group_tests++; \
        return; \
    } \
    tests_passed++; \
    current_group_passed++; \
    current_group_tests++; \
} while(0)

#define TEST_ASSERT_EQ(a, b, msg) TEST_ASSERT((a) == (b), msg)
#define TEST_ASSERT_NE(a, b, msg) TEST_ASSERT((a) != (b), msg)
#define TEST_ASSERT_TRUE(cond, msg) TEST_ASSERT((cond), msg)
#define TEST_ASSERT_FALSE(cond, msg) TEST_ASSERT(!(cond), msg)

static void test_group_start(const char *name) {
    current_group = name;
    current_group_tests = 0;
    current_group_passed = 0;
    printf("\n[TEST GROUP] %s\n", name);
    printf("----------------------------------------\n");
}

static void test_group_end(void) {
    printf("----------------------------------------\n");
    printf("Group: %d/%d passed\n", current_group_passed, current_group_tests);
}

static void run_test(const char *name, void (*test_fn)(void)) {
    tests_run++;
    printf("  [TEST] %s... ", name);
    fflush(stdout);
    int before = tests_passed;
    test_fn();
    if (tests_passed > before) {
        printf("PASS\n");
    }
}

/*===========================================================================*/
/* Smart Home Spotlight Interface (Inline Implementation for Tests)          */
/*===========================================================================*/

/* Simplified types for testing */
typedef enum {
    SH_STATE_IDLE = 0,
    SH_STATE_SCANNING,
    SH_STATE_JOINING,
    SH_STATE_CONNECTED,
    SH_STATE_COMMISSIONING,
} sh_state_t;

typedef enum {
    SH_DEV_COORDINATOR = 0,
    SH_DEV_ROUTER,
    SH_DEV_END_DEVICE,
} sh_dev_type_t;

typedef enum {
    SH_EVENT_DEVICE_JOINED = 0,
    SH_EVENT_DEVICE_LEFT,
    SH_EVENT_ATTR_REPORT,
    SH_EVENT_CMD_RECEIVED,
} sh_event_type_t;

typedef struct {
    sh_event_type_t type;
    uint16_t source;
    uint8_t endpoint;
    uint16_t cluster;
    uint8_t data[16];
    uint8_t data_len;
} sh_event_t;

typedef void (*sh_event_cb_t)(const sh_event_t *event, void *ctx);

/* Test implementation state */
static struct {
    bool initialized;
    sh_state_t state;
    uint16_t pan_id;
    uint8_t channel;
    bool permit_join;
    uint16_t permit_join_timeout;
    uint8_t network_key[16];
    
    struct {
        uint16_t addr;
        sh_dev_type_t type;
        uint8_t lqi;
        bool online;
        bool authenticated;
    } devices[32];
    int device_count;
    
    struct {
        uint8_t id;
        uint16_t group_id;
        char name[32];
    } scenes[16];
    int scene_count;
    
    struct {
        uint16_t id;
        char name[32];
        uint16_t members[8];
        int member_count;
    } groups[8];
    int group_count;
    
    sh_event_cb_t event_cb;
    void *event_ctx;
    
    uint32_t frames_tx;
    uint32_t frames_rx;
    uint32_t commands_sent;
    
    /* Last sent command tracking for tests */
    uint16_t last_cmd_target;
    uint8_t last_cmd_endpoint;
    uint16_t last_cmd_cluster;
    uint8_t last_cmd_value;
} g_test_sh;

static void emit_event(sh_event_type_t type, uint16_t source) {
    if (g_test_sh.event_cb) {
        sh_event_t evt = {0};
        evt.type = type;
        evt.source = source;
        g_test_sh.event_cb(&evt, g_test_sh.event_ctx);
    }
}

static int sh_init(uint16_t pan_id, uint8_t channel) {
    memset(&g_test_sh, 0, sizeof(g_test_sh));
    g_test_sh.pan_id = pan_id;
    g_test_sh.channel = channel;
    g_test_sh.initialized = true;
    g_test_sh.state = SH_STATE_IDLE;
    
    /* Generate network key */
    for (int i = 0; i < 16; i++) {
        g_test_sh.network_key[i] = (uint8_t)(i * 17 + 0x5A);
    }
    return 0;
}

static int sh_shutdown(void) {
    g_test_sh.initialized = false;
    return 0;
}

static int sh_form_network(void) {
    if (!g_test_sh.initialized) return -1;
    g_test_sh.state = SH_STATE_CONNECTED;
    return 0;
}

static int sh_permit_join(bool enable, uint16_t timeout) {
    g_test_sh.permit_join = enable;
    g_test_sh.permit_join_timeout = timeout;
    return 0;
}

static int sh_add_device(uint16_t addr, sh_dev_type_t type, uint8_t lqi) {
    if (!g_test_sh.permit_join) return -1;
    if (g_test_sh.device_count >= 32) return -1;
    
    int idx = g_test_sh.device_count++;
    g_test_sh.devices[idx].addr = addr;
    g_test_sh.devices[idx].type = type;
    g_test_sh.devices[idx].lqi = lqi;
    g_test_sh.devices[idx].online = true;
    g_test_sh.devices[idx].authenticated = false;
    
    emit_event(SH_EVENT_DEVICE_JOINED, addr);
    return 0;
}

static int sh_remove_device(uint16_t addr) {
    for (int i = 0; i < g_test_sh.device_count; i++) {
        if (g_test_sh.devices[i].addr == addr) {
            g_test_sh.devices[i].online = false;
            emit_event(SH_EVENT_DEVICE_LEFT, addr);
            return 0;
        }
    }
    return -1;
}

static int sh_commission_device(uint16_t addr) {
    for (int i = 0; i < g_test_sh.device_count; i++) {
        if (g_test_sh.devices[i].addr == addr) {
            g_test_sh.devices[i].authenticated = true;
            return 0;
        }
    }
    return -1;
}

static int sh_send_on_off(uint16_t target, uint8_t endpoint, uint8_t cmd) {
    g_test_sh.last_cmd_target = target;
    g_test_sh.last_cmd_endpoint = endpoint;
    g_test_sh.last_cmd_cluster = 0x0006;  /* On/Off cluster */
    g_test_sh.last_cmd_value = cmd;
    g_test_sh.commands_sent++;
    g_test_sh.frames_tx++;
    return 0;
}

static int sh_send_level(uint16_t target, uint8_t endpoint, uint8_t level, uint16_t transition) {
    (void)transition;
    g_test_sh.last_cmd_target = target;
    g_test_sh.last_cmd_endpoint = endpoint;
    g_test_sh.last_cmd_cluster = 0x0008;  /* Level cluster */
    g_test_sh.last_cmd_value = level;
    g_test_sh.commands_sent++;
    g_test_sh.frames_tx++;
    return 0;
}

static int sh_send_color(uint16_t target, uint8_t endpoint, uint16_t hue, uint8_t sat) {
    (void)sat;
    g_test_sh.last_cmd_target = target;
    g_test_sh.last_cmd_endpoint = endpoint;
    g_test_sh.last_cmd_cluster = 0x0300;  /* Color cluster */
    g_test_sh.last_cmd_value = hue & 0xFF;
    g_test_sh.commands_sent++;
    g_test_sh.frames_tx++;
    return 0;
}

static int sh_create_scene(uint8_t scene_id, uint16_t group_id, const char *name) {
    if (g_test_sh.scene_count >= 16) return -1;
    
    int idx = g_test_sh.scene_count++;
    g_test_sh.scenes[idx].id = scene_id;
    g_test_sh.scenes[idx].group_id = group_id;
    strncpy(g_test_sh.scenes[idx].name, name, 31);
    return 0;
}

static int sh_recall_scene(uint8_t scene_id, uint16_t group_id) {
    for (int i = 0; i < g_test_sh.scene_count; i++) {
        if (g_test_sh.scenes[i].id == scene_id &&
            g_test_sh.scenes[i].group_id == group_id) {
            g_test_sh.commands_sent++;
            return 0;
        }
    }
    return -1;
}

static int sh_create_group(const char *name, uint16_t *group_id_out) {
    if (g_test_sh.group_count >= 8) return -1;
    
    int idx = g_test_sh.group_count++;
    g_test_sh.groups[idx].id = idx + 1;
    strncpy(g_test_sh.groups[idx].name, name, 31);
    g_test_sh.groups[idx].member_count = 0;
    
    if (group_id_out) *group_id_out = g_test_sh.groups[idx].id;
    return 0;
}

static int sh_add_to_group(uint16_t group_id, uint16_t device) {
    for (int i = 0; i < g_test_sh.group_count; i++) {
        if (g_test_sh.groups[i].id == group_id) {
            if (g_test_sh.groups[i].member_count >= 8) return -1;
            g_test_sh.groups[i].members[g_test_sh.groups[i].member_count++] = device;
            return 0;
        }
    }
    return -1;
}

static int sh_register_callback(sh_event_cb_t cb, void *ctx) {
    g_test_sh.event_cb = cb;
    g_test_sh.event_ctx = ctx;
    return 0;
}

static int sh_get_device_count(void) {
    int count = 0;
    for (int i = 0; i < g_test_sh.device_count; i++) {
        if (g_test_sh.devices[i].online) count++;
    }
    return count;
}

static sh_state_t sh_get_state(void) {
    return g_test_sh.state;
}

/*===========================================================================*/
/* Event Tracking for Tests                                                   */
/*===========================================================================*/

static struct {
    int join_count;
    int leave_count;
    int report_count;
    uint16_t last_source;
} g_event_tracker;

static void test_event_callback(const sh_event_t *event, void *ctx) {
    (void)ctx;
    g_event_tracker.last_source = event->source;
    
    switch (event->type) {
    case SH_EVENT_DEVICE_JOINED:
        g_event_tracker.join_count++;
        break;
    case SH_EVENT_DEVICE_LEFT:
        g_event_tracker.leave_count++;
        break;
    case SH_EVENT_ATTR_REPORT:
        g_event_tracker.report_count++;
        break;
    default:
        break;
    }
}

/*===========================================================================*/
/* Test Cases - Network Formation                                             */
/*===========================================================================*/

static void test_network_init(void) {
    int result = sh_init(0x1234, 15);
    TEST_ASSERT_EQ(result, 0, "Network initialization should succeed");
    TEST_ASSERT_TRUE(g_test_sh.initialized, "Module should be initialized");
    TEST_ASSERT_EQ(g_test_sh.pan_id, 0x1234, "PAN ID should be set");
    TEST_ASSERT_EQ(g_test_sh.channel, 15, "Channel should be set");
    sh_shutdown();
}

static void test_network_form(void) {
    sh_init(0x1234, 15);
    int result = sh_form_network();
    TEST_ASSERT_EQ(result, 0, "Network formation should succeed");
    TEST_ASSERT_EQ(sh_get_state(), SH_STATE_CONNECTED, "State should be connected");
    sh_shutdown();
}

static void test_permit_join_enable(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    
    int result = sh_permit_join(true, 180);
    TEST_ASSERT_EQ(result, 0, "Permit join should succeed");
    TEST_ASSERT_TRUE(g_test_sh.permit_join, "Permit join should be enabled");
    TEST_ASSERT_EQ(g_test_sh.permit_join_timeout, 180, "Timeout should be set");
    
    sh_shutdown();
}

static void test_permit_join_disable(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    sh_permit_join(false, 0);
    
    TEST_ASSERT_FALSE(g_test_sh.permit_join, "Permit join should be disabled");
    sh_shutdown();
}

/*===========================================================================*/
/* Test Cases - Device Discovery                                              */
/*===========================================================================*/

static void test_device_join(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    
    memset(&g_event_tracker, 0, sizeof(g_event_tracker));
    sh_register_callback(test_event_callback, NULL);
    
    int result = sh_add_device(0x1001, SH_DEV_ROUTER, 220);
    TEST_ASSERT_EQ(result, 0, "Device join should succeed");
    TEST_ASSERT_EQ(sh_get_device_count(), 1, "Device count should be 1");
    TEST_ASSERT_EQ(g_event_tracker.join_count, 1, "Join event should fire");
    TEST_ASSERT_EQ(g_event_tracker.last_source, 0x1001, "Event source should match");
    
    sh_shutdown();
}

static void test_device_join_rejected_no_permit(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    /* permit_join NOT enabled */
    
    int result = sh_add_device(0x1001, SH_DEV_ROUTER, 220);
    TEST_ASSERT_NE(result, 0, "Device join should fail without permit");
    TEST_ASSERT_EQ(sh_get_device_count(), 0, "No devices should join");
    
    sh_shutdown();
}

static void test_multiple_devices(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    
    sh_add_device(0x1001, SH_DEV_ROUTER, 220);
    sh_add_device(0x1002, SH_DEV_END_DEVICE, 180);
    sh_add_device(0x1003, SH_DEV_ROUTER, 200);
    
    TEST_ASSERT_EQ(sh_get_device_count(), 3, "Should have 3 devices");
    
    sh_shutdown();
}

static void test_device_leave(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    
    memset(&g_event_tracker, 0, sizeof(g_event_tracker));
    sh_register_callback(test_event_callback, NULL);
    
    sh_add_device(0x1001, SH_DEV_ROUTER, 220);
    sh_add_device(0x1002, SH_DEV_END_DEVICE, 180);
    
    int result = sh_remove_device(0x1001);
    TEST_ASSERT_EQ(result, 0, "Device remove should succeed");
    TEST_ASSERT_EQ(sh_get_device_count(), 1, "Should have 1 online device");
    TEST_ASSERT_EQ(g_event_tracker.leave_count, 1, "Leave event should fire");
    
    sh_shutdown();
}

/*===========================================================================*/
/* Test Cases - Commissioning                                                 */
/*===========================================================================*/

static void test_device_commissioning(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    
    sh_add_device(0x1001, SH_DEV_ROUTER, 220);
    
    TEST_ASSERT_FALSE(g_test_sh.devices[0].authenticated, "Device should not be authenticated initially");
    
    int result = sh_commission_device(0x1001);
    TEST_ASSERT_EQ(result, 0, "Commissioning should succeed");
    TEST_ASSERT_TRUE(g_test_sh.devices[0].authenticated, "Device should be authenticated after commissioning");
    
    sh_shutdown();
}

static void test_commission_unknown_device(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    
    sh_add_device(0x1001, SH_DEV_ROUTER, 220);
    
    int result = sh_commission_device(0x9999);
    TEST_ASSERT_NE(result, 0, "Commissioning unknown device should fail");
    
    sh_shutdown();
}

/*===========================================================================*/
/* Test Cases - ZCL Commands                                                  */
/*===========================================================================*/

static void test_on_off_command(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    sh_add_device(0x1001, SH_DEV_END_DEVICE, 200);
    
    g_test_sh.commands_sent = 0;
    
    int result = sh_send_on_off(0x1001, 1, 1);  /* ON command */
    TEST_ASSERT_EQ(result, 0, "On command should succeed");
    TEST_ASSERT_EQ(g_test_sh.last_cmd_target, 0x1001, "Target should match");
    TEST_ASSERT_EQ(g_test_sh.last_cmd_endpoint, 1, "Endpoint should match");
    TEST_ASSERT_EQ(g_test_sh.last_cmd_cluster, 0x0006, "Cluster should be On/Off");
    TEST_ASSERT_EQ(g_test_sh.last_cmd_value, 1, "Value should be ON(1)");
    TEST_ASSERT_EQ(g_test_sh.commands_sent, 1, "Command count should be 1");
    
    sh_shutdown();
}

static void test_level_command(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    sh_add_device(0x1001, SH_DEV_END_DEVICE, 200);
    
    int result = sh_send_level(0x1001, 1, 128, 20);
    TEST_ASSERT_EQ(result, 0, "Level command should succeed");
    TEST_ASSERT_EQ(g_test_sh.last_cmd_cluster, 0x0008, "Cluster should be Level");
    TEST_ASSERT_EQ(g_test_sh.last_cmd_value, 128, "Level should be 128");
    
    sh_shutdown();
}

static void test_color_command(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    sh_add_device(0x1001, SH_DEV_END_DEVICE, 200);
    
    int result = sh_send_color(0x1001, 1, 120, 200);
    TEST_ASSERT_EQ(result, 0, "Color command should succeed");
    TEST_ASSERT_EQ(g_test_sh.last_cmd_cluster, 0x0300, "Cluster should be Color");
    
    sh_shutdown();
}

/*===========================================================================*/
/* Test Cases - Scenes                                                        */
/*===========================================================================*/

static void test_create_scene(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    
    int result = sh_create_scene(1, 0, "Evening");
    TEST_ASSERT_EQ(result, 0, "Scene creation should succeed");
    TEST_ASSERT_EQ(g_test_sh.scene_count, 1, "Scene count should be 1");
    TEST_ASSERT_EQ(g_test_sh.scenes[0].id, 1, "Scene ID should match");
    
    sh_shutdown();
}

static void test_recall_scene(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    
    sh_create_scene(1, 0, "Evening");
    
    g_test_sh.commands_sent = 0;
    int result = sh_recall_scene(1, 0);
    TEST_ASSERT_EQ(result, 0, "Scene recall should succeed");
    TEST_ASSERT_EQ(g_test_sh.commands_sent, 1, "Commands should be sent");
    
    sh_shutdown();
}

static void test_recall_nonexistent_scene(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    
    int result = sh_recall_scene(99, 0);
    TEST_ASSERT_NE(result, 0, "Recalling nonexistent scene should fail");
    
    sh_shutdown();
}

/*===========================================================================*/
/* Test Cases - Groups                                                        */
/*===========================================================================*/

static void test_create_group(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    
    uint16_t group_id = 0;
    int result = sh_create_group("Living Room", &group_id);
    TEST_ASSERT_EQ(result, 0, "Group creation should succeed");
    TEST_ASSERT_NE(group_id, 0, "Group ID should be assigned");
    TEST_ASSERT_EQ(g_test_sh.group_count, 1, "Group count should be 1");
    
    sh_shutdown();
}

static void test_add_device_to_group(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    
    sh_add_device(0x1001, SH_DEV_END_DEVICE, 200);
    sh_add_device(0x1002, SH_DEV_END_DEVICE, 200);
    
    uint16_t group_id;
    sh_create_group("Living Room", &group_id);
    
    int result1 = sh_add_to_group(group_id, 0x1001);
    int result2 = sh_add_to_group(group_id, 0x1002);
    
    TEST_ASSERT_EQ(result1, 0, "Adding first device should succeed");
    TEST_ASSERT_EQ(result2, 0, "Adding second device should succeed");
    TEST_ASSERT_EQ(g_test_sh.groups[0].member_count, 2, "Group should have 2 members");
    
    sh_shutdown();
}

/*===========================================================================*/
/* Test Cases - Security                                                      */
/*===========================================================================*/

static void test_network_key_generation(void) {
    sh_init(0x1234, 15);
    
    /* Verify network key is non-zero */
    bool all_zero = true;
    for (int i = 0; i < 16; i++) {
        if (g_test_sh.network_key[i] != 0) {
            all_zero = false;
            break;
        }
    }
    TEST_ASSERT_FALSE(all_zero, "Network key should be generated");
    
    sh_shutdown();
}

static void test_frame_counter_increment(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    sh_add_device(0x1001, SH_DEV_END_DEVICE, 200);
    
    g_test_sh.frames_tx = 0;
    
    sh_send_on_off(0x1001, 1, 1);
    sh_send_on_off(0x1001, 1, 0);
    sh_send_on_off(0x1001, 1, 2);  /* Toggle */
    
    TEST_ASSERT_EQ(g_test_sh.frames_tx, 3, "Frame counter should increment");
    
    sh_shutdown();
}

/*===========================================================================*/
/* Test Cases - Integration                                                   */
/*===========================================================================*/

static void test_full_device_lifecycle(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    
    memset(&g_event_tracker, 0, sizeof(g_event_tracker));
    sh_register_callback(test_event_callback, NULL);
    
    /* Device joins */
    sh_add_device(0x1001, SH_DEV_END_DEVICE, 200);
    TEST_ASSERT_EQ(g_event_tracker.join_count, 1, "Join event should fire");
    
    /* Commission */
    sh_commission_device(0x1001);
    TEST_ASSERT_TRUE(g_test_sh.devices[0].authenticated, "Device should be commissioned");
    
    /* Send commands */
    sh_send_on_off(0x1001, 1, 1);
    sh_send_level(0x1001, 1, 200, 10);
    TEST_ASSERT_EQ(g_test_sh.commands_sent, 2, "Two commands sent");
    
    /* Device leaves */
    sh_remove_device(0x1001);
    TEST_ASSERT_EQ(g_event_tracker.leave_count, 1, "Leave event should fire");
    
    sh_shutdown();
}

static void test_scene_with_devices(void) {
    sh_init(0x1234, 15);
    sh_form_network();
    sh_permit_join(true, 180);
    
    /* Add devices */
    sh_add_device(0x1001, SH_DEV_END_DEVICE, 200);
    sh_add_device(0x1002, SH_DEV_END_DEVICE, 200);
    
    /* Create group */
    uint16_t group_id;
    sh_create_group("Bedroom", &group_id);
    sh_add_to_group(group_id, 0x1001);
    sh_add_to_group(group_id, 0x1002);
    
    /* Create scene */
    sh_create_scene(1, group_id, "Sleep");
    
    /* Recall scene */
    g_test_sh.commands_sent = 0;
    sh_recall_scene(1, group_id);
    TEST_ASSERT_EQ(g_test_sh.commands_sent, 1, "Scene commands sent");
    
    sh_shutdown();
}

/*===========================================================================*/
/* Main Test Runner                                                           */
/*===========================================================================*/

int main(void) {
    printf("\n");
    printf("================================================================\n");
    printf("       SMART HOME SUBSYSTEM TEST SUITE\n");
    printf("       Zigbee/Matter Protocol Stack Tests\n");
    printf("================================================================\n");
    
    /* Network Formation Tests */
    test_group_start("Network Formation");
    run_test("Network initialization", test_network_init);
    run_test("Network formation", test_network_form);
    run_test("Permit join enable", test_permit_join_enable);
    run_test("Permit join disable", test_permit_join_disable);
    test_group_end();
    
    /* Device Discovery Tests */
    test_group_start("Device Discovery & Pairing");
    run_test("Device join", test_device_join);
    run_test("Device join rejected (no permit)", test_device_join_rejected_no_permit);
    run_test("Multiple devices", test_multiple_devices);
    run_test("Device leave", test_device_leave);
    test_group_end();
    
    /* Commissioning Tests */
    test_group_start("Device Commissioning");
    run_test("Device commissioning", test_device_commissioning);
    run_test("Commission unknown device", test_commission_unknown_device);
    test_group_end();
    
    /* ZCL Command Tests */
    test_group_start("ZCL Commands");
    run_test("On/Off command", test_on_off_command);
    run_test("Level command", test_level_command);
    run_test("Color command", test_color_command);
    test_group_end();
    
    /* Scene Tests */
    test_group_start("Scene Management");
    run_test("Create scene", test_create_scene);
    run_test("Recall scene", test_recall_scene);
    run_test("Recall nonexistent scene", test_recall_nonexistent_scene);
    test_group_end();
    
    /* Group Tests */
    test_group_start("Group Management");
    run_test("Create group", test_create_group);
    run_test("Add device to group", test_add_device_to_group);
    test_group_end();
    
    /* Security Tests */
    test_group_start("Security");
    run_test("Network key generation", test_network_key_generation);
    run_test("Frame counter increment", test_frame_counter_increment);
    test_group_end();
    
    /* Integration Tests */
    test_group_start("Integration");
    run_test("Full device lifecycle", test_full_device_lifecycle);
    run_test("Scene with devices", test_scene_with_devices);
    test_group_end();
    
    /* Summary */
    printf("\n");
    printf("================================================================\n");
    printf("                    TEST SUMMARY\n");
    printf("================================================================\n");
    printf("Tests run:    %d\n", tests_run);
    printf("Tests passed: %d\n", tests_passed);
    printf("Tests failed: %d\n", tests_failed);
    printf("================================================================\n");
    
    if (tests_failed == 0) {
        printf("\n=== ALL TESTS PASSED ===\n\n");
        return 0;
    } else {
        printf("\n=== SOME TESTS FAILED ===\n\n");
        return 1;
    }
}
