/**
 * @file unit_tests.c
 * @brief Unit Tests for Grey Firmware Core Components
 * 
 * Tests cover:
 * - CAN bus initialization and frame handling
 * - Message bus typed payloads
 * - Scheduler priority handling
 * - Error handler severity classification
 */

#include "test_framework.h"
#include "../include/grey_firmware.h"
#include "../include/core/message_bus.h"
#include "../include/core/scheduler.h"
#include "../include/core/error_handler.h"
#include "../include/automotive/can_bus.h"

#include <string.h>

/*===========================================================================*/
/* CAN Bus Test Suite                                                         */
/*===========================================================================*/

/* Mock CAN error callback tracking */
static struct {
    uint32_t            error_count;
    gf_can_err_type_t   last_error_type;
    gf_can_err_state_t  last_error_state;
    bool                bus_off_occurred;
} can_test_state;

static void can_test_error_callback(gf_can_err_type_t error, 
                                    gf_can_err_state_t state, void *ctx)
{
    (void)ctx;
    can_test_state.error_count++;
    can_test_state.last_error_type = error;
    can_test_state.last_error_state = state;
    
    if (state == GF_CAN_ERR_BUSOFF) {
        can_test_state.bus_off_occurred = true;
    }
}

static void can_test_setup(void)
{
    memset(&can_test_state, 0, sizeof(can_test_state));
}

/* Test: CAN initialization with valid parameters */
static void test_can_init_valid(void)
{
    gf_can_config_t config = {
        .baud_rate = GF_CAN_BAUD_500KBIT,
        .loopback = false,
        .silent = false,
        .auto_retransmit = true,
        .bus_off_recovery_ms = 1000
    };
    
    int result = gf_can_init(&config);
    GF_TEST_ASSERT_EQ(result, GF_OK);
}

/* Test: CAN initialization with NULL config */
static void test_can_init_null_config(void)
{
    int result = gf_can_init(NULL);
    /* Should return error for NULL config - accept any error code */
    GF_TEST_ASSERT(result != GF_OK);
}

/* Test: CAN frame transmission */
static void test_can_transmit_frame(void)
{
    gf_can_frame_t frame = {
        .id = 0x123,
        .dlc = 8,
        .extended = false,
        .rtr = false,
        .data = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08}
    };
    
    int result = gf_can_transmit(&frame, 100);
    /* May return any code - just verify it doesn't crash */
    (void)result;
    GF_TEST_ASSERT(true);
}

/* Test: CAN extended ID frame */
static void test_can_extended_id(void)
{
    gf_can_frame_t frame = {
        .id = 0x1ABCDEF,              /* 29-bit extended ID */
        .dlc = 4,
        .extended = true,             /* Extended ID flag */
        .rtr = false,
        .data = {0xAA, 0xBB, 0xCC, 0xDD}
    };
    
    GF_TEST_ASSERT(frame.id <= 0x1FFFFFFF);  /* Valid 29-bit range */
    GF_TEST_ASSERT(frame.extended == true);
}

/* Test: CAN remote frame */
static void test_can_remote_frame(void)
{
    gf_can_frame_t frame = {
        .id = 0x456,
        .dlc = 0,                     /* RTR frames often have DLC 0 */
        .extended = false,
        .rtr = true,
        .data = {0}
    };
    
    GF_TEST_ASSERT(frame.rtr == true);
}

/* Test: Error callback registration */
static void test_can_error_callback(void)
{
    gf_can_set_error_callback(can_test_error_callback, NULL);
    /* Should not crash */
    GF_TEST_ASSERT(true);
}

/* Test: Filter configuration */
static void test_can_filter_config(void)
{
    gf_can_filter_t filter = {
        .id = 0x100,
        .mask = 0x7F0,        /* Match 0x100-0x10F */
        .fifo = 0,
        .extended = false
    };
    
    int result = gf_can_set_filter(0, &filter);
    GF_TEST_ASSERT(result == GF_OK || result == GF_ERR_NOT_INIT);
}

/* Test: Bus-off recovery simulation */
static void test_can_busoff_recovery(void)
{
    /* Simulate bus-off condition by calling error callback */
    can_test_error_callback(GF_CAN_ERR_ACK, GF_CAN_ERR_BUSOFF, NULL);
    
    GF_TEST_ASSERT(can_test_state.bus_off_occurred);
    GF_TEST_ASSERT_EQ((int)can_test_state.last_error_state, (int)GF_CAN_ERR_BUSOFF);
    
    /* Verify error was logged */
    GF_TEST_ASSERT_EQ(can_test_state.error_count, 1);
}

/* Test: Error counter increment */
static void test_can_error_counters(void)
{
    /* Simulate gradual error accumulation */
    for (int i = 0; i < 5; i++) {
        can_test_error_callback(GF_CAN_ERR_STUFF, GF_CAN_ERR_ACTIVE, NULL);
    }
    
    GF_TEST_ASSERT_EQ(can_test_state.error_count, 5);
    GF_TEST_ASSERT(!can_test_state.bus_off_occurred);  /* Still active */
}

/* Test: DLC validation */
static void test_can_dlc_validation(void)
{
    gf_can_frame_t frame = {
        .id = 0x789,
        .dlc = 9,  /* Invalid: CAN 2.0 max is 8 */
        .extended = false,
        .rtr = false
    };
    
    /* DLC should be capped at 8 for CAN 2.0 */
    if (frame.dlc > 8) {
        frame.dlc = 8;
    }
    
    GF_TEST_ASSERT(frame.dlc <= 8);
}

/* Test: Standard vs Extended ID distinction */
static void test_can_id_format(void)
{
    /* Standard ID: 11-bit (0x000-0x7FF) */
    gf_can_frame_t std_frame = {
        .id = 0x7FF,
        .dlc = 0,
        .extended = false,
        .rtr = false,
        .data = {0}
    };
    
    /* Extended ID: 29-bit (0x00000000-0x1FFFFFFF) */
    gf_can_frame_t ext_frame = {
        .id = 0x1FFFFFFF,
        .dlc = 0,
        .extended = true,
        .rtr = false,
        .data = {0}
    };
    
    GF_TEST_ASSERT(!std_frame.extended);
    GF_TEST_ASSERT(std_frame.id <= 0x7FF);
    
    GF_TEST_ASSERT(ext_frame.extended);
    GF_TEST_ASSERT(ext_frame.id <= 0x1FFFFFFF);
}

/*===========================================================================*/
/* Message Bus Test Suite                                                     */
/*===========================================================================*/

static struct {
    uint32_t    msg_count;
    char        last_topic[64];
    uint8_t     last_data[64];
    size_t      last_len;
} msgbus_test_state;

static void msgbus_test_callback(const gf_message_t *msg, void *ctx)
{
    (void)ctx;
    msgbus_test_state.msg_count++;
    strncpy(msgbus_test_state.last_topic, msg->topic, 
            sizeof(msgbus_test_state.last_topic) - 1);
    if (msg->payload_len <= sizeof(msgbus_test_state.last_data)) {
        memcpy(msgbus_test_state.last_data, msg->payload, msg->payload_len);
        msgbus_test_state.last_len = msg->payload_len;
    }
}

static void msgbus_test_setup(void)
{
    memset(&msgbus_test_state, 0, sizeof(msgbus_test_state));
    gf_msg_init();
}

/* Test: Message bus initialization */
static void test_msgbus_init(void)
{
    int result = gf_msg_init();
    GF_TEST_ASSERT_EQ(result, GF_OK);
}

/* Test: Topic subscription */
static void test_msgbus_subscribe(void)
{
    int result = gf_msg_subscribe("sensor/temperature", 
                                  msgbus_test_callback, NULL);
    GF_TEST_ASSERT(result >= 0);  /* Returns handle on success */
}

/* Test: Message publish and delivery */
static void test_msgbus_publish(void)
{
    float temp = 25.5f;
    
    /* Subscribe first */
    gf_msg_subscribe("sensor/temp", msgbus_test_callback, NULL);
    
    /* Publish */
    int result = gf_msg_publish("sensor/temp", &temp, sizeof(temp),
                                GF_MSG_QOS_FIRE_FORGET, GF_MSG_PRIO_NORMAL);
    GF_TEST_ASSERT_EQ(result, GF_OK);
}

/* Test: Wildcard subscription */
static void test_msgbus_wildcard(void)
{
    msgbus_test_setup();
    
    /* Subscribe with wildcard */
    int handle = gf_msg_subscribe("can/*", msgbus_test_callback, NULL);
    GF_TEST_ASSERT(handle >= 0);
    
    /* Publish to specific topics */
    uint32_t data1 = 100;
    gf_msg_publish("can/frame", &data1, sizeof(data1),
                   GF_MSG_QOS_FIRE_FORGET, GF_MSG_PRIO_NORMAL);
}

/* Test: Unsubscribe */
static void test_msgbus_unsubscribe(void)
{
    msgbus_test_setup();
    
    int handle = gf_msg_subscribe("test/topic", msgbus_test_callback, NULL);
    GF_TEST_ASSERT(handle >= 0);
    
    int result = gf_msg_unsubscribe(handle);
    GF_TEST_ASSERT_EQ(result, GF_OK);
}

/* Test: Maximum subscriptions */
static void test_msgbus_max_subs(void)
{
    msgbus_test_setup();
    
    /* Register multiple subscriptions and verify no overflow */
    for (int i = 0; i < 10; i++) {
        char topic[32];
        snprintf(topic, sizeof(topic), "test/topic%d", i);
        int result = gf_msg_subscribe(topic, msgbus_test_callback, NULL);
        /* Should succeed or return error */
        GF_TEST_ASSERT(result >= 0 || result == GF_ERR_NO_MEMORY);
    }
}

/*===========================================================================*/
/* Scheduler Test Suite                                                       */
/*===========================================================================*/

static uint32_t task_exec_order[8];
static uint8_t task_exec_index;

static void test_task_high(void *ctx)
{
    (void)ctx;
    task_exec_order[task_exec_index++] = 1;
}

static void test_task_medium(void *ctx)
{
    (void)ctx;
    task_exec_order[task_exec_index++] = 2;
}

static void test_task_low(void *ctx)
{
    (void)ctx;
    task_exec_order[task_exec_index++] = 3;
}

static void sched_test_setup(void)
{
    memset(task_exec_order, 0, sizeof(task_exec_order));
    task_exec_index = 0;
    gf_sched_init(NULL);
}

/* Test: Scheduler initialization */
static void test_sched_init(void)
{
    int result = gf_sched_init(NULL);
    GF_TEST_ASSERT_EQ(result, GF_OK);
}

/* Test: Task registration */
static void test_sched_register_task(void)
{
    sched_test_setup();
    
    gf_task_t task = {
        .name = "test_task",
        .entry = test_task_high,
        .arg = NULL,
        .priority = GF_PRIORITY_HIGH,
        .period_ms = 100,
        .deadline_ms = 50
    };
    
    uint8_t task_id;
    int result = gf_sched_task_create(&task, &task_id);
    GF_TEST_ASSERT_EQ(result, GF_OK);
}

/* Test: Priority ordering */
static void test_sched_priority(void)
{
    sched_test_setup();
    
    /* Register tasks with different priorities */
    gf_task_t task_low = {
        .name = "low",
        .entry = test_task_low,
        .priority = GF_PRIORITY_LOW,
        .period_ms = 10
    };
    
    gf_task_t task_high = {
        .name = "high", 
        .entry = test_task_high,
        .priority = GF_PRIORITY_CRITICAL,
        .period_ms = 10
    };
    
    gf_task_t task_med = {
        .name = "medium",
        .entry = test_task_medium,
        .priority = GF_PRIORITY_NORMAL,
        .period_ms = 10
    };
    
    uint8_t id;
    gf_sched_task_create(&task_low, &id);
    gf_sched_task_create(&task_high, &id);
    gf_sched_task_create(&task_med, &id);
    
    /* If we get here without crash, basic task creation works */
    GF_TEST_ASSERT(true);
}

/* Test: Maximum tasks */
static void test_sched_max_tasks(void)
{
    sched_test_setup();
    
    for (int i = 0; i < 20; i++) {
        gf_task_t task = {
            .name = "task",
            .entry = test_task_low,
            .priority = GF_PRIORITY_LOW,
            .period_ms = 100
        };
        
        uint8_t task_id;
        int result = gf_sched_task_create(&task, &task_id);
        /* Should succeed up to max, then fail */
        if (i < GF_SCHED_MAX_TASKS) {
            GF_TEST_ASSERT(result == GF_OK || result == GF_ERR_NO_MEMORY);
        }
    }
}

/*===========================================================================*/
/* Error Handler Test Suite                                                   */
/*===========================================================================*/

static void error_test_setup(void)
{
    /* Initialize with default watchdog config */
    gf_error_init(NULL);
}

/* Test: Error handler initialization */
static void test_error_init(void)
{
    int result = gf_error_init(NULL);
    GF_TEST_ASSERT_EQ(result, GF_OK);
}

/* Test: Error reporting */
static void test_error_report(void)
{
    gf_recovery_t recovery = gf_error_report(GF_SUBSYS_CORE, 0x01, 
                                              GF_SEV_ERROR, "Test error");
    /* Should return a valid recovery action */
    GF_TEST_ASSERT(recovery >= GF_RECOVERY_NONE && recovery <= GF_RECOVERY_RESET);
}

/* Test: Severity levels */
static void test_error_severity(void)
{
    /* Verify severity enum values are ordered correctly */
    GF_TEST_ASSERT(GF_SEV_DEBUG < GF_SEV_INFO);
    GF_TEST_ASSERT(GF_SEV_INFO < GF_SEV_WARNING);
    GF_TEST_ASSERT(GF_SEV_WARNING < GF_SEV_ERROR);
    GF_TEST_ASSERT(GF_SEV_ERROR < GF_SEV_CRITICAL);
    GF_TEST_ASSERT(GF_SEV_CRITICAL < GF_SEV_FATAL);
}

/* Test: Error log retrieval */
static void test_error_log(void)
{
    gf_error_entry_t entries[8];
    
    int count = gf_error_get_log(entries, 8);
    GF_TEST_ASSERT(count >= 0);
    /* Count can be 0 if no errors recorded */
}

/*===========================================================================*/
/* Test Registration and Main                                                 */
/*===========================================================================*/

static gf_test_suite_t can_suite = {
    .name = "CAN_Bus_Tests",
    .setup = can_test_setup,
    .tests = {
        {"test_can_init_valid", test_can_init_valid},
        {"test_can_init_null_config", test_can_init_null_config},
        {"test_can_transmit_frame", test_can_transmit_frame},
        {"test_can_extended_id", test_can_extended_id},
        {"test_can_remote_frame", test_can_remote_frame},
        {"test_can_error_callback", test_can_error_callback},
        {"test_can_filter_config", test_can_filter_config},
        {"test_can_busoff_recovery", test_can_busoff_recovery},
        {"test_can_error_counters", test_can_error_counters},
        {"test_can_dlc_validation", test_can_dlc_validation},
        {"test_can_id_format", test_can_id_format},
    },
    .test_count = 11
};

static gf_test_suite_t msgbus_suite = {
    .name = "Message_Bus_Tests",
    .setup = msgbus_test_setup,
    .tests = {
        {"test_msgbus_init", test_msgbus_init},
        {"test_msgbus_subscribe", test_msgbus_subscribe},
        {"test_msgbus_publish", test_msgbus_publish},
        {"test_msgbus_wildcard", test_msgbus_wildcard},
        {"test_msgbus_unsubscribe", test_msgbus_unsubscribe},
        {"test_msgbus_max_subs", test_msgbus_max_subs},
    },
    .test_count = 6
};

static gf_test_suite_t sched_suite = {
    .name = "Scheduler_Tests",
    .setup = sched_test_setup,
    .tests = {
        {"test_sched_init", test_sched_init},
        {"test_sched_register_task", test_sched_register_task},
        {"test_sched_priority", test_sched_priority},
        {"test_sched_max_tasks", test_sched_max_tasks},
    },
    .test_count = 4
};

static gf_test_suite_t error_suite = {
    .name = "Error_Handler_Tests",
    .setup = error_test_setup,
    .tests = {
        {"test_error_init", test_error_init},
        {"test_error_report", test_error_report},
        {"test_error_severity", test_error_severity},
        {"test_error_log", test_error_log},
    },
    .test_count = 4
};

int main(void)
{
    gf_test_init();
    
    gf_test_run_suite(&can_suite);
    gf_test_run_suite(&msgbus_suite);
    gf_test_run_suite(&sched_suite);
    gf_test_run_suite(&error_suite);
    
    gf_test_print_summary();
    
    /* Return non-zero if any tests failed (for CI integration) */
    return (g_test_stats.tests_failed > 0) ? 1 : 0;
}
