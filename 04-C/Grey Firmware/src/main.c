/**
 * @file main.c
 * @brief Grey Firmware - Main Entry Point
 * 
 * This is the main application entry point demonstrating the Grey Firmware
 * modular architecture. The firmware showcases:
 * 
 * 1. CORE FRAMEWORK
 *    - RTOS-style cooperative scheduler with priority-based execution
 *    - Dynamic driver registry with HAL abstraction
 *    - Publish/subscribe message bus for loose coupling
 *    - Comprehensive error handling with watchdog integration
 * 
 * 2. DOMAIN MODULES
 *    - IoT: MQTT 3.1.1 client, A/B OTA updates
 *    - Automotive: CAN 2.0B driver (SPOTLIGHT), watchdog timer
 *    - Consumer: USB HID, display driver
 *    - Medical: Sensor acquisition, calibration system
 *    - Security: Secure bootloader (SPOTLIGHT), firmware signing
 * 
 * 3. INTEGRATION DEMO
 *    - Sensor → CAN bus → MQTT → Cloud pipeline
 *    - Secure boot verification before operation
 *    - Cross-domain event handling via message bus
 * 
 * SPOTLIGHTS demonstrate production-grade implementation:
 * - CAN Bus Driver: Full protocol timing, TEC/REC error handling, bus-off recovery
 * - Secure Bootloader: ECDSA verification, chain of trust, anti-rollback
 */

#include "grey_firmware.h"
#include "demo/demo.h"
#include <stdio.h>

/*===========================================================================*/
/* Version Information                                                        */
/*===========================================================================*/

static void print_banner(void) {
    printf("\n");
    printf("╔════════════════════════════════════════════════════════════╗\n");
    printf("║                     GREY FIRMWARE v%d.%d.%d                    ║\n",
           GF_VERSION_MAJOR, GF_VERSION_MINOR, GF_VERSION_PATCH);
    printf("╠════════════════════════════════════════════════════════════╣\n");
    printf("║  Modular Firmware Framework Demonstration                  ║\n");
    printf("║                                                            ║\n");
    printf("║  Modules:                                                  ║\n");
    printf("║    [CORE] Scheduler, Driver Registry, Message Bus          ║\n");
    printf("║    [IOT]  MQTT Client, OTA Updates                         ║\n");
    printf("║    [AUTO] CAN 2.0B Driver*, Watchdog                       ║\n");
    printf("║    [MED]  Sensor Acquisition, Calibration                  ║\n");
    printf("║    [SEC]  Secure Bootloader*, Firmware Signing             ║\n");
    printf("║                                                            ║\n");
    printf("║  * Spotlight: Production-grade implementation              ║\n");
    printf("╚════════════════════════════════════════════════════════════╝\n");
    printf("\n");
}

/*===========================================================================*/
/* Demo Configuration                                                         */
/*===========================================================================*/

/* Default configuration for the integration demo */
static const gf_demo_config_t demo_config = {
    .mode               = GF_DEMO_MODE_SENSOR_CAN_MQTT,
    .sensor_period_ms   = 100,      /* 10 Hz sensor sampling */
    .can_baud_rate      = GF_CAN_BAUD_500KBIT,
    .mqtt_broker        = "localhost",
    .mqtt_port          = 1883,
    .mqtt_topic         = "grey_firmware/sensor/data",
    .require_secure_boot = true
};

/*===========================================================================*/
/* Status Display                                                             */
/*===========================================================================*/

static void print_status(const gf_demo_status_t *status) {
    printf("\n--- Demo Status ---\n");
    printf("  Running:          %s\n", status->running ? "YES" : "NO");
    printf("  Secure Boot OK:   %s\n", status->secure_boot_ok ? "YES" : "NO");
    printf("  Samples:          %u\n", status->samples_processed);
    printf("  CAN Frames:       %u\n", status->can_frames_sent);
    printf("  MQTT Messages:    %u\n", status->mqtt_messages_sent);
    printf("  Errors:           %u\n", status->errors);
    printf("  Last Value:       %.2f\n", status->last_sensor_value);
    printf("-------------------\n");
}

/*===========================================================================*/
/* Main Entry Point                                                           */
/*===========================================================================*/

int main(int argc, char *argv[]) {
    (void)argc;
    (void)argv;
    
    /* Display startup banner */
    print_banner();
    
    /* ========================================
     * STEP 1: SECURE BOOT VERIFICATION
     * ========================================
     * Before any operation, verify firmware integrity.
     * This is the foundation of the trusted execution model.
     */
    printf("[1/4] Verifying secure boot...\n");
    
    int result = gf_boot_init();
    if (result != 0) {
        printf("ERROR: Secure boot init failed (error %d)\n", result);
        printf("       System halted for security\n");
        return -1;
    }
    
    gf_boot_info_t boot_info;
    gf_boot_get_info(&boot_info);
    
    printf("      Boot slot: %s\n", boot_info.active_slot == 0 ? "A" : "B");
    printf("      Version:   %u.%u.%u\n",
           (boot_info.slot_a_version >> 24) & 0xFF,
           (boot_info.slot_a_version >> 16) & 0xFF,
           boot_info.slot_a_version & 0xFFFF);
    printf("      Confirmed: %s\n", boot_info.boot_confirmed ? "YES" : "NO");
    
    /* Confirm boot if not already done */
    if (!boot_info.boot_confirmed) {
        printf("      Confirming boot...\n");
        result = gf_boot_confirm();
        if (result != 0) {
            printf("ERROR: Boot confirmation failed\n");
            return -1;
        }
        printf("      Boot confirmed successfully\n");
    }
    
    /* ========================================
     * STEP 2: INITIALIZE DEMO
     * ========================================
     * Initialize all subsystems and create tasks.
     */
    printf("\n[2/4] Initializing demo...\n");
    
    result = gf_demo_init(&demo_config);
    if (result != 0) {
        printf("ERROR: Demo init failed (error %d)\n", result);
        return -1;
    }
    
    printf("      Scheduler:  initialized\n");
    printf("      Message Bus: initialized\n");
    printf("      CAN Bus:     500 kbit/s\n");
    printf("      MQTT:        %s:%u\n", demo_config.mqtt_broker, demo_config.mqtt_port);
    printf("      Sensors:     100 Hz\n");
    
    /* ========================================
     * STEP 3: START DEMO
     * ========================================
     * Begin data flow: Sensor → CAN → MQTT → Cloud
     */
    printf("\n[3/4] Starting demo...\n");
    
    result = gf_demo_start();
    if (result != 0) {
        printf("ERROR: Demo start failed (error %d)\n", result);
        return -1;
    }
    
    printf("      Demo running: Sensor → CAN → MQTT\n");
    printf("      Press Ctrl+C to stop\n");
    
    /* ========================================
     * STEP 4: MAIN LOOP
     * ========================================
     * Run scheduler and process events.
     */
    printf("\n[4/4] Entering main loop...\n\n");
    
    /* Simulated demo loop - run for limited iterations in demo mode */
    volatile uint32_t iterations = 0;
    const uint32_t max_iterations = 1000;  /* Demo limit */
    
    while (iterations < max_iterations) {
        /* Run scheduler for one loop */
        gf_sched_yield();
        
        /* Process message bus */
        gf_msg_process(1);
        
        /* Process CAN bus */
        gf_can_process();
        
        /* Process MQTT */
        gf_mqtt_process();
        
        /* Kick watchdog */
        gf_wdt_kick();
        
        /* Periodic status update */
        if ((iterations % 100) == 0 && iterations > 0) {
            gf_demo_status_t status;
            gf_demo_get_status(&status);
            printf("  [%4u] Samples: %u, CAN: %u, MQTT: %u, Errors: %u\n",
                   iterations,
                   status.samples_processed,
                   status.can_frames_sent,
                   status.mqtt_messages_sent,
                   status.errors);
        }
        
        iterations++;
    }
    
    /* ========================================
     * CLEANUP
     * ======================================== */
    printf("\n--- Demo Complete ---\n");
    
    gf_demo_status_t final_status;
    gf_demo_get_status(&final_status);
    print_status(&final_status);
    
    gf_demo_stop();
    
    printf("\nGrey Firmware demonstration completed successfully.\n");
    printf("See README.md for architecture details and build instructions.\n\n");
    
    return 0;
}
