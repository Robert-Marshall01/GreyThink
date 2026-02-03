# Power Management System

## Overview

Grey Firmware includes a comprehensive power management subsystem implementing
industry-standard low-power patterns for battery-powered and energy-conscious
embedded systems.

## Power States

The power manager implements a four-state power model:

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Power State Diagram                         │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│   ┌─────────┐    idle     ┌─────────┐    sleep    ┌─────────┐     │
│   │   RUN   │ ──────────► │  IDLE   │ ──────────► │  SLEEP  │     │
│   │ 50+ mA  │             │  2 mA   │             │  50 µA  │     │
│   └────▲────┘             └────▲────┘             └────▲────┘     │
│        │                       │                       │           │
│        │ interrupt             │ wake                  │           │
│        │                       │                       │           │
│        └───────────────────────┴───────────────────────┘           │
│                                                                     │
│                    ┌──────────────┐                                 │
│                    │  DEEP SLEEP  │                                 │
│                    │    5 µA      │                                 │
│                    └──────────────┘                                 │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

### State Characteristics

| State      | Current   | Wake Time | RAM    | Peripherals    |
|------------|-----------|-----------|--------|----------------|
| RUN        | 50-100 mA | N/A       | Active | All available  |
| IDLE       | 1-5 mA    | <10 µs    | Active | All available  |
| SLEEP      | 10-100 µA | 100-500 µs| Retained | Most off     |
| DEEP SLEEP | 1-10 µA   | 1-10 ms   | Optional | All off      |

## Wake Sources

Configurable wake sources allow the system to return from low-power states:

### GPIO Wake
```c
gf_wake_gpio_config_t gpio_cfg = {
    .gpio_pin = 5,
    .edge = GF_WAKE_EDGE_FALLING,
    .enable_pullup = true
};
gf_power_configure_wake_gpio(&gpio_cfg);
gf_power_enable_wake_source(GF_WAKE_GPIO, true);
```

### Timer Wake (Periodic Sampling)
```c
gf_wake_timer_config_t timer_cfg = {
    .timeout_ms = 60000,    // Wake every 60 seconds
    .periodic = true
};
gf_power_configure_wake_timer(&timer_cfg);
gf_power_enable_wake_source(GF_WAKE_TIMER, true);
```

### RTC Wake (Scheduled Tasks)
```c
gf_wake_rtc_config_t rtc_cfg = {
    .hour = 8,
    .minute = 0,
    .second = 0,
    .daily_repeat = true
};
gf_power_configure_wake_rtc(&rtc_cfg);
gf_power_enable_wake_source(GF_WAKE_RTC, true);
```

## Power Locks

Power locks prevent the system from entering low-power states during
critical operations:

```c
// Lock to RUN state during radio transmission
uint32_t lock = gf_power_lock(GF_POWER_STATE_RUN);

// Perform radio operation...
radio_transmit(data, len);

// Release lock when done
gf_power_unlock(lock);
// System can now enter requested low-power state
```

## Clock Gating

Fine-grained clock control for peripheral power optimization:

```c
// Disable unused peripheral clocks
gf_power_set_clock(GF_CLOCK_USB, false);
gf_power_set_clock(GF_CLOCK_CRYPTO, false);
gf_power_set_clock(GF_CLOCK_DMA, false);

// Scale CPU frequency for low-power operation
gf_power_set_cpu_freq(8000000);  // 8 MHz instead of 48 MHz
```

## Peripheral Power Domains

Independent power domains for subsystem isolation:

```c
// Power off radio when not in use
gf_power_set_domain(GF_POWER_DOMAIN_RADIO, false);

// Check domain status
if (gf_power_is_domain_on(GF_POWER_DOMAIN_ANALOG)) {
    // ADC available
}
```

## Scheduler Integration

The power manager integrates with the scheduler for automatic
power state management:

```c
// In scheduler idle hook
void scheduler_idle_hook(void) {
    gf_power_enter_idle();
}

// Before extended sleep
uint32_t max_sleep = gf_power_prepare_sleep();
if (max_sleep > 0) {
    // Enter sleep mode
    __WFI();
    // After wake
    gf_power_restore_from_sleep();
}
```

## Energy Profiling

Built-in energy profiling for optimization:

```c
// Register profiling callback
void on_profile(const gf_power_profile_t *profile, void *ctx) {
    printf("State: %d, CPU: %u MHz, Current: %u µA\n",
           profile->state, profile->cpu_freq_mhz,
           profile->estimated_ua);
}

gf_power_set_profile_callback(on_profile, NULL);

// Take manual snapshot
gf_power_profile_t snapshot;
gf_power_take_profile_snapshot(&snapshot);
```

## Statistics

Track power behavior for optimization:

```c
gf_power_status_t status;
gf_power_get_status(&status);

printf("Time in RUN:        %u ms\n", status.time_in_run_ms);
printf("Time in IDLE:       %u ms\n", status.time_in_idle_ms);
printf("Time in SLEEP:      %u ms\n", status.time_in_sleep_ms);
printf("Time in DEEP_SLEEP: %u ms\n", status.time_in_deep_sleep_ms);
printf("Wake count:         %u\n", status.wake_count);
```

## Best Practices

1. **Lock Sparingly**: Only hold power locks during critical operations
2. **Configure Wake Sources Early**: Set up all wake sources during init
3. **Use Domains**: Power off unused subsystems rather than individual clocks
4. **Profile Often**: Use energy profiling to find optimization opportunities
5. **Test State Transitions**: Verify correct behavior in all power states

## Related Modules

- [Scheduler](integration.md#scheduler) - Work queue and timing
- [Battery Management](battery.md) - SOC and charging integration
- [Thermal Management](thermal.md) - Temperature-based throttling
