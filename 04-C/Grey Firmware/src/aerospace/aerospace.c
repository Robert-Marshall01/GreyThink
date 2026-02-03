/**
 * @file aerospace.c
 * @brief Aerospace/Avionics Subsystem Implementation Stubs
 *
 * Stub implementations for sensor fusion, redundancy manager, and
 * fault-tolerant FSM. Demonstrates aerospace firmware patterns
 * per DO-178C guidelines.
 */

#include "aerospace/sensor_fusion.h"
#include "aerospace/redundancy_manager.h"
#include "aerospace/fault_tolerant_fsm.h"
#include <string.h>
#include <math.h>

/*===========================================================================*/
/* Sensor Fusion Implementation                                              */
/*===========================================================================*/

struct gf_sensor_fusion {
    gf_fusion_config_t config;
    gf_nav_solution_t solution;
    gf_fusion_health_t health;
    gf_imu_data_t last_imu;
    gf_gps_data_t last_gps;
    gf_baro_data_t last_baro;
    float gyro_bias[3];
    float accel_bias[3];
    bool initialized;
};

static struct gf_sensor_fusion s_fusion;

int gf_fusion_init(const gf_fusion_config_t* config,
                   gf_sensor_fusion_t* fusion)
{
    if (!config || !fusion) return -1;
    
    memset(&s_fusion, 0, sizeof(s_fusion));
    s_fusion.config = *config;
    
    /* Initialize quaternion to identity */
    s_fusion.solution.quaternion.w = 1.0f;
    s_fusion.health.filter_healthy = true;
    
    *fusion = &s_fusion;
    return 0;
}

int gf_fusion_set_initial_position(gf_sensor_fusion_t fusion,
                                    const gf_position_t* position)
{
    if (!fusion || !position) return -1;
    fusion->solution.position = *position;
    fusion->solution.position_valid = true;
    fusion->initialized = true;
    return 0;
}

int gf_fusion_update_imu(gf_sensor_fusion_t fusion,
                          const gf_imu_data_t* imu)
{
    if (!fusion || !imu) return -1;
    
    fusion->last_imu = *imu;
    fusion->health.imu_updates++;
    fusion->health.imu_healthy = imu->valid;
    
    if (imu->valid) {
        /* Stub: Simple complementary filter for attitude */
        float dt = 1.0f / fusion->config.imu_rate_hz;
        
        /* Integrate gyroscope */
        fusion->solution.angular_rate = imu->gyro;
        fusion->solution.euler.roll += imu->gyro.x * dt * 57.2958f;
        fusion->solution.euler.pitch += imu->gyro.y * dt * 57.2958f;
        fusion->solution.euler.yaw += imu->gyro.z * dt * 57.2958f;
        
        /* Update body acceleration */
        fusion->solution.accel_body = imu->accel;
        fusion->solution.load_factor = sqrtf(imu->accel.x * imu->accel.x +
                                             imu->accel.y * imu->accel.y +
                                             imu->accel.z * imu->accel.z) / 9.81f;
        
        fusion->solution.attitude_valid = true;
        fusion->solution.timestamp_us = imu->timestamp_us;
    }
    
    return 0;
}

int gf_fusion_update_gps(gf_sensor_fusion_t fusion,
                          const gf_gps_data_t* gps)
{
    if (!fusion || !gps) return -1;
    
    fusion->last_gps = *gps;
    fusion->health.gps_updates++;
    fusion->health.gps_healthy = gps->valid && gps->fix_type >= 3;
    
    if (gps->valid && gps->fix_type >= 3) {
        fusion->solution.position = gps->position;
        fusion->solution.velocity = gps->velocity;
        fusion->solution.ground_speed = sqrtf(gps->velocity.north * gps->velocity.north +
                                              gps->velocity.east * gps->velocity.east);
        fusion->solution.vertical_speed = -gps->velocity.down;
        fusion->solution.course = atan2f(gps->velocity.east, gps->velocity.north) * 57.2958f;
        if (fusion->solution.course < 0) fusion->solution.course += 360.0f;
        
        fusion->solution.position_valid = true;
        fusion->solution.position_sigma = gps->hdop * 2.5f;
    }
    
    return 0;
}

int gf_fusion_update_baro(gf_sensor_fusion_t fusion,
                           const gf_baro_data_t* baro)
{
    if (!fusion || !baro) return -1;
    
    fusion->last_baro = *baro;
    fusion->health.baro_healthy = baro->valid;
    
    if (baro->valid && fusion->config.use_barometer) {
        /* Use baro altitude for vertical position */
        fusion->solution.position.altitude = baro->altitude_m;
    }
    
    return 0;
}

int gf_fusion_get_solution(gf_sensor_fusion_t fusion,
                            gf_nav_solution_t* solution)
{
    if (!fusion || !solution) return -1;
    *solution = fusion->solution;
    return 0;
}

int gf_fusion_get_health(gf_sensor_fusion_t fusion,
                          gf_fusion_health_t* health)
{
    if (!fusion || !health) return -1;
    *health = fusion->health;
    return 0;
}

int gf_fusion_reset(gf_sensor_fusion_t fusion)
{
    if (!fusion) return -1;
    
    gf_fusion_config_t config = fusion->config;
    memset(fusion, 0, sizeof(struct gf_sensor_fusion));
    fusion->config = config;
    fusion->solution.quaternion.w = 1.0f;
    fusion->health.filter_resets++;
    
    return 0;
}

void gf_fusion_deinit(gf_sensor_fusion_t fusion)
{
    if (fusion) {
        memset(fusion, 0, sizeof(struct gf_sensor_fusion));
    }
}

void gf_quat_to_euler(const gf_quat_t* q, gf_euler_t* euler)
{
    if (!q || !euler) return;
    
    /* Roll */
    float sinr = 2.0f * (q->w * q->x + q->y * q->z);
    float cosr = 1.0f - 2.0f * (q->x * q->x + q->y * q->y);
    euler->roll = atan2f(sinr, cosr) * 57.2958f;
    
    /* Pitch */
    float sinp = 2.0f * (q->w * q->y - q->z * q->x);
    if (fabsf(sinp) >= 1.0f) {
        euler->pitch = copysignf(90.0f, sinp);
    } else {
        euler->pitch = asinf(sinp) * 57.2958f;
    }
    
    /* Yaw */
    float siny = 2.0f * (q->w * q->z + q->x * q->y);
    float cosy = 1.0f - 2.0f * (q->y * q->y + q->z * q->z);
    euler->yaw = atan2f(siny, cosy) * 57.2958f;
}

void gf_euler_to_quat(const gf_euler_t* euler, gf_quat_t* q)
{
    if (!euler || !q) return;
    
    float roll = euler->roll * 0.0174533f / 2.0f;
    float pitch = euler->pitch * 0.0174533f / 2.0f;
    float yaw = euler->yaw * 0.0174533f / 2.0f;
    
    float cr = cosf(roll), sr = sinf(roll);
    float cp = cosf(pitch), sp = sinf(pitch);
    float cy = cosf(yaw), sy = sinf(yaw);
    
    q->w = cr * cp * cy + sr * sp * sy;
    q->x = sr * cp * cy - cr * sp * sy;
    q->y = cr * sp * cy + sr * cp * sy;
    q->z = cr * cp * sy - sr * sp * cy;
}

void gf_quat_rotate(const gf_quat_t* q, const gf_vec3_t* v, gf_vec3_t* result)
{
    if (!q || !v || !result) return;
    
    /* Quaternion rotation: q * v * q^-1 */
    float qv_w = -q->x * v->x - q->y * v->y - q->z * v->z;
    float qv_x =  q->w * v->x + q->y * v->z - q->z * v->y;
    float qv_y =  q->w * v->y + q->z * v->x - q->x * v->z;
    float qv_z =  q->w * v->z + q->x * v->y - q->y * v->x;
    
    result->x = -qv_w * q->x + qv_x * q->w - qv_y * q->z + qv_z * q->y;
    result->y = -qv_w * q->y + qv_y * q->w - qv_z * q->x + qv_x * q->z;
    result->z = -qv_w * q->z + qv_z * q->w - qv_x * q->y + qv_y * q->x;
}

void gf_quat_normalize(gf_quat_t* q)
{
    if (!q) return;
    
    float norm = sqrtf(q->w * q->w + q->x * q->x + q->y * q->y + q->z * q->z);
    if (norm > 0.0001f) {
        q->w /= norm;
        q->x /= norm;
        q->y /= norm;
        q->z /= norm;
    }
}

/*===========================================================================*/
/* Redundancy Manager Implementation                                         */
/*===========================================================================*/

struct gf_tmr_mgr {
    gf_tmr_config_t config;
    gf_channel_data_t channels[GF_REDUNDANCY_MAX_CHANNELS];
    gf_tmr_status_t status;
};

static struct gf_tmr_mgr s_redundancy;

int gf_tmr_init(const gf_tmr_config_t* config,
                        gf_tmr_mgr_t* mgr)
{
    if (!config || !mgr) return -1;
    if (config->channel_count < 2 || config->channel_count > GF_REDUNDANCY_MAX_CHANNELS) {
        return -2;
    }
    
    memset(&s_redundancy, 0, sizeof(s_redundancy));
    s_redundancy.config = *config;
    s_redundancy.status.system_healthy = true;
    s_redundancy.status.healthy_channels = config->channel_count;
    
    *mgr = &s_redundancy;
    return 0;
}

int gf_tmr_update(gf_tmr_mgr_t mgr,
                          uint8_t channel_id,
                          const gf_channel_data_t* data)
{
    if (!mgr || !data) return -1;
    if (channel_id >= mgr->config.channel_count) return -2;
    
    mgr->channels[channel_id] = *data;
    return 0;
}

int gf_tmr_vote(gf_tmr_mgr_t mgr,
                        gf_voted_result_t* result)
{
    if (!mgr || !result) return -1;
    
    memset(result, 0, sizeof(*result));
    
    /* Collect valid channels */
    float values[GF_REDUNDANCY_MAX_CHANNELS];
    uint8_t valid_count = 0;
    
    for (uint8_t i = 0; i < mgr->config.channel_count; i++) {
        if (mgr->channels[i].valid && 
            mgr->channels[i].health == GF_CHANNEL_HEALTHY) {
            values[valid_count++] = mgr->channels[i].value;
        } else {
            result->failed_channels |= (1 << i);
        }
    }
    
    result->valid_channels = valid_count;
    mgr->status.votes_performed++;
    
    if (valid_count == 0) return -1;
    
    /* Voting based on scheme */
    switch (mgr->config.vote_scheme) {
        case GF_VOTE_MAJORITY:
        case GF_VOTE_MEDIAN:
            /* Simple sort for median */
            for (uint8_t i = 0; i < valid_count - 1; i++) {
                for (uint8_t j = i + 1; j < valid_count; j++) {
                    if (values[j] < values[i]) {
                        float tmp = values[i];
                        values[i] = values[j];
                        values[j] = tmp;
                    }
                }
            }
            result->value = values[valid_count / 2];
            break;
            
        case GF_VOTE_AVERAGE:
        case GF_VOTE_WEIGHTED:
        default: {
            float sum = 0;
            for (uint8_t i = 0; i < valid_count; i++) {
                sum += values[i];
            }
            result->value = sum / valid_count;
            break;
        }
        
        case GF_VOTE_FIRST_VALID:
            result->value = values[0];
            break;
    }
    
    /* Check consensus */
    result->consensus = true;
    for (uint8_t i = 0; i < valid_count; i++) {
        float diff = fabsf(values[i] - result->value);
        if (diff > mgr->config.agreement_threshold) {
            result->consensus = false;
            mgr->status.disagreements++;
            break;
        }
    }
    
    result->confidence = (float)valid_count / mgr->config.channel_count;
    
    return 0;
}

int gf_tmr_set_health(gf_tmr_mgr_t mgr,
                              uint8_t channel_id,
                              gf_channel_health_t health)
{
    if (!mgr || channel_id >= mgr->config.channel_count) return -1;
    
    mgr->status.channel_health[channel_id] = health;
    
    /* Recalculate healthy count */
    mgr->status.healthy_channels = 0;
    for (uint8_t i = 0; i < mgr->config.channel_count; i++) {
        if (mgr->status.channel_health[i] == GF_CHANNEL_HEALTHY) {
            mgr->status.healthy_channels++;
        }
    }
    
    mgr->status.system_healthy = (mgr->status.healthy_channels >= 2);
    mgr->status.degraded_mode = (mgr->status.healthy_channels < mgr->config.channel_count);
    
    return 0;
}

int gf_tmr_failover(gf_tmr_mgr_t mgr, uint8_t channel_id)
{
    if (!mgr || channel_id >= mgr->config.channel_count) return -1;
    
    mgr->status.active_channel = channel_id;
    mgr->status.failovers++;
    
    return 0;
}

int gf_tmr_get_status(gf_tmr_mgr_t mgr,
                              gf_tmr_status_t* status)
{
    if (!mgr || !status) return -1;
    *status = mgr->status;
    return 0;
}

int gf_tmr_cross_check(gf_tmr_mgr_t mgr, uint8_t* mismatches)
{
    if (!mgr || !mismatches) return -1;
    
    *mismatches = 0;
    for (uint8_t i = 0; i < mgr->config.channel_count - 1; i++) {
        for (uint8_t j = i + 1; j < mgr->config.channel_count; j++) {
            if (mgr->channels[i].valid && mgr->channels[j].valid) {
                float diff = fabsf(mgr->channels[i].value - mgr->channels[j].value);
                if (diff > mgr->config.agreement_threshold) {
                    (*mismatches)++;
                }
            }
        }
    }
    
    return 0;
}

void gf_tmr_deinit(gf_tmr_mgr_t mgr)
{
    if (mgr) {
        memset(mgr, 0, sizeof(struct gf_tmr_mgr));
    }
}

/*===========================================================================*/
/* Fault-Tolerant FSM Implementation                                         */
/*===========================================================================*/

struct gf_fault_tolerant_fsm {
    gf_fsm_config_t config;
    gf_fsm_status_t status;
    bool started;
};

static struct gf_fault_tolerant_fsm s_fsm;

int gf_fsm_init(const gf_fsm_config_t* config, gf_fsm_t* fsm)
{
    if (!config || !fsm) return -1;
    
    memset(&s_fsm, 0, sizeof(s_fsm));
    s_fsm.config = *config;
    
    *fsm = &s_fsm;
    return 0;
}

int gf_fsm_start(gf_fsm_t fsm)
{
    if (!fsm) return -1;
    
    fsm->status.current_state = fsm->config.initial_state;
    fsm->status.previous_state = fsm->config.initial_state;
    fsm->started = true;
    
    /* Execute entry action */
    for (uint8_t i = 0; i < fsm->config.state_count; i++) {
        if (fsm->config.states[i].id == fsm->status.current_state) {
            if (fsm->config.states[i].on_entry) {
                fsm->config.states[i].on_entry(fsm->config.context);
            }
            fsm->status.in_safe_state = fsm->config.states[i].is_safe_state;
            break;
        }
    }
    
    return 0;
}

int gf_fsm_process_event(gf_fsm_t fsm, gf_event_id_t event)
{
    if (!fsm || !fsm->started) return -1;
    
    /* Find matching transition */
    for (uint8_t i = 0; i < fsm->config.transition_count; i++) {
        const gf_fsm_transition_t* trans = &fsm->config.transitions[i];
        
        if (trans->from_state == fsm->status.current_state &&
            trans->event == event) {
            
            /* Check guard */
            if (trans->guard && !trans->guard(fsm->config.context)) {
                fsm->status.guard_failures++;
                continue;
            }
            
            /* Execute exit action */
            for (uint8_t j = 0; j < fsm->config.state_count; j++) {
                if (fsm->config.states[j].id == fsm->status.current_state) {
                    if (fsm->config.states[j].on_exit) {
                        fsm->config.states[j].on_exit(fsm->config.context);
                    }
                    break;
                }
            }
            
            /* Execute transition action */
            if (trans->action) {
                trans->action(fsm->config.context);
            }
            
            /* Update state */
            fsm->status.previous_state = fsm->status.current_state;
            fsm->status.current_state = trans->to_state;
            fsm->status.transition_count++;
            fsm->status.state_dwell_ms = 0;
            
            /* Execute entry action */
            for (uint8_t j = 0; j < fsm->config.state_count; j++) {
                if (fsm->config.states[j].id == fsm->status.current_state) {
                    if (fsm->config.states[j].on_entry) {
                        fsm->config.states[j].on_entry(fsm->config.context);
                    }
                    fsm->status.in_safe_state = fsm->config.states[j].is_safe_state;
                    break;
                }
            }
            
            /* Record history */
            uint8_t idx = fsm->status.history_index % GF_FSM_MAX_HISTORY;
            fsm->status.history[idx].from_state = fsm->status.previous_state;
            fsm->status.history[idx].to_state = fsm->status.current_state;
            fsm->status.history[idx].event = event;
            fsm->status.history[idx].fault_transition = false;
            fsm->status.history_index++;
            
            /* Notify callback */
            if (fsm->config.on_transition) {
                fsm->config.on_transition(fsm->status.previous_state,
                                          fsm->status.current_state,
                                          fsm->config.context);
            }
            
            return 0;
        }
    }
    
    return 1;  /* No transition taken */
}

int gf_fsm_run(gf_fsm_t fsm, uint32_t current_time_ms)
{
    (void)current_time_ms;
    if (!fsm || !fsm->started) return -1;
    
    /* Run do_activity for current state */
    for (uint8_t i = 0; i < fsm->config.state_count; i++) {
        if (fsm->config.states[i].id == fsm->status.current_state) {
            if (fsm->config.states[i].do_activity) {
                fsm->config.states[i].do_activity(fsm->config.context);
            }
            break;
        }
    }
    
    return 0;
}

gf_state_id_t gf_fsm_get_state(gf_fsm_t fsm)
{
    return fsm ? fsm->status.current_state : 0;
}

bool gf_fsm_is_in_state(gf_fsm_t fsm, gf_state_id_t state)
{
    return fsm ? (fsm->status.current_state == state) : false;
}

int gf_fsm_enter_safe_state(gf_fsm_t fsm, int fault_code)
{
    if (!fsm) return -1;
    
    fsm->status.previous_state = fsm->status.current_state;
    fsm->status.current_state = fsm->config.safe_state;
    fsm->status.fault_count++;
    fsm->status.last_fault_code = fault_code;
    fsm->status.in_safe_state = true;
    
    if (fsm->config.on_fault) {
        fsm->config.on_fault(fsm->status.previous_state, fault_code, 
                             fsm->config.context);
    }
    
    return 0;
}

int gf_fsm_reset(gf_fsm_t fsm)
{
    if (!fsm) return -1;
    
    gf_fsm_config_t config = fsm->config;
    memset(&fsm->status, 0, sizeof(fsm->status));
    fsm->config = config;
    fsm->started = false;
    
    return 0;
}

int gf_fsm_get_status(gf_fsm_t fsm, gf_fsm_status_t* status)
{
    if (!fsm || !status) return -1;
    *status = fsm->status;
    return 0;
}

bool gf_fsm_can_transition(gf_fsm_t fsm, gf_event_id_t event)
{
    if (!fsm || !fsm->started) return false;
    
    for (uint8_t i = 0; i < fsm->config.transition_count; i++) {
        const gf_fsm_transition_t* trans = &fsm->config.transitions[i];
        
        if (trans->from_state == fsm->status.current_state &&
            trans->event == event) {
            if (trans->guard) {
                return trans->guard(fsm->config.context);
            }
            return true;
        }
    }
    
    return false;
}

int gf_fsm_get_history(gf_fsm_t fsm,
                        gf_fsm_history_t* history,
                        uint8_t max_entries,
                        uint8_t* out_count)
{
    if (!fsm || !history || !out_count) return -1;
    
    uint8_t count = (fsm->status.history_index < max_entries) ? 
                    fsm->status.history_index : max_entries;
    
    for (uint8_t i = 0; i < count; i++) {
        history[i] = fsm->status.history[i];
    }
    *out_count = count;
    
    return 0;
}

void gf_fsm_deinit(gf_fsm_t fsm)
{
    if (fsm) {
        memset(fsm, 0, sizeof(struct gf_fault_tolerant_fsm));
    }
}

int gf_fsm_verify_config(const gf_fsm_config_t* config)
{
    if (!config) return -1;
    if (config->state_count == 0 || config->transition_count == 0) return -2;
    return 0;
}

int gf_fsm_check_reachability(const gf_fsm_config_t* config,
                               uint32_t* unreachable)
{
    (void)config; (void)unreachable;
    return 0;  /* Stub: all reachable */
}

int gf_fsm_check_safe_reachability(const gf_fsm_config_t* config,
                                    uint32_t* unsafe)
{
    (void)config; (void)unsafe;
    return 0;  /* Stub: all can reach safe */
}
