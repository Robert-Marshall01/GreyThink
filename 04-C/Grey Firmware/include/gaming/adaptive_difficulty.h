/**
 * @file adaptive_difficulty.h
 * @brief AI Inference Pipeline for Adaptive Game Difficulty
 * 
 * INDUSTRY RELEVANCE:
 * Dynamic difficulty adjustment (DDA) uses player behavior analysis
 * to maintain engagement. Modern games use ML models to predict
 * player skill, frustration, and optimal challenge levels. This
 * reduces churn and improves player satisfaction.
 * 
 * This module provides:
 * - Real-time skill estimation
 * - Difficulty curve adjustment
 * - Flow state optimization
 * - A/B testing integration
 * 
 * TECHNIQUES:
 * - Reinforcement learning for difficulty tuning
 * - Player modeling with neural networks
 * - Statistical skill rating (ELO, TrueSkill)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ADAPTIVE_DIFFICULTY_H
#define GF_ADAPTIVE_DIFFICULTY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    DIFFICULTY_VERY_EASY,
    DIFFICULTY_EASY,
    DIFFICULTY_NORMAL,
    DIFFICULTY_HARD,
    DIFFICULTY_VERY_HARD,
    DIFFICULTY_ADAPTIVE
} difficulty_level_t;

typedef enum {
    PLAYER_STATE_BORED,      /**< Under-challenged */
    PLAYER_STATE_FLOW,       /**< Optimal engagement */
    PLAYER_STATE_ANXIOUS,    /**< Over-challenged */
    PLAYER_STATE_UNKNOWN
} player_state_t;

typedef struct {
    float skill_rating;      /**< 0-100 estimated skill */
    float skill_confidence;  /**< 0-1 confidence */
    player_state_t state;
    float win_rate;
    float avg_completion_time;
    uint32_t deaths_per_level;
    float frustration_score;
} player_profile_t;

typedef struct {
    difficulty_level_t current;
    difficulty_level_t recommended;
    float enemy_health_mult;
    float enemy_damage_mult;
    float enemy_speed_mult;
    float resource_spawn_mult;
    float hint_frequency;
} difficulty_params_t;

int adaptive_init(void);
int adaptive_update_profile(uint32_t player_id, const player_profile_t *profile);
int adaptive_get_difficulty(uint32_t player_id, difficulty_params_t *params);
int adaptive_report_outcome(uint32_t player_id, bool success, float duration);
player_state_t adaptive_estimate_state(uint32_t player_id);

#ifdef __cplusplus
}
#endif

#endif /* GF_ADAPTIVE_DIFFICULTY_H */
