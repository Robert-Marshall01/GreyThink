/**
 * @file nutritional_telemetry.h
 * @brief Nutritional Analysis and Telemetry Module
 * 
 * INDUSTRY RELEVANCE:
 * Modern food service and healthcare facilities require real-time nutritional
 * tracking for dietary compliance, allergen management, and caloric monitoring.
 * This module enables automated nutritional analysis integrated with cooking
 * and dispensing systems.
 * 
 * TECHNICAL SCOPE:
 * - Macro/micronutrient calculation
 * - Allergen flagging (14 major allergens)
 * - Portion size estimation via weight sensors
 * - Dietary restriction validation
 * - Cloud sync for meal planning platforms
 * 
 * USE CASES:
 * - Hospital meal tray systems
 * - School cafeteria compliance
 * - Commercial kitchen recipe costing
 * - Diet tracking wearable integration
 * 
 * STANDARDS COMPLIANCE:
 * - FDA 21 CFR 101 (Food labeling)
 * - EU FIC Regulation 1169/2011
 * - USDA FoodData Central compatibility
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_NUTRITIONAL_TELEMETRY_H
#define GF_NUTRITIONAL_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define NUTRI_MAX_INGREDIENTS   32    /**< Max ingredients per dish */
#define NUTRI_MAX_ALLERGENS     14    /**< Major allergen count */
#define NUTRI_HISTORY_MEALS     100   /**< Meal history retention */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Allergen flags (bitfield) */
typedef enum {
    ALLERGEN_GLUTEN     = (1 << 0),
    ALLERGEN_DAIRY      = (1 << 1),
    ALLERGEN_EGGS       = (1 << 2),
    ALLERGEN_FISH       = (1 << 3),
    ALLERGEN_SHELLFISH  = (1 << 4),
    ALLERGEN_TREE_NUTS  = (1 << 5),
    ALLERGEN_PEANUTS    = (1 << 6),
    ALLERGEN_SOY        = (1 << 7),
    ALLERGEN_WHEAT      = (1 << 8),
    ALLERGEN_SESAME     = (1 << 9),
    ALLERGEN_MUSTARD    = (1 << 10),
    ALLERGEN_CELERY     = (1 << 11),
    ALLERGEN_LUPIN      = (1 << 12),
    ALLERGEN_MOLLUSKS   = (1 << 13)
} allergen_t;

/** Dietary restriction type */
typedef enum {
    DIET_NONE,
    DIET_VEGETARIAN,
    DIET_VEGAN,
    DIET_HALAL,
    DIET_KOSHER,
    DIET_LOW_SODIUM,
    DIET_DIABETIC,
    DIET_RENAL,
    DIET_GLUTEN_FREE
} diet_type_t;

/** Nutritional profile per 100g */
typedef struct {
    float calories_kcal;
    float protein_g;
    float carbs_g;
    float fat_g;
    float fiber_g;
    float sugar_g;
    float sodium_mg;
    float cholesterol_mg;
    float saturated_fat_g;
    float trans_fat_g;
    float potassium_mg;
    float calcium_mg;
    float iron_mg;
    float vitamin_a_iu;
    float vitamin_c_mg;
    float vitamin_d_iu;
} nutrition_profile_t;

/** Meal analysis result */
typedef struct {
    nutrition_profile_t total;
    float portion_weight_g;
    uint16_t allergens_present;    /**< Bitfield of allergens */
    diet_type_t suitable_diets[8];
    uint8_t suitable_diet_count;
    bool compliant;
    char warnings[256];
} meal_analysis_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize nutritional telemetry */
int nutri_init(void);

/** Add ingredient to current meal */
int nutri_add_ingredient(uint32_t food_id, float weight_g);

/** Clear current meal */
int nutri_clear_meal(void);

/** Analyze current meal */
int nutri_analyze(meal_analysis_t *result);

/** Check allergen compliance for user */
bool nutri_check_allergens(uint16_t user_allergies);

/** Generate telemetry packet */
int nutri_generate_telemetry(uint8_t *buffer, size_t max_len);

/** Sync with cloud database */
int nutri_cloud_sync(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_NUTRITIONAL_TELEMETRY_H */
