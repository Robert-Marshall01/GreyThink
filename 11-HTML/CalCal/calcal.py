"""
IMPORTANT: AI has been mostly used to generate this code. Please review and test thoroughly.

This program calculates the total calorie intake based on user input for different food items.
Factors include:

NOTE: To run it on Visual Code you must,
1. Install Python extension
2. Change to the directory where this file is located
3. run "python calcal.py"

User inputs:
- weight
- height
- age
- gender
- activity level
"""
def calculate_bmr(weight, height, age, gender):
    if gender == "male":
        bmr = 66 + (6.23 * weight) + (12.7 * height) - (6.8 * age)
    else:
        bmr = 655 + (4.35 * weight) + (4.7 * height) - (4.7 * age)
    return bmr

def calculate_tdee(bmr, activity_level):
    activity_multipliers = {
        "sedentary": 1.2,
        "lightly active": 1.375,
        "moderately active": 1.55,
        "very active": 1.725,
        "extra active": 1.9
    }
    return bmr * activity_multipliers.get(activity_level, 1.2)

def calculate_total_calories(food_items):
    total_calories = sum(item.get("calories", 0) for item in food_items)
    return total_calories

