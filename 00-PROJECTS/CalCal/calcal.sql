/*VERY-IMPORTANT: GitHub Copilot AI-Generated Code, please review and test thoroughly*/
/*to use this, run it in a SQLite environment to create the necessary tables for the CalCal application*/
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS foods (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL,
    foodName TEXT,
    calories INTEGER,
    protein INTEGER,
    fats INTEGER,
    cholesterol INTEGER,
    carbs INTEGER,
    sugar INTEGER,
    fiber INTEGER,
    sodium INTEGER
);

CREATE TABLE IF NOT EXISTS nutritional_goals (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL,
    weightGoal TEXT,
    caloriesValue INTEGER,
    proteinValue INTEGER,
    fatsValue INTEGER,
    cholesterolValue INTEGER,
    carbsValue INTEGER,
    sugarValue INTEGER,
    fiberValue INTEGER,
    sodiumValue INTEGER
);

CREATE TABLE IF NOT EXISTS settings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL,
    theme TEXT,
    fontSize TEXT,
    contrast INTEGER,
    dyslexia INTEGER,
    weight TEXT,
    height TEXT,
    age TEXT,
    gender TEXT,
    activity TEXT,
    units TEXT
);
