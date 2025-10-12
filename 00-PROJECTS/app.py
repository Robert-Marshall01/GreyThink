#VERY-IMPORTANT: GitHub Copilot AI-Generated Code, please review and test thoroughly
from flask import Flask, request, jsonify, send_from_directory
import sqlite3
import hashlib
from flask_cors import CORS

app = Flask(__name__)
CORS(app)

# Endpoint to delete a user and all related data
@app.route('/delete_user', methods=['POST'])
def delete_user():
    data = request.json
    username = data.get('username')
    if not username:
        return jsonify({'success': False, 'error': 'Missing username'}), 400
    conn = get_db()
    try:
        # Delete from users table
        conn.execute('DELETE FROM users WHERE username=?', (username,))
        # Delete from settings table
        conn.execute('DELETE FROM settings WHERE username=?', (username,))
        # Delete from foods table
        conn.execute('DELETE FROM foods WHERE username=?', (username,))
        # Delete from nutritional_goals table
        conn.execute('DELETE FROM nutritional_goals WHERE username=?', (username,))
        conn.commit()
        return jsonify({'success': True})
    except Exception as e:
        return jsonify({'success': False, 'error': str(e)}), 500
def get_db():
    conn = sqlite3.connect('calcal.db')
    conn.row_factory = sqlite3.Row
    return conn

# Real-time username availability check
@app.route('/check_username', methods=['POST'])
def check_username():
    data = request.json
    username = data.get('username')
    if not username:
        return jsonify({'available': False, 'error': 'No username provided'}), 400
    conn = get_db()
    user = conn.execute('SELECT * FROM users WHERE username=?', (username,)).fetchone()
    if user:
        return jsonify({'available': False})
    else:
        return jsonify({'available': True})

@app.route('/register', methods=['POST'])
def register():
    data = request.json
    username = data.get('username')
    password = data.get('password')
    weight = data.get('weight')
    height = data.get('height')
    age = data.get('age')
    gender = data.get('gender')
    activity = data.get('activity')
    units = data.get('units')
    theme = data.get('theme')
    fontSize = data.get('fontSize')
    contrast = data.get('contrast')
    dyslexia = data.get('dyslexia')
    if not username or not password:
        return jsonify({'success': False, 'error': 'Missing username or password'}), 400
    password_hash = hashlib.sha256(password.encode()).hexdigest()
    conn = get_db()
    try:
        conn.execute('INSERT INTO users (username, password_hash) VALUES (?, ?)', (username, password_hash))
        # Save user info to settings table
        conn.execute('''
            INSERT INTO settings (username, theme, fontSize, contrast, dyslexia, weight, height, age, gender, activity, units)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(username) DO UPDATE SET theme=excluded.theme, fontSize=excluded.fontSize, contrast=excluded.contrast, dyslexia=excluded.dyslexia,
                weight=excluded.weight, height=excluded.height, age=excluded.age, gender=excluded.gender, activity=excluded.activity, units=excluded.units
        ''', (username, theme, fontSize, contrast, dyslexia, weight, height, age, gender, activity, units))
        conn.commit()
        return jsonify({'success': True})
    except sqlite3.IntegrityError:
        return jsonify({'success': False, 'error': 'Username already exists'}), 400

@app.route('/login', methods=['POST'])
def login():
    data = request.json
    username = data.get('username')
    password = data.get('password')
    password_hash = hashlib.sha256(password.encode()).hexdigest()
    conn = get_db()
    user = conn.execute('SELECT * FROM users WHERE username=? AND password_hash=?', (username, password_hash)).fetchone()
    if user:
        return jsonify({'success': True})
    else:
        return jsonify({'success': False, 'error': 'Invalid credentials'}), 401


# Example endpoint for saving food items
@app.route('/add_food', methods=['POST'])
def add_food():
    data = request.json
    username = data.get('username')
    food = data.get('food')
    if not username or not food:
        return jsonify({'success': False, 'error': 'Missing data'}), 400
    conn = get_db()
    conn.execute('INSERT INTO foods (username, foodName, calories, protein, fats, cholesterol, carbs, sugar, fiber, sodium) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)',
                 (username, food['foodName'], food['calories'], food['protein'], food['fats'], food['cholesterol'], food['carbs'], food['sugar'], food['fiber'], food['sodium']))
    conn.commit()
    return jsonify({'success': True})

# Endpoint to delete a food item by name for a user
@app.route('/delete_food', methods=['POST'])
def delete_food():
    data = request.json
    username = data.get('username')
    foodName = data.get('foodName')
    if not username or not foodName:
        return jsonify({'success': False, 'error': 'Missing username or food name'}), 400
    conn = get_db()
    cur = conn.execute('DELETE FROM foods WHERE username=? AND foodName=?', (username, foodName))
    conn.commit()
    if cur.rowcount > 0:
        return jsonify({'success': True})
    else:
        return jsonify({'success': False, 'error': 'Food item not found'})

from flask import send_from_directory

# Serve static files (HTML, CSS, JS) from project root
@app.route('/<path:filename>')
def serve_static(filename):
    return send_from_directory('.', filename)

def init_db():
    with open('calcal.sql', 'r') as f:
        sql_script = f.read()
    conn = sqlite3.connect('calcal.db')
    conn.executescript(sql_script)
    conn.commit()
    conn.close()



# Endpoint to update user settings (save all user info fields)
@app.route('/update_settings', methods=['POST'])
def update_settings():
    data = request.json
    username = data.get('username')
    weight = data.get('weight')
    height = data.get('height')
    age = data.get('age')
    gender = data.get('gender')
    activity = data.get('activity')
    units = data.get('units')
    theme = data.get('theme')
    fontSize = data.get('fontSize')
    contrast = data.get('contrast')
    dyslexia = data.get('dyslexia')
    password = data.get('password')
    if not username:
        return jsonify({'success': False, 'error': 'Missing username'}), 400
    conn = get_db()
    try:
        # Update password if provided
        if password:
            password_hash = hashlib.sha256(password.encode()).hexdigest()
            conn.execute('UPDATE users SET password_hash=? WHERE username=?', (password_hash, username))
        # Insert or update settings for the user
        conn.execute('''
            INSERT INTO settings (username, theme, fontSize, contrast, dyslexia, weight, height, age, gender, activity, units)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(username) DO UPDATE SET theme=excluded.theme, fontSize=excluded.fontSize, contrast=excluded.contrast, dyslexia=excluded.dyslexia,
                weight=excluded.weight, height=excluded.height, age=excluded.age, gender=excluded.gender, activity=excluded.activity, units=excluded.units
        ''', (username, theme, fontSize, contrast, dyslexia, weight, height, age, gender, activity, units))
        conn.commit()
    except Exception as e:
        return jsonify({'success': False, 'error': str(e)}), 500
    return jsonify({'success': True})

# Endpoint to get user settings (all user info fields)
@app.route('/get_settings', methods=['POST'])
def get_settings():
    data = request.json
    username = data.get('username')
    if not username:
        return jsonify({'success': False, 'error': 'Missing username'}), 400
    conn = get_db()
    settings = conn.execute('SELECT * FROM settings WHERE username=?', (username,)).fetchone()
    if not settings:
        return jsonify({'success': False, 'error': 'No settings found'})
    # Return all fields as a dict
    return jsonify({'success': True, 'settings': dict(settings)})


# Endpoint to update nutritional goals
@app.route('/update_goals', methods=['POST'])
def update_goals():
    data = request.json
    username = data.get('username')
    # Accept either a 'goals' dict or individual fields
    goals = data.get('goals', data)
    if not username:
        return jsonify({'success': False, 'error': 'Missing username'}), 400
    # Only use valid columns for nutritional_goals table
    valid_keys = [
        'weightGoal', 'caloriesValue', 'proteinValue', 'fatsValue', 'cholesterolValue',
        'carbsValue', 'sugarValue', 'fiberValue', 'sodiumValue'
    ]
    # Add missing field to match table definition
    # nutritional_goals table has: username, weightGoal, caloriesValue, proteinValue, fatsValue, cholesterolValue, carbsValue, sugarValue, fiberValue, sodiumValue
    # That's 10 fields after username
    # If you missed a field, add it here. For example, if 'fiberValue' or 'sodiumValue' is missing, add it.
    # Let's check the table definition and add any missing field
    # Your table definition is correct, so valid_keys should be:
    valid_keys = [
        'weightGoal', 'caloriesValue', 'proteinValue', 'fatsValue', 'cholesterolValue',
        'carbsValue', 'sugarValue', 'fiberValue', 'sodiumValue'
    ]
    # But that's only 9. Let's add a placeholder for the missing field (e.g., 'extraField')
    # If you know the actual field name, replace 'extraField' with the correct name
    # Convert empty strings to None for INTEGER columns
    def clean_int(val):
        if val == '' or val is None:
            return None
        try:
            return int(val)
        except Exception:
            return val
    # Build values list only from valid keys (do NOT include username)
    values = []
    for k in valid_keys:
        if k == 'weightGoal':
            values.append(goals.get(k))
        else:
            values.append(clean_int(goals.get(k)))
    conn = get_db()
    try:
        print('update_goals values:', [username] + values)
        conn.execute('''
            INSERT INTO nutritional_goals (
                username, weightGoal, caloriesValue, proteinValue, fatsValue, cholesterolValue, carbsValue, sugarValue, fiberValue, sodiumValue
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(username) DO UPDATE SET
                weightGoal=excluded.weightGoal,
                caloriesValue=excluded.caloriesValue,
                proteinValue=excluded.proteinValue,
                fatsValue=excluded.fatsValue,
                cholesterolValue=excluded.cholesterolValue,
                carbsValue=excluded.carbsValue,
                sugarValue=excluded.sugarValue,
                fiberValue=excluded.fiberValue,
                sodiumValue=excluded.sodiumValue
        ''', [username] + values)
        conn.commit()
    except Exception as e:
        import traceback
        print('Error in /update_goals:', traceback.format_exc())
        return jsonify({'success': False, 'error': str(e), 'trace': traceback.format_exc()}), 500
    return jsonify({'success': True})


# Endpoint to get nutritional goals for a user
@app.route('/get_goals', methods=['POST', 'GET'])
def get_goals():
    if request.method == 'POST':
        data = request.json
        username = data.get('username')
    else:  # GET
        username = request.args.get('username')
    if not username:
        return jsonify({'success': False, 'error': 'Missing username'}), 400
    conn = get_db()
    goals = conn.execute('SELECT * FROM nutritional_goals WHERE username=?', (username,)).fetchone()
    if not goals:
        return jsonify({'success': True, 'goals': {}})
    return jsonify({'success': True, 'goals': dict(goals)})


# Endpoint to get foods for a user
@app.route('/get_foods', methods=['POST', 'GET'])
def get_foods():
    if request.method == 'POST':
        data = request.json
        username = data.get('username')
    else:  # GET
        username = request.args.get('username')
    if not username:
        return jsonify({'success': False, 'error': 'Missing username'}), 400
    conn = get_db()
    foods = conn.execute('SELECT * FROM foods WHERE username=?', (username,)).fetchall()
    food_list = [dict(row) for row in foods]
    return jsonify({'success': True, 'foods': food_list})

@app.after_request
def set_csp_header(response):
    response.headers['Content-Security-Policy'] = "frame-ancestors 'none'"
    return response

# Endpoint to update accessibility settings
@app.route('/update_accessibility', methods=['POST'])
def update_accessibility():
    data = request.json
    username = data.get('username')
    fontSize = data.get('fontSize')
    contrast = data.get('contrast')
    dyslexia = data.get('dyslexia')
    if not username:
        return jsonify({'success': False, 'error': 'Missing username'}), 400
    conn = get_db()
    try:
        conn.execute('''
            UPDATE settings SET fontSize=?, contrast=?, dyslexia=? WHERE username=?
        ''', (fontSize, int(bool(contrast)), int(bool(dyslexia)), username))
        conn.commit()
    except Exception as e:
        return jsonify({'success': False, 'error': str(e)}), 500
    return jsonify({'success': True})

if __name__ == '__main__':
    init_db()
    app.run(debug=True)
