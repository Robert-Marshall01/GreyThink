# CalCalories 🍎

A calorie and nutrition tracking web application built with Flask and SQLite. Track your daily food intake, set nutritional goals, and monitor your progress toward a healthier lifestyle.

## Features

- **User Authentication** – Secure registration and login with password hashing
- **Food Tracking** – Log meals with detailed nutritional information (calories, protein, fats, carbs, sugar, fiber, cholesterol, sodium)
- **Nutritional Goals** – Set personalized daily goals for all macronutrients
- **User Settings** – Configure weight, height, age, gender, and activity level
- **Accessibility Options** – Adjustable font size, high contrast mode, and dyslexia-friendly fonts
- **Theme Support** – Light and dark theme options

## Tech Stack

- **Backend**: Python 3.8+ with Flask
- **Database**: SQLite
- **Frontend**: HTML, CSS, JavaScript
- **Deployment**: Docker, Docker Compose, Kubernetes

## Quick Start

### Prerequisites

- Python 3.8 or higher
- pip (Python package manager)

### Clone the Repository

```bash
git clone https://github.com/Robert-Marshall01/CalCalories.git
cd CalCalories
```

---

## Installing Dependencies

Before running the application, you need to install the required Python packages. We recommend using a virtual environment to avoid conflicts with system packages.

### Step 1: Create a Virtual Environment

```bash
python3 -m venv venv
```

This creates a `venv/` folder containing an isolated Python environment.

### Step 2: Activate the Virtual Environment

**Linux / macOS:**

```bash
source venv/bin/activate
```

**Windows (Command Prompt):**

```cmd
venv\Scripts\activate.bat
```

**Windows (PowerShell):**

```powershell
venv\Scripts\Activate.ps1
```

When activated, you'll see `(venv)` at the beginning of your terminal prompt.

### Step 3: Install Required Packages

```bash
pip install -r requirements.txt
```

This installs Flask, Flask-CORS, Gunicorn, and all other dependencies.

### Verify Installation

```bash
pip list
```

You should see `Flask`, `flask-cors`, and `gunicorn` in the list.

---

## Starting the Website

### Step 1: Activate the Virtual Environment (if not already active)

**Linux / macOS:**

```bash
source venv/bin/activate
```

**Windows:**

```cmd
venv\Scripts\activate.bat
```

### Step 2: Start the Server

```bash
python app.py
```

You should see output similar to:

```
 * Serving Flask app 'app'
 * Debug mode: on
 * Running on http://127.0.0.1:5000
```

### Step 3: Open Your Browser

Navigate to: **http://localhost:5000/calcal_signin.html**

---

## Stopping the Website

### Option 1: Keyboard Interrupt (Recommended)

In the terminal where the server is running, press:

```
Ctrl + C
```

The server will shut down gracefully.

### Option 2: Close the Terminal

Simply closing the terminal window will also stop the server.

### Option 3: Kill by Port (if terminal is unresponsive)

**Linux / macOS:**

```bash
# Find the process using port 5000
lsof -i :5000

# Kill the process (replace PID with the actual process ID)
kill -9 PID
```

**Windows:**

```cmd
# Find the process using port 5000
netstat -ano | findstr :5000

# Kill the process (replace PID with the actual process ID)
taskkill /PID PID /F
```

---

## Deactivating the Virtual Environment

When you're done working, you can deactivate the virtual environment:

```bash
deactivate
```

Your terminal prompt will return to normal (no more `(venv)` prefix).

## Docker Deployment

### Using Docker Compose (Recommended)

```bash
docker-compose up -d
```

The application will be available at `http://localhost:5000`

### Using Docker directly

```bash
# Build the image
docker build -t calcalories .

# Run the container
docker run -d -p 5000:5000 --name calcalories-app calcalories
```

## Kubernetes Deployment

Kubernetes manifests are provided in the `k8s/` directory:

```bash
kubectl apply -f k8s/
```

This includes:
- `configmap.yaml` – Application configuration
- `deployment.yaml` – Pod deployment specification
- `service.yaml` – Service exposure
- `ingress.yaml` – Ingress routing
- `pvc.yaml` – Persistent volume claim for database storage

## API Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/register` | Register a new user |
| POST | `/login` | Authenticate user |
| POST | `/check_username` | Check username availability |
| POST | `/add_food` | Add a food entry |
| POST | `/delete_food` | Delete a food entry |
| GET/POST | `/get_foods` | Get all foods for a user |
| POST | `/update_settings` | Update user settings |
| POST | `/get_settings` | Get user settings |
| POST | `/update_goals` | Update nutritional goals |
| GET/POST | `/get_goals` | Get nutritional goals |
| POST | `/update_accessibility` | Update accessibility settings |
| POST | `/delete_user` | Delete user and all related data |

## Database Schema

The application uses SQLite with four main tables:

- **users** – User credentials (username, password hash)
- **foods** – Food entries with nutritional data
- **nutritional_goals** – User-defined daily nutrition targets
- **settings** – User preferences and profile information

## Project Structure

```
CalCalories/
├── app.py                  # Flask application & API endpoints
├── calcal.py               # Additional Python utilities
├── calcal.sql              # Database schema
├── calcal_home.html        # Main dashboard
├── calcal_signin.html      # Login page
├── calcal_usersetup.html   # User registration
├── calcal_settings.html    # User settings page
├── calcal_css.css          # Styles
├── calcal_js.js            # Frontend JavaScript
├── Dockerfile              # Production Docker image
├── docker-compose.yml      # Docker Compose configuration
├── requirements.txt        # Python dependencies
├── k8s/                    # Kubernetes manifests
│   ├── configmap.yaml
│   ├── deployment.yaml
│   ├── ingress.yaml
│   ├── pvc.yaml
│   └── service.yaml
└── LICENSE                 # MIT License
```

## Security Notes

- Passwords are hashed using SHA-256
- CORS is enabled for API access
- Content Security Policy headers prevent clickjacking
- Non-root user in Docker container for enhanced security

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License – see the [LICENSE](LICENSE) file for details.

## Author

**Robert-Marshall01**

---

> ⚠️ **Note**: Parts of this codebase were AI-generated using GitHub Copilot and subsequently modified. Please review and test thoroughly before production use.
