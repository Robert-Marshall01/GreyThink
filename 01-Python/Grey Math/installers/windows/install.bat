@echo off
setlocal enabledelayedexpansion

:: ============================================================================
:: Grey Math Installer for Windows
:: Version: 0.1.0
:: Requires: Python 3.11+
:: ============================================================================

title Grey Math Installer

echo.
echo  ============================================
echo    Grey Math - Windows Installer v0.1.0
echo  ============================================
echo.

:: --- Check for administrator privileges ---
net session >nul 2>&1
if %errorlevel% neq 0 (
    echo [INFO] Running without administrator privileges.
    echo [INFO] Installation will proceed for the current user only.
    echo.
)

:: --- Configuration ---
set "INSTALL_DIR=%LOCALAPPDATA%\GreyMath"
set "VENV_DIR=%INSTALL_DIR%\venv"
set "BIN_DIR=%INSTALL_DIR%\bin"
set "UNINSTALLER=%INSTALL_DIR%\uninstall.bat"
set "MIN_PYTHON_MAJOR=3"
set "MIN_PYTHON_MINOR=11"

:: --- Detect Python ---
echo [1/6] Checking for Python %MIN_PYTHON_MAJOR%.%MIN_PYTHON_MINOR%+ ...

set "PYTHON_CMD="
for %%P in (python3 python py) do (
    where %%P >nul 2>&1
    if !errorlevel! equ 0 (
        for /f "tokens=*" %%V in ('%%P --version 2^>^&1') do (
            set "PY_VER=%%V"
        )
        for /f "tokens=2 delims= " %%A in ("!PY_VER!") do (
            for /f "tokens=1,2 delims=." %%M in ("%%A") do (
                if %%M geq %MIN_PYTHON_MAJOR% (
                    if %%N geq %MIN_PYTHON_MINOR% (
                        set "PYTHON_CMD=%%P"
                    )
                    if %%M gtr %MIN_PYTHON_MAJOR% (
                        set "PYTHON_CMD=%%P"
                    )
                )
            )
        )
        if defined PYTHON_CMD goto :python_found
    )
)

if not defined PYTHON_CMD (
    echo [ERROR] Python %MIN_PYTHON_MAJOR%.%MIN_PYTHON_MINOR%+ is required but was not found.
    echo         Please install Python from https://www.python.org/downloads/
    echo         Make sure to check "Add Python to PATH" during installation.
    pause
    exit /b 1
)

:python_found
echo [OK]    Found: !PY_VER! (!PYTHON_CMD!)

:: --- Create installation directory ---
echo [2/6] Creating installation directory ...
if exist "%INSTALL_DIR%" (
    echo [WARN] Installation directory already exists: %INSTALL_DIR%
    set /p OVERWRITE="         Overwrite existing installation? (y/N): "
    if /i not "!OVERWRITE!"=="y" (
        echo [ABORT] Installation cancelled.
        pause
        exit /b 0
    )
    rmdir /s /q "%INSTALL_DIR%" 2>nul
)
mkdir "%INSTALL_DIR%" 2>nul
if %errorlevel% neq 0 (
    echo [ERROR] Failed to create installation directory.
    pause
    exit /b 1
)
echo [OK]    %INSTALL_DIR%

:: --- Create virtual environment ---
echo [3/6] Creating Python virtual environment ...
%PYTHON_CMD% -m venv "%VENV_DIR%"
if %errorlevel% neq 0 (
    echo [ERROR] Failed to create virtual environment.
    pause
    exit /b 1
)
echo [OK]    Virtual environment created.

:: --- Install Grey Math ---
echo [4/6] Installing Grey Math ...
call "%VENV_DIR%\Scripts\activate.bat"

:: Upgrade pip first
python -m pip install --upgrade pip >nul 2>&1

:: Find the package source directory (where this script is run from)
set "SCRIPT_DIR=%~dp0"
set "PACKAGE_DIR=%SCRIPT_DIR%..\.."

:: Install the package
pip install "%PACKAGE_DIR%" 2>&1
if %errorlevel% neq 0 (
    echo [ERROR] Failed to install Grey Math.
    pause
    exit /b 1
)
echo [OK]    Grey Math installed successfully.

:: --- Create launcher scripts ---
echo [5/6] Creating launcher scripts ...
mkdir "%BIN_DIR%" 2>nul

:: Create greymath launcher
(
    echo @echo off
    echo call "%VENV_DIR%\Scripts\activate.bat"
    echo python -m greymath %%*
) > "%BIN_DIR%\greymath.bat"

:: Create greymath-python launcher (for running scripts in the greymath env)
(
    echo @echo off
    echo call "%VENV_DIR%\Scripts\activate.bat"
    echo python %%*
) > "%BIN_DIR%\greymath-python.bat"

echo [OK]    Launchers created in %BIN_DIR%

:: --- Create uninstaller ---
echo [6/6] Creating uninstaller ...
(
    echo @echo off
    echo setlocal
    echo title Grey Math Uninstaller
    echo echo.
    echo echo  ============================================
    echo echo    Grey Math - Windows Uninstaller
    echo echo  ============================================
    echo echo.
    echo set /p CONFIRM="Are you sure you want to uninstall Grey Math? (y/N): "
    echo if /i not "%%CONFIRM%%"=="y" (
    echo     echo [ABORT] Uninstallation cancelled.
    echo     pause
    echo     exit /b 0
    echo ^)
    echo echo.
    echo echo [1/3] Removing PATH entry ...
    echo set "KEY=HKCU\Environment"
    echo for /f "tokens=2*" %%%%A in ('reg query "%%KEY%%" /v Path 2^^^>nul') do set "CURRENT_PATH=%%%%B"
    echo set "CURRENT_PATH=%%CURRENT_PATH:%BIN_DIR%;=%%"
    echo reg add "%%KEY%%" /v Path /t REG_EXPAND_SZ /d "%%CURRENT_PATH%%" /f ^>nul 2^>^&1
    echo echo [OK]    PATH entry removed.
    echo echo.
    echo echo [2/3] Removing Start Menu shortcut ...
    echo del "%%APPDATA%%\Microsoft\Windows\Start Menu\Programs\Grey Math.lnk" 2^>nul
    echo echo [OK]    Shortcut removed.
    echo echo.
    echo echo [3/3] Removing installation directory ...
    echo cd /d "%%TEMP%%"
    echo rmdir /s /q "%INSTALL_DIR%" 2^>nul
    echo echo [OK]    Installation directory removed.
    echo echo.
    echo echo  Grey Math has been uninstalled successfully.
    echo echo.
    echo pause
) > "%UNINSTALLER%"
echo [OK]    Uninstaller created at %UNINSTALLER%

:: --- Add to PATH ---
set "KEY=HKCU\Environment"
for /f "tokens=2*" %%A in ('reg query "%KEY%" /v Path 2^>nul') do set "CURRENT_PATH=%%B"
echo %CURRENT_PATH% | find /i "%BIN_DIR%" >nul 2>&1
if %errorlevel% neq 0 (
    reg add "%KEY%" /v Path /t REG_EXPAND_SZ /d "%BIN_DIR%;%CURRENT_PATH%" /f >nul 2>&1
    echo [OK]    Added to user PATH. Restart your terminal to use 'greymath' command.
)

:: --- Done ---
echo.
echo  ============================================
echo    Installation Complete!
echo  ============================================
echo.
echo  Install location : %INSTALL_DIR%
echo  Launcher         : %BIN_DIR%\greymath.bat
echo  Python env       : %BIN_DIR%\greymath-python.bat
echo  Uninstaller      : %UNINSTALLER%
echo.
echo  Usage:
echo    greymath              - Launch Grey Math
echo    greymath-python       - Python with Grey Math packages
echo.
echo  NOTE: You may need to restart your terminal for PATH changes.
echo.
pause
exit /b 0
