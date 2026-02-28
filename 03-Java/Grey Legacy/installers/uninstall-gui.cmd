@echo off
rem Grey Legacy Claims System - GUI Uninstaller Launcher (Windows)
rem Run as Administrator for full functionality.
setlocal

set "SCRIPT_DIR=%~dp0"
set "JAR_FILE=%SCRIPT_DIR%gui\greylegacy-installer.jar"

where java >nul 2>&1
if %errorlevel% neq 0 (
    echo [ERROR] Java not found. Install JDK 8+ and add to PATH.
    pause
    exit /b 1
)

if not exist "%JAR_FILE%" (
    echo [ERROR] GUI jar not found. Run install-gui.cmd first to build it.
    pause
    exit /b 1
)

echo Launching Grey Legacy Uninstaller...
java -jar "%JAR_FILE%" --uninstall %*
