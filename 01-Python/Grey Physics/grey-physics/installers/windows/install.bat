@echo off
REM Grey Physics Installer for Windows
REM This batch file launches the PowerShell installer.
REM Right-click and "Run as administrator" for an all-users install.

echo.
echo Grey Physics Installer
echo ======================
echo.

REM Check if PowerShell is available
where powershell >nul 2>&1
if %ERRORLEVEL% neq 0 (
    echo ERROR: PowerShell is required but was not found.
    echo Please install PowerShell or run install.ps1 directly.
    pause
    exit /b 1
)

REM Launch the PowerShell installer with execution policy bypass for this process only
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0install.ps1" %*

if %ERRORLEVEL% neq 0 (
    echo.
    echo Installation failed. See errors above.
    pause
    exit /b %ERRORLEVEL%
)

pause
