@echo off
REM Grey Physics Uninstaller for Windows
REM This batch file launches the PowerShell uninstaller.

echo.
echo Grey Physics Uninstaller
echo ========================
echo.

REM Check if PowerShell is available
where powershell >nul 2>&1
if %ERRORLEVEL% neq 0 (
    echo ERROR: PowerShell is required but was not found.
    echo Please install PowerShell or run uninstall.ps1 directly.
    pause
    exit /b 1
)

REM Launch the PowerShell uninstaller with execution policy bypass for this process only
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0uninstall.ps1" %*

if %ERRORLEVEL% neq 0 (
    echo.
    echo Uninstallation failed. See errors above.
    pause
    exit /b %ERRORLEVEL%
)

pause
