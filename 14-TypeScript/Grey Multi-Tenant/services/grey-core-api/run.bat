@echo off
title Grey Multi-Tenant API
echo ============================================
echo   Grey Multi-Tenant Platform
echo ============================================
echo.

cd /d "%~dp0"
echo Starting Grey Core API...
echo Press Ctrl+C to stop. Window will stay open on error.
echo.

bin\grey-core-api.exe

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ERROR: Application exited with code %ERRORLEVEL%
    echo.
    pause
)
