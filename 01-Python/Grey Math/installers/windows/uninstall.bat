@echo off
setlocal enabledelayedexpansion

:: ============================================================================
:: Grey Math Uninstaller for Windows
:: Standalone uninstaller — can also be run from the installed copy
:: ============================================================================

title Grey Math Uninstaller

echo.
echo  ============================================
echo    Grey Math - Windows Uninstaller
echo  ============================================
echo.

set "INSTALL_DIR=%LOCALAPPDATA%\GreyMath"
set "BIN_DIR=%INSTALL_DIR%\bin"

:: --- Check if installed ---
if not exist "%INSTALL_DIR%" (
    echo [INFO] Grey Math does not appear to be installed at:
    echo        %INSTALL_DIR%
    echo.
    pause
    exit /b 0
)

:: --- Confirm ---
set /p CONFIRM="Are you sure you want to uninstall Grey Math? (y/N): "
if /i not "%CONFIRM%"=="y" (
    echo [ABORT] Uninstallation cancelled.
    pause
    exit /b 0
)

echo.

:: --- Remove PATH entry ---
echo [1/3] Removing PATH entry ...
set "KEY=HKCU\Environment"
for /f "tokens=2*" %%A in ('reg query "%KEY%" /v Path 2^>nul') do set "CURRENT_PATH=%%B"
if defined CURRENT_PATH (
    set "NEW_PATH=!CURRENT_PATH:%BIN_DIR%;=!"
    if not "!NEW_PATH!"=="!CURRENT_PATH!" (
        reg add "%KEY%" /v Path /t REG_EXPAND_SZ /d "!NEW_PATH!" /f >nul 2>&1
        echo [OK]    PATH entry removed.
    ) else (
        echo [OK]    No PATH entry found — skipping.
    )
) else (
    echo [OK]    No user PATH set — skipping.
)

:: --- Remove Start Menu shortcut ---
echo [2/3] Removing Start Menu shortcut ...
del "%APPDATA%\Microsoft\Windows\Start Menu\Programs\Grey Math.lnk" 2>nul
echo [OK]    Start Menu shortcut removed.

:: --- Remove installation directory ---
echo [3/3] Removing installation directory ...
cd /d "%TEMP%"
rmdir /s /q "%INSTALL_DIR%" 2>nul
if exist "%INSTALL_DIR%" (
    echo [WARN] Some files could not be removed. Please delete manually:
    echo        %INSTALL_DIR%
) else (
    echo [OK]    Installation directory removed.
)

echo.
echo  ============================================
echo    Grey Math has been uninstalled.
echo  ============================================
echo.
pause
exit /b 0
