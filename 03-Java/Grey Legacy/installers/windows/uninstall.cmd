@echo off
rem ###########################################################################
rem uninstall.cmd - Grey Legacy Claims System Uninstaller (Windows)
rem
rem Removes Grey Legacy application, stops and removes the Windows service,
rem cleans up firewall rules and registry entries.
rem
rem Usage: Run as Administrator
rem   uninstall.cmd [--silent] [--keep-data] [--keep-logs]
rem
rem Exit Codes:
rem   0 - Uninstallation successful
rem   1 - General error
rem
rem Maintainer: Grey Legacy Infrastructure Team
rem ###########################################################################
setlocal enabledelayedexpansion

rem ---------------------------------------------------------------------------
rem  Default Configuration
rem ---------------------------------------------------------------------------
set "APP_NAME=Grey Legacy Claims System"
set "APP_SHORT=greylegacy"
set "SILENT=0"
set "KEEP_DATA=0"
set "KEEP_LOGS=0"
set "SCRIPT_DIR=%~dp0"

rem ---------------------------------------------------------------------------
rem  Load install metadata
rem ---------------------------------------------------------------------------
set "META_FILE=%SCRIPT_DIR%install.meta"
if exist "%META_FILE%" (
    for /f "usebackq tokens=1,* delims==" %%a in ("%META_FILE%") do (
        set "%%a=%%b"
    )
) else (
    rem Fallback defaults
    set "INSTALL_DIR=C:\GreyLegacy"
    set "TOMCAT_DIR=C:\GreyLegacy\tomcat"
    set "SERVICE_NAME=GreyLegacyTomcat"
    set "APP_PORT=8080"
)

rem ---------------------------------------------------------------------------
rem  Parse Arguments
rem ---------------------------------------------------------------------------
:parse_args
if "%~1"=="" goto :args_done
if /i "%~1"=="--silent"    ( set "SILENT=1"    & shift & goto :parse_args )
if /i "%~1"=="--keep-data" ( set "KEEP_DATA=1" & shift & goto :parse_args )
if /i "%~1"=="--keep-logs" ( set "KEEP_LOGS=1" & shift & goto :parse_args )
shift
goto :parse_args
:args_done

rem ---------------------------------------------------------------------------
rem  Check for Administrator Privileges
rem ---------------------------------------------------------------------------
net session >nul 2>&1
if %errorlevel% neq 0 (
    echo [ERROR] This uninstaller must be run as Administrator.
    echo         Right-click and select "Run as administrator".
    exit /b 1
)

rem ---------------------------------------------------------------------------
rem  Banner
rem ---------------------------------------------------------------------------
echo.
echo  ===========================================================
echo   %APP_NAME% - Windows Uninstaller
echo  ===========================================================
echo.
echo   Install Directory : %INSTALL_DIR%
echo   Service Name      : %SERVICE_NAME%
echo   Keep Data         : %KEEP_DATA%
echo   Keep Logs         : %KEEP_LOGS%
echo.

if "%SILENT%"=="0" (
    set /p "CONFIRM=Are you sure you want to uninstall? [y/N]: "
    if /i not "!CONFIRM!"=="y" (
        echo Uninstallation cancelled by user.
        exit /b 0
    )
)

rem ---------------------------------------------------------------------------
rem  Step 1: Stop and Remove Windows Service
rem ---------------------------------------------------------------------------
echo [1/5] Stopping service...

sc query "%SERVICE_NAME%" >nul 2>&1
if %errorlevel% equ 0 (
    echo [INFO]  Stopping service '%SERVICE_NAME%'...
    net stop "%SERVICE_NAME%" >nul 2>&1
    timeout /t 5 /nobreak >nul

    rem Force kill if still running
    sc query "%SERVICE_NAME%" | findstr "RUNNING" >nul 2>&1
    if %errorlevel% equ 0 (
        echo [WARN]  Service did not stop gracefully. Forcing stop...
        taskkill /F /FI "SERVICES eq %SERVICE_NAME%" >nul 2>&1
        timeout /t 3 /nobreak >nul
    )

    rem Remove service registration
    if exist "%TOMCAT_DIR%\bin\service.bat" (
        pushd "%TOMCAT_DIR%\bin"
        call service.bat remove %SERVICE_NAME%
        popd
    ) else (
        sc delete "%SERVICE_NAME%" >nul 2>&1
    )
    echo [OK]    Service removed.
) else (
    echo [INFO]  Service '%SERVICE_NAME%' not found. Skipping.
)

rem ---------------------------------------------------------------------------
rem  Step 2: Remove Firewall Rules
rem ---------------------------------------------------------------------------
echo [2/5] Removing firewall rules...

netsh advfirewall firewall show rule name="Grey Legacy HTTP" >nul 2>&1
if %errorlevel% equ 0 (
    netsh advfirewall firewall delete rule name="Grey Legacy HTTP" >nul 2>&1
    echo [OK]    Firewall rule removed.
) else (
    echo [INFO]  No firewall rule found. Skipping.
)

rem ---------------------------------------------------------------------------
rem  Step 3: Remove Registry Entries
rem ---------------------------------------------------------------------------
echo [3/5] Removing registry entries...

reg query "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\GreyLegacy" >nul 2>&1
if %errorlevel% equ 0 (
    reg delete "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\GreyLegacy" /f >nul 2>&1
    echo [OK]    Add/Remove Programs entry removed.
) else (
    echo [INFO]  No registry entry found. Skipping.
)

rem ---------------------------------------------------------------------------
rem  Step 4: Remove Application Files
rem ---------------------------------------------------------------------------
echo [4/5] Removing application files...

if exist "%INSTALL_DIR%" (
    rem Remove WAR and exploded webapp
    if exist "%TOMCAT_DIR%\webapps\%APP_SHORT%.war" (
        del /f /q "%TOMCAT_DIR%\webapps\%APP_SHORT%.war"
    )
    if exist "%TOMCAT_DIR%\webapps\%APP_SHORT%" (
        rmdir /s /q "%TOMCAT_DIR%\webapps\%APP_SHORT%"
    )

    rem Remove Tomcat
    if exist "%TOMCAT_DIR%" (
        rmdir /s /q "%TOMCAT_DIR%"
        echo [INFO]  Tomcat removed.
    )

    rem Remove configuration
    if exist "%INSTALL_DIR%\conf" (
        rmdir /s /q "%INSTALL_DIR%\conf"
        echo [INFO]  Configuration removed.
    )

    rem Remove backups
    if exist "%INSTALL_DIR%\backups" (
        rmdir /s /q "%INSTALL_DIR%\backups"
        echo [INFO]  Backups removed.
    )

    rem Conditionally remove data
    if "%KEEP_DATA%"=="0" (
        if exist "%INSTALL_DIR%\data" (
            rmdir /s /q "%INSTALL_DIR%\data"
            echo [INFO]  Data directory removed.
        )
    ) else (
        echo [INFO]  Keeping data directory: %INSTALL_DIR%\data
    )

    rem Conditionally remove logs
    if "%KEEP_LOGS%"=="0" (
        if exist "%INSTALL_DIR%\logs" (
            rmdir /s /q "%INSTALL_DIR%\logs"
            echo [INFO]  Logs removed.
        )
    ) else (
        echo [INFO]  Keeping logs directory: %INSTALL_DIR%\logs
    )

    rem Remove metadata
    if exist "%INSTALL_DIR%\install.meta" del /f /q "%INSTALL_DIR%\install.meta"

    rem Try to remove install directory (will fail if data/logs kept)
    rmdir "%INSTALL_DIR%" 2>nul
    if exist "%INSTALL_DIR%" (
        echo [INFO]  Install directory not fully removed (preserved data/logs).
    ) else (
        echo [OK]    Install directory removed.
    )
) else (
    echo [INFO]  Install directory not found: %INSTALL_DIR%
)

rem ---------------------------------------------------------------------------
rem  Step 5: Remove Environment Variables
rem ---------------------------------------------------------------------------
echo [5/5] Cleaning up environment...

rem Remove system environment variables if they were set
reg query "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v GL_HOME >nul 2>&1
if %errorlevel% equ 0 (
    reg delete "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v GL_HOME /f >nul 2>&1
    echo [INFO]  Removed GL_HOME environment variable.
)

rem Remove Start Menu shortcut
set "SHORTCUT_DIR=%ProgramData%\Microsoft\Windows\Start Menu\Programs\Grey Legacy"
if exist "!SHORTCUT_DIR!" (
    rmdir /s /q "!SHORTCUT_DIR!"
    echo [INFO]  Start Menu shortcut removed.
)
echo [OK]    Cleanup complete.

rem ---------------------------------------------------------------------------
rem  Uninstallation Complete
rem ---------------------------------------------------------------------------
echo.
echo  ===========================================================
echo   Uninstallation Complete!
echo  ===========================================================
echo.
if "%KEEP_DATA%"=="1" echo   Data preserved at: %INSTALL_DIR%\data
if "%KEEP_LOGS%"=="1" echo   Logs preserved at: %INSTALL_DIR%\logs
echo.
echo   %APP_NAME% has been removed from this system.
echo  ===========================================================
echo.

endlocal
exit /b 0
