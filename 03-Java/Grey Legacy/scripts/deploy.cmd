@echo off
rem ###########################################################################
rem deploy.cmd - Grey Legacy Claims System Deployment (Windows)
rem
rem Deploys the Grey Legacy WAR to an Apache Tomcat Windows service.
rem Supports rollback, health checking, and CI/CD integration.
rem
rem Usage: deploy.cmd [war_path] [environment]
rem Exit Codes:
rem   0 - Deployment successful
rem   1 - General error
rem   2 - Pre-deployment check failed
rem   3 - Service stop failed
rem   4 - Deployment failed
rem   5 - Health check failed
rem
rem Maintainer: Grey Legacy Infrastructure Team
rem Last Modified: 2025-11-14
rem ###########################################################################
setlocal enabledelayedexpansion

rem ---------------------------------------------------------------------------
rem Configuration
rem ---------------------------------------------------------------------------
set "TOMCAT_HOME=C:\Program Files\Apache Software Foundation\Tomcat 8.5"
set "APP_NAME=greylegacy"
set "SERVICE_NAME=Tomcat8"
set "DEPLOY_DIR=%TOMCAT_HOME%\webapps"
set "BACKUP_DIR=C:\Backups\greylegacy\deployments"
set "LOG_DIR=C:\Logs\greylegacy"
set "HEALTH_URL=http://localhost:8080/%APP_NAME%/health"
set "HEALTH_TIMEOUT=120"
set "HEALTH_INTERVAL=5"
set "MIN_DISK_MB=500"

rem Timestamp for filenames
for /f "tokens=1-6 delims=/:. " %%a in ("%date% %time%") do (
    set "TIMESTAMP=%%c%%a%%b_%%d%%e%%f"
)

set "WAR_FILE=%~dp0..\web\target\%APP_NAME%.war"
if not "%~1"=="" set "WAR_FILE=%~1"

set "ENVIRONMENT=production"
if not "%~2"=="" set "ENVIRONMENT=%~2"

set "LOG_FILE=%LOG_DIR%\deploy-%TIMESTAMP%.log"

rem Create directories
if not exist "%BACKUP_DIR%" mkdir "%BACKUP_DIR%"
if not exist "%LOG_DIR%" mkdir "%LOG_DIR%"

call :LOG "=============================================="
call :LOG "Grey Legacy Deployment - %ENVIRONMENT%"
call :LOG "Timestamp: %TIMESTAMP%"
call :LOG "WAR: %WAR_FILE%"
call :LOG "=============================================="

rem ---------------------------------------------------------------------------
rem Pre-deployment checks
rem ---------------------------------------------------------------------------
call :LOG "[STEP] Running pre-deployment checks..."

rem Check WAR file exists
if not exist "%WAR_FILE%" (
    call :LOG "[ERROR] WAR file not found: %WAR_FILE%"
    exit /b 2
)
call :LOG "[OK]   WAR file found"

rem Check Java installation
where java >nul 2>&1
if %errorlevel% neq 0 (
    call :LOG "[ERROR] Java not found in PATH"
    exit /b 2
)
for /f "tokens=3" %%v in ('java -version 2^>^&1 ^| findstr /i "version"') do set "JAVA_VER=%%~v"
call :LOG "[OK]   Java version: %JAVA_VER%"

rem Check Tomcat home
if not exist "%TOMCAT_HOME%\bin\catalina.bat" (
    call :LOG "[ERROR] Tomcat installation not found at %TOMCAT_HOME%"
    exit /b 2
)
call :LOG "[OK]   Tomcat installation verified"

rem Check disk space
for /f "tokens=3" %%s in ('dir "%DEPLOY_DIR%" /-c 2^>nul ^| findstr /c:"bytes free"') do set "FREE_BYTES=%%s"
set /a "FREE_MB=%FREE_BYTES:~0,-6%" 2>nul
if defined FREE_MB (
    if %FREE_MB% LSS %MIN_DISK_MB% (
        call :LOG "[ERROR] Insufficient disk space: %FREE_MB%MB available"
        exit /b 2
    )
    call :LOG "[OK]   Disk space: %FREE_MB%MB available"
) else (
    call :LOG "[WARN] Could not determine free disk space"
)

rem Check service exists
sc query "%SERVICE_NAME%" >nul 2>&1
if %errorlevel% neq 0 (
    call :LOG "[ERROR] Service '%SERVICE_NAME%' not found"
    exit /b 2
)
call :LOG "[OK]   Service '%SERVICE_NAME%' exists"

call :LOG "[OK]   Pre-deployment checks passed"

rem ---------------------------------------------------------------------------
rem Backup current deployment
rem ---------------------------------------------------------------------------
call :LOG "[STEP] Backing up current deployment..."
if exist "%DEPLOY_DIR%\%APP_NAME%.war" (
    set "BACKUP_FILE=%BACKUP_DIR%\%APP_NAME%-%TIMESTAMP%.war"
    copy /y "%DEPLOY_DIR%\%APP_NAME%.war" "!BACKUP_FILE!" >nul
    if !errorlevel! neq 0 (
        call :LOG "[ERROR] Backup failed"
        exit /b 4
    )
    call :LOG "[OK]   Backup saved: !BACKUP_FILE!"
) else (
    call :LOG "[WARN] No existing WAR to back up"
)

rem Cleanup old backups (keep last 10)
set "BACKUP_COUNT=0"
for %%f in ("%BACKUP_DIR%\%APP_NAME%-*.war") do set /a BACKUP_COUNT+=1
if %BACKUP_COUNT% gtr 10 (
    call :LOG "[INFO] Pruning old backups..."
    set "DEL_COUNT=0"
    set /a "TO_DELETE=%BACKUP_COUNT%-10"
    for /f "tokens=*" %%f in ('dir /b /o:d "%BACKUP_DIR%\%APP_NAME%-*.war"') do (
        if !DEL_COUNT! lss !TO_DELETE! (
            del "%BACKUP_DIR%\%%f" >nul 2>&1
            set /a DEL_COUNT+=1
        )
    )
)

rem ---------------------------------------------------------------------------
rem Stop Tomcat service
rem ---------------------------------------------------------------------------
call :LOG "[STEP] Stopping Tomcat service..."

sc query "%SERVICE_NAME%" | findstr /i "RUNNING" >nul 2>&1
if %errorlevel% equ 0 (
    net stop "%SERVICE_NAME%" >nul 2>&1
    if !errorlevel! neq 0 (
        call :LOG "[WARN] NET STOP failed, attempting SC STOP..."
        sc stop "%SERVICE_NAME%" >nul 2>&1
    )

    rem Wait for service to stop
    set "WAIT=0"
    :WAIT_STOP
    sc query "%SERVICE_NAME%" | findstr /i "STOPPED" >nul 2>&1
    if !errorlevel! equ 0 (
        call :LOG "[OK]   Tomcat stopped after !WAIT! seconds"
        goto :DEPLOY
    )
    timeout /t 2 /nobreak >nul
    set /a WAIT+=2
    if !WAIT! gtr 60 (
        call :LOG "[ERROR] Tomcat failed to stop within 60 seconds"
        taskkill /f /fi "SERVICES eq %SERVICE_NAME%" >nul 2>&1
        timeout /t 3 /nobreak >nul
        sc query "%SERVICE_NAME%" | findstr /i "STOPPED" >nul 2>&1
        if !errorlevel! neq 0 (
            call :LOG "[ERROR] Force-stop failed"
            exit /b 3
        )
        call :LOG "[WARN] Tomcat force-stopped"
    )
    goto :WAIT_STOP
) else (
    call :LOG "[WARN] Tomcat service not running"
)

rem ---------------------------------------------------------------------------
rem Deploy WAR
rem ---------------------------------------------------------------------------
:DEPLOY
call :LOG "[STEP] Deploying %APP_NAME%.war..."

rem Remove old exploded directory
if exist "%DEPLOY_DIR%\%APP_NAME%" (
    rmdir /s /q "%DEPLOY_DIR%\%APP_NAME%" >nul 2>&1
)

rem Remove old WAR
if exist "%DEPLOY_DIR%\%APP_NAME%.war" (
    del /f /q "%DEPLOY_DIR%\%APP_NAME%.war" >nul 2>&1
)

rem Copy new WAR
xcopy /y /q "%WAR_FILE%" "%DEPLOY_DIR%\" >nul 2>&1
if %errorlevel% neq 0 (
    call :LOG "[ERROR] Failed to copy WAR file"
    goto :ROLLBACK
)

rem Rename if needed
if not "%~nx1"=="%APP_NAME%.war" (
    if exist "%DEPLOY_DIR%\%~nx1" (
        rename "%DEPLOY_DIR%\%~nx1" "%APP_NAME%.war" >nul 2>&1
    )
)

call :LOG "[OK]   WAR deployed"

rem ---------------------------------------------------------------------------
rem Start Tomcat service
rem ---------------------------------------------------------------------------
call :LOG "[STEP] Starting Tomcat service..."
net start "%SERVICE_NAME%" >nul 2>&1
if %errorlevel% neq 0 (
    call :LOG "[ERROR] Failed to start Tomcat service"
    goto :ROLLBACK
)

rem ---------------------------------------------------------------------------
rem Health check
rem ---------------------------------------------------------------------------
call :LOG "[STEP] Waiting for application health..."
set "ELAPSED=0"

:HEALTH_LOOP
for /f %%c in ('curl -s -o nul -w "%%{http_code}" --max-time 5 "%HEALTH_URL%" 2^>nul') do set "HTTP_CODE=%%c"
if "%HTTP_CODE%"=="200" (
    call :LOG "[OK]   Application is healthy (HTTP 200) after %ELAPSED%s"
    goto :SUCCESS
)

call :LOG "[INFO] Health check: HTTP %HTTP_CODE% (%ELAPSED%/%HEALTH_TIMEOUT%s)"
timeout /t %HEALTH_INTERVAL% /nobreak >nul
set /a ELAPSED+=%HEALTH_INTERVAL%
if %ELAPSED% gtr %HEALTH_TIMEOUT% (
    call :LOG "[ERROR] Health check timed out after %HEALTH_TIMEOUT%s"
    goto :ROLLBACK
)
goto :HEALTH_LOOP

rem ---------------------------------------------------------------------------
rem Rollback
rem ---------------------------------------------------------------------------
:ROLLBACK
call :LOG "[WARN] Initiating rollback..."
set "LATEST_BACKUP="
for /f "tokens=*" %%f in ('dir /b /o:-d "%BACKUP_DIR%\%APP_NAME%-*.war" 2^>nul') do (
    if not defined LATEST_BACKUP set "LATEST_BACKUP=%%f"
)
if not defined LATEST_BACKUP (
    call :LOG "[ERROR] No backup found for rollback!"
    exit /b 5
)

net stop "%SERVICE_NAME%" >nul 2>&1
timeout /t 5 /nobreak >nul

del /f /q "%DEPLOY_DIR%\%APP_NAME%.war" >nul 2>&1
rmdir /s /q "%DEPLOY_DIR%\%APP_NAME%" >nul 2>&1
xcopy /y /q "%BACKUP_DIR%\%LATEST_BACKUP%" "%DEPLOY_DIR%\%APP_NAME%.war*" >nul 2>&1

net start "%SERVICE_NAME%" >nul 2>&1
call :LOG "[WARN] Rolled back to %LATEST_BACKUP%"
exit /b 5

rem ---------------------------------------------------------------------------
rem Success
rem ---------------------------------------------------------------------------
:SUCCESS
call :LOG "=============================================="
call :LOG "Deployment completed successfully"
call :LOG "=============================================="
exit /b 0

rem ---------------------------------------------------------------------------
rem Logging subroutine
rem ---------------------------------------------------------------------------
:LOG
set "MSG=%~1"
echo [%date% %time%] %MSG%
echo [%date% %time%] %MSG% >> "%LOG_FILE%" 2>nul
goto :eof

endlocal
