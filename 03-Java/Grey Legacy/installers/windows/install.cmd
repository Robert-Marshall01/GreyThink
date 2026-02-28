@echo off
rem ###########################################################################
rem install.cmd - Grey Legacy Claims System Installer (Windows)
rem
rem Installs Grey Legacy as a Windows service with Apache Tomcat 8.5.
rem Handles prerequisite checks, directory setup, WAR deployment,
rem Windows service registration, firewall rules, and environment config.
rem
rem Usage: Run as Administrator
rem   install.cmd [--silent] [--install-dir <path>] [--port <port>]
rem
rem Exit Codes:
rem   0 - Installation successful
rem   1 - General error
rem   2 - Prerequisite check failed
rem   3 - File copy failed
rem   4 - Service registration failed
rem
rem Maintainer: Grey Legacy Infrastructure Team
rem ###########################################################################
setlocal enabledelayedexpansion

rem ---------------------------------------------------------------------------
rem  Default Configuration
rem ---------------------------------------------------------------------------
set "APP_NAME=Grey Legacy Claims System"
set "APP_SHORT=greylegacy"
set "APP_VERSION=1.0.0"
set "SERVICE_NAME=GreyLegacyTomcat"
set "SERVICE_DISPLAY=Grey Legacy Claims System (Tomcat)"
set "INSTALL_DIR=C:\GreyLegacy"
set "TOMCAT_VERSION=8.5.100"
set "REQUIRED_JAVA_MAJOR=8"
set "APP_PORT=8080"
set "SHUTDOWN_PORT=8005"
set "SILENT=0"
set "SCRIPT_DIR=%~dp0"

rem ---------------------------------------------------------------------------
rem  Parse Arguments
rem ---------------------------------------------------------------------------
:parse_args
if "%~1"=="" goto :args_done
if /i "%~1"=="--silent"      ( set "SILENT=1"         & shift & goto :parse_args )
if /i "%~1"=="--install-dir" ( set "INSTALL_DIR=%~2"   & shift & shift & goto :parse_args )
if /i "%~1"=="--port"        ( set "APP_PORT=%~2"      & shift & shift & goto :parse_args )
shift
goto :parse_args
:args_done

rem ---------------------------------------------------------------------------
rem  Check for Administrator Privileges
rem ---------------------------------------------------------------------------
net session >nul 2>&1
if %errorlevel% neq 0 (
    echo [ERROR] This installer must be run as Administrator.
    echo         Right-click and select "Run as administrator".
    exit /b 1
)

rem ---------------------------------------------------------------------------
rem  Banner
rem ---------------------------------------------------------------------------
echo.
echo  ===========================================================
echo   %APP_NAME% - Windows Installer v%APP_VERSION%
echo  ===========================================================
echo.
echo   Install Directory : %INSTALL_DIR%
echo   Application Port  : %APP_PORT%
echo   Service Name      : %SERVICE_NAME%
echo.

if "%SILENT%"=="0" (
    set /p "CONFIRM=Proceed with installation? [Y/n]: "
    if /i "!CONFIRM!"=="n" (
        echo Installation cancelled by user.
        exit /b 0
    )
)

rem ---------------------------------------------------------------------------
rem  Step 1: Verify Java Installation
rem ---------------------------------------------------------------------------
echo [1/8] Checking Java installation...

where java >nul 2>&1
if %errorlevel% neq 0 (
    echo [ERROR] Java is not installed or not in PATH.
    echo         Please install JDK 8 or later and ensure JAVA_HOME is set.
    echo         Download from: https://adoptium.net/
    exit /b 2
)

if not defined JAVA_HOME (
    echo [WARN]  JAVA_HOME is not set. Attempting to detect...
    for /f "tokens=*" %%i in ('where java') do (
        set "JAVA_BIN=%%~dpi"
        set "JAVA_HOME=!JAVA_BIN:~0,-5!"
    )
    if not defined JAVA_HOME (
        echo [ERROR] Cannot determine JAVA_HOME. Please set it manually.
        exit /b 2
    )
    echo [INFO]  Detected JAVA_HOME: !JAVA_HOME!
)

rem Validate Java version
for /f "tokens=3" %%v in ('java -version 2^>^&1 ^| findstr /i "version"') do (
    set "JAVA_VER=%%~v"
)
echo [OK]    Java found: %JAVA_VER%

rem ---------------------------------------------------------------------------
rem  Step 2: Create Directory Structure
rem ---------------------------------------------------------------------------
echo [2/8] Creating directory structure...

set "TOMCAT_DIR=%INSTALL_DIR%\tomcat"
set "CONF_DIR=%INSTALL_DIR%\conf"
set "DATA_DIR=%INSTALL_DIR%\data"
set "LOG_DIR=%INSTALL_DIR%\logs"
set "BACKUP_DIR=%INSTALL_DIR%\backups"

for %%d in ("%INSTALL_DIR%" "%TOMCAT_DIR%" "%CONF_DIR%" "%DATA_DIR%" "%LOG_DIR%" "%BACKUP_DIR%") do (
    if not exist %%d (
        mkdir %%d
        if %errorlevel% neq 0 (
            echo [ERROR] Failed to create directory: %%d
            exit /b 3
        )
    )
)
echo [OK]    Directory structure created at %INSTALL_DIR%

rem ---------------------------------------------------------------------------
rem  Step 3: Install Apache Tomcat
rem ---------------------------------------------------------------------------
echo [3/8] Setting up Apache Tomcat %TOMCAT_VERSION%...

set "TOMCAT_ARCHIVE=%SCRIPT_DIR%apache-tomcat-%TOMCAT_VERSION%-windows-x64.zip"

if exist "%TOMCAT_ARCHIVE%" (
    echo [INFO]  Found bundled Tomcat archive.
    powershell -NoProfile -Command "Expand-Archive -Path '%TOMCAT_ARCHIVE%' -DestinationPath '%INSTALL_DIR%\temp-tomcat' -Force"
    xcopy /s /e /y /q "%INSTALL_DIR%\temp-tomcat\apache-tomcat-%TOMCAT_VERSION%\*" "%TOMCAT_DIR%\" >nul
    rmdir /s /q "%INSTALL_DIR%\temp-tomcat"
    echo [OK]    Tomcat extracted to %TOMCAT_DIR%
) else (
    echo [INFO]  Tomcat archive not found at %TOMCAT_ARCHIVE%.
    echo [INFO]  Checking for existing Tomcat installation...
    if exist "%TOMCAT_DIR%\bin\catalina.bat" (
        echo [OK]    Existing Tomcat found at %TOMCAT_DIR%
    ) else (
        echo [INFO]  Downloading Apache Tomcat %TOMCAT_VERSION%...
        set "TOMCAT_URL=https://archive.apache.org/dist/tomcat/tomcat-8/v%TOMCAT_VERSION%/bin/apache-tomcat-%TOMCAT_VERSION%-windows-x64.zip"
        set "TOMCAT_DL=%TEMP%\apache-tomcat-%TOMCAT_VERSION%-windows-x64.zip"
        powershell -NoProfile -Command "[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12; Invoke-WebRequest -Uri '!TOMCAT_URL!' -OutFile '!TOMCAT_DL!'"
        if not exist "!TOMCAT_DL!" (
            echo [ERROR] Download failed. Check your internet connection.
            echo         Or place apache-tomcat-%TOMCAT_VERSION%-windows-x64.zip in %SCRIPT_DIR%
            exit /b 2
        )
        echo [INFO]  Extracting Tomcat...
        powershell -NoProfile -Command "Expand-Archive -Path '!TOMCAT_DL!' -DestinationPath '%INSTALL_DIR%\temp-tomcat' -Force"
        xcopy /s /e /y /q "%INSTALL_DIR%\temp-tomcat\apache-tomcat-%TOMCAT_VERSION%\*" "%TOMCAT_DIR%\" >nul
        rmdir /s /q "%INSTALL_DIR%\temp-tomcat"
        del /f /q "!TOMCAT_DL!"
        echo [OK]    Tomcat %TOMCAT_VERSION% downloaded and installed.
    )
)

rem ---------------------------------------------------------------------------
rem  Step 4: Deploy Application WAR
rem ---------------------------------------------------------------------------
echo [4/8] Deploying application...

set "WAR_SOURCE=%SCRIPT_DIR%..\..\web\target\%APP_SHORT%.war"
set "ALT_WAR=%SCRIPT_DIR%%APP_SHORT%.war"
set "DEPLOY_TARGET=%TOMCAT_DIR%\webapps\%APP_SHORT%.war"

if exist "%WAR_SOURCE%" (
    copy /y "%WAR_SOURCE%" "%DEPLOY_TARGET%" >nul
    echo [OK]    Deployed WAR from build output.
) else if exist "%ALT_WAR%" (
    copy /y "%ALT_WAR%" "%DEPLOY_TARGET%" >nul
    echo [OK]    Deployed WAR from installer directory.
) else (
    echo [WARN]  WAR file not found. Build the project first:
    echo         mvn clean package -pl web -am
    echo         Then re-run this installer.
    echo [INFO]  Continuing installation without WAR deployment...
)

rem ---------------------------------------------------------------------------
rem  Step 5: Configure Application
rem ---------------------------------------------------------------------------
echo [5/8] Configuring application...

rem Copy configuration files
set "CONFIG_SOURCE=%SCRIPT_DIR%..\..\config"
if exist "%CONFIG_SOURCE%\tomcat\server.xml" (
    copy /y "%CONFIG_SOURCE%\tomcat\server.xml" "%TOMCAT_DIR%\conf\server.xml" >nul
    echo [INFO]  Copied Tomcat server.xml
)
if exist "%CONFIG_SOURCE%\tomcat\tomcat-users.xml" (
    copy /y "%CONFIG_SOURCE%\tomcat\tomcat-users.xml" "%TOMCAT_DIR%\conf\tomcat-users.xml" >nul
    echo [INFO]  Copied tomcat-users.xml
)
if exist "%CONFIG_SOURCE%\security\jaas.conf" (
    copy /y "%CONFIG_SOURCE%\security\jaas.conf" "%CONF_DIR%\jaas.conf" >nul
    echo [INFO]  Copied JAAS configuration
)

rem Create application environment configuration
(
    echo # Grey Legacy Environment Configuration
    echo # Generated by installer on %date% %time%
    echo GL_HOME=%INSTALL_DIR%
    echo GL_CONF=%CONF_DIR%
    echo GL_LOG=%LOG_DIR%
    echo GL_DATA=%DATA_DIR%
    echo APP_PORT=%APP_PORT%
    echo SHUTDOWN_PORT=%SHUTDOWN_PORT%
) > "%CONF_DIR%\environment.properties"

rem Configure JVM options
(
    echo set "CATALINA_HOME=%TOMCAT_DIR%"
    echo set "CATALINA_BASE=%TOMCAT_DIR%"
    echo set "JAVA_HOME=%JAVA_HOME%"
    echo set "JAVA_OPTS=-server -Xms1024m -Xmx2048m -XX:MaxMetaspaceSize=512m"
    echo set "JAVA_OPTS=%%JAVA_OPTS%% -XX:+UseG1GC -XX:MaxGCPauseMillis=200"
    echo set "JAVA_OPTS=%%JAVA_OPTS%% -XX:+HeapDumpOnOutOfMemoryError"
    echo set "JAVA_OPTS=%%JAVA_OPTS%% -XX:HeapDumpPath=%LOG_DIR%\heapdump.hprof"
    echo set "JAVA_OPTS=%%JAVA_OPTS%% -Dgl.home=%INSTALL_DIR%"
    echo set "JAVA_OPTS=%%JAVA_OPTS%% -Dgl.conf=%CONF_DIR%"
    echo set "JAVA_OPTS=%%JAVA_OPTS%% -Dgl.log=%LOG_DIR%"
    echo set "JAVA_OPTS=%%JAVA_OPTS%% -Djava.security.auth.login.config=%CONF_DIR%\jaas.conf"
) > "%TOMCAT_DIR%\bin\setenv.bat"

echo [OK]    Application configured.

rem ---------------------------------------------------------------------------
rem  Step 6: Register Windows Service
rem ---------------------------------------------------------------------------
echo [6/8] Registering Windows service...

rem Stop existing service if present
sc query "%SERVICE_NAME%" >nul 2>&1
if %errorlevel% equ 0 (
    echo [INFO]  Stopping existing service...
    net stop "%SERVICE_NAME%" >nul 2>&1
    sc delete "%SERVICE_NAME%" >nul 2>&1
    timeout /t 3 /nobreak >nul
)

if exist "%TOMCAT_DIR%\bin\service.bat" (
    pushd "%TOMCAT_DIR%\bin"
    call service.bat install %SERVICE_NAME%
    popd

    rem Configure service startup type
    sc config "%SERVICE_NAME%" start= auto >nul 2>&1
    sc description "%SERVICE_NAME%" "%APP_NAME% - Enterprise Claims Processing on Tomcat %TOMCAT_VERSION%" >nul 2>&1

    rem Set service recovery: restart on failure
    sc failure "%SERVICE_NAME%" reset= 86400 actions= restart/60000/restart/120000/restart/300000 >nul 2>&1

    echo [OK]    Service '%SERVICE_NAME%' registered and set to auto-start.
) else (
    echo [WARN]  Tomcat service.bat not found. Service not registered.
    echo         You can start the application manually with:
    echo           %TOMCAT_DIR%\bin\startup.bat
)

rem ---------------------------------------------------------------------------
rem  Step 7: Configure Firewall
rem ---------------------------------------------------------------------------
echo [7/8] Configuring firewall rules...

netsh advfirewall firewall show rule name="Grey Legacy HTTP" >nul 2>&1
if %errorlevel% equ 0 (
    netsh advfirewall firewall delete rule name="Grey Legacy HTTP" >nul 2>&1
)
netsh advfirewall firewall add rule name="Grey Legacy HTTP" dir=in action=allow protocol=TCP localport=%APP_PORT% >nul 2>&1
echo [OK]    Firewall rule added for port %APP_PORT%.

rem ---------------------------------------------------------------------------
rem  Step 8: Write Uninstaller Registry & Start Menu
rem ---------------------------------------------------------------------------
echo [8/8] Creating uninstaller entries...

rem Save installation metadata for uninstaller
(
    echo INSTALL_DIR=%INSTALL_DIR%
    echo TOMCAT_DIR=%TOMCAT_DIR%
    echo SERVICE_NAME=%SERVICE_NAME%
    echo APP_PORT=%APP_PORT%
    echo APP_VERSION=%APP_VERSION%
    echo INSTALL_DATE=%date%
) > "%INSTALL_DIR%\install.meta"

rem Copy uninstaller to install directory
if exist "%SCRIPT_DIR%uninstall.cmd" (
    copy /y "%SCRIPT_DIR%uninstall.cmd" "%INSTALL_DIR%\uninstall.cmd" >nul
)

rem Add/Remove Programs registry entry
reg add "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\GreyLegacy" ^
    /v "DisplayName" /t REG_SZ /d "%APP_NAME%" /f >nul 2>&1
reg add "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\GreyLegacy" ^
    /v "DisplayVersion" /t REG_SZ /d "%APP_VERSION%" /f >nul 2>&1
reg add "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\GreyLegacy" ^
    /v "Publisher" /t REG_SZ /d "GreyThink" /f >nul 2>&1
reg add "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\GreyLegacy" ^
    /v "InstallLocation" /t REG_SZ /d "%INSTALL_DIR%" /f >nul 2>&1
reg add "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\GreyLegacy" ^
    /v "UninstallString" /t REG_SZ /d "\"%INSTALL_DIR%\uninstall.cmd\"" /f >nul 2>&1
reg add "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\GreyLegacy" ^
    /v "NoModify" /t REG_DWORD /d 1 /f >nul 2>&1
reg add "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\GreyLegacy" ^
    /v "NoRepair" /t REG_DWORD /d 1 /f >nul 2>&1
echo [OK]    Add/Remove Programs entry created.

rem ---------------------------------------------------------------------------
rem  Desktop Application
rem ---------------------------------------------------------------------------
set "APP_JAR_SRC=%SCRIPT_DIR%..\gui\greylegacy-app.jar"
if exist "%APP_JAR_SRC%" (
    copy /y "%APP_JAR_SRC%" "%INSTALL_DIR%\greylegacy-app.jar" >nul
    echo [OK]    Desktop app JAR installed.

    rem Create launcher batch file
    (
        echo @echo off
        echo start "" javaw -jar "%INSTALL_DIR%\greylegacy-app.jar"
    ) > "%INSTALL_DIR%\GreyLegacy.cmd"

    rem Create Start Menu shortcut via PowerShell
    set "SHORTCUT_DIR=%ProgramData%\Microsoft\Windows\Start Menu\Programs\Grey Legacy"
    if not exist "!SHORTCUT_DIR!" mkdir "!SHORTCUT_DIR!"
    powershell -Command "$ws = New-Object -ComObject WScript.Shell; $s = $ws.CreateShortcut('!SHORTCUT_DIR!\Grey Legacy Dashboard.lnk'); $s.TargetPath = 'javaw'; $s.Arguments = '-jar \"%INSTALL_DIR%\greylegacy-app.jar\"'; $s.WorkingDirectory = '%INSTALL_DIR%'; $s.Description = 'Grey Legacy Claims System Dashboard'; $s.Save()" >nul 2>&1
    echo [OK]    Start Menu shortcut created.
) else (
    echo [WARN]  Desktop app JAR not found - skipping shortcut.
)

rem ---------------------------------------------------------------------------
rem  Installation Complete
rem ---------------------------------------------------------------------------
echo.
echo  ===========================================================
echo   Installation Complete!
echo  ===========================================================
echo.
echo   Install Directory : %INSTALL_DIR%
echo   Tomcat Directory  : %TOMCAT_DIR%
echo   Application URL   : http://localhost:%APP_PORT%/%APP_SHORT%/
echo   Service Name      : %SERVICE_NAME%
echo.
echo   To start the application:
echo     net start %SERVICE_NAME%
echo   Or:
echo     %TOMCAT_DIR%\bin\startup.bat
echo.
echo   To uninstall:
echo     %INSTALL_DIR%\uninstall.cmd
echo.
echo   Logs: %LOG_DIR%
echo  ===========================================================
echo.

endlocal
exit /b 0
