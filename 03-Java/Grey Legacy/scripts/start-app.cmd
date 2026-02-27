@echo off
rem ###########################################################################
rem start-app.cmd - Grey Legacy Application Startup (Windows)
rem
rem Configures JVM options, classpath, and environment variables, then
rem starts the Tomcat application server for the Grey Legacy claims system.
rem
rem Usage: start-app.cmd [--debug]
rem
rem Maintainer: Grey Legacy Infrastructure Team
rem Last Modified: 2025-10-28
rem ###########################################################################
setlocal enabledelayedexpansion

rem ---------------------------------------------------------------------------
rem Environment paths
rem ---------------------------------------------------------------------------
set "JAVA_HOME=C:\Program Files\Java\jdk1.8.0_381"
set "CATALINA_HOME=C:\Program Files\Apache Software Foundation\Tomcat 8.5"
set "GL_HOME=C:\GreyLegacy"
set "GL_CONF=%GL_HOME%\conf"
set "GL_LIB=%GL_HOME%\lib"
set "GL_LOG=C:\Logs\greylegacy"

rem Validate JAVA_HOME
if not exist "%JAVA_HOME%\bin\java.exe" (
    echo [ERROR] JAVA_HOME not valid: %JAVA_HOME%
    exit /b 1
)

rem Validate CATALINA_HOME
if not exist "%CATALINA_HOME%\bin\catalina.bat" (
    echo [ERROR] CATALINA_HOME not valid: %CATALINA_HOME%
    exit /b 1
)

rem Ensure log directory exists
if not exist "%GL_LOG%" mkdir "%GL_LOG%"

rem ---------------------------------------------------------------------------
rem JVM Memory and GC settings
rem ---------------------------------------------------------------------------
set "HEAP_MIN=1024m"
set "HEAP_MAX=2048m"
set "METASPACE_MAX=512m"

set "JAVA_OPTS=-server"
set "JAVA_OPTS=%JAVA_OPTS% -Xms%HEAP_MIN% -Xmx%HEAP_MAX%"
set "JAVA_OPTS=%JAVA_OPTS% -XX:MaxMetaspaceSize=%METASPACE_MAX%"
set "JAVA_OPTS=%JAVA_OPTS% -XX:+UseG1GC"
set "JAVA_OPTS=%JAVA_OPTS% -XX:MaxGCPauseMillis=200"
set "JAVA_OPTS=%JAVA_OPTS% -XX:+HeapDumpOnOutOfMemoryError"
set "JAVA_OPTS=%JAVA_OPTS% -XX:HeapDumpPath=%GL_LOG%\heapdump.hprof"
set "JAVA_OPTS=%JAVA_OPTS% -XX:+UseStringDeduplication"

rem GC logging (Java 8 syntax)
set "JAVA_OPTS=%JAVA_OPTS% -verbose:gc"
set "JAVA_OPTS=%JAVA_OPTS% -XX:+PrintGCDetails"
set "JAVA_OPTS=%JAVA_OPTS% -XX:+PrintGCDateStamps"
set "JAVA_OPTS=%JAVA_OPTS% -Xloggc:%GL_LOG%\gc.log"
set "JAVA_OPTS=%JAVA_OPTS% -XX:+UseGCLogFileRotation"
set "JAVA_OPTS=%JAVA_OPTS% -XX:NumberOfGCLogFiles=5"
set "JAVA_OPTS=%JAVA_OPTS% -XX:GCLogFileSize=20M"

rem ---------------------------------------------------------------------------
rem JMX remote monitoring
rem ---------------------------------------------------------------------------
set "JMX_PORT=9010"

set "JAVA_OPTS=%JAVA_OPTS% -Dcom.sun.management.jmxremote"
set "JAVA_OPTS=%JAVA_OPTS% -Dcom.sun.management.jmxremote.port=%JMX_PORT%"
set "JAVA_OPTS=%JAVA_OPTS% -Dcom.sun.management.jmxremote.ssl=false"
set "JAVA_OPTS=%JAVA_OPTS% -Dcom.sun.management.jmxremote.authenticate=true"
set "JAVA_OPTS=%JAVA_OPTS% -Dcom.sun.management.jmxremote.password.file=%GL_CONF%\jmxremote.password"
set "JAVA_OPTS=%JAVA_OPTS% -Dcom.sun.management.jmxremote.access.file=%GL_CONF%\jmxremote.access"

rem ---------------------------------------------------------------------------
rem Application configuration
rem ---------------------------------------------------------------------------
set "JAVA_OPTS=%JAVA_OPTS% -Dlog4j.configuration=file:///%GL_CONF%\log4j.properties"
set "JAVA_OPTS=%JAVA_OPTS% -Dgreylegacy.config.dir=%GL_CONF%"
set "JAVA_OPTS=%JAVA_OPTS% -Dgreylegacy.log.dir=%GL_LOG%"
set "JAVA_OPTS=%JAVA_OPTS% -Dgreylegacy.env=production"

rem Database connection pool tuning
set "JAVA_OPTS=%JAVA_OPTS% -Dhibernate.connection.pool_size=20"
set "JAVA_OPTS=%JAVA_OPTS% -Djava.net.preferIPv4Stack=true"

rem Timezone and encoding
set "JAVA_OPTS=%JAVA_OPTS% -Duser.timezone=America/New_York"
set "JAVA_OPTS=%JAVA_OPTS% -Dfile.encoding=UTF-8"

rem ---------------------------------------------------------------------------
rem Debug mode (optional)
rem ---------------------------------------------------------------------------
if /i "%~1"=="--debug" (
    echo [INFO] Debug mode enabled on port 8000
    set "JAVA_OPTS=%JAVA_OPTS% -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=8000"
)

rem ---------------------------------------------------------------------------
rem Classpath extensions
rem ---------------------------------------------------------------------------
set "CLASSPATH=%GL_CONF%"
for %%j in ("%GL_LIB%\*.jar") do set "CLASSPATH=!CLASSPATH!;%%j"

set "JAVA_OPTS=%JAVA_OPTS% -classpath %CLASSPATH%"

rem ---------------------------------------------------------------------------
rem Export and start
rem ---------------------------------------------------------------------------
set "CATALINA_OPTS=%JAVA_OPTS%"

echo ================================================================
echo Grey Legacy Claims System - Starting
echo ================================================================
echo JAVA_HOME:     %JAVA_HOME%
echo CATALINA_HOME: %CATALINA_HOME%
echo Heap:          %HEAP_MIN% - %HEAP_MAX%
echo JMX Port:      %JMX_PORT%
echo Config:        %GL_CONF%
echo Logs:          %GL_LOG%
echo ================================================================

call "%CATALINA_HOME%\bin\catalina.bat" start

if %errorlevel% neq 0 (
    echo [ERROR] Tomcat failed to start
    exit /b 1
)

echo [OK] Tomcat start initiated. Check logs at %GL_LOG%
exit /b 0

endlocal
