@echo off
rem ###########################################################################
rem run-batch-jobs.cmd - Grey Legacy Batch Job Runner (Windows)
rem
rem Executes Spring-managed batch jobs (ClaimAgingJob, FraudScoringJob,
rem PayoutSchedulingJob, PremiumRecalculationJob) via command-line invocation.
rem
rem Usage: run-batch-jobs.cmd <JobName> [--spring-profile <profile>]
rem
rem Examples:
rem   run-batch-jobs.cmd ClaimAgingJob
rem   run-batch-jobs.cmd FraudScoringJob --spring-profile staging
rem
rem Exit Codes:
rem   0 - Job completed successfully
rem   1 - Configuration or setup error
rem   2 - Job execution failed
rem
rem Maintainer: Grey Legacy Batch Operations
rem Last Modified: 2025-10-15
rem ###########################################################################
setlocal enabledelayedexpansion

rem ---------------------------------------------------------------------------
rem Validate arguments
rem ---------------------------------------------------------------------------
if "%~1"=="" (
    echo [ERROR] Job name required.
    echo.
    echo Usage: %~nx0 ^<JobName^> [--spring-profile ^<profile^>]
    echo.
    echo Available jobs:
    echo   ClaimAgingJob             - Age open claims and update status
    echo   FraudScoringJob           - Recalculate fraud risk scores
    echo   PayoutSchedulingJob       - Schedule approved claim payouts
    echo   PremiumRecalculationJob   - Recalculate policy premiums
    exit /b 1
)

set "JOB_NAME=%~1"
set "SPRING_PROFILE=production"

rem Parse optional arguments
:PARSE_ARGS
shift
if "%~1"=="" goto :ARGS_DONE
if /i "%~1"=="--spring-profile" (
    set "SPRING_PROFILE=%~2"
    shift
    goto :PARSE_ARGS
)
echo [WARN] Unknown argument: %~1
goto :PARSE_ARGS
:ARGS_DONE

rem ---------------------------------------------------------------------------
rem Validate job name
rem ---------------------------------------------------------------------------
set "VALID_JOB=false"
for %%j in (ClaimAgingJob FraudScoringJob PayoutSchedulingJob PremiumRecalculationJob) do (
    if /i "%JOB_NAME%"=="%%j" set "VALID_JOB=true"
)

if "%VALID_JOB%"=="false" (
    echo [ERROR] Unknown job: %JOB_NAME%
    echo Valid jobs: ClaimAgingJob, FraudScoringJob, PayoutSchedulingJob, PremiumRecalculationJob
    exit /b 1
)

rem ---------------------------------------------------------------------------
rem Environment configuration
rem ---------------------------------------------------------------------------
set "JAVA_HOME=C:\Program Files\Java\jdk1.8.0_381"
set "GL_HOME=C:\GreyLegacy"
set "GL_LIB=%GL_HOME%\lib"
set "GL_CONF=%GL_HOME%\conf"
set "GL_LOG=C:\Logs\greylegacy\batch"

if not exist "%JAVA_HOME%\bin\java.exe" (
    echo [ERROR] JAVA_HOME not valid: %JAVA_HOME%
    exit /b 1
)

if not exist "%GL_HOME%" (
    echo [ERROR] GL_HOME not found: %GL_HOME%
    exit /b 1
)

if not exist "%GL_LOG%" mkdir "%GL_LOG%"

rem ---------------------------------------------------------------------------
rem Timestamp for log file
rem ---------------------------------------------------------------------------
for /f "tokens=1-6 delims=/:. " %%a in ("%date% %time%") do (
    set "TIMESTAMP=%%c%%a%%b_%%d%%e%%f"
)
set "JOB_LOG=%GL_LOG%\%JOB_NAME%_%TIMESTAMP%.log"

rem ---------------------------------------------------------------------------
rem Build classpath from lib directory
rem ---------------------------------------------------------------------------
set "CLASSPATH=%GL_CONF%"

rem Add all JARs in lib/
for %%j in ("%GL_LIB%\*.jar") do (
    set "CLASSPATH=!CLASSPATH!;%%j"
)

rem Add application modules
if exist "%GL_HOME%\modules" (
    for %%j in ("%GL_HOME%\modules\*.jar") do (
        set "CLASSPATH=!CLASSPATH!;%%j"
    )
)

rem ---------------------------------------------------------------------------
rem JVM options for batch
rem ---------------------------------------------------------------------------
set "JAVA_OPTS=-server"
set "JAVA_OPTS=%JAVA_OPTS% -Xms512m -Xmx1024m"
set "JAVA_OPTS=%JAVA_OPTS% -XX:+UseG1GC"
set "JAVA_OPTS=%JAVA_OPTS% -Dspring.profiles.active=%SPRING_PROFILE%"
set "JAVA_OPTS=%JAVA_OPTS% -Dlog4j.configuration=file:///%GL_CONF%\log4j-batch.properties"
set "JAVA_OPTS=%JAVA_OPTS% -Dgreylegacy.config.dir=%GL_CONF%"
set "JAVA_OPTS=%JAVA_OPTS% -Dgreylegacy.log.dir=%GL_LOG%"
set "JAVA_OPTS=%JAVA_OPTS% -Dgreylegacy.batch.job=%JOB_NAME%"
set "JAVA_OPTS=%JAVA_OPTS% -Duser.timezone=America/New_York"
set "JAVA_OPTS=%JAVA_OPTS% -Dfile.encoding=UTF-8"

rem ---------------------------------------------------------------------------
rem Spring context configuration
rem ---------------------------------------------------------------------------
set "SPRING_CONTEXT=classpath:applicationContext-batch.xml"
set "MAIN_CLASS=com.greylegacy.batch.BatchJobLauncher"

rem ---------------------------------------------------------------------------
rem Execute batch job
rem ---------------------------------------------------------------------------
echo ================================================================
echo Grey Legacy Batch Job Runner
echo ================================================================
echo Job:          %JOB_NAME%
echo Profile:      %SPRING_PROFILE%
echo Main Class:   %MAIN_CLASS%
echo Log:          %JOB_LOG%
echo Started:      %date% %time%
echo ================================================================

echo [INFO] [%date% %time%] Starting %JOB_NAME%... >> "%JOB_LOG%"

"%JAVA_HOME%\bin\java.exe" %JAVA_OPTS% ^
    -classpath "%CLASSPATH%" ^
    %MAIN_CLASS% ^
    --job.name=%JOB_NAME% ^
    --spring.context=%SPRING_CONTEXT% ^
    >> "%JOB_LOG%" 2>&1

set "EXIT_CODE=%errorlevel%"

rem ---------------------------------------------------------------------------
rem Check result
rem ---------------------------------------------------------------------------
echo.
if %EXIT_CODE% equ 0 (
    echo [OK] %JOB_NAME% completed successfully.
    echo [INFO] [%date% %time%] %JOB_NAME% completed successfully (exit code 0) >> "%JOB_LOG%"
) else (
    echo [ERROR] %JOB_NAME% failed with exit code %EXIT_CODE%.
    echo [ERROR] [%date% %time%] %JOB_NAME% FAILED (exit code %EXIT_CODE%) >> "%JOB_LOG%"

    rem Scan log for exceptions
    echo.
    echo Last errors from log:
    echo ----------------------------------------------------------------
    findstr /i /c:"Exception" /c:"ERROR" /c:"FATAL" "%JOB_LOG%" 2>nul | more +0 /e /p
    echo ----------------------------------------------------------------
)

echo.
echo Finished: %date% %time%
echo Log file: %JOB_LOG%
echo ================================================================

exit /b %EXIT_CODE%

endlocal
