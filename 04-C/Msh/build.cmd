@echo off
REM Simple build script for Msh on Windows (TDM-GCC)
setlocal
set SRC_DIR=%~dp0

gcc "%SRC_DIR%Msh.c" "%SRC_DIR%run_builtin_command.c" "%SRC_DIR%jobs.c" -o "%SRC_DIR%Msh.exe" -static
if %errorlevel% neq 0 (
    echo Build failed with exit code %errorlevel%
    pause
    endlocal
    exit /b %errorlevel%
)
echo Build succeeded.
endlocal
exit /b 0
