@echo off
rem Grey Legacy Claims System - GUI Installer Launcher (Windows)
rem Run as Administrator for full functionality.
rem
rem Compiles the GUI if the JAR doesn't exist, then launches it.
setlocal

set "SCRIPT_DIR=%~dp0"
set "GUI_DIR=%SCRIPT_DIR%gui"
set "JAR_FILE=%GUI_DIR%\greylegacy-installer.jar"

rem Check Java
where java >nul 2>&1
if %errorlevel% neq 0 (
    echo [ERROR] Java not found. Install JDK 8+ and add to PATH.
    pause
    exit /b 1
)

rem Build JAR if not present
if not exist "%JAR_FILE%" (
    echo Building GUI installer...
    where javac >nul 2>&1
    if %errorlevel% neq 0 (
        echo [ERROR] javac not found. Install a JDK to build the GUI.
        pause
        exit /b 1
    )

    set "SRC_DIR=%GUI_DIR%\src"
    set "BUILD_DIR=%GUI_DIR%\build"
    if exist "%BUILD_DIR%" rmdir /s /q "%BUILD_DIR%"
    mkdir "%BUILD_DIR%"

    dir /s /b "%GUI_DIR%\src\*.java" > "%BUILD_DIR%\sources.txt"
    javac -d "%BUILD_DIR%" -source 8 -target 8 @"%BUILD_DIR%\sources.txt"
    del "%BUILD_DIR%\sources.txt"

    echo Manifest-Version: 1.0> "%BUILD_DIR%\MANIFEST.MF"
    echo Main-Class: com.greylegacy.installer.InstallerGUI>> "%BUILD_DIR%\MANIFEST.MF"

    pushd "%BUILD_DIR%"
    jar cfm "%JAR_FILE%" MANIFEST.MF com\
    popd
    rmdir /s /q "%BUILD_DIR%"
    echo GUI built successfully.
)

echo Launching Grey Legacy Installer...
java -jar "%JAR_FILE%" %*
