; ============================================================================
; Grey Firmware - NSIS Windows Installer Script
; ============================================================================
; Builds a standard Windows installer/uninstaller (.exe)
;
; Requirements:
;   - NSIS 3.x (https://nsis.sourceforge.io)
;   - Build the project first: cmake --build build --config Release
;
; Usage:
;   makensis installers\windows\grey_firmware_installer.nsi
; ============================================================================

!include "MUI2.nsh"
!include "FileFunc.nsh"

; ---- General ----
Name "Grey Firmware"
OutFile "GreyFirmware-1.0.0-Setup.exe"
InstallDir "$PROGRAMFILES\Grey Firmware"
InstallDirRegKey HKLM "Software\GreyFirmware" "InstallDir"
RequestExecutionLevel admin

; ---- Version Info ----
VIProductVersion "1.0.0.0"
VIAddVersionKey "ProductName" "Grey Firmware"
VIAddVersionKey "ProductVersion" "1.0.0"
VIAddVersionKey "FileDescription" "Grey Firmware Installer"
VIAddVersionKey "LegalCopyright" "Grey Firmware Team"

; ---- Interface Settings ----
!define MUI_ABORTWARNING
!define MUI_ICON "..\..\assets\grey_firmware.ico"
!define MUI_UNICON "..\..\assets\grey_firmware.ico"
!define MUI_WELCOMEFINISHPAGE_BITMAP_NOSTRETCH

; ---- Pages ----
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "..\..\LICENSE"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

; ---- Language ----
!insertmacro MUI_LANGUAGE "English"

; ============================================================================
; Installer Sections
; ============================================================================

Section "Grey Firmware Core (required)" SecCore
    SectionIn RO  ; Read-only — always installed

    SetOutPath "$INSTDIR"

    ; Main executables
    File "..\..\build\Release\grey_firmware.exe"
    File "..\..\build\Release\grey_firmware_gui.exe"

    ; License
    File "..\..\LICENSE"
    File "..\..\README.md"

    ; GTK3 runtime DLLs (bundled by build)
    SetOutPath "$INSTDIR\lib"
    File /r "..\..\build\Release\lib\*.*"

    ; Headers (for development)
    SetOutPath "$INSTDIR\include"
    File /r "..\..\include\*.h"

    ; Write registry keys
    WriteRegStr HKLM "Software\GreyFirmware" "InstallDir" "$INSTDIR"
    WriteRegStr HKLM "Software\GreyFirmware" "Version" "1.0.0"

    ; Add/Remove Programs entry
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GreyFirmware" \
                     "DisplayName" "Grey Firmware 1.0.0"
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GreyFirmware" \
                     "UninstallString" '"$INSTDIR\uninstall.exe"'
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GreyFirmware" \
                     "InstallLocation" "$INSTDIR"
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GreyFirmware" \
                     "DisplayVersion" "1.0.0"
    WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GreyFirmware" \
                     "Publisher" "Grey Firmware Team"
    WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GreyFirmware" \
                       "NoModify" 1
    WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GreyFirmware" \
                       "NoRepair" 1

    ; Calculate installed size
    ${GetSize} "$INSTDIR" "/S=0K" $0 $1 $2
    IntFmt $0 "0x%08X" $0
    WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GreyFirmware" \
                       "EstimatedSize" "$0"

    ; Create uninstaller
    WriteUninstaller "$INSTDIR\uninstall.exe"
SectionEnd

Section "Start Menu Shortcuts" SecShortcuts
    CreateDirectory "$SMPROGRAMS\Grey Firmware"
    CreateShortcut "$SMPROGRAMS\Grey Firmware\Grey Firmware GUI.lnk" \
                   "$INSTDIR\grey_firmware_gui.exe"
    CreateShortcut "$SMPROGRAMS\Grey Firmware\Grey Firmware CLI.lnk" \
                   "$INSTDIR\grey_firmware.exe"
    CreateShortcut "$SMPROGRAMS\Grey Firmware\README.lnk" \
                   "$INSTDIR\README.md"
    CreateShortcut "$SMPROGRAMS\Grey Firmware\Uninstall.lnk" \
                   "$INSTDIR\uninstall.exe"
SectionEnd

Section "Desktop Shortcut" SecDesktop
    CreateShortcut "$DESKTOP\Grey Firmware.lnk" \
                   "$INSTDIR\grey_firmware_gui.exe"
SectionEnd

; ---- Section Descriptions ----
!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecCore} \
        "Core firmware framework files, CLI and GUI executables."
    !insertmacro MUI_DESCRIPTION_TEXT ${SecShortcuts} \
        "Create Start Menu shortcuts."
    !insertmacro MUI_DESCRIPTION_TEXT ${SecDesktop} \
        "Create a desktop shortcut to the GUI."
!insertmacro MUI_FUNCTION_DESCRIPTION_END

; ============================================================================
; Uninstaller Section
; ============================================================================

Section "Uninstall"
    ; Remove files
    Delete "$INSTDIR\grey_firmware.exe"
    Delete "$INSTDIR\grey_firmware_gui.exe"
    Delete "$INSTDIR\LICENSE"
    Delete "$INSTDIR\README.md"
    Delete "$INSTDIR\uninstall.exe"

    ; Remove bundled libraries
    RMDir /r "$INSTDIR\lib"
    RMDir /r "$INSTDIR\include"

    ; Remove shortcuts
    Delete "$SMPROGRAMS\Grey Firmware\Grey Firmware GUI.lnk"
    Delete "$SMPROGRAMS\Grey Firmware\Grey Firmware CLI.lnk"
    Delete "$SMPROGRAMS\Grey Firmware\README.lnk"
    Delete "$SMPROGRAMS\Grey Firmware\Uninstall.lnk"
    RMDir "$SMPROGRAMS\Grey Firmware"
    Delete "$DESKTOP\Grey Firmware.lnk"

    ; Remove install directory (only if empty)
    RMDir "$INSTDIR"

    ; Remove registry keys
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\GreyFirmware"
    DeleteRegKey HKLM "Software\GreyFirmware"
SectionEnd
