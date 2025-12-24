#!/bin/bash
#
# Web Search - Uninstallation Wizard
# A user-friendly uninstaller for the Web Search application
#

# Colors for terminal fallback
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Application info
APP_NAME="Web Search"
DESKTOP_FILE="$HOME/Desktop/WebSearch.desktop"
APPLICATIONS_FILE="$HOME/.local/share/applications/WebSearch.desktop"
INSTALL_PATH_FILE="$HOME/.websearch_install_path"

# Get install directory from argument or saved file
if [ -n "$1" ]; then
    INSTALL_DIR="$1"
elif [ -f "$INSTALL_PATH_FILE" ]; then
    INSTALL_DIR=$(cat "$INSTALL_PATH_FILE")
else
    INSTALL_DIR="$HOME/.local/share/websearch"
fi

# Check if zenity is available for GUI dialogs
HAS_ZENITY=$(command -v zenity &> /dev/null && echo "yes" || echo "no")

# Function to show GUI message
show_message() {
    local title="$1"
    local message="$2"
    local type="${3:-info}"
    
    if [ "$HAS_ZENITY" = "yes" ]; then
        zenity --$type --title="$title" --text="$message" --width=400 2>/dev/null
    else
        echo -e "${BLUE}[$title]${NC} $message"
    fi
}

# Function to show GUI question
ask_question() {
    local title="$1"
    local message="$2"
    
    if [ "$HAS_ZENITY" = "yes" ]; then
        zenity --question --title="$title" --text="$message" --width=400 2>/dev/null
        return $?
    else
        echo -e "${YELLOW}$message (y/n)${NC}"
        read -r answer
        [[ "$answer" =~ ^[Yy] ]]
        return $?
    fi
}

# Function to show progress
show_progress() {
    local message="$1"
    local percent="$2"
    
    if [ "$HAS_ZENITY" = "yes" ]; then
        echo "$percent"
        echo "# $message"
    else
        printf "\r${BLUE}[%3d%%]${NC} %s" "$percent" "$message"
    fi
}

# Welcome/Confirm screen
show_welcome() {
    if [ "$HAS_ZENITY" = "yes" ]; then
        zenity --warning --title="Uninstall $APP_NAME" \
            --text="<b>$APP_NAME Uninstaller</b>\n\n\
This will remove $APP_NAME from your computer.\n\n\
<b>The following will be removed:</b>\n\
• Application files in: $INSTALL_DIR\n\
• Desktop shortcut\n\
• Application menu entry\n\n\
Your search history and personal data are not stored by this application." \
            --width=450 2>/dev/null
    else
        echo ""
        echo -e "${RED}╔══════════════════════════════════════════════════════════╗${NC}"
        echo -e "${RED}║${NC}          ${YELLOW}$APP_NAME Uninstaller${NC}                          ${RED}║${NC}"
        echo -e "${RED}╠══════════════════════════════════════════════════════════╣${NC}"
        echo -e "${RED}║${NC}  This will remove $APP_NAME from your computer.           ${RED}║${NC}"
        echo -e "${RED}║${NC}                                                            ${RED}║${NC}"
        echo -e "${RED}║${NC}  The following will be removed:                            ${RED}║${NC}"
        echo -e "${RED}║${NC}  • Application files                                       ${RED}║${NC}"
        echo -e "${RED}║${NC}  • Desktop shortcut                                        ${RED}║${NC}"
        echo -e "${RED}║${NC}  • Application menu entry                                  ${RED}║${NC}"
        echo -e "${RED}╚══════════════════════════════════════════════════════════╝${NC}"
        echo ""
    fi
}

# Perform uninstallation
do_uninstall() {
    (
        show_progress "Removing desktop shortcut..." 20
        rm -f "$DESKTOP_FILE" 2>/dev/null
        sleep 0.5
        
        show_progress "Removing application menu entry..." 40
        rm -f "$APPLICATIONS_FILE" 2>/dev/null
        sleep 0.5
        
        show_progress "Removing application files..." 70
        if [ -d "$INSTALL_DIR" ]; then
            rm -rf "$INSTALL_DIR" 2>/dev/null
        fi
        sleep 0.5
        
        show_progress "Cleaning up..." 90
        rm -f "$INSTALL_PATH_FILE" 2>/dev/null
        sleep 0.5
        
        show_progress "Uninstallation complete!" 100
        sleep 1
        
    ) | if [ "$HAS_ZENITY" = "yes" ]; then
        zenity --progress --title="Uninstalling $APP_NAME" --text="Starting uninstallation..." \
               --percentage=0 --auto-close --width=400 2>/dev/null
    else
        cat
    fi
}

# Show completion message
show_complete() {
    if [ "$HAS_ZENITY" = "yes" ]; then
        zenity --info --title="Uninstallation Complete" \
            --text="<b>$APP_NAME has been successfully removed!</b>\n\n\
All application files and shortcuts have been deleted.\n\n\
Thank you for trying $APP_NAME!\n\n\
If you wish to reinstall, run the installer again." \
            --width=400 2>/dev/null
    else
        echo ""
        echo -e "${GREEN}╔══════════════════════════════════════════════════════════╗${NC}"
        echo -e "${GREEN}║${NC}          ${GREEN}Uninstallation Complete!${NC}                       ${GREEN}║${NC}"
        echo -e "${GREEN}╠══════════════════════════════════════════════════════════╣${NC}"
        echo -e "${GREEN}║${NC}  $APP_NAME has been successfully removed.                ${GREEN}║${NC}"
        echo -e "${GREEN}║${NC}                                                            ${GREEN}║${NC}"
        echo -e "${GREEN}║${NC}  Thank you for trying $APP_NAME!                           ${GREEN}║${NC}"
        echo -e "${GREEN}╚══════════════════════════════════════════════════════════╝${NC}"
        echo ""
    fi
}

# Main uninstallation flow
main() {
    echo "Starting $APP_NAME uninstaller..."
    
    # Check if app is installed
    if [ ! -d "$INSTALL_DIR" ] && [ ! -f "$DESKTOP_FILE" ] && [ ! -f "$APPLICATIONS_FILE" ]; then
        show_message "Not Installed" "$APP_NAME does not appear to be installed." "warning"
        exit 0
    fi
    
    # Show welcome/warning
    show_welcome
    
    # Confirm uninstallation
    if ! ask_question "Confirm Uninstallation" "Are you sure you want to uninstall $APP_NAME?\n\nThis action cannot be undone."; then
        show_message "Cancelled" "Uninstallation was cancelled." "info"
        exit 0
    fi
    
    # Kill any running instances
    pkill -f "searchengine.WebSearchGUI" 2>/dev/null
    sleep 1
    
    # Perform uninstallation
    do_uninstall
    
    # Show completion
    show_complete
    
    echo "Uninstallation finished successfully!"
}

# Run main
main
