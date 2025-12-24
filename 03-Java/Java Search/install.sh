#!/bin/bash
#
# Web Search - Installation Wizard
# A user-friendly installer for the Web Search application
#

# Colors for terminal fallback
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Application info
APP_NAME="Web Search"
APP_VERSION="1.0.0"
APP_DESCRIPTION="Search the World Wide Web with Google, DuckDuckGo, Bing, and more"
SOURCE_DIR="$(cd "$(dirname "$0")" && pwd)"
DEFAULT_INSTALL_DIR="$HOME/.local/share/websearch"
DESKTOP_FILE="$HOME/Desktop/WebSearch.desktop"
APPLICATIONS_DIR="$HOME/.local/share/applications"

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

# Function to get directory from user
get_install_dir() {
    if [ "$HAS_ZENITY" = "yes" ]; then
        local dir=$(zenity --file-selection --directory --title="Choose Installation Directory" \
            --filename="$DEFAULT_INSTALL_DIR" 2>/dev/null)
        if [ -z "$dir" ]; then
            echo "$DEFAULT_INSTALL_DIR"
        else
            echo "$dir"
        fi
    else
        echo -e "${YELLOW}Enter installation directory [${DEFAULT_INSTALL_DIR}]:${NC}"
        read -r dir
        if [ -z "$dir" ]; then
            echo "$DEFAULT_INSTALL_DIR"
        else
            echo "$dir"
        fi
    fi
}

# Check for Java
check_java() {
    if ! command -v java &> /dev/null; then
        show_message "Error" "Java is not installed!\n\nPlease install Java first:\nsudo apt install default-jdk" "error"
        exit 1
    fi
}

# Check for JavaFX
check_javafx() {
    if [ ! -d "/usr/share/openjfx/lib" ]; then
        if ask_question "Missing Component" "JavaFX is required but not installed.\n\nWould you like to install it now?\n(Requires sudo password)"; then
            if [ "$HAS_ZENITY" = "yes" ]; then
                pkexec apt install -y openjfx
            else
                sudo apt install -y openjfx
            fi
            
            if [ $? -ne 0 ]; then
                show_message "Error" "Failed to install JavaFX.\n\nPlease install manually:\nsudo apt install openjfx" "error"
                exit 1
            fi
        else
            show_message "Installation Cancelled" "JavaFX is required for this application." "warning"
            exit 1
        fi
    fi
}

# Main installation function
do_install() {
    local install_dir="$1"
    
    (
        show_progress "Creating installation directory..." 10
        mkdir -p "$install_dir"
        sleep 0.5
        
        show_progress "Copying application files..." 25
        cp -r "$SOURCE_DIR/src" "$install_dir/"
        cp "$SOURCE_DIR/icon.svg" "$install_dir/" 2>/dev/null
        cp "$SOURCE_DIR/README.md" "$install_dir/" 2>/dev/null
        sleep 0.5
        
        show_progress "Compiling application..." 40
        mkdir -p "$install_dir/out"
        javac --module-path /usr/share/openjfx/lib \
              --add-modules javafx.controls,javafx.web,javafx.swing \
              -d "$install_dir/out" \
              "$install_dir/src/main/java/searchengine/"*.java 2>/dev/null
        sleep 0.5
        
        show_progress "Creating launcher script..." 60
        cat > "$install_dir/run-websearch.sh" << 'LAUNCHER'
#!/bin/bash
cd "$(dirname "$0")"
exec java --module-path /usr/share/openjfx/lib \
    --add-modules javafx.controls,javafx.web,javafx.swing \
    --add-opens javafx.graphics/com.sun.glass.utils=ALL-UNNAMED \
    -cp out searchengine.WebSearchGUI
LAUNCHER
        chmod +x "$install_dir/run-websearch.sh"
        sleep 0.5
        
        show_progress "Creating desktop shortcut..." 75
        mkdir -p "$APPLICATIONS_DIR"
        cat > "$APPLICATIONS_DIR/WebSearch.desktop" << DESKTOP
[Desktop Entry]
Version=1.0
Type=Application
Name=Web Search
Comment=$APP_DESCRIPTION
Exec=$install_dir/run-websearch.sh
Icon=$install_dir/icon.svg
Terminal=false
Categories=Network;WebBrowser;Java;
Keywords=search;web;browser;google;bing;duckduckgo;
StartupNotify=true
DESKTOP
        
        # Copy to desktop if it exists
        if [ -d "$HOME/Desktop" ]; then
            cp "$APPLICATIONS_DIR/WebSearch.desktop" "$HOME/Desktop/"
            chmod +x "$HOME/Desktop/WebSearch.desktop"
            gio set "$HOME/Desktop/WebSearch.desktop" metadata::trusted true 2>/dev/null
        fi
        sleep 0.5
        
        show_progress "Creating uninstaller..." 90
        cat > "$install_dir/uninstall.sh" << UNINSTALL
#!/bin/bash
# Web Search Uninstaller
"$SOURCE_DIR/uninstall.sh" "$install_dir"
UNINSTALL
        chmod +x "$install_dir/uninstall.sh"
        
        # Save install location for uninstaller
        echo "$install_dir" > "$HOME/.websearch_install_path"
        
        show_progress "Installation complete!" 100
        sleep 1
        
    ) | if [ "$HAS_ZENITY" = "yes" ]; then
        zenity --progress --title="Installing $APP_NAME" --text="Starting installation..." \
               --percentage=0 --auto-close --width=400 2>/dev/null
    else
        cat  # Just consume the output in terminal mode
    fi
}

# Welcome screen
show_welcome() {
    if [ "$HAS_ZENITY" = "yes" ]; then
        zenity --info --title="Welcome to $APP_NAME Setup" \
            --text="<b>Welcome to the $APP_NAME Installation Wizard!</b>\n\n\
Version: $APP_VERSION\n\n\
$APP_DESCRIPTION\n\n\
<b>Features:</b>\n\
• Search with Google, DuckDuckGo, Bing, Wikipedia, YouTube\n\
• Built-in web browser\n\
• Clean, modern interface\n\
• Quick access from your desktop\n\n\
Click OK to continue with the installation." \
            --width=450 2>/dev/null
    else
        echo ""
        echo -e "${BLUE}╔══════════════════════════════════════════════════════════╗${NC}"
        echo -e "${BLUE}║${NC}     ${GREEN}Welcome to $APP_NAME Installation Wizard${NC}           ${BLUE}║${NC}"
        echo -e "${BLUE}╠══════════════════════════════════════════════════════════╣${NC}"
        echo -e "${BLUE}║${NC}  Version: $APP_VERSION                                       ${BLUE}║${NC}"
        echo -e "${BLUE}║${NC}                                                            ${BLUE}║${NC}"
        echo -e "${BLUE}║${NC}  $APP_DESCRIPTION    ${BLUE}║${NC}"
        echo -e "${BLUE}║${NC}                                                            ${BLUE}║${NC}"
        echo -e "${BLUE}║${NC}  Features:                                                 ${BLUE}║${NC}"
        echo -e "${BLUE}║${NC}  • Search with Google, DuckDuckGo, Bing, Wikipedia        ${BLUE}║${NC}"
        echo -e "${BLUE}║${NC}  • Built-in web browser                                   ${BLUE}║${NC}"
        echo -e "${BLUE}║${NC}  • Clean, modern interface                                ${BLUE}║${NC}"
        echo -e "${BLUE}║${NC}  • Quick access from your desktop                         ${BLUE}║${NC}"
        echo -e "${BLUE}╚══════════════════════════════════════════════════════════╝${NC}"
        echo ""
    fi
}

# License agreement
show_license() {
    if [ "$HAS_ZENITY" = "yes" ]; then
        zenity --text-info --title="License Agreement" \
            --width=500 --height=400 \
            --checkbox="I have read and accept the terms" << 'LICENSE' 2>/dev/null
MIT License

Copyright (c) 2024 Web Search Application

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
LICENSE
        return $?
    else
        echo -e "${YELLOW}Do you accept the MIT License terms? (y/n)${NC}"
        read -r answer
        [[ "$answer" =~ ^[Yy] ]]
        return $?
    fi
}

# Installation complete
show_complete() {
    local install_dir="$1"
    
    if [ "$HAS_ZENITY" = "yes" ]; then
        zenity --info --title="Installation Complete!" \
            --text="<b>$APP_NAME has been successfully installed!</b>\n\n\
<b>Installation Location:</b>\n$install_dir\n\n\
<b>How to launch:</b>\n\
• Click the 'Web Search' icon on your Desktop\n\
• Or search for 'Web Search' in your applications menu\n\n\
<b>To uninstall:</b>\n\
Run the uninstaller from the installation folder or use:\n\
$install_dir/uninstall.sh\n\n\
Enjoy searching the web!" \
            --width=450 2>/dev/null
        
        # Ask if user wants to launch now
        if ask_question "Launch Application" "Would you like to launch $APP_NAME now?"; then
            "$install_dir/run-websearch.sh" &
        fi
    else
        echo ""
        echo -e "${GREEN}╔══════════════════════════════════════════════════════════╗${NC}"
        echo -e "${GREEN}║${NC}          ${GREEN}Installation Complete!${NC}                         ${GREEN}║${NC}"
        echo -e "${GREEN}╠══════════════════════════════════════════════════════════╣${NC}"
        echo -e "${GREEN}║${NC}  Installed to: $install_dir"
        echo -e "${GREEN}║${NC}                                                            ${GREEN}║${NC}"
        echo -e "${GREEN}║${NC}  Launch from your Desktop or Applications menu            ${GREEN}║${NC}"
        echo -e "${GREEN}╚══════════════════════════════════════════════════════════╝${NC}"
        echo ""
        echo -e "${YELLOW}Launch now? (y/n)${NC}"
        read -r answer
        if [[ "$answer" =~ ^[Yy] ]]; then
            "$install_dir/run-websearch.sh" &
        fi
    fi
}

# Main installation flow
main() {
    echo "Starting $APP_NAME installer..."
    
    # Show welcome
    show_welcome
    
    # Show license
    if ! show_license; then
        show_message "Installation Cancelled" "You must accept the license to continue." "warning"
        exit 0
    fi
    
    # Check requirements
    check_java
    check_javafx
    
    # Get installation directory
    INSTALL_DIR=$(get_install_dir)
    
    # Confirm installation
    if ! ask_question "Confirm Installation" "Install $APP_NAME to:\n\n$INSTALL_DIR\n\nContinue?"; then
        show_message "Installation Cancelled" "Installation was cancelled by user." "info"
        exit 0
    fi
    
    # Perform installation
    do_install "$INSTALL_DIR"
    
    # Show completion
    show_complete "$INSTALL_DIR"
    
    echo "Installation finished successfully!"
}

# Run main
main
