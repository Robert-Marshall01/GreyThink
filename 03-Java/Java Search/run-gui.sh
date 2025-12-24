#!/bin/bash
# Save important environment vars before cleaning
SAVE_HOME="$HOME"
SAVE_DISPLAY="$DISPLAY"
SAVE_XAUTHORITY="$XAUTHORITY"
SAVE_XDG_RUNTIME_DIR="$XDG_RUNTIME_DIR"
SAVE_DBUS_SESSION_BUS_ADDRESS="$DBUS_SESSION_BUS_ADDRESS"
SAVE_PATH="/snap/bin:/usr/local/bin:/usr/bin:/bin"

# Clear all snap-related vars
for var in $(env | grep -E "^(LD_|SNAP|GTK_|GIO_|GDK_|GSETTINGS_|LOCPATH)" | cut -d= -f1); do
    unset "$var"
done

# Restore needed vars
export HOME="$SAVE_HOME"
export DISPLAY="$SAVE_DISPLAY"
export XAUTHORITY="$SAVE_XAUTHORITY"
export XDG_RUNTIME_DIR="$SAVE_XDG_RUNTIME_DIR"
export DBUS_SESSION_BUS_ADDRESS="$SAVE_DBUS_SESSION_BUS_ADDRESS"
export PATH="$SAVE_PATH"

cd "/home/robert-marshall01/Desktop/Java Search" || exit
exec /usr/bin/java --module-path /usr/share/openjfx/lib --add-modules javafx.controls,javafx.web,javafx.swing -cp out searchengine.WebSearchGUI
