#!/bin/bash
# Launch GUI with full debug output
cd /home/robertmarshall/Desktop/green_linux
python -m powerapp.gtk.main 2>&1 | tee /tmp/powerapp_debug.log &
echo "GUI launched. Check /tmp/powerapp_debug.log for debug output."
echo "Run: tail -f /tmp/powerapp_debug.log"
