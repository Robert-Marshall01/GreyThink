#!/bin/bash
# Launch the GUI with mock power values enabled
# This is useful for testing on systems without battery/RAPL

export POWERAPP_MOCK_POWER=1
echo "DEBUG: POWERAPP_MOCK_POWER=$POWERAPP_MOCK_POWER"
cd "$(dirname "$0")" || exit 1
echo "DEBUG: Working directory: $(pwd)"
echo "DEBUG: Python version: $(python --version)"
echo "DEBUG: About to launch GUI with mock power mode..."
python -m powerapp.gtk.main "$@"
