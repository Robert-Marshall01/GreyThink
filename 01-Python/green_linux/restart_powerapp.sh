#!/bin/bash
# Quick script to kill any running PowerApp instances and launch a fresh one

echo "Stopping any running PowerApp instances..."
pkill -f "python.*powerapp.gtk.main"
sleep 1

echo "Launching PowerApp..."
python -m powerapp.gtk.main

