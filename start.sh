#!/bin/bash
# =============================================================================
# Veil RFM Analytics — Quick Start
# =============================================================================
set -e
DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$DIR"

source venv/bin/activate

cleanup() {
    echo ""
    echo "Shutting down..."
    kill $API_PID 2>/dev/null
    kill $SHINY_PID 2>/dev/null
    wait 2>/dev/null
    echo "Done."
}
trap cleanup EXIT INT TERM

echo "=== Starting Veil Analytics ==="
echo ""

# Data API
cd source/RIUScripts/DataProcessor
python map_reduce-driver.py --datapath ../../../RIUData --port 8888 &
API_PID=$!
echo "Data API: http://127.0.0.1:8888 (PID $API_PID)"
cd "$DIR"

sleep 2

# Shiny EN
cd source/RIUScripts/Shiny
mkdir -p log
Rscript runShinyApp.R --path "http://127.0.0.1:8888" --bu 107 --port 16479 --logpath ./log/ &
SHINY_PID=$!
echo "Shiny EN: http://127.0.0.1:16479/?bu=107 (PID $SHINY_PID)"
cd "$DIR"

echo ""
echo "Dashboard: http://127.0.0.1:16479/?bu=107"
echo "Press Ctrl+C to stop all services."
echo ""

wait
