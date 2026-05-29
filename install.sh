#!/bin/bash
# =============================================================================
# Veil RFM Analytics — One-click install for macOS (Apple Silicon + Intel)
# =============================================================================
set -e

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'
log() { echo -e "${GREEN}[OK]${NC} $1"; }
warn() { echo -e "${YELLOW}[..]${NC} $1"; }
err() { echo -e "${RED}[ERR]${NC} $1"; exit 1; }

echo "========================================"
echo " Veil RFM Analytics — macOS Installer"
echo "========================================"
echo ""

# --- Check architecture ---
ARCH=$(uname -m)
[[ "$ARCH" == "arm64" ]] && LABEL="Apple Silicon (M-series)" || LABEL="Intel"
log "Detected: $ARCH ($LABEL)"

# --- Xcode CLI tools ---
if ! xcode-select -p &>/dev/null; then
    warn "Installing Xcode Command Line Tools..."
    xcode-select --install 2>/dev/null || true
    echo "Please re-run this script after Xcode CLI tools installation completes."
    exit 0
fi
log "Xcode CLI tools: OK"

# --- Homebrew ---
if ! command -v brew &>/dev/null; then
    warn "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    if [[ "$ARCH" == "arm64" ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    else
        eval "$(/usr/local/bin/brew shellenv)"
    fi
fi
log "Homebrew: $(brew --version | head -1)"

# --- R ---
if ! command -v R &>/dev/null; then
    warn "Installing R (this may take a few minutes)..."
    brew install r
fi
log "R: $(R --version | head -1)"

# --- Python 3 ---
if ! command -v python3 &>/dev/null; then
    warn "Installing Python 3..."
    brew install python@3
fi
log "Python: $(python3 --version)"

# --- Clone repo ---
REPO_DIR="$HOME/veil"
if [[ -d "$REPO_DIR" ]]; then
    warn "$REPO_DIR exists, pulling latest..."
    cd "$REPO_DIR" && git pull
else
    log "Cloning veil-dotnet to $REPO_DIR..."
    git clone https://github.com/ai-caseylai/veil-dotnet.git "$REPO_DIR"
    cd "$REPO_DIR"
fi
log "Repository: OK"

# --- Python venv + deps ---
cd "$REPO_DIR"
if [[ ! -d "venv" ]]; then
    warn "Creating Python virtual environment..."
    python3 -m venv venv
fi
source venv/bin/activate
pip install --quiet -r source/RIUScripts/requirements.txt
pip install --quiet requests
log "Python deps: OK"

# --- R packages ---
warn "Installing R packages (this takes 10-20 min)..."
Rscript -e '
packages <- c("data.table","dplyr","lubridate","ggplot2","BTYD","coda",
              "optparse","arules","visNetwork","DT","shiny","shinydashboard",
              "shinyjs","googleVis","scales","ggrepel","forcats","xts","plotly",
              "logging","htmltools","qgraph","forecast")
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install) > 0) {
  install.packages(to_install, repos="https://cran.r-project.org", dependencies=TRUE)
}
if (!require("BTYDplus", quietly=TRUE)) {
  install.packages("remotes", repos="https://cran.r-project.org")
  remotes::install_github("cran/BTYDplus")
}
cat("R packages: DONE\n")
' 2>&1 | tail -3
log "R packages: OK"

# --- Sample data ---
warn "Generating sample transaction data..."
source "$REPO_DIR/venv/bin/activate"
python "$REPO_DIR/generate_sample_data.py"
log "Sample data: OK"

# --- Start services ---
echo ""
echo "========================================"
echo " Installation Complete!"
echo "========================================"
echo ""
echo "To start the analytics dashboard:"
echo ""
echo "  cd $REPO_DIR"
echo "  source venv/bin/activate"
echo ""
echo "  # Terminal 1 — Data API"
echo "  cd source/RIUScripts/DataProcessor"
echo "  python map_reduce-driver.py --datapath ../../../RIUData --port 8888"
echo ""
echo "  # Terminal 2 — Shiny Dashboard"
echo "  cd $REPO_DIR/source/RIUScripts/Shiny"
echo "  Rscript runShinyApp.R --path http://127.0.0.1:8888 --bu 107 --port 16479"
echo ""
echo "Then open: http://127.0.0.1:16479/?bu=107"
echo ""
echo "Or run everything with:"
echo "  $REPO_DIR/start.sh"
echo ""
