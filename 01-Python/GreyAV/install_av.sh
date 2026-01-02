#!/bin/bash
#
# install_av.sh - GreyAV Cross-Platform Installer Script
#
# Installs GreyAV system with dependencies and service registration.
#
# This script performs the following:
#   1. Detects the operating system (Linux, macOS, Windows/WSL)
#   2. Detects the specific distribution (Ubuntu, Mint, Fedora, Arch, etc.)
#   3. Installs required system dependencies using the appropriate package manager
#   4. Creates the installation directory structure
#   5. Sets up a Python virtual environment
#   6. Installs Python dependencies
#   7. Registers and starts the system service (systemd, launchd, or Windows Service)
#
# Supported Platforms:
#   - Ubuntu (18.04+)
#   - Linux Mint (19+)
#   - Debian (10+)
#   - Pop!_OS (20.04+)
#   - Zorin OS (15+)
#   - MX Linux (19+)
#   - Kali Linux
#   - Fedora (35+)
#   - Bazzite (Fedora immutable)
#   - CentOS/RHEL (8+)
#   - Arch Linux
#   - EndeavourOS
#   - Garuda Linux
#   - Manjaro
#   - NixOS
#   - openSUSE
#   - macOS (10.15+)
#   - Windows (via WSL2 or native PowerShell)
#
# Usage:
#   sudo ./install_av.sh           # Linux/macOS
#   ./install_av.sh                # Windows (run as Administrator)
#
# Requirements:
#   - Must be run as root/Administrator
#   - Internet connection for package downloads
#

set -e  # Exit immediately on any error
set -u  # Treat unset variables as errors
set -o pipefail  # Pipe failures cause script failure

# =============================================================================
# OS Detection Variables
# =============================================================================

OS_TYPE=""           # linux, macos, windows
DISTRO_ID=""         # ubuntu, fedora, linuxmint, arch, darwin, windows
DISTRO_ID_LIKE=""    # Parent distro family
DISTRO_VERSION=""    # Version number
DISTRO_CODENAME=""   # Release codename
PKG_MANAGER=""       # apt, dnf, yum, pacman, brew, choco
SERVICE_MANAGER=""   # systemd, launchd, windows
ARCH=""              # x86_64, arm64, aarch64

# =============================================================================
# Configuration (OS-specific paths set later)
# =============================================================================

INSTALL_DIR=""
VENV_DIR=""
SERVICE_NAME="greyav"
SERVICE_FILE=""
LOG_DIR=""
CONFIG_DIR=""
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly MAGENTA='\033[0;35m'
readonly NC='\033[0m' # No Color

# =============================================================================
# Helper Functions
# =============================================================================

# Print colored status messages
print_status() {
    echo -e "${BLUE}[*]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[!]${NC} $1"
}

print_error() {
    echo -e "${RED}[✗]${NC} $1"
}

print_info() {
    echo -e "${CYAN}[i]${NC} $1"
}

# Check if a command exists
command_exists() {
    command -v "$1" &> /dev/null
}

# =============================================================================
# OS Detection Functions
# =============================================================================

# Detect the operating system type
detect_os_type() {
    case "$(uname -s)" in
        Linux*)
            OS_TYPE="linux"
            # Check if running in WSL
            if grep -qi microsoft /proc/version 2>/dev/null; then
                print_info "Detected Windows Subsystem for Linux (WSL)"
            fi
            ;;
        Darwin*)
            OS_TYPE="macos"
            ;;
        CYGWIN*|MINGW*|MSYS*)
            OS_TYPE="windows"
            ;;
        *)
            # Check for Windows native
            if [[ -n "${OS:-}" ]] && [[ "${OS}" == "Windows_NT" ]]; then
                OS_TYPE="windows"
            else
                print_error "Unsupported operating system: $(uname -s)"
                exit 1
            fi
            ;;
    esac
    
    # Detect architecture
    ARCH="$(uname -m)"
    case "$ARCH" in
        x86_64|amd64)
            ARCH="x86_64"
            ;;
        arm64|aarch64)
            ARCH="arm64"
            ;;
        armv7l)
            ARCH="armv7"
            ;;
    esac
    
    print_status "Detected OS type: ${OS_TYPE} (${ARCH})"
}

# Detect Linux distribution
detect_linux_distro() {
    if [[ -f /etc/os-release ]]; then
        source /etc/os-release
        DISTRO_ID="${ID,,}"  # lowercase
        DISTRO_ID_LIKE="${ID_LIKE:-}"
        DISTRO_VERSION="${VERSION_ID:-}"
        DISTRO_CODENAME="${VERSION_CODENAME:-}"
        print_status "Detected distribution: ${PRETTY_NAME:-$DISTRO_ID}"
    elif [[ -f /etc/lsb-release ]]; then
        source /etc/lsb-release
        DISTRO_ID="${DISTRIB_ID,,}"
        DISTRO_VERSION="${DISTRIB_RELEASE:-}"
        DISTRO_CODENAME="${DISTRIB_CODENAME:-}"
        print_status "Detected distribution: ${DISTRIB_DESCRIPTION:-$DISTRO_ID}"
    elif [[ -f /etc/debian_version ]]; then
        DISTRO_ID="debian"
        DISTRO_ID_LIKE="debian"
        DISTRO_VERSION="$(cat /etc/debian_version)"
        print_status "Detected distribution: Debian ${DISTRO_VERSION}"
    elif [[ -f /etc/redhat-release ]]; then
        DISTRO_ID="rhel"
        DISTRO_ID_LIKE="rhel fedora"
        print_status "Detected distribution: RedHat/CentOS"
    elif [[ -f /etc/arch-release ]]; then
        DISTRO_ID="arch"
        DISTRO_ID_LIKE="arch"
        print_status "Detected distribution: Arch Linux"
    elif [[ -f /etc/SuSE-release ]] || [[ -f /etc/SUSE-brand ]]; then
        DISTRO_ID="opensuse"
        DISTRO_ID_LIKE="suse"
        print_status "Detected distribution: openSUSE"
    else
        print_error "Unable to detect Linux distribution"
        exit 1
    fi
    
    # Determine package manager
    determine_linux_package_manager
}

# Determine Linux package manager
determine_linux_package_manager() {
    case "${DISTRO_ID}" in
        # Debian/Ubuntu-based distributions
        ubuntu|debian|linuxmint|pop|elementary|zorin|kali|raspbian|mx|mxlinux|antix)
            PKG_MANAGER="apt"
            ;;
        # Fedora and RPM-based distributions
        fedora)
            PKG_MANAGER="dnf"
            ;;
        # Immutable Fedora-based (Bazzite, Silverblue, etc.)
        bazzite|silverblue|kinoite)
            PKG_MANAGER="rpm-ostree"
            ;;
        rhel|centos|rocky|almalinux|ol|amzn)
            if command_exists dnf; then
                PKG_MANAGER="dnf"
            else
                PKG_MANAGER="yum"
            fi
            ;;
        # Arch-based distributions
        arch|manjaro|endeavouros|garuda|artix|arcolinux)
            PKG_MANAGER="pacman"
            ;;
        # NixOS
        nixos)
            PKG_MANAGER="nix"
            ;;
        opensuse|opensuse-leap|opensuse-tumbleweed|sles)
            PKG_MANAGER="zypper"
            ;;
        alpine)
            PKG_MANAGER="apk"
            ;;
        gentoo)
            PKG_MANAGER="emerge"
            ;;
        void)
            PKG_MANAGER="xbps"
            ;;
        *)
            # Try to detect by ID_LIKE
            if [[ "${DISTRO_ID_LIKE}" == *"debian"* ]] || [[ "${DISTRO_ID_LIKE}" == *"ubuntu"* ]]; then
                PKG_MANAGER="apt"
            elif [[ "${DISTRO_ID_LIKE}" == *"rhel"* ]] || [[ "${DISTRO_ID_LIKE}" == *"fedora"* ]]; then
                # Check for immutable variants
                if command_exists rpm-ostree; then
                    PKG_MANAGER="rpm-ostree"
                else
                    PKG_MANAGER="dnf"
                fi
            elif [[ "${DISTRO_ID_LIKE}" == *"arch"* ]]; then
                PKG_MANAGER="pacman"
            elif [[ "${DISTRO_ID_LIKE}" == *"suse"* ]]; then
                PKG_MANAGER="zypper"
            elif [[ -f /etc/NIXOS ]] || [[ "${DISTRO_ID}" == "nixos" ]]; then
                PKG_MANAGER="nix"
            else
                print_error "Unsupported distribution: ${DISTRO_ID}"
                print_info "Please install dependencies manually"
                exit 1
            fi
            ;;
    esac
    
    print_info "Using package manager: ${PKG_MANAGER}"
}

# Detect macOS version
detect_macos() {
    DISTRO_ID="darwin"
    DISTRO_VERSION="$(sw_vers -productVersion)"
    local major_version="${DISTRO_VERSION%%.*}"
    
    # Map version to codename
    case "$major_version" in
        15) DISTRO_CODENAME="Sequoia" ;;
        14) DISTRO_CODENAME="Sonoma" ;;
        13) DISTRO_CODENAME="Ventura" ;;
        12) DISTRO_CODENAME="Monterey" ;;
        11) DISTRO_CODENAME="Big Sur" ;;
        10) DISTRO_CODENAME="Catalina" ;;
        *)  DISTRO_CODENAME="Unknown" ;;
    esac
    
    print_status "Detected macOS ${DISTRO_VERSION} (${DISTRO_CODENAME})"
    
    # Check for Homebrew
    if command_exists brew; then
        PKG_MANAGER="brew"
        print_info "Using package manager: Homebrew"
    else
        print_warning "Homebrew not found. Installing..."
        install_homebrew
        PKG_MANAGER="brew"
    fi
}

# Detect Windows version
detect_windows() {
    DISTRO_ID="windows"
    
    if command_exists powershell.exe; then
        DISTRO_VERSION="$(powershell.exe -Command '[System.Environment]::OSVersion.Version.ToString()' 2>/dev/null | tr -d '\r')"
    elif command_exists cmd.exe; then
        DISTRO_VERSION="$(cmd.exe /c ver 2>/dev/null | grep -oP '\d+\.\d+' | head -1)"
    fi
    
    print_status "Detected Windows ${DISTRO_VERSION:-Unknown}"
    
    # Check for Chocolatey or winget
    if command_exists choco; then
        PKG_MANAGER="choco"
        print_info "Using package manager: Chocolatey"
    elif command_exists winget; then
        PKG_MANAGER="winget"
        print_info "Using package manager: winget"
    else
        print_warning "No package manager found. Will attempt manual installation."
        PKG_MANAGER="none"
    fi
}

# Main OS detection function
detect_os() {
    detect_os_type
    
    case "$OS_TYPE" in
        linux)
            detect_linux_distro
            SERVICE_MANAGER="systemd"
            # Check for non-systemd systems
            if ! command_exists systemctl; then
                if command_exists rc-service; then
                    SERVICE_MANAGER="openrc"
                elif command_exists sv; then
                    SERVICE_MANAGER="runit"
                else
                    SERVICE_MANAGER="sysvinit"
                fi
            fi
            ;;
        macos)
            detect_macos
            SERVICE_MANAGER="launchd"
            ;;
        windows)
            detect_windows
            SERVICE_MANAGER="windows"
            ;;
    esac
    
    print_info "Service manager: ${SERVICE_MANAGER}"
}

# Set OS-specific paths
set_os_paths() {
    case "$OS_TYPE" in
        linux)
            INSTALL_DIR="/opt/greyav"
            LOG_DIR="/var/log/greyav"
            CONFIG_DIR="/etc/greyav"
            SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"
            ;;
        macos)
            INSTALL_DIR="/usr/local/greyav"
            LOG_DIR="/usr/local/var/log/greyav"
            CONFIG_DIR="/usr/local/etc/greyav"
            SERVICE_FILE="/Library/LaunchDaemons/com.greyav.daemon.plist"
            ;;
        windows)
            INSTALL_DIR="/c/Program Files/GreyAV"
            LOG_DIR="/c/ProgramData/GreyAV/logs"
            CONFIG_DIR="/c/ProgramData/GreyAV/config"
            SERVICE_FILE=""  # Handled differently on Windows
            ;;
    esac
    
    VENV_DIR="${INSTALL_DIR}/venv"
    
    print_info "Installation directory: ${INSTALL_DIR}"
}

# =============================================================================
# Privilege Check Functions
# =============================================================================

# Check if running with appropriate privileges
check_privileges() {
    case "$OS_TYPE" in
        linux|macos)
            if [[ $EUID -ne 0 ]]; then
                print_error "This script must be run as root (use sudo)"
                exit 1
            fi
            ;;
        windows)
            # For Windows/WSL, check if running as admin
            if ! net session &>/dev/null 2>&1; then
                print_warning "Running without administrator privileges"
                print_info "Some features may not work correctly"
            fi
            ;;
    esac
}

# Install Homebrew on macOS
install_homebrew() {
    print_status "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    
    # Add Homebrew to PATH for Apple Silicon
    if [[ "$ARCH" == "arm64" ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi
    
    print_success "Homebrew installed"
}

# =============================================================================
# Package Installation Functions - OS Specific
# =============================================================================

# Install packages on Ubuntu/Debian/Mint
install_debian_packages() {
    print_status "Updating package lists..."
    apt-get update -qq

    local distro_name="${DISTRO_ID}"
    print_status "Installing dependencies for ${distro_name^}..."
    
    apt-get install -y -qq \
        python3 \
        python3-pip \
        python3-venv \
        python3-dev \
        build-essential \
        curl \
        wget \
        ca-certificates \
        gnupg \
        lsb-release \
        libffi-dev \
        libssl-dev
    
    # Distribution-specific extras
    case "${DISTRO_ID}" in
        ubuntu)
            # Ubuntu-specific packages
            apt-get install -y -qq software-properties-common apt-transport-https || true
            print_info "Ubuntu: Installed additional tools"
            ;;
        linuxmint)
            # Linux Mint specific packages
            apt-get install -y -qq software-properties-common || true
            print_info "Linux Mint: Installed additional tools"
            ;;
        debian)
            # Debian-specific packages
            apt-get install -y -qq apt-transport-https || true
            print_info "Debian: Installed additional tools"
            ;;
        pop)
            # Pop!_OS specific (System76)
            apt-get install -y -qq software-properties-common || true
            print_info "Pop!_OS: Installed additional tools"
            ;;
        zorin)
            # Zorin OS specific
            apt-get install -y -qq software-properties-common || true
            print_info "Zorin OS: Installed additional tools"
            ;;
        kali)
            # Kali Linux specific (security-focused)
            apt-get install -y -qq kali-linux-core 2>/dev/null || true
            print_info "Kali Linux: Security tools available"
            ;;
        mx|mxlinux)
            # MX Linux specific
            apt-get install -y -qq mx-repo-list 2>/dev/null || true
            print_info "MX Linux: Installed additional tools"
            ;;
    esac
    
    print_success "Debian/Ubuntu packages installed"
}

# Install packages on Fedora
install_fedora_packages() {
    print_status "Installing dependencies for Fedora ${DISTRO_VERSION}..."
    
    dnf install -y -q \
        python3 \
        python3-pip \
        python3-devel \
        python3-virtualenv \
        gcc \
        gcc-c++ \
        make \
        curl \
        wget \
        ca-certificates \
        openssl-devel \
        libffi-devel \
        redhat-rpm-config
    
    # Fedora-specific: Enable Fusion repos for extra tools (optional)
    if [[ "${DISTRO_VERSION:-0}" -ge 38 ]]; then
        print_info "Fedora ${DISTRO_VERSION}: Modern toolchain enabled"
    fi
    
    print_success "Fedora packages installed"
}

# Install packages on RHEL/CentOS/Rocky/Alma
install_rhel_packages() {
    print_status "Installing dependencies for RHEL/CentOS..."
    
    # Enable EPEL repository for additional packages
    $PKG_MANAGER install -y -q epel-release || true
    
    $PKG_MANAGER install -y -q \
        python3 \
        python3-pip \
        python3-devel \
        gcc \
        gcc-c++ \
        make \
        curl \
        wget \
        ca-certificates \
        openssl-devel \
        libffi-devel
    
    # Install virtualenv via pip if not available as package
    if ! python3 -m venv --help &> /dev/null; then
        pip3 install virtualenv
    fi
    
    print_success "RHEL/CentOS packages installed"
}

# Install packages on Arch Linux
install_arch_packages() {
    print_status "Installing dependencies for Arch Linux..."
    
    # Update package database
    pacman -Sy --noconfirm
    
    pacman -S --needed --noconfirm \
        python \
        python-pip \
        python-virtualenv \
        base-devel \
        curl \
        wget \
        ca-certificates \
        openssl \
        libffi
    
    print_success "Arch Linux packages installed"
}

# Install packages on openSUSE
install_suse_packages() {
    print_status "Installing dependencies for openSUSE..."
    
    zypper --non-interactive refresh
    
    zypper --non-interactive install \
        python3 \
        python3-pip \
        python3-virtualenv \
        python3-devel \
        gcc \
        gcc-c++ \
        make \
        curl \
        wget \
        ca-certificates \
        libopenssl-devel \
        libffi-devel
    
    print_success "openSUSE packages installed"
}

# Install packages on NixOS
install_nixos_packages() {
    print_status "Installing dependencies for NixOS..."
    
    # NixOS uses declarative configuration, but we can use nix-env for user packages
    # or nix-shell for development environments
    print_info "NixOS detected - using nix-env for installation"
    
    # Check if running as root in NixOS
    if [[ $EUID -eq 0 ]]; then
        # System-wide installation (requires configuration.nix modification)
        print_warning "For NixOS, consider adding to configuration.nix instead:"
        print_info "  environment.systemPackages = with pkgs; ["
        print_info "    python311"
        print_info "    python311Packages.pip"
        print_info "    python311Packages.virtualenv"
        print_info "    openssl"
        print_info "    curl"
        print_info "    wget"
        print_info "  ];"
        print_info ""
    fi
    
    # Use nix-env for immediate installation
    if command_exists nix-env; then
        nix-env -iA nixpkgs.python311 || nix-env -iA nixpkgs.python3
        nix-env -iA nixpkgs.python311Packages.pip || true
        nix-env -iA nixpkgs.python311Packages.virtualenv || true
        nix-env -iA nixpkgs.curl nixpkgs.wget || true
        print_success "NixOS packages installed via nix-env"
    elif command_exists nix; then
        # Use nix profile for newer Nix versions
        nix profile install nixpkgs#python311 || nix profile install nixpkgs#python3
        print_success "NixOS packages installed via nix profile"
    else
        print_error "Neither nix-env nor nix command found"
        exit 1
    fi
}

# Install packages on immutable Fedora variants (Bazzite, Silverblue, Kinoite)
install_immutable_fedora_packages() {
    print_status "Installing dependencies for ${DISTRO_ID^} (immutable)..."
    
    print_info "Detected immutable Fedora variant - using rpm-ostree"
    print_warning "Note: System will need to reboot after package layering"
    
    # Layer essential packages
    rpm-ostree install --idempotent --allow-inactive \
        python3 \
        python3-pip \
        python3-devel \
        python3-virtualenv \
        gcc \
        gcc-c++ \
        make \
        curl \
        wget \
        openssl-devel \
        libffi-devel \
        || true
    
    # Check if reboot is needed
    if rpm-ostree status | grep -q "pending"; then
        print_warning "Packages layered - reboot required to complete installation"
        print_info "After reboot, re-run this installer to continue setup"
        
        read -rp "Reboot now? [y/N] " reboot_choice
        if [[ "${reboot_choice,,}" == "y" ]]; then
            systemctl reboot
        else
            print_info "Please reboot manually and re-run installer"
            exit 0
        fi
    fi
    
    print_success "Immutable Fedora packages layered"
}

# Install packages on macOS
install_macos_packages() {
    print_status "Installing dependencies for macOS ${DISTRO_CODENAME}..."
    
    # Update Homebrew
    brew update --quiet
    
    # Install packages
    brew install --quiet \
        python@3.11 \
        openssl@3 \
        libffi \
        curl \
        wget || true
    
    # Ensure Python is linked
    brew link --overwrite python@3.11 2>/dev/null || true
    
    # macOS version-specific configurations
    case "${DISTRO_CODENAME}" in
        "Sonoma"|"Sequoia")
            print_info "macOS ${DISTRO_CODENAME}: Using modern security settings"
            ;;
        "Ventura"|"Monterey")
            print_info "macOS ${DISTRO_CODENAME}: Compatible configuration"
            ;;
        *)
            print_warning "macOS ${DISTRO_CODENAME}: Legacy support mode"
            ;;
    esac
    
    print_success "macOS packages installed"
}

# Install packages on Windows (via Chocolatey or manual)
install_windows_packages() {
    print_status "Installing dependencies for Windows..."
    
    if [[ "${PKG_MANAGER}" == "choco" ]]; then
        choco install -y python3 curl wget || true
    elif [[ "${PKG_MANAGER}" == "winget" ]]; then
        winget install -e --id Python.Python.3.11 --silent || true
        winget install -e --id cURL.cURL --silent || true
    else
        print_warning "No package manager available"
        print_info "Please install Python 3.11+ manually from python.org"
        
        # Check if Python is already installed
        if command_exists python3 || command_exists python; then
            print_success "Python already installed"
        else
            print_error "Python not found. Please install Python 3.11+ and re-run this script"
            exit 1
        fi
    fi
    
    print_success "Windows packages installed"
}

# Install dependencies based on detected OS
install_dependencies() {
    print_status "Installing system dependencies..."
    
    case "$OS_TYPE" in
        linux)
            install_linux_dependencies
            ;;
        macos)
            install_macos_packages
            ;;
        windows)
            install_windows_packages
            ;;
    esac
    
    print_success "System dependencies installed"
}

# Install Linux dependencies based on distribution
install_linux_dependencies() {
    case "${PKG_MANAGER}" in
        apt)
            install_debian_packages
            ;;
        dnf)
            if [[ "${DISTRO_ID}" == "fedora" ]]; then
                install_fedora_packages
            else
                install_rhel_packages
            fi
            ;;
        yum)
            install_rhel_packages
            ;;
        pacman)
            install_arch_packages
            ;;
        zypper)
            install_suse_packages
            ;;
        nix)
            install_nixos_packages
            ;;
        rpm-ostree)
            install_immutable_fedora_packages
            ;;
        *)
            print_error "Unsupported package manager: ${PKG_MANAGER}"
            exit 1
            ;;
    esac
}

# =============================================================================
# Installation Functions
# =============================================================================

# Create installation directory structure
create_directories() {
    print_status "Creating installation directories..."
    
    # Create main installation directory
    mkdir -p "${INSTALL_DIR}"
    mkdir -p "${INSTALL_DIR}/logs"
    mkdir -p "${INSTALL_DIR}/data"
    mkdir -p "${INSTALL_DIR}/quarantine"
    mkdir -p "${INSTALL_DIR}/config"
    
    # Create log directory
    mkdir -p "${LOG_DIR}"
    
    print_success "Directories created at ${INSTALL_DIR}"
}

# Copy source files to installation directory
copy_source_files() {
    print_status "Copying source files to ${INSTALL_DIR}..."
    
    # Copy Python files
    if ls "${SCRIPT_DIR}"/*.py 1> /dev/null 2>&1; then
        cp -v "${SCRIPT_DIR}"/*.py "${INSTALL_DIR}/"
    else
        print_warning "No Python files found in ${SCRIPT_DIR}"
    fi
    
    # Copy JSON files (configurations, signatures, etc.)
    if ls "${SCRIPT_DIR}"/*.json 1> /dev/null 2>&1; then
        cp -v "${SCRIPT_DIR}"/*.json "${INSTALL_DIR}/"
    fi
    
    # Copy requirements.txt if exists
    if [[ -f "${SCRIPT_DIR}/requirements.txt" ]]; then
        cp -v "${SCRIPT_DIR}/requirements.txt" "${INSTALL_DIR}/"
    else
        print_warning "No requirements.txt found, creating minimal one..."
        create_requirements_file
    fi
    
    # Copy any additional directories (excluding venv, __pycache__, etc.)
    for dir in "${SCRIPT_DIR}"/*/; do
        dirname=$(basename "$dir")
        if [[ "$dirname" != "venv" ]] && \
           [[ "$dirname" != "__pycache__" ]] && \
           [[ "$dirname" != ".git" ]] && \
           [[ "$dirname" != "node_modules" ]]; then
            if [[ -d "$dir" ]]; then
                cp -rv "$dir" "${INSTALL_DIR}/"
            fi
        fi
    done
    
    print_success "Source files copied"
}

# Create requirements.txt if not exists
create_requirements_file() {
    cat > "${INSTALL_DIR}/requirements.txt" << 'EOF'
# MyAV Python Dependencies
# Auto-generated by install_av.sh

# No external dependencies - using standard library only
# Add dependencies here as needed:
# requests>=2.28.0
# watchdog>=3.0.0
# psutil>=5.9.0
EOF
    print_status "Created minimal requirements.txt"
}

# Create main.py entry point if not exists
create_main_entry() {
    if [[ ! -f "${INSTALL_DIR}/main.py" ]]; then
        print_status "Creating main.py entry point..."
        cat > "${INSTALL_DIR}/main.py" << 'EOF'
#!/usr/bin/env python3
"""
GreyAV Main Entry Point

This is the main entry point for the GreyAV antivirus/EDR system.
It initializes and runs the core components including Socket Intake.
"""

import logging
import sys
import signal
import time
from pathlib import Path

# Add installation directory to path
install_dir = Path(__file__).parent
sys.path.insert(0, str(install_dir))

# Configure logging
log_dir = install_dir / 'logs'
log_dir.mkdir(exist_ok=True)

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout),
        logging.FileHandler(log_dir / 'greyav.log')
    ]
)

logger = logging.getLogger('GreyAV')

# Global flag for graceful shutdown
running = True
socket_intake_started = False

def signal_handler(signum, frame):
    """Handle shutdown signals gracefully."""
    global running
    logger.info(f"Received signal {signum}, initiating graceful shutdown...")
    running = False

def main():
    """Main entry point for GreyAV service."""
    global running, socket_intake_started
    
    # Register signal handlers
    signal.signal(signal.SIGTERM, signal_handler)
    signal.signal(signal.SIGINT, signal_handler)
    
    logger.info("=" * 60)
    logger.info("GreyAV Antivirus/EDR System Starting...")
    logger.info("=" * 60)
    
    # Import available modules
    modules_loaded = []
    
    try:
        from greyav import GreyAV
        modules_loaded.append("GreyAV Core")
    except ImportError as e:
        logger.warning(f"GreyAV core module not found: {e}")
    
    try:
        from behavioral_engine import BehavioralEngine
        modules_loaded.append("Behavioral Engine")
    except ImportError:
        logger.debug("Behavioral Engine module not available")
    
    try:
        from av_immune_system import AVImmuneSystem
        modules_loaded.append("AV Immune System")
    except ImportError:
        logger.debug("AV Immune System module not available")
    
    # Start Socket Intake listener on all default ports
    try:
        from socket_intake import (
            start_listener, stop_listener, is_running,
            get_active_ports, DEFAULT_PORTS
        )
        start_listener(host='0.0.0.0', ports=DEFAULT_PORTS, blocking=False)
        socket_intake_started = True
        
        # Give listeners time to start
        time.sleep(0.5)
        active = get_active_ports()
        
        if active:
            modules_loaded.append(f"Socket Intake ({len(active)} ports)")
            logger.info(f"Socket Intake listening on {len(active)} ports")
            logger.info(f"Active ports: {', '.join(str(p) for p in sorted(active))}")
        else:
            modules_loaded.append("Socket Intake (pending)")
            logger.info(f"Socket Intake started for {len(DEFAULT_PORTS)} ports")
            logger.info("Note: Ports < 1024 require root privileges")
    except ImportError:
        logger.debug("Socket Intake module not available")
    except Exception as e:
        logger.warning(f"Failed to start Socket Intake: {e}")
    
    logger.info(f"Loaded modules: {', '.join(modules_loaded) or 'None'}")
    logger.info("GreyAV service is running. Press Ctrl+C to stop.")
    
    # Main service loop
    while running:
        try:
            # Service heartbeat
            time.sleep(10)
            logger.debug("Service heartbeat")
        except Exception as e:
            logger.error(f"Error in main loop: {e}")
            time.sleep(5)
    
    # Cleanup
    if socket_intake_started:
        try:
            from socket_intake import stop_listener
            stop_listener()
            logger.info("Socket Intake stopped")
        except Exception:
            pass
    
    logger.info("GreyAV service stopped.")
    return 0

if __name__ == "__main__":
    sys.exit(main())
EOF
        chmod +x "${INSTALL_DIR}/main.py"
        print_success "Created main.py entry point"
    else
        print_success "main.py already exists"
    fi
}

# Create Python virtual environment
create_virtualenv() {
    print_status "Creating Python virtual environment..."
    
    # Remove existing venv if corrupted
    if [[ -d "${VENV_DIR}" ]]; then
        if ! "${VENV_DIR}/bin/python3" --version &> /dev/null; then
            print_warning "Existing venv appears corrupted, recreating..."
            rm -rf "${VENV_DIR}"
        else
            print_success "Virtual environment already exists"
            return
        fi
    fi
    
    # Create new virtual environment
    python3 -m venv "${VENV_DIR}"
    
    # Upgrade pip in the virtual environment
    "${VENV_DIR}/bin/pip" install --upgrade pip --quiet
    
    print_success "Virtual environment created at ${VENV_DIR}"
}

# Install Python requirements
install_python_requirements() {
    print_status "Installing Python requirements..."
    
    if [[ -f "${INSTALL_DIR}/requirements.txt" ]]; then
        "${VENV_DIR}/bin/pip" install -r "${INSTALL_DIR}/requirements.txt" --quiet
        print_success "Python requirements installed"
    else
        print_warning "No requirements.txt found, skipping Python package installation"
    fi
}

# Create systemd service file (Linux)
create_systemd_service() {
    print_status "Creating systemd service..."
    
    cat > "${SERVICE_FILE}" << EOF
[Unit]
Description=GreyAV Antivirus/EDR System
Documentation=https://github.com/greyav/greyav
After=network.target docker.service
Wants=network.target

[Service]
Type=simple
User=root
Group=root
WorkingDirectory=${INSTALL_DIR}
Environment="PATH=${VENV_DIR}/bin:/usr/local/bin:/usr/bin:/bin"
Environment="PYTHONUNBUFFERED=1"
ExecStart=${VENV_DIR}/bin/python3 ${INSTALL_DIR}/main.py
ExecReload=/bin/kill -HUP \$MAINPID
Restart=on-failure
RestartSec=10
StandardOutput=append:${LOG_DIR}/greyav.log
StandardError=append:${LOG_DIR}/greyav-error.log

# Network capabilities for binding to privileged ports
AmbientCapabilities=CAP_NET_BIND_SERVICE
CapabilityBoundingSet=CAP_NET_BIND_SERVICE CAP_NET_RAW CAP_NET_ADMIN

# Security hardening
NoNewPrivileges=false
ProtectSystem=false
ProtectHome=read-only
PrivateTmp=true

# Resource limits
LimitNOFILE=65536
LimitNPROC=4096

[Install]
WantedBy=multi-user.target
EOF
    
    # Reload systemd to recognize new service
    systemctl daemon-reload
    
    print_success "Systemd service created: ${SERVICE_NAME}.service"
}

# Create launchd service file (macOS)
create_launchd_service() {
    print_status "Creating launchd service..."
    
    cat > "${SERVICE_FILE}" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.greyav.daemon</string>
    
    <key>ProgramArguments</key>
    <array>
        <string>${VENV_DIR}/bin/python3</string>
        <string>${INSTALL_DIR}/main.py</string>
    </array>
    
    <key>WorkingDirectory</key>
    <string>${INSTALL_DIR}</string>
    
    <key>RunAtLoad</key>
    <true/>
    
    <key>KeepAlive</key>
    <dict>
        <key>SuccessfulExit</key>
        <false/>
    </dict>
    
    <key>StandardOutPath</key>
    <string>${LOG_DIR}/greyav.log</string>
    
    <key>StandardErrorPath</key>
    <string>${LOG_DIR}/greyav-error.log</string>
    
    <key>EnvironmentVariables</key>
    <dict>
        <key>PATH</key>
        <string>${VENV_DIR}/bin:/usr/local/bin:/usr/bin:/bin</string>
        <key>PYTHONUNBUFFERED</key>
        <string>1</string>
    </dict>
    
    <key>ProcessType</key>
    <string>Background</string>
    
    <key>LowPriorityIO</key>
    <false/>
    
    <key>Nice</key>
    <integer>-5</integer>
</dict>
</plist>
EOF
    
    # Set proper permissions for launchd plist
    chmod 644 "${SERVICE_FILE}"
    chown root:wheel "${SERVICE_FILE}"
    
    print_success "launchd service created: com.greyav.daemon"
}

# Create Windows service (using NSSM or native sc)
create_windows_service() {
    print_status "Creating Windows service..."
    
    # Create a PowerShell script to manage the service
    local service_script="${INSTALL_DIR}/greyav-service.ps1"
    
    cat > "${service_script}" << 'PWSH'
# GreyAV Windows Service Management Script
param(
    [Parameter(Mandatory=$true)]
    [ValidateSet('install', 'uninstall', 'start', 'stop', 'status')]
    [string]$Action
)

$ServiceName = "GreyAV"
$ServiceDisplayName = "GreyAV Antivirus/EDR System"
$ServiceDescription = "Advanced threat detection and response system"
$InstallDir = "C:\Program Files\GreyAV"
$PythonExe = "$InstallDir\venv\Scripts\python.exe"
$MainScript = "$InstallDir\main.py"

function Install-GreyAVService {
    # Check for NSSM (Non-Sucking Service Manager)
    $nssm = Get-Command nssm -ErrorAction SilentlyContinue
    
    if ($nssm) {
        # Use NSSM for better service management
        & nssm install $ServiceName $PythonExe $MainScript
        & nssm set $ServiceName DisplayName $ServiceDisplayName
        & nssm set $ServiceName Description $ServiceDescription
        & nssm set $ServiceName AppDirectory $InstallDir
        & nssm set $ServiceName Start SERVICE_AUTO_START
        & nssm set $ServiceName AppStdout "$InstallDir\logs\greyav.log"
        & nssm set $ServiceName AppStderr "$InstallDir\logs\greyav-error.log"
        Write-Host "Service installed using NSSM"
    } else {
        # Use native sc.exe
        $binPath = "`"$PythonExe`" `"$MainScript`""
        sc.exe create $ServiceName binPath= $binPath DisplayName= $ServiceDisplayName start= auto
        sc.exe description $ServiceName $ServiceDescription
        Write-Host "Service installed using sc.exe"
        Write-Host "Note: Consider installing NSSM for better service management"
    }
}

function Uninstall-GreyAVService {
    Stop-Service -Name $ServiceName -Force -ErrorAction SilentlyContinue
    $nssm = Get-Command nssm -ErrorAction SilentlyContinue
    if ($nssm) {
        & nssm remove $ServiceName confirm
    } else {
        sc.exe delete $ServiceName
    }
    Write-Host "Service uninstalled"
}

function Start-GreyAVService {
    Start-Service -Name $ServiceName
    Write-Host "Service started"
}

function Stop-GreyAVService {
    Stop-Service -Name $ServiceName -Force
    Write-Host "Service stopped"
}

function Get-GreyAVServiceStatus {
    Get-Service -Name $ServiceName | Format-List Name, Status, StartType
}

switch ($Action) {
    'install'   { Install-GreyAVService }
    'uninstall' { Uninstall-GreyAVService }
    'start'     { Start-GreyAVService }
    'stop'      { Stop-GreyAVService }
    'status'    { Get-GreyAVServiceStatus }
}
PWSH
    
    print_success "Windows service script created: ${service_script}"
    print_info "Run: powershell -ExecutionPolicy Bypass -File greyav-service.ps1 -Action install"
}

# Create service based on OS
create_service() {
    print_status "Creating system service..."
    
    case "${SERVICE_MANAGER}" in
        systemd)
            create_systemd_service
            ;;
        launchd)
            create_launchd_service
            ;;
        windows)
            create_windows_service
            ;;
        openrc)
            create_openrc_service
            ;;
        *)
            print_warning "No service manager configured"
            print_info "You will need to start GreyAV manually"
            ;;
    esac
}

# Create OpenRC service (Alpine, Gentoo)
create_openrc_service() {
    print_status "Creating OpenRC service..."
    
    local service_file="/etc/init.d/${SERVICE_NAME}"
    
    cat > "${service_file}" << EOF
#!/sbin/openrc-run

name="GreyAV Antivirus/EDR System"
description="Advanced threat detection and response"
command="${VENV_DIR}/bin/python3"
command_args="${INSTALL_DIR}/main.py"
command_background=true
pidfile="/run/${SERVICE_NAME}.pid"
directory="${INSTALL_DIR}"

depend() {
    need net
    after firewall
}

start_pre() {
    checkpath --directory --owner root:root --mode 0755 /run
}
EOF
    
    chmod +x "${service_file}"
    print_success "OpenRC service created"
}

# Enable and start the service
enable_and_start_service() {
    print_status "Enabling and starting ${SERVICE_NAME} service..."
    
    case "${SERVICE_MANAGER}" in
        systemd)
            systemctl enable "${SERVICE_NAME}.service"
            systemctl start "${SERVICE_NAME}.service"
            sleep 2
            if systemctl is-active --quiet "${SERVICE_NAME}.service"; then
                print_success "Service ${SERVICE_NAME} is running"
            else
                print_warning "Service started but may not be running correctly"
                print_status "Check status with: systemctl status ${SERVICE_NAME}"
            fi
            ;;
        launchd)
            launchctl load "${SERVICE_FILE}"
            sleep 2
            if launchctl list | grep -q "com.greyav.daemon"; then
                print_success "Service com.greyav.daemon is running"
            else
                print_warning "Service may not have started correctly"
                print_status "Check status with: launchctl list | grep greyav"
            fi
            ;;
        windows)
            print_info "To start the Windows service, run:"
            print_info "  powershell -ExecutionPolicy Bypass -File '${INSTALL_DIR}/greyav-service.ps1' -Action install"
            print_info "  powershell -ExecutionPolicy Bypass -File '${INSTALL_DIR}/greyav-service.ps1' -Action start"
            ;;
        openrc)
            rc-update add "${SERVICE_NAME}" default
            rc-service "${SERVICE_NAME}" start
            print_success "OpenRC service started"
            ;;
        *)
            print_warning "Cannot auto-start service"
            print_info "Start manually with: ${VENV_DIR}/bin/python3 ${INSTALL_DIR}/main.py"
            ;;
    esac
}

# Set proper permissions
set_permissions() {
    print_status "Setting file permissions..."
    
    case "$OS_TYPE" in
        linux)
            set_linux_permissions
            ;;
        macos)
            set_macos_permissions
            ;;
        windows)
            set_windows_permissions
            ;;
    esac
    
    print_success "Permissions set"
}

# Set Linux permissions
set_linux_permissions() {
    chown -R root:root "${INSTALL_DIR}"
    chmod 755 "${INSTALL_DIR}"
    chmod 755 "${INSTALL_DIR}/logs"
    chmod 755 "${INSTALL_DIR}/data"
    chmod 700 "${INSTALL_DIR}/quarantine"
    
    find "${INSTALL_DIR}" -type f -name "*.py" -exec chmod 644 {} \;
    find "${INSTALL_DIR}" -type f -name "*.json" -exec chmod 644 {} \;
    
    if [[ -f "${INSTALL_DIR}/main.py" ]]; then
        chmod 755 "${INSTALL_DIR}/main.py"
    fi
    
    chown -R root:root "${LOG_DIR}"
    chmod 755 "${LOG_DIR}"
}

# Set macOS permissions
set_macos_permissions() {
    chown -R root:wheel "${INSTALL_DIR}"
    chmod 755 "${INSTALL_DIR}"
    chmod 755 "${INSTALL_DIR}/logs"
    chmod 755 "${INSTALL_DIR}/data"
    chmod 700 "${INSTALL_DIR}/quarantine"
    
    find "${INSTALL_DIR}" -type f -name "*.py" -exec chmod 644 {} \;
    find "${INSTALL_DIR}" -type f -name "*.json" -exec chmod 644 {} \;
    
    if [[ -f "${INSTALL_DIR}/main.py" ]]; then
        chmod 755 "${INSTALL_DIR}/main.py"
    fi
    
    chown -R root:wheel "${LOG_DIR}"
    chmod 755 "${LOG_DIR}"
}

# Set Windows permissions
set_windows_permissions() {
    # Windows handles permissions differently
    print_info "Setting Windows ACLs..."
    if command_exists icacls.exe; then
        icacls.exe "$(cygpath -w "${INSTALL_DIR}")" /inheritance:r /grant:r "Administrators:(OI)(CI)F" /grant:r "SYSTEM:(OI)(CI)F" 2>/dev/null || true
    fi
}

# =============================================================================
# Port and Firewall Configuration
# =============================================================================

# Essential ports that MUST remain accessible for normal system operation
# These will NEVER be blocked and will be explicitly allowed
readonly ESSENTIAL_PORTS=(
    22      # SSH - remote access
    53      # DNS - name resolution
    80      # HTTP - web browsing
    443     # HTTPS - secure web
    67      # DHCP server
    68      # DHCP client
    123     # NTP - time sync
)

# Common user service ports to ensure remain accessible
readonly USER_SERVICE_PORTS=(
    8000    # Development servers
    8080    # HTTP proxy/alt
    8443    # HTTPS alt
    3000    # Node.js default
    4200    # Angular
    5000    # Flask/Python
    9000    # PHP-FPM
    3306    # MySQL (local)
    5432    # PostgreSQL (local)
    6379    # Redis (local)
    27017   # MongoDB (local)
    1194    # OpenVPN
    51820   # WireGuard
    2222    # Alt SSH
    631     # CUPS printing
    5353    # mDNS/Bonjour
)

# Check if a port is in a protected list (essential or user service)
is_protected_port() {
    local check_port="$1"
    local port
    
    for port in "${ESSENTIAL_PORTS[@]}"; do
        [[ "$port" == "$check_port" ]] && return 0
    done
    
    for port in "${USER_SERVICE_PORTS[@]}"; do
        [[ "$port" == "$check_port" ]] && return 0
    done
    
    return 1
}

# Backup current firewall state before making changes
backup_firewall_state() {
    print_status "Backing up current firewall configuration..."
    
    local backup_dir="${INSTALL_DIR}/config/firewall_backup"
    mkdir -p "$backup_dir"
    
    local timestamp
    timestamp=$(date +%Y%m%d_%H%M%S)
    
    # Backup UFW if active
    if command -v ufw &> /dev/null; then
        ufw status verbose > "$backup_dir/ufw_status_${timestamp}.txt" 2>/dev/null || true
        if [[ -f /etc/ufw/user.rules ]]; then
            cp /etc/ufw/user.rules "$backup_dir/ufw_user_rules_${timestamp}.backup" 2>/dev/null || true
        fi
    fi
    
    # Backup firewalld if active
    if command -v firewall-cmd &> /dev/null; then
        firewall-cmd --list-all > "$backup_dir/firewalld_${timestamp}.txt" 2>/dev/null || true
    fi
    
    # Backup iptables
    if command -v iptables-save &> /dev/null; then
        iptables-save > "$backup_dir/iptables_${timestamp}.backup" 2>/dev/null || true
    fi
    
    print_success "Firewall state backed up to $backup_dir"
}

# Check if a port is currently in use by a legitimate service
check_port_in_use() {
    local port="$1"
    
    # Check if something is listening on this port
    if command -v ss &> /dev/null; then
        ss -tlnp 2>/dev/null | grep -q ":${port} " && return 0
    elif command -v netstat &> /dev/null; then
        netstat -tlnp 2>/dev/null | grep -q ":${port} " && return 0
    fi
    
    return 1
}

# Configure firewall rules for security
configure_firewall_ports() {
    print_status "Configuring firewall port rules..."
    print_status "Note: Essential ports (SSH, HTTP, HTTPS, DNS, etc.) will remain accessible"
    
    # First, backup current state
    backup_firewall_state
    
    # C2/Backdoor ports - these are ONLY used by malware, safe to block
    local C2_PORTS=(
        4444    # Metasploit default
        4445    # Metasploit alt
        6666    # Common backdoor (NOT 6667 IRC which may be legitimate)
        7777    # Common backdoor
        12345   # NetBus
        27374   # SubSeven
        31337   # Elite/Back Orifice
        31338   # Back Orifice alt
        50050   # Cobalt Strike
        14444   # Monero mining (specific stratum)
        1234    # Common backdoor
    )
    
    # Dangerous legacy ports - block inbound only, user may need outbound
    local DANGEROUS_INBOUND_PORTS=(
        23      # Telnet - never accept inbound
        512     # rexec
        513     # rlogin
        514     # rsh (TCP only, not UDP syslog)
    )
    
    # Note: Ports like 5555 (ADB), 9999, 3333, 69 (TFTP), 111 (RPC) are monitored
    # by the AV but NOT blocked since they may have legitimate uses
    
    # Detect firewall and configure appropriately
    local firewall_type="none"
    
    if command -v ufw &> /dev/null && ufw status 2>/dev/null | grep -q "active"; then
        firewall_type="ufw"
        configure_ufw_safe "${C2_PORTS[@]}" -- "${DANGEROUS_INBOUND_PORTS[@]}"
    elif command -v firewall-cmd &> /dev/null && systemctl is-active --quiet firewalld; then
        firewall_type="firewalld"
        configure_firewalld_safe "${C2_PORTS[@]}" -- "${DANGEROUS_INBOUND_PORTS[@]}"
    elif command -v iptables &> /dev/null; then
        firewall_type="iptables"
        configure_iptables_safe "${C2_PORTS[@]}" -- "${DANGEROUS_INBOUND_PORTS[@]}"
    else
        print_warning "No active firewall found."
        print_status "MyAV will monitor ports but cannot block malicious ones."
        print_status "Consider enabling UFW: sudo ufw enable"
        save_port_configuration "none"
        return 0
    fi
    
    # Save port configuration for reference
    save_port_configuration "$firewall_type"
    
    # Verify essential ports are still accessible
    verify_essential_ports
    
    print_success "Firewall port rules configured (firewall: $firewall_type)"
}

# Configure UFW firewall - SAFE mode that preserves user access
configure_ufw_safe() {
    print_status "Configuring UFW firewall rules (safe mode)..."
    
    local in_dangerous=false
    local blocked_count=0
    
    # FIRST: Ensure all essential ports are ALLOWED before any blocking
    print_status "  Ensuring essential ports remain accessible..."
    for port in "${ESSENTIAL_PORTS[@]}"; do
        # Allow both TCP and UDP for essential services
        ufw allow "$port" comment "MyAV: Essential port" 2>/dev/null || true
    done
    print_success "  Essential ports protected: ${ESSENTIAL_PORTS[*]}"
    
    # Allow common user service ports (insert rules to allow these)
    print_status "  Preserving common service ports..."
    for port in "${USER_SERVICE_PORTS[@]}"; do
        # Only add allow rule if port is currently in use
        if check_port_in_use "$port"; then
            ufw allow "$port"/tcp comment "MyAV: User service" 2>/dev/null || true
            print_status "    Preserved active port: $port"
        fi
    done
    
    # NOW block only the truly malicious C2 ports
    print_status "  Blocking known C2/backdoor ports..."
    for port in "$@"; do
        if [[ "$port" == "--" ]]; then
            in_dangerous=true
            continue
        fi
        
        # Skip if this port is in our essential or user service lists
        if is_protected_port "$port"; then
            print_warning "    Skipping port $port (essential/user service)"
            continue
        fi
        
        # Block C2/backdoor ports - both directions
        if [[ "$in_dangerous" == "false" ]]; then
            ufw deny in "$port"/tcp comment "MyAV: Block C2" 2>/dev/null || true
            ufw deny out "$port"/tcp comment "MyAV: Block C2" 2>/dev/null || true
            ((blocked_count++))
        else
            # Dangerous legacy ports - block inbound only
            ufw deny in "$port"/tcp comment "MyAV: Block legacy" 2>/dev/null || true
            ((blocked_count++))
        fi
    done
    
    print_success "  UFW configured: $blocked_count ports blocked, essential ports preserved"
}

# Configure firewalld - SAFE mode
configure_firewalld_safe() {
    print_status "Configuring firewalld rules (safe mode)..."
    
    local in_dangerous=false
    local blocked_count=0
    
    # Get the default zone
    local default_zone
    default_zone=$(firewall-cmd --get-default-zone 2>/dev/null || echo "public")
    
    # FIRST: Ensure essential services are allowed
    print_status "  Ensuring essential services remain accessible..."
    firewall-cmd --permanent --zone="$default_zone" --add-service=ssh 2>/dev/null || true
    firewall-cmd --permanent --zone="$default_zone" --add-service=http 2>/dev/null || true
    firewall-cmd --permanent --zone="$default_zone" --add-service=https 2>/dev/null || true
    firewall-cmd --permanent --zone="$default_zone" --add-service=dns 2>/dev/null || true
    firewall-cmd --permanent --zone="$default_zone" --add-service=dhcp 2>/dev/null || true
    firewall-cmd --permanent --zone="$default_zone" --add-service=ntp 2>/dev/null || true
    
    # Allow common development ports
    for port in "${USER_SERVICE_PORTS[@]}"; do
        if check_port_in_use "$port"; then
            firewall-cmd --permanent --zone="$default_zone" --add-port="${port}/tcp" 2>/dev/null || true
        fi
    done
    
    # Block C2 and dangerous ports using rich rules (lower priority than allows)
    print_status "  Blocking known C2/backdoor ports..."
    for port in "$@"; do
        if [[ "$port" == "--" ]]; then
            in_dangerous=true
            continue
        fi
        
        # Skip essential ports
        if is_protected_port "$port"; then
            continue
        fi
        
        # Use rich rules with reject (not drop) for better diagnostics
        firewall-cmd --permanent --zone="$default_zone" \
            --add-rich-rule="rule family='ipv4' port port='$port' protocol='tcp' reject" 2>/dev/null || true
        ((blocked_count++))
    done
    
    # Reload to apply changes
    firewall-cmd --reload 2>/dev/null || true
    
    print_success "  firewalld configured: $blocked_count ports blocked, essential services preserved"
}

# Configure iptables directly - SAFE mode
configure_iptables_safe() {
    print_status "Configuring iptables rules (safe mode)..."
    
    local in_dangerous=false
    local blocked_count=0
    
    # Create MyAV chains if they don't exist
    iptables -N MYAV_ALLOW 2>/dev/null || iptables -F MYAV_ALLOW
    iptables -N MYAV_BLOCK 2>/dev/null || iptables -F MYAV_BLOCK
    
    # FIRST: Add ALLOW rules for essential ports (these take precedence)
    print_status "  Adding allow rules for essential ports..."
    for port in "${ESSENTIAL_PORTS[@]}"; do
        # Allow established connections and new connections to essential ports
        iptables -A MYAV_ALLOW -p tcp --dport "$port" -j ACCEPT 2>/dev/null || true
        iptables -A MYAV_ALLOW -p udp --dport "$port" -j ACCEPT 2>/dev/null || true
    done
    
    # Allow common user service ports that are in use
    for port in "${USER_SERVICE_PORTS[@]}"; do
        if check_port_in_use "$port"; then
            iptables -A MYAV_ALLOW -p tcp --dport "$port" -j ACCEPT 2>/dev/null || true
            print_status "    Allowed active service port: $port"
        fi
    done
    
    # Now add BLOCK rules for C2/malicious ports
    print_status "  Blocking known C2/backdoor ports..."
    for port in "$@"; do
        if [[ "$port" == "--" ]]; then
            in_dangerous=true
            continue
        fi
        
        # Skip essential ports
        if is_protected_port "$port"; then
            continue
        fi
        
        # Block inbound to C2 ports
        iptables -A MYAV_BLOCK -p tcp --dport "$port" -j DROP 2>/dev/null || true
        
        # Block outbound to C2 ports (prevent beaconing) - but not for dangerous_inbound
        if [[ "$in_dangerous" == "false" ]]; then
            iptables -A MYAV_BLOCK -p tcp --dport "$port" -j DROP 2>/dev/null || true
        fi
        ((blocked_count++))
    done
    
    # Insert ALLOW chain BEFORE BLOCK chain (order matters!)
    # This ensures essential ports are never blocked
    iptables -C INPUT -j MYAV_ALLOW 2>/dev/null || iptables -I INPUT 1 -j MYAV_ALLOW
    iptables -C OUTPUT -j MYAV_ALLOW 2>/dev/null || iptables -I OUTPUT 1 -j MYAV_ALLOW
    iptables -C INPUT -j MYAV_BLOCK 2>/dev/null || iptables -I INPUT 2 -j MYAV_BLOCK
    iptables -C OUTPUT -j MYAV_BLOCK 2>/dev/null || iptables -I OUTPUT 2 -j MYAV_BLOCK
    
    # Save iptables rules
    if command -v iptables-save &> /dev/null; then
        mkdir -p /etc/iptables
        iptables-save > /etc/iptables/myav-rules.v4
    fi
    
    print_success "  iptables configured: $blocked_count ports blocked, essential ports preserved"
}

# Verify that essential ports are still accessible after configuration
verify_essential_ports() {
    print_status "Verifying essential port accessibility..."
    
    local issues=0
    
    for port in "${ESSENTIAL_PORTS[@]}"; do
        # Check if we can still reach essential ports
        # This is a basic check - just ensure they're not in DROP rules
        case "$port" in
            22)
                if ! timeout 1 bash -c "echo >/dev/tcp/127.0.0.1/$port" 2>/dev/null; then
                    # SSH might not be running, that's OK
                    if check_port_in_use "$port"; then
                        print_warning "  Port $port (SSH) may have connectivity issues"
                        ((issues++))
                    fi
                fi
                ;;
        esac
    done
    
    if [[ $issues -eq 0 ]]; then
        print_success "  All essential ports verified accessible"
    else
        print_warning "  Found $issues potential port issues - review firewall rules"
    fi
}

# Save port configuration for reference and the AV to use
save_port_configuration() {
    local firewall_type="${1:-none}"
    
    print_status "Saving port configuration..."
    
    # Create the configuration directory
    mkdir -p "${INSTALL_DIR}/config"
    
    cat > "${INSTALL_DIR}/config/port_rules.json" << PORTCONFIG
{
    "version": "2.0",
    "generated": "$(date -Iseconds)",
    "firewall": "${firewall_type}",
    "description": "MyAV Port Security Configuration",
    "essential_ports": {
        "description": "Critical ports that are ALWAYS allowed - never blocked",
        "ports": [
            {"port": 22, "service": "ssh", "reason": "Remote administration"},
            {"port": 53, "service": "dns", "reason": "Name resolution"},
            {"port": 80, "service": "http", "reason": "Web browsing"},
            {"port": 443, "service": "https", "reason": "Secure web"},
            {"port": 67, "service": "dhcp-server", "reason": "Network config"},
            {"port": 68, "service": "dhcp-client", "reason": "Network config"},
            {"port": 123, "service": "ntp", "reason": "Time synchronization"}
        ]
    },
    "user_service_ports": {
        "description": "Common development/service ports preserved if in use",
        "ports": [8000, 8080, 8443, 3000, 4200, 5000, 9000, 3306, 5432, 6379, 27017]
    },
    "blocked_c2_ports": {
        "description": "Known C2/Backdoor ports - completely blocked",
        "ports": [
            {"port": 4444, "service": "metasploit-default", "reason": "Metasploit Meterpreter"},
            {"port": 4445, "service": "metasploit-alt", "reason": "Metasploit alternative"},
            {"port": 6666, "service": "backdoor", "reason": "Common backdoor"},
            {"port": 7777, "service": "backdoor", "reason": "Common backdoor"},
            {"port": 12345, "service": "netbus", "reason": "NetBus trojan"},
            {"port": 27374, "service": "subseven", "reason": "SubSeven trojan"},
            {"port": 31337, "service": "elite", "reason": "Elite/Back Orifice"},
            {"port": 31338, "service": "back-orifice", "reason": "Back Orifice alt"},
            {"port": 50050, "service": "cobalt-strike", "reason": "Cobalt Strike"},
            {"port": 14444, "service": "monero-stratum", "reason": "Monero mining"},
            {"port": 1234, "service": "backdoor", "reason": "Common backdoor"}
        ]
    },
    "blocked_dangerous_ports": {
        "description": "Legacy insecure protocols - blocked inbound only",
        "ports": [
            {"port": 23, "service": "telnet", "reason": "Plaintext remote access"},
            {"port": 512, "service": "rexec", "reason": "Remote execution"},
            {"port": 513, "service": "rlogin", "reason": "Insecure remote login"},
            {"port": 514, "service": "rsh", "reason": "Remote shell (TCP)"}
        ]
    },
    "monitored_ports": {
        "description": "Ports monitored for suspicious activity but not blocked",
        "ports": [
            {"port": 22, "service": "ssh", "monitor": "brute-force attempts"},
            {"port": 3389, "service": "rdp", "monitor": "unexpected on Linux"},
            {"port": 445, "service": "smb", "monitor": "ransomware vector"},
            {"port": 5555, "service": "adb", "monitor": "potential backdoor"},
            {"port": 9999, "service": "various", "monitor": "check legitimate use"},
            {"port": 3333, "service": "stratum", "monitor": "mining activity"}
        ]
    },
    "notes": [
        "Essential ports are ALWAYS allowed and take precedence over any block rules",
        "User service ports are preserved if they were in use at install time",
        "Backup of previous firewall state saved in ${INSTALL_DIR}/config/firewall_backup/"
    ]
}
PORTCONFIG
    
    print_success "Port configuration saved to ${INSTALL_DIR}/config/port_rules.json"
}

# Validate port configuration is working
validate_port_config() {
    print_status "Validating port configuration..."
    
    local errors=0
    
    # Check if port_manager.py exists
    if [[ -f "${INSTALL_DIR}/port_manager.py" ]]; then
        print_success "  port_manager.py found"
        
        # Test import
        if "${VENV_DIR}/bin/python3" -c "import sys; sys.path.insert(0, '${INSTALL_DIR}'); from port_manager import get_port_manager; pm = get_port_manager(); print(f'Loaded {len(pm.get_all_known_ports())} port definitions')" 2>/dev/null; then
            print_success "  port_manager.py validated"
        else
            print_warning "  port_manager.py import failed"
            ((errors++))
        fi
    else
        print_warning "  port_manager.py not found"
        ((errors++))
    fi
    
    # Check if port_rules.json exists
    if [[ -f "${INSTALL_DIR}/config/port_rules.json" ]]; then
        print_success "  port_rules.json found"
    else
        print_warning "  port_rules.json not found"
        ((errors++))
    fi
    
    # Check firewall status
    if command -v ufw &> /dev/null && ufw status 2>/dev/null | grep -q "active"; then
        local blocked_count
        blocked_count=$(ufw status | grep -c "MyAV" || echo "0")
        print_success "  UFW active with ${blocked_count} MyAV rules"
    elif command -v firewall-cmd &> /dev/null && systemctl is-active --quiet firewalld; then
        print_success "  firewalld active"
    elif iptables -L MYAV_BLOCK &> /dev/null; then
        local rule_count
        rule_count=$(iptables -L MYAV_BLOCK | wc -l)
        print_success "  iptables MYAV_BLOCK chain with ${rule_count} rules"
    else
        print_warning "  No firewall rules detected"
    fi
    
    if [[ $errors -eq 0 ]]; then
        print_success "Port configuration validated"
        return 0
    else
        print_warning "Port configuration has ${errors} issues"
        return 1
    fi
}

# Print installation summary
print_summary() {
    echo ""
    echo -e "${GREEN}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║                GreyAV Installation Complete!                   ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "  ${BLUE}System:${NC}                ${DISTRO_ID^} ${DISTRO_VERSION} (${OS_TYPE})"
    echo -e "  ${BLUE}Architecture:${NC}          ${ARCH}"
    echo -e "  ${BLUE}Installation Directory:${NC} ${INSTALL_DIR}"
    echo -e "  ${BLUE}Virtual Environment:${NC}   ${VENV_DIR}"
    echo -e "  ${BLUE}Log Directory:${NC}         ${LOG_DIR}"
    echo ""
    
    case "${SERVICE_MANAGER}" in
        systemd)
            echo -e "  ${BLUE}Service Manager:${NC}       systemd"
            echo -e "  ${BLUE}Service Name:${NC}          ${SERVICE_NAME}.service"
            echo ""
            echo -e "  ${YELLOW}Useful Commands:${NC}"
            echo -e "    Check status:   ${GREEN}systemctl status ${SERVICE_NAME}${NC}"
            echo -e "    View logs:      ${GREEN}journalctl -u ${SERVICE_NAME} -f${NC}"
            echo -e "    Restart:        ${GREEN}systemctl restart ${SERVICE_NAME}${NC}"
            echo -e "    Stop:           ${GREEN}systemctl stop ${SERVICE_NAME}${NC}"
            ;;
        launchd)
            echo -e "  ${BLUE}Service Manager:${NC}       launchd"
            echo -e "  ${BLUE}Service Name:${NC}          com.greyav.daemon"
            echo ""
            echo -e "  ${YELLOW}Useful Commands:${NC}"
            echo -e "    Check status:   ${GREEN}launchctl list | grep greyav${NC}"
            echo -e "    View logs:      ${GREEN}tail -f ${LOG_DIR}/greyav.log${NC}"
            echo -e "    Restart:        ${GREEN}launchctl kickstart -k system/com.greyav.daemon${NC}"
            echo -e "    Stop:           ${GREEN}launchctl unload ${SERVICE_FILE}${NC}"
            ;;
        windows)
            echo -e "  ${BLUE}Service Manager:${NC}       Windows Service Control"
            echo ""
            echo -e "  ${YELLOW}Useful Commands (PowerShell as Administrator):${NC}"
            echo -e "    Install service: ${GREEN}powershell -File greyav-service.ps1 -Action install${NC}"
            echo -e "    Check status:    ${GREEN}Get-Service GreyAV${NC}"
            echo -e "    View logs:       ${GREEN}Get-Content ${LOG_DIR}\\greyav.log -Tail 50${NC}"
            echo -e "    Restart:         ${GREEN}Restart-Service GreyAV${NC}"
            echo -e "    Stop:            ${GREEN}Stop-Service GreyAV${NC}"
            ;;
        openrc)
            echo -e "  ${BLUE}Service Manager:${NC}       OpenRC"
            echo ""
            echo -e "  ${YELLOW}Useful Commands:${NC}"
            echo -e "    Check status:   ${GREEN}rc-service ${SERVICE_NAME} status${NC}"
            echo -e "    View logs:      ${GREEN}tail -f ${LOG_DIR}/greyav.log${NC}"
            echo -e "    Restart:        ${GREEN}rc-service ${SERVICE_NAME} restart${NC}"
            echo -e "    Stop:           ${GREEN}rc-service ${SERVICE_NAME} stop${NC}"
            ;;
    esac
    
    echo ""
    
    # Only show firewall info on Linux
    if [[ "$OS_TYPE" == "linux" ]]; then
        echo -e "  ${YELLOW}Port Security:${NC}"
        echo -e "    • Blocked ${RED}11${NC} known C2/backdoor ports"
        echo -e "    • Blocked ${RED}4${NC} dangerous legacy protocol ports"
        echo -e "    • Config: ${GREEN}${INSTALL_DIR}/config/port_rules.json${NC}"
        echo ""
    fi
    
    echo -e "  ${YELLOW}Run GreyAV CLI:${NC}"
    echo -e "    ${GREEN}python3 ${INSTALL_DIR}/greyav.py scan /path/to/scan${NC}"
    echo ""
    echo -e "  ${YELLOW}To uninstall:${NC}"
    case "$OS_TYPE" in
        linux)
            echo -e "    ${GREEN}sudo ./uninstall_av.sh${NC}"
            ;;
        macos)
            echo -e "    ${GREEN}sudo ./uninstall_av.sh${NC}"
            ;;
        windows)
            echo -e "    ${GREEN}powershell -ExecutionPolicy Bypass -File uninstall_av.ps1${NC}"
            ;;
    esac
    echo ""
}

# =============================================================================
# Main Installation Flow
# =============================================================================

main() {
    echo ""
    echo -e "${BLUE}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║            GreyAV Antivirus/EDR System Installer               ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    # Detect OS and set paths
    detect_os
    set_os_paths
    check_privileges
    
    # Installation steps
    install_dependencies
    create_directories
    copy_source_files
    create_main_entry
    create_virtualenv
    install_python_requirements
    set_permissions
    
    # Configure port security (Linux only)
    if [[ "$OS_TYPE" == "linux" ]]; then
        configure_firewall_ports || print_warning "Firewall configuration incomplete"
        validate_port_config || true
    fi
    
    # Create and start service
    create_service
    enable_and_start_service
    
    # Done!
    print_summary
}

# Run main function
main "$@"
