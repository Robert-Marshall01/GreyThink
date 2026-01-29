# Grey Multi-Tenant GUI - Build Instructions

Complete instructions for building the Grey Multi-Tenant desktop application on Windows, macOS, and Linux.

## Prerequisites

### All Platforms
- **Node.js** 18.x or later
- **pnpm** (recommended) or npm
- **Git** for cloning the repository

### Platform-Specific Requirements

#### Windows
- Visual Studio Build Tools 2019 or later
- Windows 10/11 SDK
- Python 3.x (for native module compilation)

```powershell
# Install build tools via npm
npm install --global windows-build-tools
```

#### macOS
- Xcode Command Line Tools
- For notarization: Apple Developer account

```bash
# Install Xcode CLI tools
xcode-select --install
```

#### Linux
- Build essentials (gcc, g++, make)
- Additional libraries for Electron

```bash
# Debian/Ubuntu
sudo apt-get install build-essential libssl-dev libffi-dev python3-dev
sudo apt-get install libx11-dev libxkbfile-dev libsecret-1-dev

# Fedora/RHEL
sudo dnf install gcc-c++ make openssl-devel python3-devel
sudo dnf install libX11-devel libxkbfile-devel libsecret-devel

# Arch Linux
sudo pacman -S base-devel openssl python libsecret
```

## Quick Start

```bash
# 1. Navigate to the GUI directory
cd gui

# 2. Install dependencies
pnpm install

# 3. Run in development mode
pnpm dev

# 4. Build for current platform
pnpm build
pnpm package
```

## Development Mode

Development mode provides hot-reload for rapid iteration:

```bash
# Start development server
pnpm dev
```

This will:
1. Start the Vite development server for the renderer
2. Launch Electron with hot-reload enabled
3. Open DevTools automatically

### Development Shortcuts

| Shortcut | Action |
|----------|--------|
| `F12` | Toggle DevTools |
| `Ctrl+R` / `Cmd+R` | Reload |
| `Ctrl+Shift+R` | Force reload |
| `Ctrl+,` / `Cmd+,` | Open Settings |

## Production Build

### Step 1: Build TypeScript

```bash
pnpm build
```

This compiles TypeScript and bundles the application with Vite.

### Step 2: Package for Distribution

#### Windows

```bash
# Create installer and portable versions
pnpm package:win
```

Outputs in `release/`:
- `Grey Multi-Tenant Setup x.x.x.exe` - NSIS installer
- `Grey Multi-Tenant x.x.x.exe` - Portable executable

#### macOS

```bash
# Create DMG and ZIP
pnpm package:mac
```

Outputs in `release/`:
- `Grey Multi-Tenant-x.x.x.dmg` - Disk image
- `Grey Multi-Tenant-x.x.x-mac.zip` - Zipped app bundle

**Code Signing (Optional):**

```bash
# Set environment variables for signing
export CSC_LINK="path/to/certificate.p12"
export CSC_KEY_PASSWORD="certificate_password"
pnpm package:mac
```

**Notarization (Required for distribution):**

```bash
# Set Apple credentials
export APPLE_ID="your@apple.id"
export APPLE_ID_PASSWORD="app-specific-password"
export APPLE_TEAM_ID="XXXXXXXXXX"
pnpm package:mac
```

#### Linux

```bash
# Create AppImage, DEB, and RPM packages
pnpm package:linux
```

Outputs in `release/`:
- `Grey Multi-Tenant-x.x.x.AppImage` - Universal Linux app
- `grey-multi-tenant_x.x.x_amd64.deb` - Debian/Ubuntu package
- `grey-multi-tenant-x.x.x.x86_64.rpm` - Fedora/RHEL package

### Build All Platforms

To build for all platforms at once (requires running on macOS for macOS builds):

```bash
pnpm package:all
```

## Build Configuration

The build is configured in `package.json` under the `build` key:

```json
{
  "build": {
    "appId": "com.grey.multitenant",
    "productName": "Grey Multi-Tenant",
    "directories": {
      "output": "release"
    },
    "win": {
      "target": ["nsis", "portable"],
      "icon": "resources/icon.ico"
    },
    "mac": {
      "target": ["dmg", "zip"],
      "icon": "resources/icon.icns",
      "category": "public.app-category.developer-tools"
    },
    "linux": {
      "target": ["AppImage", "deb"],
      "icon": "resources/icons",
      "category": "Development"
    }
  }
}
```

## Icon Generation

Icons must be generated for each platform before building.

### Required Icons

| Platform | Format | Size |
|----------|--------|------|
| Windows | `.ico` | 256x256 (multi-resolution) |
| macOS | `.icns` | 1024x1024 (multi-resolution) |
| Linux | `.png` | 16x16 to 512x512 |

### Generate from SVG

```bash
cd resources

# Using ImageMagick (all platforms)
convert icon.svg -resize 256x256 icon.ico
convert icon.svg -resize 1024x1024 icon.png

# For macOS .icns (macOS only)
mkdir icon.iconset
sips -z 16 16     icon.png --out icon.iconset/icon_16x16.png
sips -z 32 32     icon.png --out icon.iconset/icon_16x16@2x.png
sips -z 32 32     icon.png --out icon.iconset/icon_32x32.png
sips -z 64 64     icon.png --out icon.iconset/icon_32x32@2x.png
sips -z 128 128   icon.png --out icon.iconset/icon_128x128.png
sips -z 256 256   icon.png --out icon.iconset/icon_128x128@2x.png
sips -z 256 256   icon.png --out icon.iconset/icon_256x256.png
sips -z 512 512   icon.png --out icon.iconset/icon_256x256@2x.png
sips -z 512 512   icon.png --out icon.iconset/icon_512x512.png
sips -z 1024 1024 icon.png --out icon.iconset/icon_512x512@2x.png
iconutil -c icns icon.iconset

# For Linux (multiple sizes)
mkdir -p icons
for size in 16 32 48 64 128 256 512; do
  convert icon.svg -resize ${size}x${size} icons/${size}x${size}.png
done
```

## Troubleshooting Builds

### Error: "Cannot find module" during build

```bash
# Clear node_modules and reinstall
rm -rf node_modules
pnpm install
```

### Error: ENOENT on Windows

```powershell
# Run as Administrator
npm install --global windows-build-tools
```

### Error: "code signing" on macOS

You can skip signing for development:

```bash
export CSC_IDENTITY_AUTO_DISCOVERY=false
pnpm package:mac
```

### Error: Missing libraries on Linux

Install required development libraries:

```bash
sudo apt-get install libnss3-dev libatk-bridge2.0-dev libgtk-3-dev
```

### Build fails with memory error

Increase Node.js memory limit:

```bash
export NODE_OPTIONS="--max_old_space_size=4096"
pnpm build
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Build GUI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
    
    runs-on: ${{ matrix.os }}
    
    steps:
      - uses: actions/checkout@v4
      
      - uses: pnpm/action-setup@v2
        with:
          version: 8
      
      - uses: actions/setup-node@v4
        with:
          node-version: '20'
          cache: 'pnpm'
          cache-dependency-path: 'gui/pnpm-lock.yaml'
      
      - name: Install dependencies
        working-directory: gui
        run: pnpm install
      
      - name: Build
        working-directory: gui
        run: pnpm build
      
      - name: Package
        working-directory: gui
        run: pnpm package
      
      - name: Upload artifacts
        uses: actions/upload-artifact@v4
        with:
          name: gui-${{ matrix.os }}
          path: gui/release/*
```

## Release Checklist

Before releasing:

1. [ ] Update version in `package.json`
2. [ ] Ensure all tests pass
3. [ ] Verify icons are present for all platforms
4. [ ] Test the build on target platform
5. [ ] Sign and notarize (macOS)
6. [ ] Create GitHub release
7. [ ] Upload build artifacts

## File Locations

| Platform | Default Install Location |
|----------|-------------------------|
| Windows | `C:\Program Files\Grey Multi-Tenant\` |
| macOS | `/Applications/Grey Multi-Tenant.app` |
| Linux (AppImage) | Portable - any location |
| Linux (deb/rpm) | `/usr/lib/grey-multi-tenant/` |

## Support

For build issues, please open an issue on GitHub with:
- Platform and version
- Node.js version (`node --version`)
- Error message and full build log
- Steps to reproduce
