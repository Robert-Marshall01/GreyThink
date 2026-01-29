# Grey Multi-Tenant GUI - Resources

This directory contains application icons and assets.

## Icon Files

| File | Platform | Description |
|------|----------|-------------|
| `icon.svg` | All | Source vector icon |
| `icon.png` | All | 512x512 PNG icon |
| `icon.ico` | Windows | Multi-resolution Windows icon |
| `icon.icns` | macOS | Apple icon format |
| `icons/` | Linux | Various sizes for Linux |

## Generating Icons

Run the `generate-icons.sh` script to generate platform-specific icons from the SVG source:

```bash
chmod +x generate-icons.sh
./generate-icons.sh
```

### Requirements

- **ImageMagick**: For PNG and ICO generation
  - Windows: `choco install imagemagick`
  - macOS: `brew install imagemagick`
  - Linux: `apt install imagemagick`

- **iconutil**: For macOS ICNS (included with Xcode on macOS)

## Manual Icon Creation

If you don't have ImageMagick, you can manually create icons:

1. Open `icon.svg` in a graphics editor (Inkscape, Figma, etc.)
2. Export as PNG at various sizes: 16, 32, 48, 64, 128, 256, 512, 1024
3. Use an online converter for ICO and ICNS formats

## Icon Specifications

- Background: Gradient blue (#3b82f6 to #1d4ed8)
- Symbol: White diamond shape
- Corner radius: 80px on 512x512 base
