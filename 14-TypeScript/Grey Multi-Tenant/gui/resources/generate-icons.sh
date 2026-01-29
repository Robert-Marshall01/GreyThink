# Icon Generation Script
# Converts icon.svg to platform-specific formats

# Requirements:
# - ImageMagick (convert command)
# - For macOS .icns: iconutil (included in Xcode)
# - For Windows .ico: ImageMagick

# Create PNG sizes for various uses
# Run from the resources/ directory

# Generate PNGs
for size in 16 32 48 64 128 256 512 1024; do
  convert icon.svg -resize ${size}x${size} icon-${size}.png
  echo "Generated icon-${size}.png"
done

# Create icon.png (512x512 for general use)
cp icon-512.png icon.png

# Create Linux icon directory structure
mkdir -p icons/16x16 icons/32x32 icons/48x48 icons/64x64 icons/128x128 icons/256x256 icons/512x512
cp icon-16.png icons/16x16/icon.png
cp icon-32.png icons/32x32/icon.png
cp icon-48.png icons/48x48/icon.png
cp icon-64.png icons/64x64/icon.png
cp icon-128.png icons/128x128/icon.png
cp icon-256.png icons/256x256/icon.png
cp icon-512.png icons/512x512/icon.png

echo "Linux icons created"

# Create Windows .ico (requires ImageMagick)
convert icon-16.png icon-32.png icon-48.png icon-256.png icon.ico
echo "Windows icon.ico created"

# Create macOS .icns (requires iconutil on macOS)
if command -v iconutil &> /dev/null; then
  mkdir -p icon.iconset
  cp icon-16.png icon.iconset/icon_16x16.png
  cp icon-32.png icon.iconset/icon_16x16@2x.png
  cp icon-32.png icon.iconset/icon_32x32.png
  cp icon-64.png icon.iconset/icon_32x32@2x.png
  cp icon-128.png icon.iconset/icon_128x128.png
  cp icon-256.png icon.iconset/icon_128x128@2x.png
  cp icon-256.png icon.iconset/icon_256x256.png
  cp icon-512.png icon.iconset/icon_256x256@2x.png
  cp icon-512.png icon.iconset/icon_512x512.png
  cp icon-1024.png icon.iconset/icon_512x512@2x.png
  iconutil -c icns icon.iconset
  rm -rf icon.iconset
  echo "macOS icon.icns created"
else
  echo "iconutil not found - run on macOS to create .icns"
fi

# Cleanup intermediate PNGs
rm -f icon-16.png icon-32.png icon-48.png icon-64.png icon-128.png icon-256.png icon-512.png icon-1024.png

echo "Icon generation complete!"
