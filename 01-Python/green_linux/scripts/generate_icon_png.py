#!/usr/bin/env python3
"""Generate a 128x128 PNG fallback icon from the SVG design using Pillow."""
import logging
from PIL import Image, ImageDraw

logging.basicConfig(level=logging.INFO)

W = H = 128
BG = "#0B9E58"
WHITE = "#FFFFFF"

im = Image.new("RGBA", (W, H), (0, 0, 0, 0))
d = ImageDraw.Draw(im)

# rounded rect background
r = 18
d.rounded_rectangle([(0, 0), (W, H)], radius=r, fill=BG)

# draw leaf - a simple stylized path using polygon + ellipse
leaf_poly = [
    (W*0.5, H*0.12),
    (W*0.83, H*0.28),
    (W*0.68, H*0.6),
    (W*0.5, H*0.8),
    (W*0.23, H*0.6),
    (W*0.32, H*0.28),
]
# fill leaf white
d.polygon(leaf_poly, fill=WHITE)
# add inner mask to create a leaf vein with BG color to simulate leaf look
vein = [(W*0.5, H*0.16), (W*0.58, H*0.36), (W*0.52, H*0.62), (W*0.5, H*0.72)]
d.line(vein, fill=BG, width=8, joint="curve")

# power glyph: a vertical rounded rectangle and an arc
# vertical bar
bar_w = 10
bar_h = 28
bar_x = W*0.5 - bar_w/2
bar_y = H*0.28
d.rounded_rectangle([(bar_x, bar_y), (bar_x+bar_w, bar_y+bar_h)], radius=5, fill=BG)

# arc (part of circle) beneath the bar - draw outline in white
bbox = [W*0.34, H*0.42, W*0.66, H*0.82]
d.arc(bbox, start=200, end=340, fill=WHITE, width=8)

# save
out = "scripts/assets/powerapp-128.png"
im.save(out)
logging.info("Wrote %s", out)
