import io
import cairo
from PIL import Image, ImageStat
from powerapp.gtk.main import draw_mini_sparkline


def _render(vals, color=(0.2, 0.4, 0.8)):
    W, H = 64, 16
    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, W, H)
    cr = cairo.Context(surface)
    draw_mini_sparkline(cr, W, H, vals, color=color)
    buf = io.BytesIO()
    surface.write_to_png(buf)
    buf.seek(0)
    return Image.open(buf).convert('L')


def test_mini_sparkline_nonempty():
    img = _render([1, 2, 4, 2, 1])
    stat = sum(ImageStat.Stat(img).sum)
    assert stat > 0


def test_mini_sparkline_differs_with_values():
    img1 = _render([1, 1, 1, 1])
    img2 = _render([1, 2, 4, 2, 1])
    # difference image should be non-empty
    from PIL import ImageChops
    diff = ImageChops.difference(img1, img2)
    assert sum(ImageStat.Stat(diff).sum) > 0
