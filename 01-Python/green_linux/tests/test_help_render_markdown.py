import pytest
from powerapp.utils.help import get_help_html


@pytest.mark.skipif(importlib:=__import__('importlib'), reason='runs only when markdown is available in environment')
def test_help_render_markdown_present():
    try:
        import markdown  # noqa: F401
    except Exception:
        pytest.skip('markdown package not available')
    html_text, rendered = get_help_html()
    assert rendered is True
    assert '<html' in html_text.lower()
