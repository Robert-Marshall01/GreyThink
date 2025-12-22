"""Help rendering utilities.

Provides helpers to load local Markdown help and convert to HTML (if the
`markdown` package is available). Intended to be displayed in a WebKit view
for a richer in-app help experience with simple styling fallback.
"""
from pathlib import Path
from typing import Tuple
import html


def _help_md_path() -> Path:
    # docs/HELP.md relative to repository root
    # repository root: move up two levels from this file (powerapp/utils/help.py)
    base = Path(__file__).resolve().parents[2]
    return (base / 'docs' / 'HELP.md').resolve()


def get_help_html() -> Tuple[str, bool]:
    """Return (html_string, rendered), where `rendered` is True when Markdown
    was converted to HTML (markdown package found) and False when a simple
    preformatted fallback was used.
    """
    p = _help_md_path()
    text = ''
    try:
        text = p.read_text(encoding='utf-8')
    except Exception as e:
        return (f"<html><body><pre>Could not read help: {html.escape(str(e))}</pre></body></html>", False)

    # Try to convert with markdown if available
    try:
        import markdown
        md_html = markdown.markdown(text, extensions=['extra', 'sane_lists'])
        body = md_html
        rendered = True
    except Exception:
        # Fallback: escape and wrap in <pre>
        body = f'<pre>{html.escape(text)}</pre>'
        rendered = False

    html_doc = f"""<!doctype html>
<html>
<head>
<meta charset="utf-8" />
<title>PowerApp Help</title>
<style>
body {{ font-family: system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial; margin: 16px; line-height:1.4 }}
pre {{ white-space: pre-wrap; font-family: monospace; background:#f8f8f8; padding:12px; border-radius:6px; }}
h1,h2,h3 {{ color: #2b6cb0 }}
</style>
</head>
<body>
{body}
</body>
</html>"""
    return (html_doc, rendered)
