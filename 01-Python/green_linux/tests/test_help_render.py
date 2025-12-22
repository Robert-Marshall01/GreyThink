from powerapp.utils.help import get_help_html


def test_get_help_html_returns_html():
    html_text, rendered = get_help_html()
    assert isinstance(html_text, str)
    assert '<html' in html_text.lower()
    # HELP.md contains the string 'PowerApp' from the template; ensure it's present in some form
    assert 'PowerApp' in html_text
