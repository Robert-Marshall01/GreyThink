from powerapp.utils.export import generate_suggested_filename


def test_generate_suggested_filename_with_minutes():
    s = generate_suggested_filename(last_minutes=60)
    assert s.endswith('-last60min.csv')


def test_generate_suggested_filename_without_minutes():
    s = generate_suggested_filename()
    assert s.endswith('.csv')
    assert '-last' not in s
