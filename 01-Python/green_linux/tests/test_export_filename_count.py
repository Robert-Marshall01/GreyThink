from powerapp.utils.export import generate_suggested_filename


def test_generate_suggested_filename_with_minutes_and_count():
    s = generate_suggested_filename(last_minutes=60, sample_count=42)
    assert s.endswith('-last60min-42samples.csv')


def test_generate_suggested_filename_with_count_only():
    s = generate_suggested_filename(sample_count=5)
    assert s.endswith('-5samples.csv')
