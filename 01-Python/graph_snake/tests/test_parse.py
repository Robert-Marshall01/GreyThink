import pytest
from graph_plotter import parse_numbers, parse_labels


def test_parse_numbers_basic():
    assert parse_numbers("1, 2,3\n4") == [1.0, 2.0, 3.0, 4.0]
    with pytest.raises(ValueError):
        parse_numbers("1 2 three")


def test_parse_labels_basic():
    assert parse_labels("a, b c\nd") == ["a", "b", "c", "d"]
    assert parse_labels("") == []
