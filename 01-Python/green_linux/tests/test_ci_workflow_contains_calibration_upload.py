from pathlib import Path


def test_workflow_contains_calibration_upload():
    wf = Path('.github/workflows/model-quantization.yml').read_text()
    assert 'Upload calibration artifact' in wf
    assert 'evaluation/calibration.npz' in wf
