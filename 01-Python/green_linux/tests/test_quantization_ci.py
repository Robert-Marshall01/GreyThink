import os
import subprocess
import sys
from pathlib import Path

import pytest

# These integration tests exercise the full convert -> fix opset -> quantize flow.
pytest.importorskip('onnx')
pytest.importorskip('onnxruntime')
pytest.importorskip('skl2onnx')
pytest.importorskip('joblib')

QUANT_PATH = Path(os.getcwd()) / 'models' / 'window_predictor.opsetfixed.quant.onnx'
ONNX_PATH = Path(os.getcwd()) / 'models' / 'window_predictor.onnx'
HARD_LIMIT_MB = float(os.environ.get('MODEL_SIZE_LIMIT_MB', '5'))


def run(cmd):
    print('RUN:', ' '.join(cmd))
    env = os.environ.copy()
    env['PYTHONPATH'] = os.getcwd()
    subprocess.check_call(cmd, env=env, cwd=os.getcwd())


def test_full_quantization_flow(tmp_path):
    # Train a small model (fast)
    run([sys.executable, 'scripts/train.py', '--out', 'models/window_predictor.joblib', '--n-series', '50', '--n-estimators', '5'])

    # Convert to ONNX (convert_to_onnx will emit .onnx)
    run([sys.executable, 'scripts/convert_to_onnx.py'])

    # If quantization succeeded in convert_to_onnx, good. Otherwise, attempt opset fix then quantize.
    if not QUANT_PATH.exists():
        # run opset fixer
        run([sys.executable, 'scripts/fix_onnx_opset.py'])
        # perform dynamic quantization on opsetfixed output
        fixed = Path('models') / 'window_predictor.opsetfixed.onnx'
        assert fixed.exists(), 'opsetfixed.onnx missing after fix'

        # perform quantize_dynamic programmatically
        try:
            from onnxruntime.quantization import quantize_dynamic, QuantType
        except Exception:
            pytest.skip('onnxruntime quantization not available')
        out = Path('models') / 'window_predictor.opsetfixed.quant.onnx'
        quantize_dynamic(str(fixed), str(out), weight_type=QuantType.QInt8)

    # Verify quantized file exists and size under hard limit
    assert QUANT_PATH.exists(), f'Quantized model not found: {QUANT_PATH}'
    size_mb = QUANT_PATH.stat().st_size / (1024.0 * 1024.0)
    assert size_mb <= HARD_LIMIT_MB, f'Quantized model too large: {size_mb:.2f} MB (limit {HARD_LIMIT_MB} MB)'
