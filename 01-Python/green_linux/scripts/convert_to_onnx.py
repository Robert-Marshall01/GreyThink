"""Convert trained sklearn joblib model to ONNX and produce a quantized ONNX model.

Saves:
 - models/window_predictor.onnx
 - models/window_predictor.quant.onnx
"""
from pathlib import Path
import joblib

try:
    from skl2onnx import convert_sklearn
    from skl2onnx.common.data_types import FloatTensorType
except Exception:
    raise SystemExit('skl2onnx is required. Install it with pip install skl2onnx')

try:
    from onnxruntime.quantization import quantize_dynamic, QuantType
except Exception:
    raise SystemExit('onnxruntime quantization is required. Ensure onnxruntime is installed')


def main():
    repo = Path(__file__).resolve().parent.parent
    model_path = repo / 'models' / 'window_predictor.joblib'
    if not model_path.exists():
        raise SystemExit(f'Model not found: {model_path}')

    data = joblib.load(str(model_path))
    model = data.get('model') if isinstance(data, dict) else data
    app_keys = data.get('app_keys', []) if isinstance(data, dict) else []

    # Determine feature dimension: 3 + len(app_keys)
    n_features = 3 + len(app_keys)
    initial_type = [('input', FloatTensorType([None, n_features]))]

    print('Converting sklearn model to ONNX (opset 15)...')
    # Try a higher opset which often works better with quantization
    onnx_model = convert_sklearn(model, initial_types=initial_type, target_opset=15)

    out_onnx = repo / 'models' / 'window_predictor.onnx'
    with open(out_onnx, 'wb') as fh:
        fh.write(onnx_model.SerializeToString())
    print('Saved ONNX model to', out_onnx)

    # Quantize dynamically (weights): produces smaller model with QInt8 weights
    out_quant = repo / 'models' / 'window_predictor.quant.onnx'
    print('Quantizing ONNX model (dynamic quantization)...')
    try:
        quantize_dynamic(str(out_onnx), str(out_quant), weight_type=QuantType.QInt8)
        print('Saved quantized ONNX model to', out_quant)
    except Exception as e:
        print('Quantization failed:', e)
        print('Attempting fallback: save non-quantized ONNX only')
        if out_quant.exists():
            out_quant.unlink()

    # Print sizes (quantized file may not exist if quantization failed)
    def size_mb(p):
        return p.stat().st_size / (1024.0 * 1024.0)

    print(f"Joblib size: {size_mb(model_path):.2f} MB")
    print(f"ONNX size: {size_mb(out_onnx):.2f} MB")
    if out_quant.exists():
        print(f"Quantized ONNX size: {size_mb(out_quant):.2f} MB")
    else:
        print('Quantized ONNX not produced')


if __name__ == '__main__':
    main()
