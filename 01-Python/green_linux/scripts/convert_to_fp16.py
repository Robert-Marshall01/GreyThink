"""Convert existing ONNX model to float16 to reduce size.

Produces: models/window_predictor_small.fp16.onnx
"""
from pathlib import Path
import onnx
from onnxconverter_common import convert_float_to_float16


def main():
    repo = Path(__file__).resolve().parent.parent
    in_p = repo / 'models' / 'window_predictor_small.onnx'
    out_p = repo / 'models' / 'window_predictor_small.fp16.onnx'
    if not in_p.exists():
        raise SystemExit('Input ONNX model missing')
    print('Loading ONNX...')
    model = onnx.load(str(in_p))
    print('Converting to float16...')
    model16 = convert_float_to_float16(model)
    onnx.save(model16, str(out_p))
    print('Saved FP16 ONNX to', out_p)
    print('Size (MB):', out_p.stat().st_size / (1024.0 * 1024.0))


if __name__ == '__main__':
    main()
