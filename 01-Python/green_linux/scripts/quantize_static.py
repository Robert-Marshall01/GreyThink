"""Perform static quantization (weights + activations) of an ONNX model using a calibration dataset.

Outputs:
 - models/window_predictor_small.static.quant.onnx

"""
from pathlib import Path
import joblib
import numpy as np

try:
    from onnxruntime.quantization import quantize_static, CalibrationDataReader, QuantType, QuantFormat
    import onnxruntime as ort
    HAS_ONNXRUNTIME = True
except Exception:
    # onnxruntime is optional for tests that only need the calibration reader; avoid
    # failing import time so the reader classes can be used in tests without installing
    # the heavy onnxruntime dependency. When attempting to perform quantization in the
    # main script we will raise an explicit error.
    HAS_ONNXRUNTIME = False
    class CalibrationDataReader:
        """Minimal stand-in for onnxruntime.quantization.CalibrationDataReader when ONNX Runtime is not installed."""
        pass

# import data generator to create calibration samples
try:
    from powerapp.ml.dataset import generate_simulated_series
except Exception:
    # fallback to minimal synthetic data
    def generate_simulated_series(n_series=100, length=48):
        res = []
        for _ in range(n_series):
            series = []
            for h in range(length):
                series.append({'timestamp': f'2025-01-01T{h:02d}:00:00+00:00', 'forecast_intensity': 100.0 + h, 'total_power_w': 50.0, 'per_app_cpu': {'appA': 0.1, 'appB': 0.2}})
            res.append({'series': series})
        return res


class WindowCalibrationReader(CalibrationDataReader):
    def __init__(self, onnx_model_path: str, joblib_model_path: str, num_samples: int = 200):
        self.onnx_model_path = onnx_model_path
        self.joblib_model_path = joblib_model_path
        self.num_samples = num_samples
        # load model data to determine feature dims
        data = joblib.load(str(joblib_model_path))
        self.app_keys = data.get('app_keys', []) if isinstance(data, dict) else []
        self.n_features = 3 + len(self.app_keys)
        # prepare calibration dataset using generator
        # extract per-window features like training
        series_list = generate_simulated_series(n_series= max(10, num_samples//5), length=48)
        feats = []
        for s in series_list:
            series = s['series']
            for i in range(0, max(0, len(series) - 2 + 1)):
                window = series[i:i+2]
                start_hour = int(window[0]['timestamp'].split('T')[1].split(':')[0])
                avg_forecast = float(sum(p['forecast_intensity'] for p in window) / len(window))
                mean_power = float(sum(p['total_power_w'] for p in window) / len(window))
                per_app_means = [float(sum(p['per_app_cpu'].get(k, 0.0) for p in window) / len(window)) for k in sorted(window[0]['per_app_cpu'].keys())]
                feat = [start_hour, avg_forecast, mean_power] + per_app_means
                feats.append(np.array(feat, dtype=np.float32).reshape(1, -1))
                if len(feats) >= self.num_samples:
                    break
            if len(feats) >= self.num_samples:
                break
        # store as iterator
        self._data_iter = iter(feats)
        # get input name by loading model (may not have ONNX Runtime available in test env)
        try:
            sess = ort.InferenceSession(str(self.onnx_model_path), providers=['CPUExecutionProvider'])
            self.input_name = sess.get_inputs()[0].name
        except Exception:
            self.input_name = 'input'

    def get_next(self):
        try:
            nxt = next(self._data_iter)
            return {self.input_name: nxt}
        except StopIteration:
            return None


class NpzCalibrationReader(CalibrationDataReader):
    """Read calibration samples from a .npz file created by `scripts/generate_dataset.py --calibration-out`.

    Expected .npz contains 'X' (float32 array) and 'meta' with JSON metadata. The reader will yield rows as inputs to the quantizer.
    """
    def __init__(self, onnx_model_path: str, npz_path: str):
        self.onnx_model_path = onnx_model_path
        self.npz_path = npz_path
        data = np.load(str(npz_path), allow_pickle=True)
        self.X = data['X']
        meta_json = data['meta'].tolist()
        try:
            import json
            self.meta = json.loads(meta_json)
        except Exception:
            self.meta = {}
        # load model to get input name (ONNX Runtime may be missing in test env)
        try:
            sess = ort.InferenceSession(str(self.onnx_model_path), providers=['CPUExecutionProvider'])
            self.input_name = sess.get_inputs()[0].name
        except Exception:
            self.input_name = 'input'
        # ensure dtype float32/float16 conversion as needed
        self._data_iter = iter([row.reshape(1, -1).astype(np.float32) for row in self.X])

    def get_next(self):
        try:
            nxt = next(self._data_iter)
            return {self.input_name: nxt}
        except StopIteration:
            return None


def main():
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--onnx', default=None, help='ONNX model path to quantize (default: models/window_predictor_small.onnx)')
    p.add_argument('--calibration', default=None, help='Optional calibration .npz file produced by generate_dataset.py')
    p.add_argument('--out', default=None, help='Optional output quantized model path')
    args = p.parse_args()

    repo = Path(__file__).resolve().parent.parent
    onnx_in = Path(args.onnx) if args.onnx else repo / 'models' / 'window_predictor_small.onnx'
    if not onnx_in.exists():
        raise SystemExit(f'ONNX model not found: {onnx_in}')

    joblib_p = repo / 'models' / 'window_predictor_small.joblib'
    if not joblib_p.exists():
        raise SystemExit(f'Joblib model missing (needed to infer feature/order): {joblib_p}')

    # choose calibration reader
    reader = None
    if args.calibration:
        cal_p = Path(args.calibration)
        if not cal_p.exists():
            raise SystemExit(f'Calibration file not found: {cal_p}')
        reader = NpzCalibrationReader(str(onnx_in), str(cal_p))
    else:
        reader = WindowCalibrationReader(str(onnx_in), str(joblib_p), num_samples=200)

    out = Path(args.out) if args.out else repo / 'models' / 'window_predictor_small.static.quant.onnx'

    print('Running static quantization (this may take a few seconds)...')
    if not HAS_ONNXRUNTIME:
        raise SystemExit('onnxruntime with quantization support is required')
    try:
        quantize_static(model_input=str(onnx_in), model_output=str(out), calibration_data_reader=reader, quant_format=QuantFormat.QOperator, activation_type=QuantType.QInt8, weight_type=QuantType.QInt8)
    except TypeError:
        # fallback to positional or minimal kwargs
        quantize_static(str(onnx_in), str(out), reader, QuantFormat.QOperator, QuantType.QInt8, QuantType.QInt8)
    print('Saved static quantized model to', out)
    print('Size (MB):', out.stat().st_size / (1024.0 * 1024.0))


if __name__ == '__main__':
    main()
