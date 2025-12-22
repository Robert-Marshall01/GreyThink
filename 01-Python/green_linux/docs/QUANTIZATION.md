Model quantization notes

This project includes scripts to convert sklearn models to ONNX and produce compact variants.

Scripts
- `scripts/train.py` - trains a RandomForest model and writes `models/window_predictor.joblib`.
- `scripts/convert_to_onnx.py` - converts the joblib model to `models/window_predictor.onnx` and attempts dynamic quantization.
- `scripts/fix_onnx_opset.py` - normalizes `opset_import` entries (ensures `ai.onnx` domain is present) and writes `models/window_predictor.opsetfixed.onnx`.
- `scripts/quantize_static.py` - (attempts) static quantization using a calibration dataset (may fail on some envs).
- `scripts/convert_to_fp16.py` - converts an ONNX model to FP16 `models/window_predictor_small.fp16.onnx`.

Notes
- In some environments, ONNX models produced by `skl2onnx` may contain empty-domain opset entries which break `onnxruntime` quantization tools. If quantization fails with "Failed to find proper ai.onnx domain", run `scripts/fix_onnx_opset.py` and then retry quantization.
- We provide both FP16 and dynamic int8 quantized artifacts; FP16 and dynamic quantized models are small and suitable as demo models in releases.

CI
- The GitHub Action `model-quantization.yml` runs the train->convert->fix->quantize flow and asserts a quantized model is present and under the size threshold.

CI gating and model quality thresholds
- The CI workflow also runs `scripts/evaluate_model.py` with a small dataset and checks model quality against a heuristic baseline. The gating requires the model to outperform the heuristic on at least one metric by a small delta (configurable via env vars):
  - `MIN_DELTA_NDCG` (default in CI: `0.01`) — required improvement in NDCG@k
  - `MIN_DELTA_TOP1` (default in CI: `0.00`) — required improvement in top-1 accuracy
- For local runs the test defaults are permissive (deltas = 0.0) to avoid flakiness; CI sets stricter values to decide whether a model is promotable. If a gate fails, check the evaluation report (artifact or job log) to see the detailed metrics and tune models/thresholds accordingly.
