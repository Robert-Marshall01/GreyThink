#!/usr/bin/env python3
"""Simple verification script that checks a quantized model and its metadata.

Usage: scripts/verify_quantized_model.py [--run-proto] <model-dir>
Exits non-zero if checks fail. When --run-proto is provided, a tiny prototype model is trained,
exported and quantized into a temp directory and that directory is verified.
"""
import os
import sys
import json
import argparse


def _run_inference_check(fp32_path, q_path, n_samples=8, rtol=1e-3, atol=1e-2):
    try:
        import numpy as _np
        import onnxruntime as ort
    except Exception:
        print('onnxruntime or numpy not available; skipping inference check')
        return True

    # load sessions
    try:
        sess_fp = ort.InferenceSession(fp32_path, providers=['CPUExecutionProvider'])
        sess_q = ort.InferenceSession(q_path, providers=['CPUExecutionProvider'])
    except Exception as e:
        print(f'Failed to create ONNX runtime sessions: {e}')
        return False

    # prepare dummy input based on input name and shape
    input_name = sess_fp.get_inputs()[0].name
    shape = sess_fp.get_inputs()[0].shape
    feat = None
    if shape and len(shape) >= 2:
        try:
            feat = int(shape[1]) if shape[1] not in (None, 'None') else None
        except Exception:
            feat = None
    if feat is None:
        feat = 4

    rng = _np.random.RandomState(0)
    xs = rng.randn(n_samples, feat).astype(_np.float32)

    for x in xs:
        inp = {input_name: x.reshape((1, feat))}
        try:
            out_fp = sess_fp.run(None, inp)[0]
            out_q = sess_q.run(None, inp)[0]
        except Exception as e:
            print(f'Inference failed: {e}')
            return False
        if not _np.allclose(out_fp, out_q, atol=atol, rtol=rtol):
            print('Inference mismatch beyond tolerance')
            return False
    return True


def verify_dir(d, allow_inference=True):
    meta = os.path.join(d, 'quant_meta.json')
    if not os.path.exists(meta):
        print('Missing quant_meta.json')
        return 3
    with open(meta, 'r', encoding='utf-8') as f:
        j = json.load(f)
    m = j.get('meta') or j
    s_fp = m.get('size_fp32')
    s_q = m.get('size_quantized')
    fp_path = os.path.join(d, m.get('source')) if m.get('source') else None
    q_path = os.path.join(d, m.get('quantized')) if m.get('quantized') else None

    if s_fp is None or s_q is None:
        print('Metadata lacks size info; skipping size check')
    else:
        print(f'FP32 size: {s_fp}, quantized size: {s_q}')
        if s_q >= s_fp:
            print('Warning: quantized size not smaller than FP32; quantization may have failed')
            return 4
        print('Quantized model looks smaller than FP32: OK')

    # inference check
    if allow_inference and fp_path and q_path and os.path.exists(fp_path) and os.path.exists(q_path):
        good = _run_inference_check(fp_path, q_path)
        if not good:
            print('Inference verification failed')
            return 5
        print('Inference verification: OK')
    else:
        print('Skipping inference check (missing files or dependencies)')
    return 0


def main(argv):
    p = argparse.ArgumentParser(description='Verify quantized model outputs and metadata')
    p.add_argument('--run-proto', action='store_true', help='Run prototype finetune/export/quantize flow and verify its output')
    p.add_argument('model_dir', nargs='?', help='Directory containing quant_meta.json')
    args = p.parse_args(argv[1:])

    if args.run_proto:
        try:
            from powerapp.ml.fine_tune import prototype_finetune_and_export
        except Exception as e:
            print('Prototype finetune unavailable:', e)
            return 8
        out = None
        try:
            fp, q, meta = prototype_finetune_and_export()
            out = os.path.dirname(fp)
            print('Prototype exported artifacts to:', out)
        except Exception as e:
            print('Prototype export/quantize failed:', e)
            return 9
        return verify_dir(out)

    if not args.model_dir:
        print('Usage: verify_quantized_model.py [--run-proto] <model-dir>')
        return 2

    return verify_dir(args.model_dir)


if __name__ == '__main__':
    sys.exit(main(sys.argv))
