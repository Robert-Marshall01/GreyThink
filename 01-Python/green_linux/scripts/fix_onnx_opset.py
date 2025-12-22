"""Fix ONNX opset_import entries to include an explicit 'ai.onnx' domain when missing.

Saves a fixed model as <original>.opsetfixed.onnx
"""
from pathlib import Path
import logging
import onnx

logging.basicConfig(level=logging.INFO)

# choose input ONNX path: prefer models/window_predictor.onnx, fallback to small variant
candidates = [Path('models/window_predictor.onnx'), Path('models/window_predictor_small.onnx')]
for c in candidates:
    if c.exists():
        p = c
        break
else:
    raise SystemExit('ONNX model not found (looked for window_predictor.onnx / window_predictor_small.onnx)')

m = onnx.load(str(p))
logging.info('Before opset_imports:')
for oi in m.opset_import:
    logging.info(' - domain: %s version: %s', oi.domain, oi.version)

# normalize: replace empty domain entries with 'ai.onnx'
seen_versions = set()
new_ops = []
for oi in m.opset_import:
    dom = oi.domain if oi.domain else 'ai.onnx'
    ver = oi.version
    if (dom, ver) in seen_versions:
        continue
    seen_versions.add((dom, ver))
    new_ops.append(onnx.helper.make_operatorsetid(dom, ver))

# replace opset_import entries
m.opset_import.clear()
for oi in new_ops:
    m.opset_import.append(oi)
logging.info('After opset_imports:')
for oi in m.opset_import:
    logging.info(' - domain: %s version: %s', oi.domain, oi.version)

out = p.with_suffix('.opsetfixed.onnx')
onnx.save(m, str(out))
logging.info('Wrote fixed model to %s', out)
