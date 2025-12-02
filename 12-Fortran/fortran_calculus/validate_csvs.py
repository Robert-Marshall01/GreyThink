import glob
import pandas as pd
import numpy as np

files = sorted(glob.glob('*.csv'))
print(f'Found {len(files)} CSV files')
problems = []
for f in files:
    # Robust numeric parsing without relying on pandas delimiter inference
    import re
    try:
        with open(f, 'rb') as fh:
            raw = fh.read()
        # try utf-8, fallback to latin-1
        try:
            text = raw.decode('utf-8')
        except Exception:
            try:
                text = raw.decode('latin-1')
            except Exception as e:
                problems.append((f, 'read_error', str(e)))
                continue
        lines = [ln for ln in text.splitlines() if ln.strip()]
        if not lines:
            continue
        num_rows = []
        for ln in lines[1:]:
            # find all floating point token patterns in the line
            toks = re.findall(r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?", ln)
            if toks:
                try:
                    row = [float(t) for t in toks]
                except Exception:
                    continue
                num_rows.append(row)
        if not num_rows:
            # maybe the file contains a single-column numeric series; try header-less parse
            toks0 = re.findall(r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?", lines[0])
            if toks0:
                try:
                    num_rows = [[float(t)] for t in toks0]
                except Exception:
                    num_rows = []
        if not num_rows:
            # no numeric rows found
            continue
        # normalize to 2D numpy array, pad shorter rows with NaN
        maxcols = max(len(r) for r in num_rows)
        arr = np.array([r + [np.nan]*(maxcols-len(r)) for r in num_rows], dtype=float)
        # create a DataFrame from numeric array
        df = pd.DataFrame(arr)
    except Exception as e:
        problems.append((f, 'read_error', str(e)))
        continue
    # check numeric columns
    numeric = df.select_dtypes(include=[np.number])
    if numeric.size == 0:
        # nothing numeric; skip
        continue
    # check NaN/inf
    if numeric.isnull().values.any():
        problems.append((f, 'nan',))
        continue
    if np.isinf(numeric.values).any():
        problems.append((f, 'inf',))
        continue
    # check negative norms or nonsensical large values
    vals = numeric.values
    # if there's any column named 'L2' or 'L2_error' ensure non-negative
    cols = [str(c).lower() for c in numeric.columns]
    for i,c in enumerate(cols):
        if 'l2' in c or 'l2_error' in c or 'l2err' in c:
            if (numeric.iloc[:,i] < 0).any():
                problems.append((f, 'negative_l2', c))
    # check finite ranges
    med = np.median(np.abs(vals))
    if med > 1e30:
        problems.append((f, 'huge_median', med))

if not problems:
    print('No obvious numeric problems found in CSVs')
else:
    print('Problems found:')
    for p in problems:
        print(p)
