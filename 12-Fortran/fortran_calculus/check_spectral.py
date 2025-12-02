import pandas as pd
import numpy as np

def read_spectral_csv(path='spectral_convergence.csv'):
    # Try to read with pandas and let it infer the delimiter first
    try:
        df = pd.read_csv(path, sep=None, engine='python')
        return df
    except Exception:
        pass
    # Try common fallbacks: comma, whitespace
    for sep in [',', '\\s+']:
        try:
            df = pd.read_csv(path, sep=sep, engine='python', header=0)
            return df
        except Exception:
            continue
    # Last resort: manual forgiving parse
    with open(path,'r') as fh:
        lines = [ln.rstrip('\n') for ln in fh.readlines() if ln.strip()]
    if not lines:
        raise RuntimeError('Empty spectral_convergence.csv')
    header = lines[0].strip()
    if ',' in header:
        names = [h.strip() for h in header.split(',')]
    else:
        names = ['N','fourier_L2','fourier_Linf','fd_L2','fd_Linf','cheb_L2','cheb_Linf']
    data = []
    for ln in lines[1:]:
        # accept either comma or whitespace separated numeric rows
        parts = [p for p in (ln.replace(',', ' ').split()) if p]
        if len(parts) < 7:
            continue
        try:
            row = [float(p) for p in parts[:7]]
        except Exception:
            continue
        data.append(row)
    if not data:
        raise RuntimeError('No numeric rows parsed from spectral_convergence.csv')
    ncols = min(len(names), 7)
    return pd.DataFrame(data, columns=names[:ncols])


df = read_spectral_csv('spectral_convergence.csv')

def df_columns_valid(names):
    # number of columns we will actually use (cap at 7)
    return min(len(names), 7)

print(df.to_string(index=False))

N = df['N'].values.astype(float)
fourier = df['fourier_L2'].values.astype(float)
fd = df['fd_L2'].values.astype(float)
cheb = df['cheb_L2'].values.astype(float)


def fit_slope(x, y):
    mask = np.isfinite(y) & (y > 0)
    if mask.sum() < 2:
        return None
    p = np.polyfit(np.log(x[mask]), np.log(y[mask]), 1)
    return p[0]

print('\nFitted slopes (log-log):')
print(' FD L2 slope  =', fit_slope(N, fd))
print('Cheb L2 slope =', fit_slope(N, cheb))
print('Fourier L2 slope =', fit_slope(N, fourier))

print('\nRow-wise details:')
for idx, row in df.iterrows():
    print(f"N={int(row['N'])}, fourier={row['fourier_L2']:.3e}, fd={row['fd_L2']:.3e}, cheb={row['cheb_L2']:.3e}")
