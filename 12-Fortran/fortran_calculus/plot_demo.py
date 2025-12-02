#!/usr/bin/env python3
"""plot_demo.py

Small visualization utilities for the Fortran demos.

Usage:
  python plot_demo.py --all
  python plot_demo.py --sigma
  python plot_demo.py --stability

This script reads CSVs created by `fortran_calculus.exe` in the same directory
and writes PNG files. It is robust to header rows and string-formatted numbers.
"""

from pathlib import Path
import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


def coerce_numeric_df(df: pd.DataFrame) -> pd.DataFrame:
    for c in df.columns:
        df[c] = pd.to_numeric(df[c], errors='coerce')
    return df


def plot_sigma_sweep(cwd: Path) -> Path | None:
    csv = cwd / 'rough_distributions_sigma_sweep.csv'
    out = cwd / 'rough_distributions_sigma_sweep.png'
    if not csv.exists():
        print(f'{csv} not found — run the Fortran demo first')
        return None

    try:
        df = pd.read_csv(csv)
    except Exception:
        df = pd.read_csv(csv, header=None)

    df = coerce_numeric_df(df)

    arr = df.values
    sigma = None; L2_f = None; L2_u = None
    # heuristics by name
    names = [str(c).lower() for c in df.columns]
    if 'sigma' in names:
        sidx = names.index('sigma')
        sigma = df.iloc[:, sidx].values
        numeric_cols = [i for i in range(df.shape[1]) if i != sidx and np.issubdtype(df.iloc[:,i].dtype, np.number)]
        if len(numeric_cols) >= 1:
            L2_f = df.iloc[:, numeric_cols[0]].values
        if len(numeric_cols) >= 2:
            L2_u = df.iloc[:, numeric_cols[1]].values
    else:
        if arr.shape[1] >= 3:
            sigma = arr[:,0]; L2_f = arr[:,1]; L2_u = arr[:,2]
        elif arr.shape[1] == 2:
            sigma = arr[:,0]; L2_u = arr[:,1]
        else:
            print(f'Unexpected columns in {csv}: {df.shape}')
            return None

    plt.figure(figsize=(6,4))
    if L2_u is not None:
        plt.loglog(sigma, L2_u, '-o', label='L2_u')
    if L2_f is not None:
        plt.loglog(sigma, L2_f, '-s', label='L2_f')
    plt.xlabel('sigma')
    plt.ylabel('L2 (discrete)')
    plt.title('Rough distributions: sigma sweep')
    plt.grid(True, which='both', ls='--', alpha=0.6)
    plt.legend()
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    print(f'Wrote {out}')
    return out


def plot_stability_region(cwd: Path) -> Path | None:
    csv = cwd / 'stability_regions.csv'
    out = cwd / 'stability_regions.png'
    if not csv.exists():
        print(f'{csv} not found — run the Fortran demo first')
        return None

    try:
        df = pd.read_csv(csv)
    except Exception:
        df = pd.read_csv(csv, header=None)

    df = coerce_numeric_df(df)
    names = [str(c).lower() for c in df.columns]

    # attempt to find re, im, abs columns
    if 'abs_rk4' in names:
        abs_col = names.index('abs_rk4')
        absvals = df.iloc[:, abs_col].values
        re = df.iloc[:,0].values
        im = df.iloc[:,1].values
    elif df.shape[1] >= 4:
        re = df.iloc[:,0].values
        im = df.iloc[:,1].values
        absvals = df.iloc[:,3].values
    elif df.shape[1] == 3:
        re = df.iloc[:,0].values
        im = df.iloc[:,1].values
        absvals = df.iloc[:,2].values
    else:
        print(f'Unexpected shape for {csv}: {df.shape}')
        return None

    plt.figure(figsize=(6,5))
    absvals = np.array(absvals, dtype=float)
    sc = plt.scatter(re, im, c=absvals, cmap='viridis', s=6, marker='s')
    plt.colorbar(sc, label='|R|')
    plt.xlabel('Re(z)')
    plt.ylabel('Im(z)')
    plt.title('Stability region (|R| colormap)')
    plt.axvline(0, color='k', ls=':', alpha=0.4)
    plt.axhline(0, color='k', ls=':', alpha=0.4)
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    print(f'Wrote {out}')
    return out


def plot_delta_convergence(cwd: Path) -> Path | None:
    """Plot delta convergence errors (L1/L2/Linf) vs sigma from Fortran CSVs.

    Looks for `delta_convergence_extended.csv` first, then falls back to
    `delta_convergence.csv` if needed. Saves a PNG with log-log curves for
    each test function present in the CSV.
    """
    candidates = [cwd / 'delta_convergence_extended.csv', cwd / 'delta_convergence.csv']
    csv = None
    for c in candidates:
        if c.exists():
            csv = c
            break
    if csv is None:
        print('No delta_convergence CSV found — run the Fortran demo first')
        return None

    out = cwd / (csv.stem + '.png')
    try:
        df = pd.read_csv(csv)
    except Exception:
        df = pd.read_csv(csv, header=None)

    df = coerce_numeric_df(df)
    names = [str(c).lower() for c in df.columns]

    # Attempt to find columns by name; fall back to positional indices
    def find_col(substrs):
        for s in substrs:
            if s in names:
                return names.index(s)
        return -1

    sidx = find_col(['sigma'])
    tidx = find_col(['test'])
    l1idx = find_col(['l1', 'l1_error'])
    l2idx = find_col(['l2', 'l2_error'])
    linfidx = find_col(['linf', 'linf_error', 'l_inf'])

    arr = df.values
    if sidx == -1:
        # assume first column is sigma
        sidx = 0
    sigma = df.iloc[:, sidx].values

    # if no explicit test column, assume single test and plot aggregated errors
    if tidx == -1:
        tests = ['all']
        idxs = { 'all': df.index }
    else:
        tests = list(pd.unique(df.iloc[:, tidx].astype(str)))

    plt.figure(figsize=(7,5))
    markers = ['o', 's', '^', 'D', 'x']
    for ti, test in enumerate(tests):
        if tidx == -1:
            sel = df
        else:
            sel = df[df.iloc[:, tidx].astype(str) == str(test)]
        if sel.shape[0] == 0:
            continue
        s = sel.iloc[:, sidx].values
        if l1idx != -1:
            y1 = sel.iloc[:, l1idx].values
            plt.loglog(s, y1, marker=markers[0], ls='-', label=f'{test} L1')
        if l2idx != -1:
            y2 = sel.iloc[:, l2idx].values
            plt.loglog(s, y2, marker=markers[1], ls='--', label=f'{test} L2')
        if linfidx != -1:
            yinf = sel.iloc[:, linfidx].values
            plt.loglog(s, yinf, marker=markers[2], ls='-.', label=f'{test} Linf')

    plt.xlabel('sigma')
    plt.ylabel('error (norm)')
    plt.title('Delta (mollifier) convergence: error vs sigma')
    plt.grid(True, which='both', ls='--', alpha=0.6)
    plt.legend(fontsize='small')
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    print(f'Wrote {out}')
    return out


def plot_cn2d_convergence(cwd: Path) -> Path | None:
    """Plot Crank–Nicolson 2D convergence (L2 and Linf vs Nx) saved by Fortran demo.

    Expects `crank_nicolson2d_convergence.csv` with columns like
    `Nx,L2_error,Linf_error,avg_iter` (header tolerant).
    """
    csv = cwd / 'crank_nicolson2d_convergence.csv'
    out = cwd / 'crank_nicolson2d_convergence.png'
    if not csv.exists():
        print(f'{csv} not found — run the Fortran demo first')
        return None

    try:
        df = pd.read_csv(csv)
    except Exception:
        df = pd.read_csv(csv, header=None)

    df = coerce_numeric_df(df)
    names = [str(c).lower() for c in df.columns]

    def find_col(substrs):
        for s in substrs:
            if s in names:
                return names.index(s)
        return -1

    nx_idx = find_col(['nx', 'n', 'nx_grid'])
    l2_idx = find_col(['l2', 'l2_error', 'l2_err'])
    linf_idx = find_col(['linf', 'linf_error', 'linf_err', 'l_inf'])

    arr = df.values
    if nx_idx == -1:
        # assume first column is Nx
        nx_idx = 0
    nx = df.iloc[:, nx_idx].astype(float).values

    if l2_idx == -1 and df.shape[1] >= 2:
        l2_idx = 1
    if linf_idx == -1 and df.shape[1] >= 3:
        linf_idx = 2

    plt.figure(figsize=(7,5))
    markers = ['o', 's']
    if l2_idx != -1:
        l2 = df.iloc[:, l2_idx].values
        plt.loglog(nx, l2, marker=markers[0], ls='-', label='L2 error')
        try:
            # fit slope in log-log
            mask = np.isfinite(nx) & np.isfinite(l2) & (nx > 0) & (l2 > 0)
            if np.count_nonzero(mask) >= 2:
                p = np.polyfit(np.log(nx[mask]), np.log(l2[mask]), 1)
                slope = p[0]
                plt.annotate(f'L2 slope ≈ {slope:.2f}', xy=(0.05, 0.95), xycoords='axes fraction', fontsize=9)
        except Exception:
            pass
    if linf_idx != -1:
        linf = df.iloc[:, linf_idx].values
        plt.loglog(nx, linf, marker=markers[1], ls='--', label='Linf error')

    plt.xlabel('Nx (grid points)')
    plt.ylabel('error (norm)')
    plt.title('Crank–Nicolson 2D: spatial convergence')
    plt.grid(True, which='both', ls='--', alpha=0.6)
    plt.legend()
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    print(f'Wrote {out}')
    return out


def plot_cn2d_snapshots(cwd: Path) -> list[Path]:
    """Plot any `cn2d_snap_N*.csv` snapshots as heatmaps.

    Produces `cn2d_snap_N{Nx}.png` for each CSV found and returns list of
    output paths written.
    """
    out_paths = []
    files = sorted(cwd.glob('cn2d_snap_N*.csv'))
    if not files:
        print('No CN2D snapshot CSVs found (look for cn2d_snap_N*.csv)')
        return out_paths

    for f in files:
        try:
            df = pd.read_csv(f)
        except Exception:
            df = pd.read_csv(f, header=None)
        df = coerce_numeric_df(df)
        names = [str(c).lower() for c in df.columns]
        # heuristics for columns
        def find_col(substrs):
            for s in substrs:
                if s in names:
                    return names.index(s)
            return -1

        xidx = find_col(['x'])
        yidx = find_col(['y'])
        uidx = find_col(['u'])
        if xidx == -1 or yidx == -1 or uidx == -1:
            # fallback to positional
            xidx, yidx, uidx = 0, 1, 2

        xs = df.iloc[:, xidx].values
        ys = df.iloc[:, yidx].values
        us = df.iloc[:, uidx].values

        # try pivot into grid
        try:
            pivot = df.iloc[:, [xidx, yidx, uidx]].pivot(index=df.columns[yidx], columns=df.columns[xidx], values=df.columns[uidx])
            # pivot returns indexes in ascending order; extract coords
            xgrid = np.array([float(c) for c in pivot.columns])
            ygrid = np.array([float(r) for r in pivot.index])
            U = np.array(pivot.values, dtype=float)
        except Exception:
            # fallback: infer unique sorted coords and reshape
            ux = np.unique(xs)
            uy = np.unique(ys)
            nx = ux.size; ny = uy.size
            if ux.size * uy.size != xs.size:
                print(f'Unexpected grid shape in {f}; skipping')
                continue
            # build 2D array assuming rows written with j outer then i inner (y major)
            U = us.reshape((uy.size, ux.size))
            xgrid = ux
            ygrid = uy

        out = f.with_suffix('.png')
        plt.figure(figsize=(5,4))
        extent = (xgrid.min(), xgrid.max(), ygrid.min(), ygrid.max())
        plt.imshow(U, origin='lower', extent=extent, aspect='auto', cmap='inferno')
        plt.colorbar(label='u')
        plt.xlabel('x')
        plt.ylabel('y')
        plt.title(f.stem)
        plt.tight_layout()
        plt.savefig(out, dpi=150)
        plt.close()
        print(f'Wrote {out}')
        out_paths.append(out)

    return out_paths


def main():
    parser = argparse.ArgumentParser(description='Plot demo CSVs into PNGs')
    parser.add_argument('--sigma', action='store_true', help='Plot sigma sweep')
    parser.add_argument('--spectral', action='store_true', help='Plot spectral derivative convergence')
    parser.add_argument('--spectral-derivative', action='store_true', help='Plot spectral derivative snapshot')
    parser.add_argument('--spectral-fft', action='store_true', help='Compare spectral derivative with NumPy FFT')
    parser.add_argument('--stability', action='store_true', help='Plot stability region')
    parser.add_argument('--delta', action='store_true', help='Plot delta convergence (sigma sweep)')
    parser.add_argument('--cn2d', action='store_true', help='Plot Crank–Nicolson 2D convergence')
    parser.add_argument('--cn2d-snap', action='store_true', help='Plot Crank–Nicolson 2D snapshots (heatmaps)')
    parser.add_argument('--all', action='store_true', help='Plot everything')
    parser.add_argument('--outdir', type=str, default='.', help='Output directory (default: script dir)')
    args = parser.parse_args()

    cwd = Path(args.outdir).expanduser().resolve()
    if args.all or not (args.sigma or args.stability or args.delta or args.cn2d):
        # default is to run existing plots plus delta and CN2D if available
        plot_sigma_sweep(cwd)
        plot_stability_region(cwd)
        plot_delta_convergence(cwd)
        plot_spectral_convergence(cwd)
        plot_spectral_derivative_snapshot(cwd)
        plot_spectral_fft_compare(cwd)
        plot_cn2d_convergence(cwd)
        plot_cn2d_snapshots(cwd)
    else:
        if args.sigma:
            plot_sigma_sweep(cwd)
        if args.stability:
            plot_stability_region(cwd)
        if args.delta:
            plot_delta_convergence(cwd)
        if args.cn2d:
            plot_cn2d_convergence(cwd)
        if args.cn2d_snap:
            plot_cn2d_snapshots(cwd)
        if args.spectral:
            plot_spectral_convergence(cwd)
        if args.spectral_derivative:
            plot_spectral_derivative_snapshot(cwd)
        if args.spectral_fft:
            plot_spectral_fft_compare(cwd)


def plot_spectral_convergence(cwd: Path) -> Path | None:
    csv = cwd / 'spectral_convergence.csv'
    out = cwd / 'spectral_convergence.png'
    if not csv.exists():
        print(f'{csv} not found — run the Fortran demo first')
        return None
    try:
        df = pd.read_csv(csv)
    except Exception:
        df = pd.read_csv(csv, header=None)
    df = coerce_numeric_df(df)
    names = [str(c).lower() for c in df.columns]
    # expect columns: N,fourier_L2,fourier_Linf,fd_L2,fd_Linf,cheb_L2,cheb_Linf
    if df.shape[1] < 7:
        print(f'Unexpected columns in {csv}: {df.columns}')
        return None
    N = df.iloc[:,0].astype(float).values
    fourier_L2 = df.iloc[:,1].values
    fourier_Linf = df.iloc[:,2].values
    fd_L2 = df.iloc[:,3].values
    fd_Linf = df.iloc[:,4].values
    cheb_L2 = df.iloc[:,5].values
    cheb_Linf = df.iloc[:,6].values

    plt.figure(figsize=(7,5))
    plt.loglog(N, fourier_L2, '-o', label='Fourier L2')
    plt.loglog(N, fourier_Linf, '--o', label='Fourier Linf')
    plt.loglog(N, fd_L2, '-s', label='FD L2')
    plt.loglog(N, fd_Linf, '--s', label='FD Linf')
    plt.loglog(N, cheb_L2, '-^', label='Cheb L2')
    plt.loglog(N, cheb_Linf, '--^', label='Cheb Linf')
    plt.xlabel('N (grid points)')
    plt.ylabel('error (norm)')
    plt.title('Spectral derivative convergence')
    plt.grid(True, which='both', ls='--', alpha=0.6)
    plt.legend(fontsize='small')
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    print(f'Wrote {out}')
    return out


def plot_spectral_derivative_snapshot(cwd: Path) -> Path | None:
    csv = cwd / 'spectral_derivative.csv'
    out = cwd / 'spectral_derivative.png'
    if not csv.exists():
        print(f'{csv} not found — run the Fortran demo first')
        return None
    try:
        df = pd.read_csv(csv)
    except Exception:
        df = pd.read_csv(csv, header=None)
    df = coerce_numeric_df(df)
    names = [str(c).lower() for c in df.columns]
    # expected columns: x,f,df_spec,df_fd,df_exact,err_spec,err_fd
    if 'x' in names:
        x = df.iloc[:, names.index('x')].values
    else:
        x = df.iloc[:,0].values
    f = df.iloc[:,1].values
    df_spec = df.iloc[:,2].values
    df_fd = df.iloc[:,3].values
    df_exact = df.iloc[:,4].values

    plt.figure(figsize=(7,5))
    plt.plot(x, f, label='f')
    plt.plot(x, df_exact, '--', label='df exact')
    plt.plot(x, df_spec, '-', label='df spectral')
    plt.plot(x, df_fd, ':', label='df fd')
    plt.xlabel('x')
    plt.ylabel('value')
    plt.title('Spectral derivative (snapshot)')
    plt.legend()
    plt.grid(True, ls='--', alpha=0.5)
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    print(f'Wrote {out}')
    return out


def plot_spectral_fft_compare(cwd: Path) -> Path | None:
    """Compute an FFT-based spectral derivative using NumPy and compare to the Fortran spectral column.

    Expects `spectral_derivative.csv` (x,f,df_spec,df_fd,df_exact,...) and writes `spectral_fft_compare.png`.
    """
    csv = cwd / 'spectral_derivative.csv'
    out = cwd / 'spectral_fft_compare.png'
    if not csv.exists():
        print(f'{csv} not found — run the Fortran demo first')
        return None
    try:
        df = pd.read_csv(csv)
    except Exception:
        df = pd.read_csv(csv, header=None)
    df = coerce_numeric_df(df)
    # parse columns
    x = df.iloc[:,0].astype(float).values
    f = df.iloc[:,1].astype(float).values
    df_spec = df.iloc[:,2].astype(float).values
    df_exact = df.iloc[:,4].astype(float).values

    # use numpy FFT on periodic [0,2pi] grid
    N = x.size
    L = x.max() - x.min() + (x[1]-x[0])
    k = np.fft.fftfreq(N, d=(x[1]-x[0])/(2*np.pi))  # frequencies in cycles per rad
    F = np.fft.fft(f)
    dF = (1j * k) * F
    df_fft = np.real(np.fft.ifft(dF))

    plt.figure(figsize=(7,5))
    plt.plot(x, df_exact, '--', label='df exact')
    plt.plot(x, df_spec, '-', label='df fortran spectral')
    plt.plot(x, df_fft, ':', label='df numpy FFT')
    plt.xlabel('x')
    plt.ylabel('df/dx')
    plt.title('Spectral derivative: Fortran spectral vs NumPy FFT')
    plt.legend()
    plt.grid(True, ls='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    plt.close()
    print(f'Wrote {out}')
    return out


if __name__ == '__main__':
    main()
