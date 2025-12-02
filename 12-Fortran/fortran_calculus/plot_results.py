"""
Quick plotting helper for CSV outputs produced by fortran_calculus.f90.
Generates PNGs for:
 - `stft_spectrogram.csv` -> `stft_spectrogram.png` (spectrogram heatmap)
 - `fft_filter_output.csv` -> `fft_filter_compare.png` (original vs filtered time-series)
 - `symplectic_higher_order.csv` -> `symplectic_energy.png` (energy comparison)
 - `poisson2d_multigrid_solution.csv` -> `poisson2d_solution.png` (heatmap)
 - `stiff_integrator_comparison.csv` -> `stiff_integrator_comparison.png` (relative error vs dt)

Usage:
  python plot_results.py

This script is intentionally conservative with memory (reads CSVs with pandas)
and attempts to be robust to minor formatting variants.
"""

import os
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm

ROOT = os.path.dirname(__file__)

def plot_stft(csv='stft_spectrogram.csv', out='stft_spectrogram.png'):
    p = os.path.join(ROOT, csv)
    if not os.path.exists(p):
        print(f"SKIP: {csv} not found")
        return
    # try comma-separated first, fall back to whitespace-separated
    try:
        df = pd.read_csv(p)
        if df.shape[1] <= 1:
            raise ValueError('single-column; try whitespace')
    except Exception:
        df = pd.read_csv(p, delim_whitespace=True, header=None)
    # first column is time, rest are freq bin magnitudes
    times = df.iloc[:,0].to_numpy()
    mags = df.iloc[:,1:].to_numpy().T  # shape: (freq_bins, time)
    if mags.size == 0:
        print(f"SKIP: {csv} appears empty or malformed (no magnitude columns)")
        return
    fig, ax = plt.subplots(figsize=(10,6))
    extent = [times.min(), times.max(), 0, mags.shape[0]]
    im = ax.imshow(mags, aspect='auto', origin='lower', extent=extent, cmap='magma', norm=LogNorm(vmin=max(mags.min(),1e-12), vmax=mags.max()))
    ax.set_xlabel('time (index)')
    ax.set_ylabel('frequency bin')
    ax.set_title('STFT Spectrogram')
    plt.colorbar(im, ax=ax, label='magnitude')
    fig.tight_layout()
    fig.savefig(os.path.join(ROOT, out), dpi=150)
    plt.close(fig)
    print(f'WROTE: {out}')


def plot_fft_filter(csv='fft_filter_output.csv', out='fft_filter_compare.png'):
    p = os.path.join(ROOT, csv)
    if not os.path.exists(p):
        print(f"SKIP: {csv} not found")
        return
    df = pd.read_csv(p)
    x = df.iloc[:,0].to_numpy()
    orig = df.iloc[:,1].to_numpy()
    filt = df.iloc[:,2].to_numpy()
    fig, ax = plt.subplots(figsize=(10,4))
    ax.plot(x, orig, label='original', alpha=0.7)
    ax.plot(x, filt, label='filtered', alpha=0.9)
    ax.set_xlabel('x')
    ax.set_ylabel('signal')
    ax.set_title('FFT filtering: original vs filtered')
    ax.legend()
    fig.tight_layout()
    fig.savefig(os.path.join(ROOT, out), dpi=150)
    plt.close(fig)
    print(f'WROTE: {out}')


def plot_symplectic(csv='symplectic_higher_order.csv', out='symplectic_energy.png'):
    p = os.path.join(ROOT, csv)
    if not os.path.exists(p):
        print(f"SKIP: {csv} not found")
        return
    df = pd.read_csv(p)
    # Expect columns: t,q_ver,p_ver,E_ver,q_mid,p_mid,E_mid
    if 't' in df.columns:
        t = df['t'].to_numpy()
    else:
        t = df.iloc[:,0].to_numpy()
    E_ver = df.iloc[:,-4].to_numpy() if df.shape[1] >= 7 else df.iloc[:,3].to_numpy()
    E_mid = df.iloc[:,-1].to_numpy()
    fig, ax = plt.subplots(figsize=(10,4))
    ax.plot(t, E_ver, label='Velocity-Verlet')
    ax.plot(t, E_mid, label='Implicit Midpoint')
    ax.set_xlabel('t')
    ax.set_ylabel('Energy')
    ax.set_title('Symplectic integrator energy comparison')
    ax.legend()
    fig.tight_layout()
    fig.savefig(os.path.join(ROOT, out), dpi=150)
    plt.close(fig)
    print(f'WROTE: {out}')


def plot_poisson(csv='poisson2d_multigrid_solution.csv', out='poisson2d_solution.png'):
    p = os.path.join(ROOT, csv)
    if not os.path.exists(p):
        print(f"SKIP: {csv} not found")
        return
    # Robust custom parser: accept header like 'x,y,u' followed by whitespace-separated numeric rows
    xs_list = []
    ys_list = []
    us_list = []
    with open(p, 'r') as fh:
        for raw in fh:
            line = raw.strip()
            if not line:
                continue
            # normalize delimiters: replace commas with spaces
            line2 = line.replace(',', ' ')
            parts = line2.split()
            # try to parse first three tokens as floats
            if len(parts) < 3:
                continue
            try:
                x = float(parts[0])
                y = float(parts[1])
                u = float(parts[2])
            except Exception:
                # skip header or non-numeric lines
                continue
            xs_list.append(x)
            ys_list.append(y)
            us_list.append(u)

    if len(xs_list) == 0:
        print('SKIP: poisson2d CSV empty or no numeric rows found')
        return

    xs_arr = np.array(xs_list)
    ys_arr = np.array(ys_list)
    u_arr = np.array(us_list)

    ux = np.unique(xs_arr)
    uy = np.unique(ys_arr)
    nx = ux.size
    ny = uy.size
    if nx * ny != u_arr.size:
        # maybe rows are not exactly ordered; try pivot via pandas with constructed DataFrame
        df2 = pd.DataFrame({'x': xs_arr, 'y': ys_arr, 'u': u_arr})
        try:
            pivot = df2.pivot(index='y', columns='x', values='u')
            xs = np.array(pivot.columns.tolist())
            ys = np.array(pivot.index.tolist())
            U = pivot.values
        except Exception:
            print('SKIP: cannot reshape poisson2d data into grid')
            return
    else:
        # reshape assuming rows vary fastest in x then y (as written by writer)
        # find ordering
        # sort unique coordinates
        xs = np.sort(ux)
        ys = np.sort(uy)
        # build grid
        # create index map for (x,y) pairs
        idx = {(round(xs_arr[i],12), round(ys_arr[i],12)): u_arr[i] for i in range(u_arr.size)}
        U = np.zeros((ys.size, xs.size))
        for j, yv in enumerate(ys):
            for i, xv in enumerate(xs):
                key = (round(xv,12), round(yv,12))
                U[j, i] = idx.get(key, np.nan)

    fig, ax = plt.subplots(figsize=(6,5))
    im = ax.imshow(U, origin='lower', extent=(xs.min(), xs.max(), ys.min(), ys.max()), aspect='auto', cmap='viridis')
    ax.set_title('Poisson2D solution')
    plt.colorbar(im, ax=ax, label='u')
    fig.tight_layout()
    fig.savefig(os.path.join(ROOT, out), dpi=150)
    plt.close(fig)
    print(f'WROTE: {out}')


def plot_stiff(csv='stiff_integrator_comparison.csv', out='stiff_integrator_comparison.png'):
    p = os.path.join(ROOT, csv)
    if not os.path.exists(p):
        print(f"SKIP: {csv} not found")
        return
    try:
        df = pd.read_csv(p, delim_whitespace=True, header=None)
    except Exception:
        df = pd.read_csv(p, header=0)
    # heuristics: find dt column
    # if header present
    if df.shape[1] >= 6:
        # method, dt, steps, y_numeric, y_exact, rel_error
        methods = df.iloc[:,0].to_numpy()
        dts = df.iloc[:,1].astype(float).to_numpy()
        rel = df.iloc[:,5].astype(float).to_numpy()
        fig, ax = plt.subplots(figsize=(8,4))
        for m in np.unique(methods):
            sel = methods == m
            ax.plot(dts[sel], rel[sel], 'o-', label=str(m))
        ax.set_xscale('log')
        ax.set_yscale('log')
        ax.set_xlabel('dt')
        ax.set_ylabel('relative error')
        ax.set_title('Stiff integrator relative error vs dt')
        ax.legend()
        fig.tight_layout()
        fig.savefig(os.path.join(ROOT, out), dpi=150)
        plt.close(fig)
        print(f'WROTE: {out}')
    else:
        print('SKIP: stiff CSV has unexpected format')


def main():
    plot_stft()
    plot_fft_filter()
    plot_symplectic()
    plot_poisson()
    plot_stiff()

if __name__ == '__main__':
    main()
