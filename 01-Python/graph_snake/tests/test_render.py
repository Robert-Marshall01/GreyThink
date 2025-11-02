import matplotlib
matplotlib.use('Agg')
from matplotlib.figure import Figure
import numpy as np
import os
from graph_plotter import render_plot


def test_render_headless(tmp_path):
    fig = Figure(figsize=(4, 3))
    ax = fig.add_subplot(111)
    x = np.array([1, 2, 3, 4])
    y = np.array([4, 3, 2, 1])
    # basic Line and Scatter
    render_plot(ax, fig, 'Line', x, y)
    out = tmp_path / "line.png"
    fig.savefig(out, bbox_inches='tight')
    assert out.exists()

    render_plot(ax, fig, 'Scatter', x, y, z=np.array([1,2,3,4]))
    out2 = tmp_path / "scatter.png"
    fig.savefig(out2, bbox_inches='tight')
    assert out2.exists()

    # Hexbin with z
    render_plot(ax, fig, 'Hexbin', x, y, z=np.array([1,2,3,4]))
    out3 = tmp_path / 'hex.png'
    fig.savefig(out3, bbox_inches='tight')
    assert out3.exists()
