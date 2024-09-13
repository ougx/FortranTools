import numpy as np
import pandas as pd
from scipy.spatial.distance import cdist, pdist, squareform

import matplotlib.pyplot as plt
from matplotlib.animation import FFMpegFileWriter as MovieWriter


def c_spherical(d, psill=1., rng=np.inf, nugget=0.):
    """Spherical model"""
    hr = d / rng
    return psill * np.where(d < rng, (1.-hr*(1.5-.5*hr*hr)), 0.) + np.where(d==0., nugget, 0.)

if __name__ == "__main__":
#%%
    obs = pd.read_csv(r'..\test_data\pc2d.csv')
    grid = pd.read_csv(r'..\test_data\grid2d.csv')

    obsloc = obs[['x', 'y']].values
    obsval = obs['pc'].values

    newloc = grid[['x', 'y']].values

    rng = 5000.
    vgm1 = lambda d: c_spherical(d, 0.12, rng)

    nobs = len(obs)
    nnew = len(grid)
    matA = np.zeros([nobs+1, nobs+1])
    rhsB = np.zeros([nobs+1, nnew])

    matA[:nobs, :nobs] = vgm1(squareform(pdist(obsloc)))
    matA[ nobs, :nobs] = 1.0
    matA[:nobs,  nobs] = 1.0

    rhsB[:nobs] = vgm1(cdist(obsloc, newloc))
    rhsB[ nobs] = 1.0
    # obsloc, obsval, newloc, cov, obsdrift=None, newdrift=None, nmax=None, unbias=True
    weights = np.linalg.solve(matA, rhsB)
    grid['estimate'], grid['variance'] = obsval.dot(weights[:nobs]), 0.12 - (weights * rhsB).sum(axis=0)
    grid['simulate'] = np.minimum(np.maximum(grid['estimate'] + np.sqrt(grid['variance']) * np.random.standard_normal(nnew), 0.0), 1.0)
    grid.to_csv('py_ok.csv', index=False)

#%%
def plot_sim():
    nrow = 80
    ncol = 60
    grid = pd.read_csv('grid_sim.csv')


    fig, ax = plt.subplots(figsize=(6,8), dpi=100)
    results = np.zeros([nrow, ncol])
    results.fill(np.nan)
    im = ax.imshow(results, vmin=0, vmax=1, cmap='jet')
    ax.axis('off')
    ax.xaxis.set_label("")
    ax.yaxis.set_label("")
    ax.xaxis.set_visible(False)
    ax.yaxis.set_visible(False)

    def update_figure(j):
        nfps = 10
        v = grid.iloc[nfps*(j-1):nfps*j, 3]
        jj = grid.iloc[nfps*(j-1):nfps*j, 0] - 1
        irow = jj // ncol
        icol = jj - irow * ncol
        results[irow, icol] = v
        im.set_data(results)

    moviewriter = MovieWriter(fps=30)
    with moviewriter.saving(fig, 'sgsim.mp4', dpi=100):
        for j in range(1, 481):
            update_figure(j)
            moviewriter.grab_frame()
