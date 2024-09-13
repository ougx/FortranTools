import pandas as pd
import numpy as np

from scipy.spatial.distance import cdist, pdist, squareform

obs = pd.read_csv('obs.csv')
grid = pd.read_csv('grid01.csv')

vgm = lambda x: 10 * x

nobs = len(obs)
matsize = nobs + 1

matA = np.zeros([matsize, matsize])
rhsB = np.zeros(matsize)

matA[:nobs, :nobs] = vgm(squareform(pdist(obs[['x', 'y']])))
matA[nobs, :nobs] = 1
matA[:nobs, nobs] = 1

rhsB[:nobs] =  vgm(cdist(obs[['x', 'y']], grid[['x', 'y']])[:,0])
rhsB[nobs] = 1


weights = np.linalg.solve(matA, rhsB)

(obs['z'] * weights[:nobs]).sum()
(rhsB * weights).sum() ** 0.5
