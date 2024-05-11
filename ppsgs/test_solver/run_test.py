import pandas as pd
import numpy as np
from scipy.spatial.distance import cdist, pdist, squareform



obs = pd.DataFrame(dict(
    x=[0.3,1.9,1.1,3.3,4.7],
    y=[1.2,0.6,3.2,4.4,3.8],
    z=[0.47,0.56,0.74,1.47,1.74]
))

nn = len(obs)
matA = np.zeros([nn+1,nn+1])
matA[:nn,:nn] = squareform(np.round(pdist(obs[['x', 'y']].values), 2))
matA[nn,:nn] = 1
matA[:nn, nn] = 1

rhsB = np.zeros(nn+1)
rhsB[:nn] = np.round(cdist(obs[['x', 'y']].values, [[2, 2]]), 2).flatten()
rhsB[nn] = 1
np.savetxt('matA.txt', matA, fmt='%.2f')
np.savetxt('rhsB.txt', rhsB, fmt='%.2f')

weights = np.linalg.solve(matA, rhsB)
sum(rhsB * weights)
