import numpy as np
import pandas as pd
from scipy.spatial.distance import cdist, pdist, squareform

def ck5(obsloc, obsval, newloc, cov, obsdrift=None, newdrift=None, nmax=None, unbias=True, standardized=True, verbose=False):

    # ck5 based on uk5

    nnew = len(newloc)
    nvar = len(cov)
    nobs  = [len(obsv) for obsv in obsval]
    distances = [cdist(newloc, obsl) for obsl in obsloc]

    if obsdrift is not None:
        obsdrift = [np.array(df) for df in obsdrift]
        obsdrift = [df.reshape([-1,1]) if df.ndim==1 else df for df in obsdrift]
        ndrift = obsdrift[0].shape[1]
        if newdrift is not None:
            newdrift = np.array(newdrift)
            if newdrift.ndim==1:
                newdrift = newdrift.reshape([-1,1])
    else:
        ndrift = 0

    if nmax is None:
        search = False
        nmax = nobs
        ilocs_mask = [np.full(nob, True) for nob in nobs] # mask for nearest obs
    else:
        search = True
        nmax = [min(n,m) for n,m in zip(nmax, nobs)]
        ilocs = [np.sort(np.argsort(dd, axis=1)[:,:nm], axis=1) for nm, dd in zip(nmax, distances)]
        ilocs_mask = [np.isin(np.arange(nob), np.unique(iloc)) for nob,iloc in zip(nobs,ilocs)]


    # calculate all the covvariance beforehand
    cov_full  = {}
    for ivar in range(nvar):
        nobs0 = nobs[ivar]
        iloc0 = ilocs_mask[ivar]
        obsl0 = obsloc[ivar][iloc0]
        mat = np.zeros([nobs0, nobs0])
        mat[np.ix_(iloc0,iloc0)] = squareform(cov[ivar][ivar](pdist(obsl0)))
        np.fill_diagonal(mat, cov[ivar][ivar](0.))
        cov_full[(ivar,ivar)] = mat
        for jvar in range(ivar+1, nvar):
            nobs1 = nobs[jvar]
            iloc1 = ilocs_mask[jvar]
            obsl1 = obsloc[jvar][iloc1]
            mat = np.zeros([nobs0, nobs1])
            mat[np.ix_(iloc0,iloc1)] = cov[ivar][jvar](cdist(obsl0, obsl1))
            cov_full[(ivar,jvar)] = mat


    # allocate matrix A
    total_obs = sum(nmax)
    if unbias:
        if standardized:
            matAsize = total_obs+1
        else:
            matAsize = total_obs+nvar
    else:
        matAsize = total_obs
    matAsize += ndrift
    matA = np.zeros([matAsize, matAsize])

    if unbias:
        if standardized:
            matA[:total_obs, total_obs] = 1
            matA[total_obs, :total_obs] = 1
        else:
            i1 = 0
            for i in range(nvar):
                n1 = nmax[i]
                matA[i1:i1+n1, total_obs+i] = 1
                matA[total_obs+i, i1:i1+n1] = 1
                i1 += n1

    solv_all = np.full_like(np.array([obsval[0][0]] * nnew), np.nan)
    variance = np.zeros(nnew, )

    # loop through points
    def inverse(iis, ig):

        i0 = 0
        for ivar in range(nvar):
            # set auto-covariance
            n0 = nmax[ivar]
            matA[i0:i0+n0, i0:i0+n0] = cov_full[(ivar, ivar)][np.ix_(iis[ivar], iis[ivar])]
            i1 = i0 + n0
            for jvar in range(ivar+1, nvar):
                # set cross-covariance
                n1 = nmax[jvar]
                aa = cov_full[(ivar,jvar)][np.ix_(iis[ivar], iis[jvar])]
                matA[i0:i0+n0, i1:i1+n1] = aa
                matA[i1:i1+n1, i0:i0+n0] = aa.T
                i1 += n1
            i0 += n0


        rhsB = np.hstack([cov[0][ivar](dd[np.ix_(ig, iis[ivar])]) for ivar, dd in enumerate(distances)]).T


        obsvals_ = [o[iis[ivar]] for ivar, o in enumerate(obsval)]
        if unbias:
            rhsB = np.insert(rhsB, total_obs, 1, axis=0, )
            if not standardized:
                rhsB = np.insert(rhsB, total_obs+1, [0]*(nvar-1), axis=0,)
            else:
                mean1 = obsvals_[0].mean()
                for i, o in enumerate(obsvals_[1:]):
                    mean2 = o.mean()
                    obsvals_[1+i] = o + mean1 - mean2   # ISAAKS and SRIVASTAVA, An Introduction to Applied Geostatistics, pp410

        # set drifts
        if ndrift > 0:
            drifts = np.vstack([obsd[ii] for ii, obsd in zip(iis, obsdrift)])
            matA[:total_obs, -ndrift:] = drifts
            matA[-ndrift:, :total_obs] = drifts.T
            rhsB = np.vstack([rhsB, newdrift[ig].T])                             # drift

        if verbose:
            print('matA')
            print(matA)
            print('rhsB')
            print(rhsB)

        try:
            X = np.linalg.solve(matA, rhsB)
        except:
            print('LinAlgError: Singular matrix; using the (Moore-Penrose) pseudo-inverse of a matrix.')
            matAinv = np.linalg.pinv(matA)
            X = matAinv.dot(rhsB)

        solv_all[ig] = X[:total_obs,:].T.dot(np.concatenate(obsvals_))
        variance[ig] = cov[0][0](0) - (X * rhsB).sum(axis=0)

    if search:
        iiloc = np.hstack(ilocs)
        ilocs_unique = np.unique(iiloc, axis=0)
        for ii in ilocs_unique:
            inverse(np.split(ii, np.cumsum(nmax[:-1])), (iiloc == ii).all(axis=1))
    else:
        inverse(ilocs_mask, np.full(nnew, True))

    return solv_all, variance

def c_spherical(d, psill=1., rng=np.inf, nugget=0.):
    """Spherical model"""
    hr = d / rng
    return psill * np.where(d < rng, (1.-hr*(1.5-.5*hr*hr)), 0.) + np.where(d==0., nugget, 0.)

if __name__ == "__main__":
#%%
    obs = pd.read_csv(r'obs.csv')
    co  = pd.read_csv(r'rho2.csv')
    grid = pd.read_csv(r'grid.csv')

    obsloc = [obs[['x', 'y']].values, co[['x', 'y']].values]
    obsval = [obs['pc'].values, co['logRho'].values]

    newloc = grid[['x', 'y']].values

    rng = 5000.
    vgm1 = lambda d: c_spherical(d, 0.12, rng)
    vgm2 = lambda d: c_spherical(d, 0.04, rng, 0.05)
    vgm3 = lambda d: c_spherical(d, 0.068, rng)


    vgms    = [[vgm1, vgm2],[vgm2, vgm3]]
    # obsloc, obsval, newloc, cov, obsdrift=None, newdrift=None, nmax=None, unbias=True
    grid['ck_estimate'], grid['ck_variance'] = ck5(obsloc, obsval, newloc, vgms, nmax=[300, 300])
    grid.to_csv('py_ck.csv', index=False)
