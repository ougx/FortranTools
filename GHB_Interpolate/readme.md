ghb_interp linearly interpolate GHB heads and conductance based on control points

Usage:
```
  ghb_interp nctl nghb mfdis filectl filecell fileout
```
 - `nctl` -- number of GHB control points for interpolation
 - `nghb` -- number of GHB cells
 - `mfdis` -- MODFLOW DIS file. The program will read grid dimensions from the DIS file.
 - `filectl` -- control point file
 - `filecell` -- ghb cell file
 - `fileout` -- output GHB file.

Note: Parameters are not supported.


### `filectl` format
Line 1 : headers
Line 2+: `href`   `dhtr`        `cond`

### `filecell` format
`ndeact`  # number of cells GHB will be deactivated
`decell`  # cell id of deactivated GHB `(layer -1) * nrow * ncol + (row - 1) * ncol + column`; repeat `ndeact` times

### `fileout` format
`ndeact`  # number of cells GHB will be deactivated
`decell`  # cell id of deactivated GHB `(layer -1) * nrow * ncol + (row - 1) * ncol + column`; repeat `ndeact` times
