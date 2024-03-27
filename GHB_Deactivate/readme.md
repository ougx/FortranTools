ghb_deactivate set GHB conductance to zero at specific cells.

Usage:
```
  ghb_deactivate disfile deactfile ghb_old ghb_new
```
 - `disfile` -- MODFLOW DIS file. The program will read grid dimensions from the DIS file.
 - `deactfile` -- file includes the cells at which GHB conductance will be set to zero.
 - `ghb_old` -- original GHB file.
 - `ghb_new` -- new GHB file to be written to.

Note: Parameters are not supported.


### deactfile format
`ndeact`  # number of cells GHB will be deactivated
`decell`  # cell id of deactivated GHB `(layer -1) * nrow * ncol + (row - 1) * ncol + column`; repeat `ndeact` times
