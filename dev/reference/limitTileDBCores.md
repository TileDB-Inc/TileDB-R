# Limit TileDB core use to a given number of cores

By default, TileDB will use all available cores on a given machine. In
multi-user or multi-process settings, one may want to reduce the number
of core. This function will take a given number, or default to smaller
of the ‘Ncpus’ options value or the ‘"OMP_THREAD_LIMIT"’ environment
variable (or two as hard fallback).

## Usage

``` r
limitTileDBCores(ncores, verbose = FALSE)
```

## Arguments

- ncores:

  Value of CPUs used, if missing the smaller of a fallback of two, the
  value of ‘Ncpus’ (if set) and the value of environment variable
  ‘"OMP_THREAD_LIMIT"’ is used.

- verbose:

  Optional logical toggle; if set, a short message is displayed
  informing the user about the value set.

## Value

The modified configuration object is returned invisibly.

## Details

As this function returns a config object, its intended use is as
argument to the context creating functions:
`ctx <- tiledb_ctx(limitTileDBCores())`. To check that the values are
set (or at a later point, still set) the config object should be
retrieved via the corresponding method and this `ctx` object:
`cfg <- config(ctx)`.
