# Dump the TileDB Group to String

Dump the TileDB Group to String

## Usage

``` r
tiledb_group_member_dump(grp, recursive = FALSE)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- recursive:

  A logical value indicating whether a recursive dump is desired,
  defaults to ‘FALSE’. Note that recursive listings on remote object may
  be an expensive or slow operation.

## Value

A character string
