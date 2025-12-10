# Open a TileDB Group

Open a TileDB Group

## Usage

``` r
tiledb_group_open(grp, type = c("READ", "WRITE", "MODIFY_EXCLUSIVE"))
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- type:

  A character value that must be either ‘READ’, ‘WRITE’ or
  ‘MODIFY_EXCLUSIVE’

## Value

The TileDB Group object but opened for reading or writing
