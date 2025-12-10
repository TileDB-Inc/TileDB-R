# Write Metadata to a TileDB Group

Write Metadata to a TileDB Group

## Usage

``` r
tiledb_group_put_metadata(grp, key, val)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- key:

  A character value with they index under which the data will be written

- val:

  An R object (numeric, int, or char vector) that will be stored

## Value

On success boolean ‘TRUE’ is returned
