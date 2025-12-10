# Deletes Metadata from a TileDB Group

Deletes Metadata from a TileDB Group

## Usage

``` r
tiledb_group_delete_metadata(grp, key)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- key:

  A character value with they index under which the data will be written

## Value

The TileDB Group object, invisibly
