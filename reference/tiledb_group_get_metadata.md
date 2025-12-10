# Accesses Metadata from a TileDB Group

Accesses Metadata from a TileDB Group

## Usage

``` r
tiledb_group_get_metadata(grp, key)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- key:

  A character value with the key of the metadata object to be retrieved

## Value

The requested object, or NULL is not found
