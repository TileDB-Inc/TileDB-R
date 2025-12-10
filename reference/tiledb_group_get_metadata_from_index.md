# Accesses Metadata by Index from a TileDB Group

Accesses Metadata by Index from a TileDB Group

## Usage

``` r
tiledb_group_get_metadata_from_index(grp, idx)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- idx:

  A numeric value with the index of the metadata object to be retrieved

## Value

The requested object, or NULL is not found
