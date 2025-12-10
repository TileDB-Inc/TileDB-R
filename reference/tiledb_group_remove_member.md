# Remove Member from TileDB Group

Remove Member from TileDB Group

## Usage

``` r
tiledb_group_remove_member(grp, uri)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- uri:

  A character value with a the URI of the member to be removed, or (if
  added with a name) the name of the member

## Value

The TileDB Group object, invisibly
