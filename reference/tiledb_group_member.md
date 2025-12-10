# Get a Member (Description) by Index from TileDB Group

This function returns a three-element character vector with the member
object translated to character, uri, and optional name.

## Usage

``` r
tiledb_group_member(grp, idx)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- idx:

  A numeric value with the index of the metadata object to be retrieved

## Value

A character vector with three elements: the member type, its uri, and
name (or `""` if the member is unnamed).
