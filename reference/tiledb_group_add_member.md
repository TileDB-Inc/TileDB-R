# Add Member to TileDB Group

Add Member to TileDB Group

## Usage

``` r
tiledb_group_add_member(grp, uri, relative, name = NULL)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- uri:

  A character value with a new URI

- relative:

  A logical value indicating whether URI is relative to the group

- name:

  An optional character providing a name for the object, defaults to
  `NULL`

## Value

The TileDB Group object, invisibly
