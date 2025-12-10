# Test if a Named Group is Using a Relative URI

Test if a Named Group is Using a Relative URI

## Usage

``` r
tiledb_group_is_relative(grp, name)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- name:

  A character value with a group name

## Value

A boolean indicating whether the group uses a relative URI or not
