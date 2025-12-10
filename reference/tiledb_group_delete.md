# Deletes all written data from a 'tiledb_group' object

The group must be opened in ‘MODIFY_EXCLUSIVE’ mode, otherwise the
function will error out.

## Usage

``` r
tiledb_group_delete(grp, uri, recursive = FALSE)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

- uri:

  Character variable with the URI of the group item to be deleted

- recursive:

  A logical value indicating whether all data inside the group is to be
  deleted

## Value

Nothing is returned, the function is invoked for the side-effect of
group data removal.
