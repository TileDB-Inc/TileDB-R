# Return a TileDB Group query type

Return a TileDB Group query type

## Usage

``` r
tiledb_group_query_type(grp)
```

## Arguments

- grp:

  A TileDB Group object as for example returned by
  [`tiledb_group()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_group.md)

## Value

A character value with the query type i.e. one of “READ” or “WRITE”.
