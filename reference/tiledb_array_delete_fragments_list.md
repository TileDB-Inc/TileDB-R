# Delete fragments written given by their URIs

Delete fragments written given by their URIs

## Usage

``` r
tiledb_array_delete_fragments_list(arr, fragments, ctx = tiledb_get_context())
```

## Arguments

- arr:

  A TileDB Array object as for example returned by
  [`tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.md)

- fragments:

  A character vector with fragment URIs

- ctx:

  A tiledb_ctx object (optional)

## Value

A boolean indicating success
