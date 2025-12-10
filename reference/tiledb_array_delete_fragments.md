# Delete fragments written between the start and end times given

Delete fragments written between the start and end times given

## Usage

``` r
tiledb_array_delete_fragments(
  arr,
  ts_start,
  ts_end,
  ctx = tiledb_get_context()
)
```

## Arguments

- arr:

  A TileDB Array object as for example returned by
  [`tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.md)

- ts_start:

  A Datetime object that will be converted to millisecond granularity

- ts_end:

  A Datetime object that will be converted to millisecond granularity

- ctx:

  A tiledb_ctx object (optional)

## Value

A boolean indicating success
