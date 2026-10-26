# Export Query Buffer to Pair of Arrow IO Pointers

This function exports the named buffer from a ‘READ’ query to two Arrow
C pointers.

## Usage

``` r
tiledb_query_export_buffer(query, name, ctx = tiledb_get_context())
```

## Arguments

- query:

  A TileDB Query object

- name:

  A character variable identifying the buffer

- ctx:

  tiledb_ctx object (optional)

## Value

A `nanoarrow` object (which is an external pointer to an Arrow Array
with the Arrow Schema stored as the external pointer tag) classed as an
S3 object
