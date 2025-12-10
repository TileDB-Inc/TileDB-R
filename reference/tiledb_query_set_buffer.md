# Set TileDB Query buffer

This function allocates query buffers directly from R vectors in case
the types match: `integer`, `double`, `logical`. For more general types
see `tiledb_query_buffer_alloc_ptr` and `tiledb_query_buffer_assign_ptr`

## Usage

``` r
tiledb_query_set_buffer(query, attr, buffer)
```

## Arguments

- query:

  A TileDB Query object

- attr:

  A character value containing the attribute

- buffer:

  A vector providing the query buffer

## Value

The modified query object, invisisibly
