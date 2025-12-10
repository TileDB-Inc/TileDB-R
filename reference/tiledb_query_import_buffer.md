# Import to Query Buffer from Pair of Arrow IO Pointers

This function imports to the named buffer for a ‘WRITE’ query from two
Arrow exerternal pointers.

## Usage

``` r
tiledb_query_import_buffer(
  query,
  name,
  nanoarrowptr,
  ctx = tiledb_get_context()
)
```

## Arguments

- query:

  A TileDB Query object

- name:

  A character variable identifying the buffer

- nanoarrowptr:

  A `nanoarrow` object (which is an external pointer to an Arrow Array
  with the Arrow Schema stored as the external pointer tag) classed as
  an S3 object

- ctx:

  tiledb_ctx object (optional)

## Value

The update Query external pointer is returned
