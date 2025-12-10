# Get TileDB Query result buffer element size

The underlying library functions returns a pair of values as a vector of
length two. The first number is the number of element offsets for
variable size attributes (and always zero for fixed-sized attributes and
coordinates). The second is the number of elements in the data buffer.
For variable-sized attributes the first number is the number of cells
read (and hence the number of offsets), the second number is the number
of elements in the data buffer.

## Usage

``` r
tiledb_query_result_buffer_elements(query, attr)
```

## Arguments

- query:

  A TileDB Query object

- attr:

  A character value containing the attribute

## Value

A integer with the number of elements in the results buffer for the
given attribute

## Details

As this function was first made available when only a scalar
(corresponding to the second result) was returned, we still return that
value.

## See also

tiledb_query_result_buffer_elements_vec
