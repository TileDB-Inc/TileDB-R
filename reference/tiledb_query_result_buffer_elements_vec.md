# Get TileDB Query result buffer element size pair as vector

The underlying library functions returns a pair of values as a vector of
length two. The first number is the number of element offsets for
variable size attributes (and always zero for fixed-sized attributes and
coordinates). The second is the number of elements in the data buffer.
For variable-sized attributes the first number is the number of cells
read (and hence the number of offsets), the second number is the number
of elements in the data buffer. In the case of a nullable attribute, a
third element is returned with the size of the validity buffer.

## Usage

``` r
tiledb_query_result_buffer_elements_vec(query, attr, nullable = FALSE)
```

## Arguments

- query:

  A TileDB Query object

- attr:

  A character value containing the attribute

- nullable:

  A logical variable that is ‘TRUE’ to signal that the attribute is
  nullable, and ‘FALSE’ otherwise

## Value

A vector with the number of elements in the offsets buffer (and zero for
fixed-size attribute or dimensions), the number elements in the results
buffer for the given attribute, and (if nullable) a third element with
the validity buffer size.

## See also

tiledb_query_result_buffer_elements
