# Run an aggregate query on the given (sparse) array and attribute

For dense arrays, use `tiledb_query_apply_aggregate` after setting an
appropriate subarray.

## Usage

``` r
tiledb_array_apply_aggregate(
  array,
  attrname,
  operation = c("Count", "NullCount", "Min", "Max", "Mean", "Sum"),
  nullable = TRUE
)
```

## Arguments

- array:

  A TileDB Array object

- attrname:

  The name of an attribute

- operation:

  The name of aggregation operation

- nullable:

  A boolean toggle whether the attribute is nullable

## Value

The value of the aggregation
