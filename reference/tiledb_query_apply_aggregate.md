# Run an aggregate oprtation on the given query attribute

Run an aggregate oprtation on the given query attribute

## Usage

``` r
tiledb_query_apply_aggregate(
  query,
  attrname,
  operation = c("Count", "NullCount", "Min", "Max", "Mean", "Sum"),
  nullable = TRUE
)
```

## Arguments

- query:

  A TileDB Query object

- attrname:

  The name of an attribute

- operation:

  The name of aggregation operation

- nullable:

  A boolean toggle whether the attribute is nullable

## Value

The value of the aggregation
