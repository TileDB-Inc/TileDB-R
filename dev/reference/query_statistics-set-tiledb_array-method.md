# Set query_statistics toggle

A `tiledb_array` object can, if requested, return query statistics as a
JSON string in an attribute ‘query_statistics’ attached to the return
object. The default value of the logical switch is ‘FALSE’. This method
sets the value.

## Usage

``` r
query_statistics(x) <- value

# S4 method for class 'tiledb_array'
query_statistics(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A logical value with the selection

## Value

The modified `tiledb_array` array object
