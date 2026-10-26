# Retrieve query_statistics toggle

A `tiledb_array` object can, if requested, return query statistics as a
JSON string in an attribute ‘query_statistics’ attached to the return
object. The default value of the logical switch is ‘FALSE’. This method
returns the current value.

## Usage

``` r
query_statistics(object, ...)

# S4 method for class 'tiledb_array'
query_statistics(object)
```

## Arguments

- object:

  A `tiledb_array` object

- ...:

  Currently unused

## Value

A logical value indicating whether query statistics are returned.
