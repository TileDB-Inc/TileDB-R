# Set query_layout return values for the array

A `tiledb_array` object can have an associated query with a specific
layout. This methods sets the selection value for ‘query_layout’ from a
character value.

## Usage

``` r
query_layout(x) <- value

# S4 method for class 'tiledb_array'
query_layout(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A character variable for the query layout. Permitted values are
  “ROW_MAJOR”, “COL_MAJOR”, “GLOBAL_ORDER”, or “UNORDERD”.

## Value

The modified `tiledb_array` array object
