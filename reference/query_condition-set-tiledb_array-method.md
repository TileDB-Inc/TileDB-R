# Set query_condition object for the array

A `tiledb_array` object can have an associated query condition object to
set conditions on the read queries. This methods sets the
‘query_condition’ object.

## Usage

``` r
query_condition(x) <- value

# S4 method for class 'tiledb_array'
query_condition(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A `tiledb_query_conditon_object`

## Value

The modified `tiledb_array` array object
