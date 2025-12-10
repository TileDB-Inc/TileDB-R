# Retrieve query_layout values for the array

A `tiledb_array` object can have a corresponding query with a given
layout given layout. This methods returns the selection value for
‘query_layout’ as a character value.

## Usage

``` r
query_layout(object)

# S4 method for class 'tiledb_array'
query_layout(object)
```

## Arguments

- object:

  A `tiledb_array` object

## Value

A character value describing the query layout
