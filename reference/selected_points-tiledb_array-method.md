# Retrieve selected_points values for the array

A `tiledb_array` object can have a range selection for each dimension
attribute. This methods returns the selection value for
‘selected_points’ and returns a list (with one element per dimension) of
vectors where each row describes one selected points. Alternatively, the
list can be named with the names providing the match to the
corresponding dimension.

## Usage

``` r
selected_points(object)

# S4 method for class 'tiledb_array'
selected_points(object)
```

## Arguments

- object:

  A `tiledb_array` object

## Value

A list which can contain a vector for each dimension
