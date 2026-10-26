# Retrieve selected_ranges values for the array

A `tiledb_array` object can have a range selection for each dimension
attribute. This methods returns the selection value for
‘selected_ranges’ and returns a list (with one element per dimension) of
two-column matrices where each row describes one pair of minimum and
maximum values. Alternatively, the list can be named with the names
providing the match to the corresponding dimension.

## Usage

``` r
selected_ranges(object)

# S4 method for class 'tiledb_array'
selected_ranges(object)
```

## Arguments

- object:

  A `tiledb_array` object

## Value

A list which can contain a matrix for each dimension
