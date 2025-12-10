# Set selected_ranges return values for the array

A `tiledb_array` object can have a range selection for each dimension
attribute. This methods sets the selection value for ‘selected_ranges’
which is a list (with one element per dimension) of two-column matrices
where each row describes one pair of minimum and maximum values.
Alternatively, the list can be named with the names providing the match
to the corresponding dimension.

## Usage

``` r
selected_ranges(x) <- value

# S4 method for class 'tiledb_array'
selected_ranges(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A list of two-column matrices where each list element ‘i’ corresponds
  to the dimension attribute ‘i’. The matrices can contain rows where
  each row contains the minimum and maximum value of a range.

## Value

The modified `tiledb_array` array object
