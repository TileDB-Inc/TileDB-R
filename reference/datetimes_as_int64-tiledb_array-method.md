# Retrieve datetimes_as_int64 toggle

A `tiledb_array` object may contain date and datetime objects. While
their internal representation is generally shielded from the user, it
can useful to access them as the ‘native’ format which is an
`integer64`. This function retrieves the current value of the selection
variable, which has a default of `FALSE`.

## Usage

``` r
datetimes_as_int64(object)

# S4 method for class 'tiledb_array'
datetimes_as_int64(object)
```

## Arguments

- object:

  A `tiledb_array` object

## Value

A logical value indicating whether `datetimes_as_int64` is selected
