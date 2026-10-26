# Set datetimes_as_int64 toggle

A `tiledb_array` object may contain date and datetime objects. While
their internal representation is generally shielded from the user, it
can useful to access them as the ‘native’ format which is an
`integer64`. This function set the current value of the selection
variable, which has a default of `FALSE`.

## Usage

``` r
datetimes_as_int64(x) <- value

# S4 method for class 'tiledb_array'
datetimes_as_int64(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A logical value with the selection

## Value

The modified `tiledb_array` array object
