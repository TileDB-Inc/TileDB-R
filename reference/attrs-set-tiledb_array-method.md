# Selects attributes for the given TileDB array

Selects attributes for the given TileDB array

## Usage

``` r
# S4 method for class 'tiledb_array'
attrs(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A character vector with attributes; the value `NA_character_` signals
  no attributes should be returned; default is an empty character vector
  implying all columns are returned.

## Value

The modified `tiledb_array` object
