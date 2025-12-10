# Set data.frame extended return columns toggle

A `tiledb_array` object can be returned as `data.frame`. This methods
set the selection value for ‘extended’ format including row (and column,
if present) indices.

## Usage

``` r
extended(x) <- value

# S4 method for class 'tiledb_array'
extended(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A logical value with the selection

## Value

The modified `tiledb_array` array object
