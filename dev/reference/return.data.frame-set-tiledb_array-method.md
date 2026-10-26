# Set data.frame return toggle

A `tiledb_array` object can be returned as an array (or list of arrays),
or, if select, as a `data.frame`. This methods sets the selection value.

## Usage

``` r
# S4 method for class 'tiledb_array'
return.data.frame(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A logical value with the selection

## Value

The modified `tiledb_array` array object
