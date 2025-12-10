# Set matrix return toggle

A `tiledb_array` object can be returned as an array (or list of arrays),
or, if select, as a `data.frame` or a `matrix`. This methods sets the
selection value for a `matrix`.

## Usage

``` r
return.matrix(x) <- value

# S4 method for class 'tiledb_array'
return.matrix(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A logical value with the selection

## Value

The modified `tiledb_array` array object
