# Set array return toggle

A `tiledb_array` object can be returned as an array (or list of arrays),
or, if select, as a `data.frame` or a `matrix`. This methods sets the
selection value for a `array`.

## Usage

``` r
return.array(x) <- value

# S4 method for class 'tiledb_array'
return.array(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A logical value with the selection

## Value

The modified `tiledb_array` array object
