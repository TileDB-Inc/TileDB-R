# Retrieve matrix return toggle

A `tiledb_array` object can be returned as an array (or list of arrays),
or, if select, as a `data.frame` or as a `matrix`. This methods returns
the selection value for the `matrix` selection.

## Usage

``` r
return.matrix(object, ...)

# S4 method for class 'tiledb_array'
return.matrix(object)
```

## Arguments

- object:

  A `tiledb_array` object

- ...:

  Currently unused

## Value

A logical value indicating whether `matrix` return is selected
