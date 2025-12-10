# Retrieve array return toggle

A `tiledb_array` object can be returned as an array (or list of arrays),
or, if select, as a `data.frame` or as a `matrix`. This methods returns
the selection value for the `array` selection.

## Usage

``` r
return.array(object, ...)

# S4 method for class 'tiledb_array'
return.array(object)
```

## Arguments

- object:

  A `tiledb_array` object

- ...:

  Currently unused

## Value

A logical value indicating whether `array` return is selected
