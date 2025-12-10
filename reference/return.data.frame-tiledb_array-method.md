# Retrieve data.frame return toggle

A `tiledb_array` object can be returned as an array (or list of arrays),
or, if select, as a `data.frame`. This methods returns the selection
value.

## Usage

``` r
# S4 method for class 'tiledb_array'
return.data.frame(object)
```

## Arguments

- object:

  A `tiledb_array` object

## Value

A logical value indicating whether `data.frame` return is selected
