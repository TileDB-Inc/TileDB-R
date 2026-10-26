# Retrieve attributes from `tiledb_array` object

By default, all attributes will be selected. But if a subset of
attribute names is assigned to the internal slot `attrs`, then only
those attributes#' will be queried. This methods accesses the slot.

## Usage

``` r
# S4 method for class 'tiledb_array,ANY'
attrs(object)
```

## Arguments

- object:

  A `tiledb_array` object

## Value

An empty character vector if no attributes have been selected or else a
vector with attributes; `NA` means no attributes will be returned.
