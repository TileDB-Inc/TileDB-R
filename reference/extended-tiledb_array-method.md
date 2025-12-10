# Retrieve data.frame extended returns columns toggle

A `tiledb_array` object can be returned as `data.frame`. This methods
returns the selection value for ‘extended’ format including row (and
column, if present) indices.

## Usage

``` r
extended(object)

# S4 method for class 'tiledb_array'
extended(object)
```

## Arguments

- object:

  A `tiledb_array` object

## Value

A logical value indicating whether an `extended` return is selected
