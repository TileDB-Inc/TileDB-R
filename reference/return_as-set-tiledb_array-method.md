# Retrieve return_as conversion preference

A `tiledb_array` object can be returned as a ‘list’ (default), ‘array’,
‘matrix’, ‘data.frame’, ‘data.table’ or ‘tibble’. This method This
methods permits to set a preference of returning a `list`, `array`,
`matrix`, `data.frame`, a `data.table`, or a `tibble`. The default value
of “asis” means that no conversion is performed and a `list` is
returned.

## Usage

``` r
return_as(x) <- value

# S4 method for class 'tiledb_array'
return_as(x) <- value
```

## Arguments

- x:

  A `tiledb_array` object

- value:

  A character value with the selection

## Value

The modified `tiledb_array` array object
