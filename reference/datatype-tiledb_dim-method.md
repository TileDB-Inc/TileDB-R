# Return the `tiledb_dim` datatype

Return the `tiledb_dim` datatype

## Usage

``` r
# S4 method for class 'tiledb_dim'
datatype(object)
```

## Arguments

- object:

  A `tiledb_dim` object

## Value

A character string with tiledb's datatype.

## Examples

``` r
d1 <- tiledb_dim("d1", domain = c(5L, 10L), tile = 2L, type = "INT32")
datatype(d1)
#> [1] "INT32"
```
