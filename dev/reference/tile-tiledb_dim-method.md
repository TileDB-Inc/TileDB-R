# Return the `tiledb_dim` tile extent

Return the `tiledb_dim` tile extent

## Usage

``` r
# S4 method for class 'tiledb_dim'
tile(object)
```

## Arguments

- object:

  A `tiledb_dim` object

## Value

A scalar tile extent

## Examples

``` r
d1 <- tiledb_dim("d1", domain = c(5L, 10L), tile = 2L)
tile(d1)
#> [1] 2
```
