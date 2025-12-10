# Return the `tiledb_dim` domain

Return the `tiledb_dim` domain

## Usage

``` r
# S4 method for class 'tiledb_dim'
domain(object)
```

## Arguments

- object:

  A `tiledb_dim` object

## Value

a vector of (lb, ub) inclusive domain of the dimension

## Examples

``` r
d1 <- tiledb_dim("d1", domain = c(5L, 10L))
domain(d1)
#> [1]  5 10
```
