# Returns the number of dimensions for a tiledb domain object

Returns the number of dimensions for a tiledb domain object

## Usage

``` r
# S4 method for class 'tiledb_dim'
tiledb_ndim(object)
```

## Arguments

- object:

  A `tiledb_dim` object

## Value

An integer with the number of dimensions.

## Examples

``` r
d1 <- tiledb_dim("d1", c(1L, 10L), 10L)
tiledb_ndim(d1)
#> [1] 1
```
