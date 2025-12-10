# Retrieves the dimension of the tiledb_dim domain

Retrieves the dimension of the tiledb_dim domain

## Usage

``` r
# S3 method for class 'tiledb_dim'
dim(x)
```

## Arguments

- x:

  A `tiledb_dim` object

## Value

A vector of the tile_dim domain type, of the dim domain dimension
(extent)

## Examples

``` r
d1 <- tiledb_dim("d1", c(1L, 10L), 5L)
dim(d1)
#> [1] 10
```
