# Retrieve the dimension (domain extent) of the domain

Only valid for integral (integer) domains

## Usage

``` r
# S3 method for class 'tiledb_domain'
dim(x)
```

## Arguments

- x:

  tiledb_domain

## Value

dimension vector

## Examples

``` r
dom <- tiledb_domain(dims = c(
  tiledb_dim("d1", c(1L, 100L), type = "INT32"),
  tiledb_dim("d2", c(1L, 100L), type = "INT32")
))
dim(dom)
#> [1] 100 100
```
