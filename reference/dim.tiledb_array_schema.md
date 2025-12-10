# Retrieve the dimension (domain extent) of the domain

Only valid for integral (integer) domains

## Usage

``` r
# S3 method for class 'tiledb_array_schema'
dim(x)
```

## Arguments

- x:

  A TileDB Schema object

## Value

a dimension vector

## Examples

``` r
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
sch <- tiledb_array_schema(dom, attrs = c(
  tiledb_attr("a1", type = "INT32"),
  tiledb_attr("a2", type = "FLOAT64")
))
dim(sch)
#> [1] 10
```
