# Returns the tiledb_domain TileDB type string

Returns the tiledb_domain TileDB type string

## Usage

``` r
# S4 method for class 'tiledb_domain'
datatype(object)
```

## Arguments

- object:

  tiledb_domain

## Value

tiledb_domain type string

## Examples

``` r
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 100L), type = "INT32")))
datatype(dom)
#> [1] "INT32"
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(0.5, 100.0), type = "FLOAT64")))
datatype(dom)
#> [1] "FLOAT64"
```
