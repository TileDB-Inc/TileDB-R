# Returns TRUE is tiledb_domain is an integral (integer) domain

Returns TRUE is tiledb_domain is an integral (integer) domain

## Usage

``` r
# S4 method for class 'tiledb_domain'
is.integral(object)
```

## Arguments

- object:

  tiledb_domain

## Value

TRUE if the domain is an integral domain, else FALSE

## Examples

``` r
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 100L), type = "INT32")))
is.integral(dom)
#> [1] TRUE
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(0.5, 100.0), type = "FLOAT64")))
is.integral(dom)
#> [1] TRUE
```
