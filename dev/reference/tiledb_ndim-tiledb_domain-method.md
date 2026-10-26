# Returns the number of dimensions of the `tiledb_domain`

Returns the number of dimensions of the `tiledb_domain`

## Usage

``` r
# S4 method for class 'tiledb_domain'
tiledb_ndim(object)
```

## Arguments

- object:

  tiledb_domain

## Value

integer number of dimensions

## Examples

``` r
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(0.5, 100.0), type = "FLOAT64")))
tiledb_ndim(dom)
#> [1] 1
dom <- tiledb_domain(dims = c(
  tiledb_dim("d1", c(0.5, 100.0), type = "FLOAT64"),
  tiledb_dim("d2", c(0.5, 100.0), type = "FLOAT64")
))
tiledb_ndim(dom)
#> [1] 2
```
