# The version of the libtiledb library

The version of the libtiledb library

## Usage

``` r
tiledb_version(compact = FALSE)
```

## Arguments

- compact:

  Logical value indicating wheter a compact `package_version` object
  should be returned

## Value

An named int vector c(major, minor, patch), or if select, a
`package_version` object

## Examples

``` r
tiledb_version()
#> major minor patch 
#>     2    30     0 
tiledb_version(compact = TRUE)
#> [1] ‘2.30.0’
```
