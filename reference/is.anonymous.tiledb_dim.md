# Returns TRUE if the tiledb_dim is anonymous

A TileDB dimension is anonymous if no name/label is defined

## Usage

``` r
# S3 method for class 'tiledb_dim'
is.anonymous(object)
```

## Arguments

- object:

  A `tiledb_dim` object

## Value

TRUE or FALSE

## Examples

``` r
d1 <- tiledb_dim("d1", c(1L, 10L), 10L)
is.anonymous(d1)
#> [1] FALSE

d2 <- tiledb_dim("", c(1L, 10L), 10L)
is.anonymous(d2)
#> [1] TRUE
```
