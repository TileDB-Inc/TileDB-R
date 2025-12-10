# Return the `tiledb_dim` name

Return the `tiledb_dim` name

## Usage

``` r
# S4 method for class 'tiledb_dim'
name(object)
```

## Arguments

- object:

  A `tiledb_dim` object

## Value

string name, empty string if the dimension is anonymous

## Examples

``` r
d1 <- tiledb_dim("d1", c(1L, 10L))
name(d1)
#> [1] "d1"

d2 <- tiledb_dim("", c(1L, 10L))
name(d2)
#> [1] ""
```
