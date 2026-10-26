# Returns TRUE if the tiledb_dim is anonymous

A TileDB attribute is anonymous if no name/label is defined

## Usage

``` r
is.anonymous(object)

# S3 method for class 'tiledb_attr'
is.anonymous(object)
```

## Arguments

- object:

  `tiledb_attr` object

## Value

TRUE or FALSE

## Examples

``` r
a1 <- tiledb_attr("a1", type = "FLOAT64")
is.anonymous(a1)
#> [1] FALSE

a2 <- tiledb_attr("", type = "FLOAT64")
is.anonymous(a2)
#> [1] TRUE
```
