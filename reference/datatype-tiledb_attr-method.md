# Return the `tiledb_attr` datatype

Return the `tiledb_attr` datatype

## Usage

``` r
# S4 method for class 'tiledb_attr'
datatype(object)
```

## Arguments

- object:

  `tiledb_attr` object

## Value

tiledb datatype string

## Examples

``` r
a1 <- tiledb_attr("a1", type = "INT32")
datatype(a1)
#> [1] "INT32"

a2 <- tiledb_attr("a1", type = "FLOAT64")
datatype(a2)
#> [1] "FLOAT64"
```
