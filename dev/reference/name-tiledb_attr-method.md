# Return the `tiledb_attr` name

Return the `tiledb_attr` name

## Usage

``` r
# S4 method for class 'tiledb_attr'
name(object)
```

## Arguments

- object:

  `tiledb_attr` object

## Value

string name, empty string if the attribute is anonymous

## Examples

``` r
a1 <- tiledb_attr("a1", type = "INT32")
name(a1)
#> [1] "a1"

a2 <- tiledb_attr(type = "INT32")
name(a2)
#> [1] ""
```
