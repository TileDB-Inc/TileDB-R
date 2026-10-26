# Returns the `tiledb_domain` object associated with a given `tiledb_array_schema`

Returns the `tiledb_domain` object associated with a given
`tiledb_array_schema`

## Usage

``` r
# S4 method for class 'tiledb_array_schema'
domain(object)
```

## Arguments

- object:

  A TileDB Schema object

## Value

A `tiledb_domain` object

## Examples

``` r
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32")))
domain(sch)
#> tiledb_domain(c(
#>         tiledb_dim(name="d1", domain=c(1L,10L), tile=10L, type="INT32")
#>     )) 
```
