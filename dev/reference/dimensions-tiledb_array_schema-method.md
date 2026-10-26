# Returns a list of `tiledb_dim` objects associated with the `tiledb_array_schema`

Returns a list of `tiledb_dim` objects associated with the
`tiledb_array_schema`

## Usage

``` r
# S4 method for class 'tiledb_array_schema'
dimensions(object)
```

## Arguments

- object:

  A TileDB Schema object

## Value

A list of `tiledb_dim` objects

## Examples

``` r
dom <- tiledb_domain(dims = c(
  tiledb_dim("d1", c(1L, 100L), type = "INT32"),
  tiledb_dim("d2", c(1L, 50L), type = "INT32")
))
sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32")))
dimensions(dom)
#> [[1]]
#> tiledb_dim(name="d1", domain=c(1L,100L), tile=100L, type="INT32") 
#> 
#> [[2]]
#> tiledb_dim(name="d2", domain=c(1L,50L), tile=50L, type="INT32") 
#> 

lapply(dimensions(dom), name)
#> [[1]]
#> [1] "d1"
#> 
#> [[2]]
#> [1] "d2"
#> 
```
