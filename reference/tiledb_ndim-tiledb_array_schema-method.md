# Return the number of dimensions associated with the `tiledb_array_schema`

Return the number of dimensions associated with the
`tiledb_array_schema`

## Usage

``` r
# S4 method for class 'tiledb_array_schema'
tiledb_ndim(object)
```

## Arguments

- object:

  A TileDB Schema object

## Value

integer number of dimensions

## Examples

``` r
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
sch <- tiledb_array_schema(dom, attrs = c(
  tiledb_attr("a1", type = "INT32"),
  tiledb_attr("a2", type = "FLOAT64")
))
tiledb_ndim(sch)
#> [1] 1
```
