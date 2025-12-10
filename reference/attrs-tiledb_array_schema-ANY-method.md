# Returns a list of all `tiledb_attr` objects associated with the `tiledb_array_schema`

Returns a list of all `tiledb_attr` objects associated with the
`tiledb_array_schema`

## Usage

``` r
# S4 method for class 'tiledb_array_schema,ANY'
attrs(object, idx, ...)
```

## Arguments

- object:

  A TileDB Schema object

- idx:

  index argument, currently unused.

- ...:

  Extra parameter for method signature, currently unused.

## Value

A list of `tiledb_attr` objects

## Examples

``` r
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
sch <- tiledb_array_schema(dom, attrs = c(
  tiledb_attr("a1", type = "INT32"),
  tiledb_attr("a2", type = "FLOAT64")
))
attrs(sch)
#> $a1
#> tiledb_attr(name="a1", type="INT32", ncells=1, nullable=FALSE) 
#> 
#> $a2
#> tiledb_attr(name="a2", type="FLOAT64", ncells=1, nullable=FALSE) 
#> 

lapply(attrs(sch), datatype)
#> $a1
#> [1] "INT32"
#> 
#> $a2
#> [1] "FLOAT64"
#> 
```
