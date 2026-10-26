# Returns a `tiledb_attr` object associated with the `tiledb_array_schema` with a given name.

Returns a `tiledb_attr` object associated with the `tiledb_array_schema`
with a given name.

## Usage

``` r
# S4 method for class 'tiledb_array_schema,character'
attrs(object, idx, ...)
```

## Arguments

- object:

  A TileDB Schema object

- idx:

  attribute name string

- ...:

  Extra parameter for method signature, currently unused.

## Value

A `tiledb_attr` object

## Examples

``` r
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
sch <- tiledb_array_schema(dom, attrs = c(
  tiledb_attr("a1", type = "INT32"),
  tiledb_attr("a2", type = "FLOAT64")
))
attrs(sch, "a2")
#> tiledb_attr(name="a2", type="FLOAT64", ncells=1, nullable=FALSE) 
```
