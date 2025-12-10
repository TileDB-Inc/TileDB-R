# Creates a `tiledb_ndrectangle` object

Creates a `tiledb_ndrectangle` object

## Usage

``` r
tiledb_ndrectangle(dom, ctx = tiledb_get_context())
```

## Arguments

- dom:

  A TileDB Domain object for which the NDRectangle object is created

- ctx:

  (optional) A TileDB Ctx object

## Value

The `tiledb_ndrectangle` object

## Examples

``` r
if (tiledb_version(TRUE) >= "2.25.0") {
  dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
  ndr <- tiledb_ndrectangle(dom)
}
```
