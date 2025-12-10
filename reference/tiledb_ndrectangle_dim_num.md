# Get the number of dimensions for `tiledb_ndrectangle` object

Get the number of dimensions for `tiledb_ndrectangle` object

## Usage

``` r
tiledb_ndrectangle_dim_num(ndr)
```

## Arguments

- ndr:

  A TileDB NDRectangle object

## Value

The number of dimentiones for the `tiledb_ndrectangle`

## Examples

``` r
if (tiledb_version(TRUE) >= "2.26.0") {
  dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
  ndr <- tiledb_ndrectangle(dom)
  tiledb_ndrectangle_dim_num(ndr)
}
#> [1] 1
```
