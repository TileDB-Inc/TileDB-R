# Get a range from a `tiledb_ndrectangle` object

Get a range from a `tiledb_ndrectangle` object

## Usage

``` r
tiledb_ndrectangle_get_range(ndr, dimname)
```

## Arguments

- ndr:

  A TileDB NDRectangle object

- dimname:

  A character variable with the dimension for which to get a range

## Value

The `tiledb_ndrectangle` range as a two-element vector

## Examples

``` r
if (tiledb_version(TRUE) >= "2.26.0") {
  dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
  ndr <- tiledb_ndrectangle(dom)
  ndr <- tiledb_ndrectangle_set_range(ndr, "d1", 50, 500)
  tiledb_ndrectangle_get_range(ndr, "d1")
}
#> [1]  50 500
```
