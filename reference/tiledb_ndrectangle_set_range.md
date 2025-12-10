# Set a range on a `tiledb_ndrectangle` object

Set a range on a `tiledb_ndrectangle` object

## Usage

``` r
tiledb_ndrectangle_set_range(ndr, dimname, start, end)
```

## Arguments

- ndr:

  A TileDB NDRectangle object

- dimname:

  A character variable with the dimension for which to set a range

- start:

  The lower end of the range to be set

- end:

  The upper end of the range to be set

## Value

The modified `tiledb_ndrectangle` object

Start and end values have to be of the same data type as the type of the
selected dimension. The set of allowed type includes the different
integer types as well as string dimensions.

## Examples

``` r
if (tiledb_version(TRUE) >= "2.26.0") {
  dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
  ndr <- tiledb_ndrectangle(dom)
  ndr <- tiledb_ndrectangle_set_range(ndr, "d1", 50, 500)
}
```
