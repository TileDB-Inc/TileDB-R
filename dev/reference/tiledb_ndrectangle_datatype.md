# Get the datatype of a named `tiledb_ndrectangle` dimension

Get the datatype of a named `tiledb_ndrectangle` dimension

## Usage

``` r
tiledb_ndrectangle_datatype(ndr, dimname)
```

## Arguments

- ndr:

  A TileDB NDRectangle object

- dimname:

  A character variable with the dimension for which to get a datatype

## Value

The `tiledb_ndrectangle` dimension datatype as a character

## Examples

``` r
if (tiledb_version(TRUE) >= "2.26.0") {
  dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
  ndr <- tiledb_ndrectangle(dom)
  tiledb_ndrectangle_datatype(ndr, "d1")
}
#> [1] "INT32"
```
