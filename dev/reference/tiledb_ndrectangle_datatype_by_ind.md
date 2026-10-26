# Get the datatype of a `tiledb_ndrectangle` dimension by index

Get the datatype of a `tiledb_ndrectangle` dimension by index

## Usage

``` r
tiledb_ndrectangle_datatype_by_ind(ndr, dim)
```

## Arguments

- ndr:

  A TileDB NDRectangle object

- dim:

  Am integer value for the dimension for which to get a datatype

## Value

The `tiledb_ndrectangle` dimension datatype as a character

## Examples

``` r
if (tiledb_version(TRUE) >= "2.26.0") {
  dom <- tiledb_domain(dim = tiledb_dim("d1", c(1L, 100L), type = "INT32"))
  ndr <- tiledb_ndrectangle(dom)
  tiledb_ndrectangle_datatype_by_ind(ndr, 0)
}
#> [1] "INT32"
```
