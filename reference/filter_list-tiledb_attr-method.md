# Returns the TileDB Filter List object associated with the given TileDB Attribute

Returns the TileDB Filter List object associated with the given TileDB
Attribute

## Usage

``` r
# S4 method for class 'tiledb_attr'
filter_list(object)
```

## Arguments

- object:

  TileDB Attribute

## Value

a tiledb_filter_list object

## Examples

``` r
attr <- tiledb_attr(
  type = "INT32",
  filter_list = tiledb_filter_list(list(tiledb_filter("ZSTD")))
)
filter_list(attr)
#> tiledb_filter_list(c(tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",-1))) 
```
