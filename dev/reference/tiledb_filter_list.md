# Constructs a `tiledb_filter_list` object

Constructs a `tiledb_filter_list` object

## Usage

``` r
tiledb_filter_list(filters = c(), ctx = tiledb_get_context())
```

## Arguments

- filters:

  an optional list of one or more tiledb_filter_list objects

- ctx:

  tiledb_ctx object (optional)

## Value

tiledb_filter_list object

## Examples

``` r
flt <- tiledb_filter("ZSTD")
tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#> tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",5) 
filter_list <- tiledb_filter_list(c(flt))
filter_list
#> tiledb_filter_list(c(tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",5))) 
```
