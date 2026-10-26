# Returns the filter_list's number of filters

Returns the filter_list's number of filters

## Usage

``` r
# S4 method for class 'tiledb_filter_list'
nfilters(object)
```

## Arguments

- object:

  tiledb_filter_list

## Value

integer number of filters

## Examples

``` r
flt <- tiledb_filter("ZSTD")
tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#> tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",5) 
filter_list <- tiledb_filter_list(c(flt))
nfilters(filter_list)
#> [1] 1
```
