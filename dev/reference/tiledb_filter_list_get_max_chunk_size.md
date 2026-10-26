# Returns the filter_list's max_chunk_size

Returns the filter_list's max_chunk_size

## Usage

``` r
max_chunk_size(object)

# S4 method for class 'tiledb_filter_list'
max_chunk_size(object)

tiledb_filter_list_get_max_chunk_size(object)
```

## Arguments

- object:

  tiledb_filter_list

## Value

integer max_chunk_size

## Examples

``` r
flt <- tiledb_filter("ZSTD")
tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#> tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",5) 
filter_list <- tiledb_filter_list(c(flt))
max_chunk_size(filter_list)
#> [1] 65536
```
