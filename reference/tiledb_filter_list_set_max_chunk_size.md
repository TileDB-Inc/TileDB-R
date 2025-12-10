# Set the filter_list's max_chunk_size

Set the filter_list's max_chunk_size

## Usage

``` r
set_max_chunk_size(object, value)

# S4 method for class 'tiledb_filter_list,numeric'
set_max_chunk_size(object, value)

tiledb_filter_list_set_max_chunk_size(object, value)
```

## Arguments

- object:

  tiledb_filter_list

- value:

  A numeric value

## Examples

``` r
flt <- tiledb_filter("ZSTD")
tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#> tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",5) 
filter_list <- tiledb_filter_list(c(flt))
set_max_chunk_size(filter_list, 10)
```
