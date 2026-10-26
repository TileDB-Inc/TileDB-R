# Returns the filter at given index

Returns the filter at given index

## Usage

``` r
# S4 method for class 'tiledb_filter_list,ANY'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x:

  `tiledb_config` object

- i:

  parameter key string

- j:

  parameter key string, currently unused.

- ...:

  Extra parameter for method signature, currently unused.

- drop:

  Optional logical switch to drop dimensions, default false.

## Value

object tiledb_filter

## Examples

``` r
flt <- tiledb_filter("ZSTD")
tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 5)
#> tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",5) 
filter_list <- tiledb_filter_list(c(flt))
filter_list[0]
#> tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",5) 
```
