# Returns the filter's option

Returns the filter's option

## Usage

``` r
tiledb_filter_get_option(object, option)
```

## Arguments

- object:

  tiledb_filter

- option:

  string

## Value

Integer value

## Examples

``` r
c <- tiledb_filter("ZSTD")
tiledb_filter_set_option(c, "COMPRESSION_LEVEL", 5)
#> tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",5) 
tiledb_filter_get_option(c, "COMPRESSION_LEVEL")
#> [1] 5
```
