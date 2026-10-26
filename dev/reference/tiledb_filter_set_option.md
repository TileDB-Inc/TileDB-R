# Set the option for a filter

Set the option for a filter

## Usage

``` r
tiledb_filter_set_option(object, option, value)
```

## Arguments

- object:

  tiledb_filter

- option:

  string

- value:

  int

## Value

The modified filter object is returned.

## Examples

``` r
c <- tiledb_filter("ZSTD")
tiledb_filter_set_option(c, "COMPRESSION_LEVEL", 5)
#> tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",5) 
tiledb_filter_get_option(c, "COMPRESSION_LEVEL")
#> [1] 5
```
