# Returns the type of the filter used

Returns the type of the filter used

## Usage

``` r
tiledb_filter_type(object)
```

## Arguments

- object:

  tiledb_filter

## Value

TileDB filter type string

## Examples

``` r
c <- tiledb_filter("ZSTD")
tiledb_filter_type(c)
#> [1] "ZSTD"
```
