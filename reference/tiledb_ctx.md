# Creates a `tiledb_ctx` object

Creates a `tiledb_ctx` object

## Usage

``` r
tiledb_ctx(config = NULL, cached = TRUE)
```

## Arguments

- config:

  (optional) character vector of config parameter names, values

- cached:

  (optional) logical switch to force new creation

## Value

`tiledb_ctx` object

## Examples

``` r
# default configuration
ctx <- tiledb_ctx()

# optionally set config parameters
ctx <- tiledb_ctx(c("sm.tile_cache_size" = "100"))
```
