# Retrieve the `tiledb_config` object from the `tiledb_ctx`

Retrieve the `tiledb_config` object from the `tiledb_ctx`

## Usage

``` r
# S4 method for class 'tiledb_ctx'
config(object = tiledb_get_context())
```

## Arguments

- object:

  tiledb_ctx object

## Value

`tiledb_config` object associated with the `tiledb_ctx` instance

## Examples

``` r
ctx <- tiledb_ctx(c("sm.tile_cache_size" = "10"))
cfg <- config(ctx)
cfg["sm.tile_cache_size"]
#> sm.tile_cache_size 
#>               "10" 
```
