# Creates a TileDB Config object

Note that for actually setting persistent values, the (altered) config
object needs to used to create (or update) the `tiledb_ctx` object.
Similarly, to check whether values are set, one should use the `config`
method of the `tiledb_ctx` object. Examples for this are
`ctx <- tiledb_ctx(limitTileDBCores())` to use updated configuration
values to create a context object, and `cfg <- config(ctx)` to retrieve
it.

## Usage

``` r
tiledb_config(config = NA_character_)
```

## Arguments

- config:

  (optional) character vector of config parameter names, values

## Value

`tiledb_config` object

## Examples

``` r
cfg <- tiledb_config()
cfg["sm.tile_cache_size"]
#> NA 
#> NA 

# set tile cache size to custom value
cfg <- tiledb_config(c("sm.tile_cache_size" = "100"))
cfg["sm.tile_cache_size"]
#> sm.tile_cache_size 
#>              "100" 
```
