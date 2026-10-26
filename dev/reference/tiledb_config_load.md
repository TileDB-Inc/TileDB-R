# Load a saved TileDB Config file from disk

Load a saved TileDB Config file from disk

## Usage

``` r
tiledb_config_load(path)
```

## Arguments

- path:

  The path to the config file to be loaded

## Examples

``` r
tmp <- tempfile()
cfg <- tiledb_config(c("sm.tile_cache_size" = "10"))
pth <- tiledb_config_save(cfg, tmp)
cfg <- tiledb_config_load(pth)
cfg["sm.tile_cache_size"]
#> sm.tile_cache_size 
#>               "10" 
```
