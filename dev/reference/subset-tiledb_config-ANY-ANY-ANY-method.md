# Sets a config parameter value

Sets a config parameter value

## Usage

``` r
# S4 method for class 'tiledb_config,ANY,ANY,ANY'
x[i, j] <- value
```

## Arguments

- x:

  A `tiledb_config` object

- i:

  parameter key string

- j:

  parameter key string

- value:

  value to set, will be converted into a stringa

## Value

The updated `tiledb_config` object

## Examples

``` r
cfg <- tiledb_config()
cfg["sm.tile_cache_size"]
#> NA 
#> NA 

# set tile cache size to custom value
cfg["sm.tile_cache_size"] <- 100
cfg["sm.tile_cache_size"]
#> sm.tile_cache_size 
#>              "100" 
```
