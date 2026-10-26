# Gets a config parameter value

Gets a config parameter value

## Usage

``` r
# S4 method for class 'tiledb_config,ANY'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x:

  A `tiledb_config` object

- i:

  parameter key string

- j:

  parameter key string, currently unused.

- ...:

  Extra parameter for method signature, currently unused.

- drop:

  Optional logical switch to drop dimensions, default FALSE, currently
  unused.

## Value

A config string value if parameter exists, else NA

## Examples

``` r
cfg <- tiledb_config()
cfg["sm.tile_cache_size"]
#> NA 
#> NA 
cfg["does_not_exist"]
#> NA 
#> NA 
```
