# Sets a string:string "tag" on the Ctx

Sets a string:string "tag" on the Ctx

## Usage

``` r
tiledb_ctx_set_tag(object, key, value)
```

## Arguments

- object:

  `tiledb_ctx` object

- key:

  string

- value:

  string

## Examples

``` r
ctx <- tiledb_ctx(c("sm.tile_cache_size" = "10"))
cfg <- tiledb_ctx_set_tag(ctx, "tag", "value")
```
