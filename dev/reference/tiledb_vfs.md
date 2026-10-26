# Creates a `tiledb_vfs` object

Creates a `tiledb_vfs` object

## Usage

``` r
tiledb_vfs(config = NULL, ctx = tiledb_get_context())
```

## Arguments

- config:

  (optional) character vector of config parameter names, values

- ctx:

  (optional) A TileDB Ctx object

## Value

The `tiledb_vfs` object

## Examples

``` r
# default configuration
vfs <- tiledb_vfs()
```
