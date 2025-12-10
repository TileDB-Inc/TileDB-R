# Check for empty VFS Bucket

Check for empty VFS Bucket

## Usage

``` r
tiledb_vfs_is_empty_bucket(uri, vfs = tiledb_get_vfs())
```

## Arguments

- uri:

  Character variable with a URI describing a cloud bucket

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

A boolean value indicating if it is an empty bucket

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- tiledb_config()
cfg["vfs.s3.region"] <- "us-west-1"
ctx <- tiledb_ctx(cfg)
vfs <- tiledb_vfs()
tiledb_vfs_is_empty_bucket(vfs, "s3://tiledb-public-us-west-1/test-array-4x4")
} # }
```
