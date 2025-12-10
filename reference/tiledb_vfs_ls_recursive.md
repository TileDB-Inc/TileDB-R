# Recursively list objects from given URI

This functionality is currently limited to S3 URIs.

## Usage

``` r
tiledb_vfs_ls_recursive(
  uri,
  vfs = tiledb_get_vfs(),
  ctx = tiledb_get_context()
)
```

## Arguments

- uri:

  Character variable with a URI describing a file path

- vfs:

  (optiona) A TileDB VFS object; default is to use a cached value.

- ctx:

  (optional) A TileDB Ctx object

## Value

A data.frame object with two columns for the full path and the object
size in bytes
