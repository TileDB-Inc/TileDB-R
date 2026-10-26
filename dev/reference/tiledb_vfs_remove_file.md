# Remove a VFS File

Remove a VFS File

## Usage

``` r
tiledb_vfs_remove_file(uri, vfs = tiledb_get_vfs())
```

## Arguments

- uri:

  Character variable with a URI describing a file path

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The uri value of the removed file
