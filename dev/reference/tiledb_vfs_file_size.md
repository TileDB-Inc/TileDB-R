# Return VFS File Size

Return VFS File Size

## Usage

``` r
tiledb_vfs_file_size(uri, vfs = tiledb_get_vfs())
```

## Arguments

- uri:

  Character variable with a URI describing a file path

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The size of the file
