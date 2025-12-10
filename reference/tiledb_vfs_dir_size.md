# Return VFS Directory Size

Return VFS Directory Size

## Usage

``` r
tiledb_vfs_dir_size(uri, vfs = tiledb_get_vfs())
```

## Arguments

- uri:

  Character variable with a URI describing a file path

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The size of the directory
