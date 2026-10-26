# Copy a directory recursively to VFS

Copy a directory recursively to VFS

## Usage

``` r
tiledb_vfs_copy_dir(dir, uri, vfs = tiledb_get_vfs())
```

## Arguments

- dir:

  Character variable with a local directory path

- uri:

  Character variable with a URI describing a directory path

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The uri value of the copied directory
