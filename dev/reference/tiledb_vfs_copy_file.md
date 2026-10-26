# Copy a file to VFS

Copy a file to VFS

## Usage

``` r
tiledb_vfs_copy_file(file, uri, vfs = tiledb_get_vfs())
```

## Arguments

- file:

  Character variable with a local file path

- uri:

  Character variable with a URI describing a file path

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The uri value of the copied file
