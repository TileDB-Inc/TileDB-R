# Return VFS Directory Listing

Return VFS Directory Listing

## Usage

``` r
tiledb_vfs_ls(uri, vfs = tiledb_get_vfs())
```

## Arguments

- uri:

  Character variable with a URI describing a file path

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The content of the directory, non-recursive
