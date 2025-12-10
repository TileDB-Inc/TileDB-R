# Remove a VFS Directory

Remove a VFS Directory

## Usage

``` r
tiledb_vfs_remove_dir(uri, vfs = tiledb_get_vfs())
```

## Arguments

- uri:

  Character variable with a URI describing a diretory path

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The uri value of the removed directory
