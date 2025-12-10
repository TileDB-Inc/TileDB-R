# Move (or rename) a VFS Directory

Move (or rename) a VFS Directory

## Usage

``` r
tiledb_vfs_move_dir(olduri, newuri, vfs = tiledb_get_vfs())
```

## Arguments

- olduri:

  Character variable with an existing URI describing a directory path

- newuri:

  Character variable with a new desired URI directory path

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The newuri value of the moved directory
