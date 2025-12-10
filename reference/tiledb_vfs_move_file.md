# Move (or rename) a VFS File

Move (or rename) a VFS File

## Usage

``` r
tiledb_vfs_move_file(olduri, newuri, vfs = tiledb_get_vfs())
```

## Arguments

- olduri:

  Character variable with an existing URI describing a file path

- newuri:

  Character variable with a new desired URI file path

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The newuri value of the moved file
