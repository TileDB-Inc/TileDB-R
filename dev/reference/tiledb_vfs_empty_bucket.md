# Empty a VFS Bucket

Empty a VFS Bucket

## Usage

``` r
tiledb_vfs_empty_bucket(uri, vfs = tiledb_get_vfs())
```

## Arguments

- uri:

  Character variable with a URI describing a cloud bucket

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The URI value that was emptied
