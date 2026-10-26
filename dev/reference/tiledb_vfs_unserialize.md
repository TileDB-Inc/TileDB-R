# Unserialize an R Object from a VFS-accessible URI

Unserialize an R Object from a VFS-accessible URI

## Usage

``` r
tiledb_vfs_unserialize(uri, vfs = tiledb_get_vfs())
```

## Arguments

- uri:

  Character variable with a URI describing a file path to an RDS file

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The unserialized object
