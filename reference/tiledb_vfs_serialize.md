# Serialize an R Object to a VFS-accessible URI

Serialize an R Object to a VFS-accessible URI

## Usage

``` r
tiledb_vfs_serialize(obj, uri, vfs = tiledb_get_vfs())
```

## Arguments

- obj:

  An R object which will be passed to
  [`serialize()`](https://rdrr.io/r/base/serialize.html)

- uri:

  Character variable with a URI describing a file path to an RDS file

- vfs:

  A TileDB VFS object; default is to use a cached value.

## Value

The uri is returned invisibly
