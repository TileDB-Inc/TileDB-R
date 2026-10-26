# Open a TileDB VFS Filehandle for reading or writing

Open a TileDB VFS Filehandle for reading or writing

## Usage

``` r
tiledb_vfs_open(
  binfile,
  mode = c("READ", "WRITE", "APPEND"),
  vfs = tiledb_get_vfs(),
  ctx = tiledb_get_context()
)
```

## Arguments

- binfile:

  A character variable describing the (binary) file to be opened

- mode:

  A character variable with value ‘READ’, ‘WRITE’ or ‘APPEND’

- vfs:

  A TileDB VFS object; default is to use a cached value.

- ctx:

  (optional) A TileDB Ctx object

## Value

A TileDB VFS Filehandle object (as an external pointer)
