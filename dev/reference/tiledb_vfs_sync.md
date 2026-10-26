# Sync a TileDB VFS Filehandle

Sync a TileDB VFS Filehandle

## Usage

``` r
tiledb_vfs_sync(fh, ctx = tiledb_get_context())
```

## Arguments

- fh:

  A TileDB VFS Filehandle external pointer as returned from
  `tiledb_vfs_open`

- ctx:

  (optional) A TileDB Ctx object

## Value

The result of the sync operation is returned.
