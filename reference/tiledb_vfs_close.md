# Close a TileDB VFS Filehandle

Close a TileDB VFS Filehandle

## Usage

``` r
tiledb_vfs_close(fh, ctx = tiledb_get_context())
```

## Arguments

- fh:

  A TileDB VFS Filehandle external pointer as returned from
  `tiledb_vfs_open`

- ctx:

  (optional) A TileDB Ctx object

## Value

The result of the close operation is returned.
