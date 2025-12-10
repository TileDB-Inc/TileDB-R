# Write to a TileDB VFS Filehandle

This interface currently defaults to using an integer vector. This is
suitable for R objects as the raw vector result from serialization can
be mapped easily to an integer vector. It is also possible to `memcpy`
to the contiguous memory of an integer vector should other (non-R) data
be transferred.

## Usage

``` r
tiledb_vfs_write(fh, vec, ctx = tiledb_get_context())
```

## Arguments

- fh:

  A TileDB VFS Filehandle external pointer as returned from
  `tiledb_vfs_open`

- vec:

  An integer vector of content to be written

- ctx:

  (optional) A TileDB Ctx object

## Value

The result of the write operation is returned.
