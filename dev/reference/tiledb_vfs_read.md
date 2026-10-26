# Read from a TileDB VFS Filehandle

This interface currently defaults to reading an integer vector. This is
suitable for R objects as a raw vector used for (de)serialization can be
mapped easily to an integer vector. It is also possible to `memcpy` to
the contiguous memory of an integer vector should other (non-R) data be
transferred.

## Usage

``` r
tiledb_vfs_read(fh, offset, nbytes, ctx = tiledb_get_context())
```

## Arguments

- fh:

  A TileDB VFS Filehandle external pointer as returned from
  `tiledb_vfs_open`

- offset:

  A scalar value with the byte offset from the beginning of the file
  with a of zero.

- nbytes:

  A scalar value with the number of bytes to be read.

- ctx:

  (optional) A TileDB Ctx object

## Value

The binary file content is returned as an integer vector.
