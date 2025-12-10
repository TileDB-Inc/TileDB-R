# Export from a TileDB Filestore to a character variable

Export from a TileDB Filestore to a character variable

## Usage

``` r
tiledb_filestore_buffer_export(
  filestore_uri,
  offset,
  bytes,
  ctx = tiledb_get_context()
)
```

## Arguments

- filestore_uri:

  Character with an TileDB Array Schema URI

- offset:

  (optional) Numeric variable with offset from beginnig, default is zero

- bytes:

  (optional) Numeric variable with number of bytes to read, default is
  zero

- ctx:

  (optional) A TileDB Ctx object; if not supplied the default context
  object is retrieved

## Value

A character variable containing the filestore content (subject to offset
and bytes) is returned
