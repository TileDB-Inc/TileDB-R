# Import size bytes from a string into a TileDB Filestore

Import size bytes from a string into a TileDB Filestore

## Usage

``` r
tiledb_filestore_buffer_import(
  filestore_uri,
  buf,
  bytes,
  ctx = tiledb_get_context()
)
```

## Arguments

- filestore_uri:

  Character with an TileDB Array Schema URI

- buf:

  Character variable with content to be imported

- bytes:

  Number of bytes to be import, defaults to length of `buf`

- ctx:

  (optional) A TileDB Ctx object; if not supplied the default context
  object is retrieved

## Value

A boolean is returned to indicate successful completion
