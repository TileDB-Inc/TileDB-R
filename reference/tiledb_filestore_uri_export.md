# Export a file from a TileDB Filestore

Export a file from a TileDB Filestore

## Usage

``` r
tiledb_filestore_uri_export(
  file_uri,
  filestore_uri,
  ctx = tiledb_get_context()
)
```

## Arguments

- file_uri:

  Character with a file URI

- filestore_uri:

  Character with an TileDB Array Schema URI

- ctx:

  (optional) A TileDB Ctx object; if not supplied the default context
  object is retrieved

## Value

A boolean is returned to indicate successful completion
