# Import a file into a TileDB Filestore

Import a file into a TileDB Filestore

## Usage

``` r
tiledb_filestore_uri_import(
  filestore_uri,
  file_uri,
  ctx = tiledb_get_context()
)
```

## Arguments

- filestore_uri:

  Character with an TileDB Array Schema URI

- file_uri:

  Character with a file URI

- ctx:

  (optional) A TileDB Ctx object; if not supplied the default context
  object is retrieved

## Value

A boolean is returned to indicate successful completion
