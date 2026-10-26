# Return (uncompressed) TileDB Filestore size

Return (uncompressed) TileDB Filestore size

## Usage

``` r
tiledb_filestore_size(filestore_uri, ctx = tiledb_get_context())
```

## Arguments

- filestore_uri:

  Character with an TileDB Array Schema URI

- ctx:

  (optional) A TileDB Ctx object; if not supplied the default context
  object is retrieved

## Value

A numeric with the size is returned
