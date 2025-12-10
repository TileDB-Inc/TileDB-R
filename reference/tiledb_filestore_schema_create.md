# Create an array schema from a given URI with schema

Create an array schema from a given URI with schema

## Usage

``` r
tiledb_filestore_schema_create(uri = NULL, ctx = tiledb_get_context())
```

## Arguments

- uri:

  Character with an TileDB Array Schema URI, if missing or NULL a
  default schema is returned

- ctx:

  (optional) A TileDB Ctx object; if not supplied the default context
  object is retrieved

## Value

An ArraySchema object corresponding to the supplied schema, or a default
if missing
