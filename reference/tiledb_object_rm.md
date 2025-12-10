# Removes a TileDB resource

Raises an error if the uri is invalid, or the uri resource is not a
tiledb object

## Usage

``` r
tiledb_object_rm(uri, ctx = tiledb_get_context())
```

## Arguments

- uri:

  path to TileDB resource

- ctx:

  tiledb_ctx object (optional)

## Value

uri of removed TileDB resource
