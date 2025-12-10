# Return the TileDB object type string of a TileDB resource

Object types:

- `"ARRAY"`, dense or sparse TileDB array

- `"GROUP"`, TileDB group

- \`"INVALID"“, not a TileDB resource

## Usage

``` r
tiledb_object_type(uri, ctx = tiledb_get_context())
```

## Arguments

- uri:

  path to TileDB resource

- ctx:

  tiledb_ctx object (optional)

## Value

TileDB object type string
