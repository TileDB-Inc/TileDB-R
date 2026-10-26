# Set a Current Domain of an Array Schema

Set a Current Domain of an Array Schema

## Usage

``` r
tiledb_array_schema_set_current_domain(schema, cd, ctx = tiledb_get_context())
```

## Arguments

- schema:

  A TileDB Schema object

- cd:

  A TileDB Current Domain object

- ctx:

  Optional tiledb_ctx object

## Value

Nothing is returned from this function (but an error, should it occur is
reported)
