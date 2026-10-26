# Get the Current Domain of an Array Schema

Note that `tiledb_current_domain` object may be empty.

## Usage

``` r
tiledb_array_schema_get_current_domain(schema, ctx = tiledb_get_context())
```

## Arguments

- schema:

  A TileDB Schema object

- ctx:

  Optional tiledb_ctx object

## Value

An object of class `tiledb_current_domain`.
