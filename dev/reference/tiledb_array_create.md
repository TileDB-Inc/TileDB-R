# Creates a new TileDB array given an input schema.

Creates a new TileDB array given an input schema.

## Usage

``` r
tiledb_array_create(uri, schema, encryption_key)
```

## Arguments

- uri:

  URI specifying path to create the TileDB array object

- schema:

  tiledb_array_schema object

- encryption_key:

  optional A character value with an AES-256 encryption key in case the
  array should be encrypted.

## Examples

``` r
if (FALSE) { # \dontrun{
pth <- tempdir()
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 10L), type = "INT32")))
sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a1", type = "INT32")))
tiledb_array_create(pth, sch)
tiledb_object_type(pth)
} # }
```
