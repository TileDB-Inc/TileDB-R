# Add an empty Enumeration to a Schema

Add an empty Enumeration to a Schema

## Usage

``` r
tiledb_array_schema_set_enumeration_empty(
  schema,
  attr,
  enum_name,
  type_str = "ASCII",
  cell_val_num = NA_integer_,
  ordered = FALSE,
  ctx = tiledb_get_context()
)
```

## Arguments

- schema:

  A TileDB Schema object

- attr:

  An Attribute for which an empty Enumeration will be added

- enum_name:

  A character value with the Enumeration name

- type_str:

  A character value with the TileDB type, defaults to ‘ASCII’

- cell_val_num:

  An integer with number values per cell, defaults to `NA_integer_` to
  flag the `NA` value use for character values

- ordered:

  A logical value indicated standard `factor` (when `FALSE`, the
  default) or `ordered` (when `TRUE`)

- ctx:

  Optional tiledb_ctx object

## Value

The modified `tiledb_array_schema` object.
