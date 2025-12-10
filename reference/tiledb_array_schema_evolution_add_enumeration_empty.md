# Evolve an Array Schema by adding an empty Enumeration

Evolve an Array Schema by adding an empty Enumeration

## Usage

``` r
tiledb_array_schema_evolution_add_enumeration_empty(
  ase,
  enum_name,
  type_str = "ASCII",
  cell_val_num = NA_integer_,
  ordered = FALSE,
  ctx = tiledb_get_context()
)
```

## Arguments

- ase:

  An ArraySchemaEvolution object

- enum_name:

  A character value with the Enumeration name

- type_str:

  A character value with the TileDB type, defaults to ‘ASCII’

- cell_val_num:

  An integer with number values per cell, defaults to `NA_integer_` to
  flag the `NA` value use for character values

- ordered:

  A logical value indicating standard `factor` (when `FALSE`, the
  default) or `ordered` (when `TRUE`)

- ctx:

  Optional tiledb_ctx object
