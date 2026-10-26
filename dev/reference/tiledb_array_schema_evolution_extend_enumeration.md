# Extend an Evolution via Array Schema Evolution

Extend an Evolution via Array Schema Evolution

## Usage

``` r
tiledb_array_schema_evolution_extend_enumeration(
  ase,
  array,
  enum_name,
  new_values,
  nullable = FALSE,
  ordered = FALSE,
  ctx = tiledb_get_context()
)
```

## Arguments

- ase:

  An ArraySchemaEvolution object

- array:

  A TileDB Array object

- enum_name:

  A character value with the Enumeration name

- new_values:

  A character vector with the new Enumeration values

- nullable:

  A logical value indicating if the Enumeration can contain missing
  values (with a default of `FALSE`)

- ordered:

  A logical value indicating standard `factor` (when `FALSE`, the
  default) or `ordered` (when `TRUE`)

- ctx:

  Optional tiledb_ctx object

## Value

The modified ArraySchemaEvolution object
