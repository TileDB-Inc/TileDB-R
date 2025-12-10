# Add an Enumeration to a TileDB Array Schema Evolution object

Add an Enumeration to a TileDB Array Schema Evolution object

## Usage

``` r
tiledb_array_schema_evolution_add_enumeration(
  object,
  name,
  enums,
  ordered = FALSE,
  ctx = tiledb_get_context()
)
```

## Arguments

- object:

  A TileDB 'array_schema_evolution' object

- name:

  A character value with the name for the Enumeration

- enums:

  A character vector

- ordered:

  (optional) A boolean switch whether the enumeration is ordered

- ctx:

  (optional) A TileDB Ctx object; if not supplied the default context
  object is retrieved

## Value

The modified 'array_schema_evolution' object, invisibly
