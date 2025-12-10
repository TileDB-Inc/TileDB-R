# Enable use of enumeration in query condition

Set a boolean toggle to signal use of enumeration in query condition
(TileDB 2.17 or later)

## Usage

``` r
tiledb_query_condition_set_use_enumeration(
  qc,
  use_enum,
  ctx = tiledb_get_context()
)
```

## Arguments

- qc:

  A 'tiledb_query_condition' object

- use_enum:

  A boolean to set (if TRUE) or unset (if FALSE) enumeration use

- ctx:

  (optional) A TileDB Ctx object; if not supplied the default context
  object is retrieved

## Value

Nothing is retuned, the function is invoked for the side effect
