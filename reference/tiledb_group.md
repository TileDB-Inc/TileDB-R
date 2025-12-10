# Creates a 'tiledb_group' object

Creates a 'tiledb_group' object

## Usage

``` r
tiledb_group(
  uri,
  type = c("READ", "WRITE"),
  ctx = tiledb_get_context(),
  cfg = NULL
)
```

## Arguments

- uri:

  Character variable with the URI of the new group object

- type:

  Character variable with the query type value: one of “READ” or “WRITE”

- ctx:

  (optional) A TileDB Context object; if not supplied the default
  context object is retrieved

- cfg:

  (optional) A TileConfig object

## Value

A 'group' object
