# Create a query condition for vector 'IN' and 'NOT_IN' operations

Uses ‘IN’ and ‘NOT_IN’ operators on given attribute

## Usage

``` r
tiledb_query_condition_create(
  name,
  values,
  op = "IN",
  ctx = tiledb_get_context()
)
```

## Arguments

- name:

  A character value with attribute name

- values:

  A vector with the given values, supported types are integer, double,
  integer64 and character

- op:

  (optional) A character value with the chosen set operation, this must
  be one of ‘IN’ or ‘NOT_IN’; default to ‘IN’

- ctx:

  (optional) A TileDB Ctx object; if not supplied the default context
  object is retrieved

## Value

A query condition object is returned
