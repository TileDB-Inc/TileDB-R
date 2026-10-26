# Combine two 'tiledb_query_condition' objects

Combines two query condition object using a relational operator. Support
for operator 'AND' is generally available, the 'OR' operator is
available if TileDB 2.10 or newer is used.

## Usage

``` r
tiledb_query_condition_combine(lhs, rhs, op)
```

## Arguments

- lhs:

  A 'tiledb_query_condition' object on the left-hand side of the
  relation

- rhs:

  A 'tiledb_query_condition' object on the left-hand side of the
  relation

- op:

  A character value with then relation, this must be one of 'AND', 'OR'
  or 'NOT'.

## Value

The combined 'tiledb_query_condition' object
