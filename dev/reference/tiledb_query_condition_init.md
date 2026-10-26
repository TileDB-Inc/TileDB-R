# Initialize a 'tiledb_query_condition' object

Initializes (and possibly allocates) a query condition object using a
triplet of attribute name, comparison value, and operator. Six types of
conditions are supported, they all take a single scalar comparison
argument and attribute to compare against. At present only integer or
numeric attribute comparisons are implemented.

## Usage

``` r
tiledb_query_condition_init(
  attr,
  value,
  dtype,
  op,
  qc = tiledb_query_condition()
)
```

## Arguments

- attr:

  A character value with the attribute name

- value:

  A scalar value that the attribute is compared against

- dtype:

  A character value with the TileDB data type of the attribute column,
  for example 'FLOAT64' or 'INT32'

- op:

  A character value with then comparison operation, this must be one of
  'LT', 'LE', 'GT', 'GE', 'EQ', 'NE'.

- qc:

  (optional) A 'tiledb_query_condition' object to be initialized by this
  call, if none is given a new one is allocated.

## Value

The initialized 'tiledb_query_condition' object
