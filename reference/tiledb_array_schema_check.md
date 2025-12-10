# Check the schema for correctness

Returns the `tiledb_array` schema for correctness

## Usage

``` r
schema_check(object)

# S4 method for class 'tiledb_array_schema'
schema_check(object)

check(object)

# S4 method for class 'tiledb_array_schema'
check(object)

tiledb_array_schema_check(object)
```

## Arguments

- object:

  A TileDB Schema object

## Value

The boolean value `TRUE` is returned for a correct schema; for an
incorrect schema an error condition is triggered.
