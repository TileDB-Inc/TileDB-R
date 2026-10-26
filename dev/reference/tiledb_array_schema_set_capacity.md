# Sets the schema capacity (for sparse fragments)

Sets the `tiledb_array` schema tile capacity for sparse fragments.

## Usage

``` r
capacity(x) <- value

# S4 method for class 'tiledb_array_schema'
capacity(x) <- value

tiledb_array_schema_set_capacity(x, value)
```

## Arguments

- x:

  A TileDB Schema object

- value:

  An integer or numeric value for the new tile capacity

## Value

The modified `tiledb_array_schema` object.
