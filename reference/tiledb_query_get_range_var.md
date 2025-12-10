# Retrieve the query range for a variable-sized query dimension and range index

Retrieve the query range for a variable-sized query dimension and range
index

## Usage

``` r
tiledb_query_get_range_var(query, dimidx, rngidx)
```

## Arguments

- query:

  A TileDB Query object

- dimidx:

  An integer index selecting the variable-sized dimension

- rngidx:

  An integer index selection the given range for the dimension

## Value

An string vector with elements start and end for the query range for the
given dimension and range index
