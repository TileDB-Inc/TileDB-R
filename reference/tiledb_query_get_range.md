# Retrieve the query range for a query dimension and range index

Retrieve the query range for a query dimension and range index

## Usage

``` r
tiledb_query_get_range(query, dimidx, rngidx)
```

## Arguments

- query:

  A TileDB Query object

- dimidx:

  An integer or numeric index selecting the dimension

- rngidx:

  An integer or numeric index selection the given range for the
  dimension

## Value

An integer vector with elements start, end and stride for the query
range for the given dimension and range index
