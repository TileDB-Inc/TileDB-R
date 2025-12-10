# Assigns to a Query buffer for a given attribute

This function assigns a given query buffer to a query.

## Usage

``` r
tiledb_query_set_buffer_ptr(query, attr, bufptr)
```

## Arguments

- query:

  A TileDB Query object

- attr:

  A character value containing the attribute

- bufptr:

  An external pointer with a query buffer

## Value

The modified query object, invisibly
