# Assign a buffer to a Query attribute

Assign a buffer to a Query attribute

## Usage

``` r
tiledb_query_set_buffer_ptr_char(query, attr, bufptr)
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
