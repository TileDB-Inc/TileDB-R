# Allocate and populate a Query buffer for writing the given char vector

Allocate and populate a Query buffer for writing the given char vector

## Usage

``` r
tiledb_query_create_buffer_ptr_char(query, varvec)
```

## Arguments

- query:

  A TileDB Query object

- varvec:

  A vector of strings

## Value

An external pointer to the allocated buffer object
