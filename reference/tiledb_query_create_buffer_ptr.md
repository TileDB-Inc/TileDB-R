# Allocate and populate a Query buffer for a given object of a given data type.

This function allocates a query buffer for the given data object of the
given type and assigns the object content to the buffer.

## Usage

``` r
tiledb_query_create_buffer_ptr(query, datatype, object)
```

## Arguments

- query:

  A TileDB Query object

- datatype:

  A character value containing the data type

- object:

  A vector object of the given type

## Value

An external pointer to the allocated buffer object
