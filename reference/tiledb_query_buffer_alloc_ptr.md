# Allocate a Query buffer for a given type

This function allocates a query buffer for the given data type.

## Usage

``` r
tiledb_query_buffer_alloc_ptr(
  query,
  datatype,
  ncells,
  nullable = FALSE,
  varnum = 1
)
```

## Arguments

- query:

  A TileDB Query object

- datatype:

  A character value containing the data type

- ncells:

  A number of elements (not bytes)

- nullable:

  Optional boolean parameter indicating whether missing values are
  allowed (for which another column is allocated), default is FALSE

- varnum:

  Option intgeter parameter for the number of elemements per variable,
  default is one

## Value

An external pointer to the allocated buffer object
