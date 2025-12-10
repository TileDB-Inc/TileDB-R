# Allocate a Query buffer for reading a character attribute

Allocate a Query buffer for reading a character attribute

## Usage

``` r
tiledb_query_alloc_buffer_ptr_char(sizeoffsets, sizedata, nullable = FALSE)
```

## Arguments

- sizeoffsets:

  A numeric value with the size of the offsets vector

- sizedata:

  A numeric value of the size of the data string

- nullable:

  An optional boolean indicating whether the column can have NULLs

## Value

An external pointer to the allocated buffer object
