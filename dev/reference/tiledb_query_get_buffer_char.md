# Retrieve content from a Query character buffer

This function uses a query buffer for a character attribute or dimension
and returns its content.

## Usage

``` r
tiledb_query_get_buffer_char(bufptr, sizeoffsets = 0, sizestring = 0)
```

## Arguments

- bufptr:

  An external pointer with a query buffer

- sizeoffsets:

  An optional argument for the length of the internal offsets vector

- sizestring:

  An optional argument for the length of the internal string

## Value

An R object as resulting from the query
