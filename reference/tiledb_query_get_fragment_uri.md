# Retrieve the URI for a given Query Fragment

This function is only applicable to ‘WRITE’ queries.

## Usage

``` r
tiledb_query_get_fragment_uri(query, idx)
```

## Arguments

- query:

  A TileDB Query object

- idx:

  An integer (or numeric) index ranging from zero to the number of
  fragments minus 1

## Value

An character value with the fragment URI
