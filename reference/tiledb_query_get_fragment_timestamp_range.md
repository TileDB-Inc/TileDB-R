# Retrieve the timestamp range for a given Query Fragment

This function is only applicable to ‘WRITE’ queries. The time resolution
in TileDB is millseconds since the epoch so an R `Datetime` vector is
returned.

## Usage

``` r
tiledb_query_get_fragment_timestamp_range(query, idx)
```

## Arguments

- query:

  A TileDB Query object

- idx:

  An integer (or numeric) index ranging from zero to the number of
  fragments minus 1

## Value

A two-element datetime vector with the start and end time of the
fragment write.
