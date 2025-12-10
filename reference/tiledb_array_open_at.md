# Open a TileDB Array at Timestamp

Open a TileDB Array at Timestamp

## Usage

``` r
tiledb_array_open_at(arr, type = c("READ", "WRITE"), timestamp)
```

## Arguments

- arr:

  A TileDB Array object as for example returned by
  [`tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.md)

- type:

  A character value that must be either ‘READ’ or ‘WRITE’

- timestamp:

  A Datetime object that will be converted to millisecond granularity

## Value

The TileDB Array object but opened for reading or writing
