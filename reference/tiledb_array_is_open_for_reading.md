# Test if TileDB Array is open for reading

Test if TileDB Array is open for reading

## Usage

``` r
tiledb_array_is_open_for_reading(arr)
```

## Arguments

- arr:

  A TileDB Array object as for example returned by
  [`tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.md)

## Value

A boolean indicating whether the TileDB Array object is open, and the
mode is "READ".
