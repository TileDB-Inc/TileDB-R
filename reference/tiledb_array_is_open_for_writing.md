# Test if TileDB Array is open for writing

Test if TileDB Array is open for writing

## Usage

``` r
tiledb_array_is_open_for_writing(arr)
```

## Arguments

- arr:

  A TileDB Array object as for example returned by
  [`tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.md)

## Value

A boolean indicating whether the TileDB Array object is open, and the
mode is "WRITE".
