# Open a TileDB Array

Open a TileDB Array

## Usage

``` r
tiledb_array_open(
  arr,
  type = if (tiledb_version(TRUE) >= "2.12.0") {
     c("READ", "WRITE", "DELETE",
    "MODIFY_EXCLUSIVE")
 } else {
     c("READ", "WRITE")
 }
)
```

## Arguments

- arr:

  A TileDB Array object as for example returned by
  [`tiledb_array()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_array.md)

- type:

  A character value that must be either ‘READ’, ‘WRITE’ or (for TileDB
  2.12.0 or later) ‘DELETE’ or ‘MODIFY_EXCLUSIVE’

## Value

The TileDB Array object but opened for reading or writing
