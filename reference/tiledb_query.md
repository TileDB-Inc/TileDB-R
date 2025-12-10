# Creates a 'tiledb_query' object

Creates a 'tiledb_query' object

## Usage

``` r
tiledb_query(
  array,
  type = if (tiledb_version(TRUE) >= "2.12.0") {
     c("READ", "WRITE", "DELETE",
    "MODIFY_EXCLUSIVE")
 } else {
     c("READ", "WRITE")
 },
  ctx = tiledb_get_context()
)
```

## Arguments

- array:

  A TileDB Array object

- type:

  A character value that must be one of 'READ', 'WRITE', or 'DELETE'
  (for TileDB \>= 2.12.0)

- ctx:

  (optional) A TileDB Ctx object

## Value

'tiledb_query' object
