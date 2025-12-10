# Returns the tile layout string associated with the `tiledb_array_schema`

Returns the tile layout string associated with the `tiledb_array_schema`

## Usage

``` r
# S4 method for class 'tiledb_array_schema'
tile_order(object)
```

## Arguments

- object:

  A TileDB Schema object

## Value

A character string with tile's layout, e.g., `"COL_MAJOR"`.
