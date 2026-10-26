# Set TileDB Query layout

Set TileDB Query layout

## Usage

``` r
tiledb_query_set_layout(
  query,
  layout = c("COL_MAJOR", "ROW_MAJOR", "GLOBAL_ORDER", "UNORDERED")
)
```

## Arguments

- query:

  A TileDB Query object

- layout:

  A character variable with the layout; must be one of "COL_MAJOR",
  "ROW_MAJOR", "GLOBAL_ORDER", "UNORDERED")

## Value

The modified query object, invisibly
