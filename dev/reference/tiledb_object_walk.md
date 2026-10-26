# Recursively discover TileDB resources at a given root URI path

Recursively discover TileDB resources at a given root URI path

## Usage

``` r
tiledb_object_walk(
  uri,
  order = c("PREORDER", "POSTORDER"),
  ctx = tiledb_get_context()
)
```

## Arguments

- uri:

  root uri path to walk

- order:

  traversal order, one of "PREORDER" and "POSTORDER" (default
  "PREORDER")

- ctx:

  tiledb_ctx object (optional)

## Value

a dataframe with object type, object uri string columns
