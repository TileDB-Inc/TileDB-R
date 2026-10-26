# List TileDB resources at a given root URI path

List TileDB resources at a given root URI path

## Usage

``` r
tiledb_object_ls(uri, filter = NULL, ctx = tiledb_get_context())
```

## Arguments

- uri:

  uri path to walk

- filter:

  optional filtering argument, default is "NULL", currently unused

- ctx:

  tiledb_ctx object (optional)

## Value

a dataframe with object type, object uri string columns
