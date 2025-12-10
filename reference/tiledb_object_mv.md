# Move a TileDB resource to new uri path

Raises an error if either uri is invalid, or the old uri resource is not
a tiledb object

## Usage

``` r
tiledb_object_mv(old_uri, new_uri, ctx = tiledb_get_context())
```

## Arguments

- old_uri:

  old uri of existing tiledb resource

- new_uri:

  new uri to move tiledb resource

- ctx:

  tiledb_ctx object (optional)

## Value

new uri of moved tiledb resource
