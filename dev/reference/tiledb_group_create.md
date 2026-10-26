# Create a TileDB Group at the given path

Create a TileDB Group at the given path

## Usage

``` r
tiledb_group_create(uri, ctx = tiledb_get_context())
```

## Arguments

- uri:

  Character variable with the URI of the new group

- ctx:

  (optional) A TileDB Ctx object; if not supplied the default context
  object is retrieved

## Value

The uri path, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
pth <- tempdir()
tiledb_group_create(pth)
tiledb_object_type(pth)
} # }
```
