# Creates a `tiledb_current_domain` object

Creates a `tiledb_current_domain` object

## Usage

``` r
tiledb_current_domain(ctx = tiledb_get_context())
```

## Arguments

- ctx:

  (optional) A TileDB Ctx object

## Value

The `tiledb_current_domain` object

## Examples

``` r
if (tiledb_version(TRUE) >= "2.25.0") {
  cd <- tiledb_current_domain()
}
```
