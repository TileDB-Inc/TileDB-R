# Constructs a `tiledb_domain` object

All `tiledb_dim` must be of the same TileDB type.

## Usage

``` r
tiledb_domain(dims, ctx = tiledb_get_context())
```

## Arguments

- dims:

  list() of tiledb_dim objects

- ctx:

  tiledb_ctx (optional)

## Value

tiledb_domain

## Examples

``` r
dom <- tiledb_domain(dims = c(
  tiledb_dim("d1", c(1L, 100L), type = "INT32"),
  tiledb_dim("d2", c(1L, 50L), type = "INT32")
))
```
