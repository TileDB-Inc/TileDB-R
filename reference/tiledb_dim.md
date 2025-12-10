# Constructs a `tiledb_dim` object

Constructs a `tiledb_dim` object

## Usage

``` r
tiledb_dim(
  name,
  domain,
  tile,
  type,
  filter_list = tiledb_filter_list(),
  ctx = tiledb_get_context()
)
```

## Arguments

- name:

  The dimension name / label string. This argument is required.

- domain:

  The dimension (inclusive) domain. The domain of a dimension is defined
  by a (lower bound, upper bound) vector. For type `ASCII`, `NULL` is
  expected.

- tile:

  The tile dimension tile extent. For type `ASCII`, `NULL` is expected.

- type:

  The dimension TileDB datatype string.

- filter_list:

  An optional `tiledb_filter_list` object, default is no filter

- ctx:

  tiledb_ctx object (optional)

## Value

A `tiledb_dim` object

## Examples

``` r
tiledb_dim(name = "d1", domain = c(1L, 10L), tile = 5L, type = "INT32")
#> tiledb_dim(name="d1", domain=c(1L,10L), tile=5L, type="INT32") 
```
