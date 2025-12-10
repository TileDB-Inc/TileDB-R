# Constructs a `tiledb_array_schema` object

Constructs a `tiledb_array_schema` object

## Usage

``` r
tiledb_array_schema(
  domain,
  attrs,
  cell_order = "COL_MAJOR",
  tile_order = "COL_MAJOR",
  sparse = FALSE,
  coords_filter_list = NULL,
  offsets_filter_list = NULL,
  validity_filter_list = NULL,
  capacity = 10000L,
  allows_dups = FALSE,
  enumerations = NULL,
  ctx = tiledb_get_context()
)
```

## Arguments

- domain:

  tiledb_domain object

- attrs:

  a list of one or more tiledb_attr objects

- cell_order:

  (default "COL_MAJOR")

- tile_order:

  (default "COL_MAJOR")

- sparse:

  (default FALSE)

- coords_filter_list:

  (optional)

- offsets_filter_list:

  (optional)

- validity_filter_list:

  (optional)

- capacity:

  (optional)

- allows_dups:

  (optional, requires ‘sparse’ to be TRUE)

- enumerations:

  (optional) named list of enumerations

- ctx:

  tiledb_ctx object (optional)

## Value

An object of class `tiledb_array_schema`.

## Examples

``` r
schema <- tiledb_array_schema(
  dom = tiledb_domain(
    dims = c(
      tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
      tiledb_dim("cols", c(1L, 4L), 4L, "INT32")
    )
  ),
  attrs = c(tiledb_attr("a", type = "INT32")),
  cell_order = "COL_MAJOR",
  tile_order = "COL_MAJOR",
  sparse = FALSE
)
schema
#> tiledb_array_schema(
#>     domain=tiledb_domain(c(
#>         tiledb_dim(name="rows", domain=c(1L,4L), tile=4L, type="INT32"),
#>         tiledb_dim(name="cols", domain=c(1L,4L), tile=4L, type="INT32")
#>     )),
#>     attrs=c(
#>         tiledb_attr(name="a", type="INT32", ncells=1, nullable=FALSE)
#>     ),
#>     cell_order="COL_MAJOR", tile_order="COL_MAJOR", capacity=10000, sparse=FALSE, allows_dups=FALSE,
#>     coords_filter_list=tiledb_filter_list(c(tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",-1))),
#>     offsets_filter_list=tiledb_filter_list(c(tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",-1))),
#>     validity_filter_list=tiledb_filter_list(c(tiledb_filter_set_option(tiledb_filter("RLE"),"COMPRESSION_LEVEL",-1)))
#> )
```
