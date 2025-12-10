# Constructs a `tiledb_attr` object

Constructs a `tiledb_attr` object

## Usage

``` r
tiledb_attr(
  name,
  type,
  filter_list = tiledb_filter_list(),
  ncells = 1,
  nullable = FALSE,
  enumeration = NULL,
  ctx = tiledb_get_context()
)
```

## Arguments

- name:

  The dimension name / label string; if missing default `""` is used.

- type:

  The tiledb_attr TileDB datatype string; if missing the user is alerted
  that this is a *required* parameter.

- filter_list:

  (default filter_list("NONE")) An optional tiledb_filter_list object

- ncells:

  (default 1) The number of cells, use `NA` to signal variable length

- nullable:

  (default FALSE) A logical switch whether the attribute can have
  missing values

- enumeration:

  (default NULL) A character vector of dictionary values

- ctx:

  tiledb_ctx object (optional)

## Value

A `tiledb_attr` object

## Examples

``` r
flt <- tiledb_filter_list(list(tiledb_filter("GZIP")))
attr <- tiledb_attr(
  name = "a1", type = "INT32",
  filter_list = flt
)
attr
#> tiledb_attr(name="a1", type="INT32", ncells=1, nullable=FALSE, filter_list=tiledb_filter_list(c(tiledb_filter_set_option(tiledb_filter("GZIP"),"COMPRESSION_LEVEL",-1)))) 
```
