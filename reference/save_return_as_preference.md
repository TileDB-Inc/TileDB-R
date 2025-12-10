# Store object conversion preference

Save (or load) ‘return_as’ conversion preference in an optional config
file

## Usage

``` r
save_return_as_preference(
  value = c("asis", "array", "matrix", "data.frame", "data.table", "tibble")
)

load_return_as_preference()

get_return_as_preference()

set_return_as_preference(
  value = c("asis", "array", "matrix", "data.frame", "data.table", "tibble")
)
```

## Arguments

- value:

  A character variable with one of the six permitted values

## Value

For the setter, `TRUE` is returned invisibly but the function is invoked
for the side effect of storing the value. For either getter, the
character value.

## Details

The `tiledb_array` object can set a preference for conversion for each
retrieved object. This preference can also be encoded in a configuration
file as R (version 4.0.0 or later) allows a user- and package specific
configuration files. These helper functions set and retrieve the value,
respectively, or retrieve the cached value from the package environment
where it is set at package load.

Note that the value must be one of ‘asis’ (the default), ‘array’,
‘matrix’‘data.frame’, ‘data.table’ or ‘tibble’. The latter two require
the corresponding package to be installed.

## Note

This function requires R version 4.0.0 or later to utilise the per-user
config directory accessor function. For older R versions, please set the
attribute directly when creating the `tiledb_array` object, or via the
[`return_as()`](https://tiledb-inc.github.io/TileDB-R/reference/return_as-tiledb_array-method.md)
method.
