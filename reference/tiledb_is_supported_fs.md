# Query if a TileDB backend is supported

The scheme corresponds to the URI scheme for TileDB resouces.

## Usage

``` r
tiledb_is_supported_fs(scheme, object = tiledb_get_context())
```

## Arguments

- scheme:

  URI string scheme ("file", "hdfs", "s3")

- object:

  `tiledb_ctx` object

## Value

TRUE if tiledb backend is supported, FALSE otherwise

## Details

Ex:

- `{file}:///path/to/file`

- `{hdfs}:///path/to/file`

- `{s3}://hostname:port/path/to/file`

## Examples

``` r
tiledb_is_supported_fs("file")
#> [1] TRUE
tiledb_is_supported_fs("s3")
#> [1] TRUE
```
