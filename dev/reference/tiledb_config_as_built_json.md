# Return the 'AsBuilt' JSON string

Return the 'AsBuilt' JSON string

## Usage

``` r
tiledb_config_as_built_json()
```

## Value

The JSON string containing 'AsBuilt' information

## Examples

``` r
if (tiledb_version(TRUE) > "2.17") {
  txt <- tiledb::tiledb_config_as_built_json()
}
## now eg either one of
##   sapply(jsonlite::fromJSON(txt)$as_built$parameters$storage_backends, \(x) x[[1]])
##   sapply(RcppSimdJson::fparse(txt)$as_built$parameters$storage_backends, \(x) x[[1]])
## will return a named vector such as
##   c(azure = FALSE, gcs = FALSE, hdfs = FALSE, s3 = TRUE)
```
