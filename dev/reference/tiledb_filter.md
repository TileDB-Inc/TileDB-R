# Constructs a `tiledb_filter` object

Available filters:

- "NONE"

- "GZIP"

- "ZSTD"

- "LZ4"

- "RLE"

- "BZIP2"

- "DOUBLE_DELTA"

- "BIT_WIDTH_REDUCTION"

- "BITSHUFFLE"

- "BYTESHUFFLE"

- "POSITIVE_DELTA"

- "CHECKSUM_MD5"

- "CHECKSUM_SHA256"

- "DICTIONARY"

- "SCALE_FLOAT" (TileDB 2.11.0 or later)

- "FILTER_XOR" (TileDB 2.12.0 or later)

## Usage

``` r
tiledb_filter(name = "NONE", ctx = tiledb_get_context())
```

## Arguments

- name:

  (default "NONE") TileDB filter name string

- ctx:

  tiledb_ctx object (optional)

## Value

tiledb_filter object

## Details

Valid compression options vary depending on the filter used, consult the
TileDB docs for more information.

## Examples

``` r
tiledb_filter("ZSTD")
#> tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",-1) 
```
