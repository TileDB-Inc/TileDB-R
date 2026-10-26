# After consolidation, remove consolidated fragments of a TileDB Array

This function can remove fragments following a consolidation step. Note
that vacuuming should *not* be run if one intends to use the TileDB
*time-traveling* feature of opening arrays at particular timestamps.

## Usage

``` r
array_vacuum(uri, cfg = NULL, start_time, end_time, ctx = tiledb_get_context())
```

## Arguments

- uri:

  A character value with the URI of a TileDB Array

- cfg:

  An optional TileDB Configuration object

- start_time:

  An optional timestamp value, if missing config default is used

- end_time:

  An optional timestamp value, if missing config default is used

- ctx:

  An option TileDB Context object

## Value

NULL is returned invisibly

## Details

Parameters affecting the operation can be set via an optional
configuration object. Start and end timestamps can also be set directly.
