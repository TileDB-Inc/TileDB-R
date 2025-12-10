# Return a TileDB Array Metadata object given by key

Return a TileDB Array Metadata object given by key

## Usage

``` r
tiledb_get_metadata(arr, key)
```

## Arguments

- arr:

  A TileDB Array object

- key:

  A character value describing a metadata key

## Value

A object stored in the Metadata under the given key, or ‘NULL’ if none
found.
