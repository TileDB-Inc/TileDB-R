# Submit TileDB Query asynchronously without a callback returning immediately

Note that the query object may need to be finalized via
`tiledb_query_finalize`.

## Usage

``` r
tiledb_query_submit_async(query)
```

## Arguments

- query:

  A TileDB Query object

## Value

The modified query object, invisibly
