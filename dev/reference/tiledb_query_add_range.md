# Set a range for a given query

Set a range for a given query

## Usage

``` r
tiledb_query_add_range(query, schema, attr, lowval, highval, stride = NULL)
```

## Arguments

- query:

  A TileDB Query object

- schema:

  A TileDB Schema object

- attr:

  An character variable with a dimension name for which the range is set

- lowval:

  The lower value of the range to be set

- highval:

  The higher value of the range to be set

- stride:

  An optional stride value for the range to be set

## Value

The query object, invisibly
