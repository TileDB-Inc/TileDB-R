# Set a range for a given query, also supplying type

Set a range for a given query, also supplying type

## Usage

``` r
tiledb_query_add_range_with_type(
  query,
  idx,
  datatype,
  lowval,
  highval,
  stride = NULL
)
```

## Arguments

- query:

  A TileDB Query object

- idx:

  An integer index, zero based, of the dimensions

- datatype:

  A character value containing the data type

- lowval:

  The lower value of the range to be set

- highval:

  The highre value of the range to be set

- stride:

  An optional stride value for the range to be set

## Value

The query object, invisibly
