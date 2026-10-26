# Create a ‘batched’ query object

Batched queries return an initial result set even when it is incomplete.
Where the normal retrieval process will loop in place to complete a
(potentially large) result set, this function will return a result
(which may be part of a larger result set) allowing the user to assemble
all part.

## Usage

``` r
createBatched(x)
```

## Arguments

- x:

  A `tiledb_array` object

## Value

A `batchedquery` object, that is a list containing an external pointer
to a TileDB Query object along with other support variables used by
`fetchBatched`

## Details

The `tiledb_array` object can be parameterised as usual.
