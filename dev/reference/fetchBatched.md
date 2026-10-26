# Run a ‘batched’ query

Batched queries return an initial result set even when it is incomplete.
Where the normal retrieval process will loop in place to complete a
(potentially large) result set, this function will return a result
(which may be part of a larger result set) allowing the user to assemble
all part.

## Usage

``` r
fetchBatched(x, obj)
```

## Arguments

- x:

  A `tiledb_array` object

- obj:

  A `batchedquery` object as returned by `createBatched`

## Value

A data.frame object with the (potentially partial) result of a batched
query

## Details

The `tiledb_array` object can be parameterised as usual.
