# Return ‘batched’ status

Batched queries return an initial result set even when it is incomplete.
Where the normal retrieval process will loop in place to complete a
(potentially large) result set, this function will return a result
(which may be part of a larger result set) allowing the user to assemble
all part.

## Usage

``` r
statusBatched(obj)
```

## Arguments

- obj:

  A list object as returned by `createBatched`

## Value

The Query status as a character variable
