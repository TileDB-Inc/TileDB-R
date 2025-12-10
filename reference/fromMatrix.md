# Create a TileDB array from an R matrix, or return an R matrix

The functions `fromMatrix` and `toMatrix` help in storing (and
retrieving) matrices using a TileDB backend. In particular they help for
matrices with explicit rownames.

## Usage

``` r
fromMatrix(obj, uri, filter = "ZSTD", capacity = 10000L)

toMatrix(uri)
```

## Arguments

- obj:

  A sparse matrix object.

- uri:

  A character variable with an Array URI.

- filter:

  A character variable vector, defaults to ‘ZSTD’, for one or more
  filters to be applied to each attribute;

- capacity:

  A integer value with the schema capacity, default is 10000.

## Value

Null, invisibly.
