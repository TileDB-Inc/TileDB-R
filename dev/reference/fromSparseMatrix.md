# Create (or return) a TileDB sparse array

The functions `fromSparseMatrix` and `toSparseMatrix` help in storing
(and retrieving) sparse matrices using a TileDB backend.

## Usage

``` r
fromSparseMatrix(
  obj,
  uri,
  cell_order = "ROW_MAJOR",
  tile_order = "ROW_MAJOR",
  filter = "ZSTD",
  capacity = 10000L
)

toSparseMatrix(uri)
```

## Arguments

- obj:

  A sparse matrix object.

- uri:

  A character variable with an Array URI.

- cell_order:

  A character variable with one of the TileDB cell order values, default
  is “COL_MAJOR”.

- tile_order:

  A character variable with one of the TileDB tile order values, default
  is “COL_MAJOR”.

- filter:

  A character variable vector, defaults to ‘ZSTD’, for one or more
  filters to be applied to each attribute;

- capacity:

  A integer value with the schema capacity, default is 10000.

## Value

Null, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("Matrix", quietly=TRUE)) {
  library(Matrix)
  set.seed(123)      # just to fix it
  mat <- matrix(0, nrow=20, ncol=10)
  mat[sample(seq_len(200), 20)] <- seq(1, 20)
  spmat <- as(mat, "dgTMatrix")  # sparse matrix in dgTMatrix format
  uri <- "sparse_matrix"
  fromSparseMatrix(spmat, uri)   # now written
  chk <- toSparseMatrix(uri)     # and re-read
  print(chk)
  all.equal(spmat, chk)
}
} # }
```
