library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

if (!requireNamespace("Matrix", quietly=TRUE)) exit_file("Need the 'Matrix' package")
library(Matrix)
if (packageVersion("Matrix") < "1.3.0") exit_file("Old 'Matrix' package?")
if (tiledb_version(TRUE) < "2.7.0") exit_file("Needs TileDB 2.7.* or later")

set.seed(123)                           # just to fix it
n <- 60
k <- 50
mat <- matrix(0, nrow=n, ncol=k)
nelem <- 0.1 * n * k
mat[sample(seq_len(n*k), nelem)] <- seq(1, nelem)

## Convert dense matrix to sparse matrix
spmat <- as(mat, "TsparseMatrix")

uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromSparseMatrix(spmat, uri)
chk <- toSparseMatrix(uri)
expect_true(is(chk, "sparseMatrix"))
expect_true(inherits(chk, "dgTMatrix"))
expect_equivalent(spmat, chk)


set.seed(123)                           # just to fix it
n <- 25
k <- 15
mat <- matrix(0, nrow=n, ncol=k, dimnames=list(LETTERS[1:n], letters[1:k]))
nelem <- 0.2 * n * k
mat[sample(seq_len(n*k), nelem)] <- seq(1, nelem)
## Convert dense matrix to sparse matrix
spmat <- as(mat, "TsparseMatrix")
uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromSparseMatrix(spmat, uri)
chk <- toSparseMatrix(uri)
expect_true(is(chk, "sparseMatrix"))
expect_true(inherits(chk, "dgTMatrix"))
expect_equivalent(spmat, chk)
expect_equal(rownames(spmat), rownames(chk))
expect_equal(colnames(spmat), colnames(chk))
