
library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

if (!requireNamespace("Matrix", quietly=TRUE)) exit_file("Need the 'Matrix' package")
library(Matrix)
if (packageVersion("Matrix") < "1.3.0") exit_file("Old 'Matrix' package?")

set.seed(123)                           # just to fix it
n <- 60
k <- 50
mat <- matrix(0, nrow=n, ncol=k)
nelem <- 0.1 * n * k
mat[sample(seq_len(n*k), nelem)] <- seq(1, nelem)

## Convert dense matrix to sparse matrix
spmat <- as(mat, "dgTMatrix")

uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromSparseMatrix(spmat, uri)

chk <- toSparseMatrix(uri)
expect_true(is(chk, "sparseMatrix"))
expect_true(inherits(chk, "dgTMatrix"))
expect_true(all.equal(spmat, chk))
expect_equivalent(spmat, chk)
