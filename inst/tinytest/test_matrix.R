
library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

uri <- tempfile()
M <- matrix(1:16, 4, 4, dimnames=list(LETTERS[1:4], letters[1:4]))
fromMatrix(M, uri)

M2 <- toMatrix(uri)
expect_equivalent(M, t(M2))             # because we now default to UNORDERED we need to transpose


uri <- tempfile()
M <- matrix(1:20, 2, 10)                # matrix without rownames
fromMatrix(M, uri)

M2 <- toMatrix(uri)
expect_equivalent(M, M2)


uri <- tempfile()
M <- matrix(sqrt(1:20), 5, 4)           # matrix without rownames, float values
fromMatrix(M, uri)

M3 <- toMatrix(uri)
expect_equivalent(M, M3)
