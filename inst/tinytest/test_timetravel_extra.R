
library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")
isMacOS <- (Sys.info()['sysname'] == "Darwin")

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.3.0") exit_file("TileDB time travel tests require TileDB 2.3.* or greater")

## earlier time travel test recast via timestamp_{start,end}
## time travel

tmp <- tempfile()
dir.create(tmp)
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 10L), 5L, "INT32"),
                              tiledb_dim("cols", c(1L, 10L), 5L, "INT32")))
schema <- tiledb_array_schema(dom, attrs=c(tiledb_attr("a", type = "INT32")), sparse = TRUE)
invisible( tiledb_array_create(tmp, schema) )

I <- c(1, 2, 2)
J <- c(1, 4, 3)
data <- c(1L, 2L, 3L)
now1 <- Sys.time()
A <- tiledb_array(uri = tmp, timestamp_start=now1)
A[I, J] <- data

Sys.sleep(deltat)

I <- c(8, 6, 9)
J <- c(5, 7, 8)
data <- c(11L, 22L, 33L)
now2 <- Sys.time()
A <- tiledb_array(uri = tmp, timestamp_start=now2)
A[I, J] <- data

A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp_end=now1 - epst)
expect_equal(nrow(A[]), 0)
A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp_start=now1 + epst)
expect_equal(nrow(A[]), 3)
A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp_start=now1 - epst, timestamp_end=now2 - epst)
expect_equal(nrow(A[]), 3)
A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp_start=now1 - epst, timestamp_end=now2 + epst)
expect_true(nrow(A[]) >= 3)
