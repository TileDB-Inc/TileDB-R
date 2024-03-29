
library(tinytest)
library(tiledb)

isMacOS <- (Sys.info()['sysname'] == "Darwin")

ctx <- tiledb_ctx(limitTileDBCores())

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

deltat <- 0.1
epst <- deltat/2
Sys.sleep(deltat)

I <- c(8, 6, 9)
J <- c(5, 7, 8)
data <- c(11L, 22L, 33L)
now2 <- Sys.time()
A <- tiledb_array(uri = tmp, timestamp_start=now2)
A[I, J] <- data

A <- tiledb_array(uri = tmp, return_as="data.frame", timestamp_end=now1 - epst)
expect_equal(nrow(A[]), 0)
A <- tiledb_array(uri = tmp, return_as="data.frame", timestamp_start=now1 + epst)
expect_equal(nrow(A[]), 3)
A <- tiledb_array(uri = tmp, return_as="data.frame", timestamp_start=now1 - epst, timestamp_end=now2 - epst)
expect_equal(nrow(A[]), 3)
A <- tiledb_array(uri = tmp, return_as="data.frame", timestamp_start=now1 - epst, timestamp_end=now2 + epst)
expect_true(nrow(A[]) >= 3)
