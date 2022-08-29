library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")
isMacOS <- (Sys.info()['sysname'] == "Darwin")

ctx <- tiledb_ctx(limitTileDBCores())

hasDataTable <- requireNamespace("data.table", quietly=TRUE)
hasTibble <- requireNamespace("tibble", quietly=TRUE)

## GitHub Actions had some jobs killed on the larger data portion so we dial mem use down
if (Sys.getenv("CI") != "") set_allocation_size_preference(1024*1024*5)



## this test tickles a valgrind issue 'Conditional jump or move depends on uninitialized value'
## test encrypted arrays via high-level accessor
## (lower-level tests in test_densearray and test_arrayschema)
tmp <- tempfile()
dir.create(tmp)
encryption_key <- "0123456789abcdeF0123456789abcdeF"

## create 4x4 with single attribute
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))
schema <- tiledb_array_schema(dom, attrs=c(tiledb_attr("a", type = "INT32")), sparse = TRUE)
invisible( tiledb_array_create(tmp, schema, encryption_key) )

## write
I <- c(1, 2, 2)
J <- c(1, 4, 3)
data <- c(1L, 2L, 3L)
A <- tiledb_array(uri = tmp, encryption_key = encryption_key)
A[I, J] <- data

## read
A <- tiledb_array(uri = tmp, as.data.frame=TRUE, encryption_key = encryption_key)
chk <- A[1:2, 2:4]
expect_equal(nrow(chk), 2)
expect_equal(chk[,"rows"], c(2L,2L))
expect_equal(chk[,"cols"], c(3L,4L))
expect_equal(chk[,"a"], c(3L,2L))

unlink(tmp, recursive = TRUE)
