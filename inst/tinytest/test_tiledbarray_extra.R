library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")
isMacOS <- (Sys.info()['sysname'] == "Darwin")
if (tiledb_version(TRUE) < "2.7.0") exit_file("Needs TileDB 2.7.* or later")

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



## delete fragment (2.12.0 or later)
if (tiledb_version(TRUE) < "2.12.0") exit_file("Remainder needs 2.12.0 or later")
N <- 5
ts <- rep(Sys.time(), N)
tmp <- tempfile()
dir.create(tmp)
uri <- file.path(tmp, "array")
D <- data.frame(index = paste0("A", format(trunc(runif(10)*1000))), value = cumsum(runif(10)))
fromDataFrame(D, uri, col_index=1, sparse=TRUE)
ts[1] <- Sys.time()
for (i in 2:N) {
    Sys.sleep(0.25)
    D <- data.frame(index = paste0(LETTERS[i], format(trunc(runif(10)*1000))), value = cumsum(runif(10)))
    fromDataFrame(D, uri, col_index=1, mode="append", sparse=TRUE)
    ts[i] <- Sys.time()
}

fraginfo <- tiledb_fragment_info(uri)
expect_equal(tiledb_fragment_info_get_num(fraginfo), N) # N (ie 5) before deletion

arr <- tiledb_array(uri)
arr <- tiledb_array_open(arr, "MODIFY_EXCLUSIVE")
expect_true(tiledb_array_is_open(arr))
expect_true(is(arr, "tiledb_array"))
expect_true(tiledb_array_delete_fragments(arr, ts[2]-0.1, ts[4]+0.1))
arr <- tiledb_array_close(arr)
fraginfo <- tiledb_fragment_info(uri)
expect_equal(tiledb_fragment_info_get_num(fraginfo), 2) # 2 after three deleted

unlink(tmp, recursive = TRUE)
