library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.2.0") exit_file("TileDB ArrowIO types required TileDB 2.2.* or greater")

if (!requireNamespace("arrow", quietly=TRUE)) exit_file("No 'arrow' package.")
suppressMessages(library(arrow))

if (get_return_as_preference() != "asis") set_return_as_preference("asis") 		# baseline value

## -- RecordBatch test (and TileDB does not yet export to / import from these)
df <- data.frame(one = c(-1, NA, 2.5),
                 two = c("foo", "bar", "baz"),
                 three = c(TRUE, FALSE, TRUE))
batch <- record_batch(df)
expect_true(is(batch, "RecordBatch"))
expect_true(is(as.data.frame(batch), "data.frame"))


## allocate two structures (and release at end)
aa <- tiledb_arrow_array_ptr()
as <- tiledb_arrow_schema_ptr()
arrow:::ExportRecordBatch(batch, aa, as)

newrb <- arrow:::ImportRecordBatch(aa, as)
expect_true(is(newrb, "RecordBatch"))
expect_true(is(as.data.frame(newrb), "data.frame"))
expect_equal(batch, newrb)

tiledb_arrow_schema_del(as)
tiledb_arrow_array_del(aa)


## round-turn test 1: write tiledb first, create arrow object via zero-copy
suppressMessages(library(bit64))
n <- 10L
dir.create(tmp <- tempfile())
dim <- tiledb_dim("rows", domain=c(1L,n), type="INT32", tile=1L)
dom <- tiledb_domain(dim)
sch <- tiledb_array_schema(dom,
                           attrs = c(tiledb_attr("int8",   type="INT8"),
                                     tiledb_attr("uint8",  type="UINT8"),
                                     tiledb_attr("int16",  type="INT16"),
                                     tiledb_attr("uint16", type="UINT16"),
                                     tiledb_attr("int32",  type="INT32"),
                                     tiledb_attr("uint32", type="UINT32"),
                                     tiledb_attr("int64",  type="INT64"),
                                     tiledb_attr("uint64", type="UINT64"),
                                     tiledb_attr("float64",type="FLOAT64")),
                           sparse = TRUE)
tiledb_array_create(tmp, sch)

arr <- tiledb_array(tmp)
qry <- tiledb_query(arr, "WRITE")
dimptr <- tiledb_query_create_buffer_ptr(qry, "INT32", seq(1,n))
qry <- tiledb_query_set_buffer_ptr(qry, "rows", dimptr)
attrlst <- list()
for (col in c("int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "uint64", "float64")) {
    if (grepl("int64", col)) {
        vals <- as.integer64(seq(1,n))
    } else {
        vals <- seq(1,n)
    }
    attrlst[[col]] <- tiledb_query_create_buffer_ptr(qry, toupper(col), vals)
    qry <- tiledb_query_set_buffer_ptr(qry, col, attrlst[[col]])
}
tiledb_query_set_layout(qry, "UNORDERED")
tiledb_query_submit(qry)
tiledb_query_finalize(qry)

#arr <- tiledb_array(tmp, return_as="data.frame")
#print(arr[])


arr <- tiledb_array(tmp)
qry <- tiledb_query(arr, "READ")
dimptr <- tiledb_query_buffer_alloc_ptr(qry, "INT32", n)
qry <- tiledb_query_set_buffer_ptr(qry, "rows", dimptr)
tiledb_query_set_subarray(qry, c(4L,7L), "INT32")

attrlst <- list()
for (col in c("int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "uint64", "float64")) {
    attrlst[[col]] <- tiledb_query_buffer_alloc_ptr(qry, toupper(col), 4L)
    qry <- tiledb_query_set_buffer_ptr(qry, col, attrlst[[col]])
}
tiledb_query_submit(qry)
tiledb_query_finalize(qry)

res <- tiledb_query_export_buffer(qry, "rows")
v <- Array$create(arrow:::ImportArray(res[[1]], res[[2]]))
tiledb_arrow_array_del(res[[1]])
tiledb_arrow_schema_del(res[[2]])

expect_equal(v$as_vector(), 4:7)

for (col in c("int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "uint64", "float64")) {
    qry <- tiledb_query_set_buffer_ptr(qry, col, attrlst[[col]])
    res <- tiledb_query_export_buffer(qry, col)
    v <- Array$create(arrow:::ImportArray(res[[1]], res[[2]]))
    tiledb_arrow_array_del(res[[1]])
    tiledb_arrow_schema_del(res[[2]])

    expect_equal(v$as_vector(), 4:7)
}


## round-turn test 2: create arrow object, write tiledb second via zero-copy
dir.create(tmp <- tempfile())
n <- 10L

## create a schema but don't fill it yet
dim <- tiledb_dim("rows", domain=c(1L,n), type="INT32", tile=1L)
dom <- tiledb_domain(dim)
sch <- tiledb_array_schema(dom,
                           attrs = c(tiledb_attr("int8",   type="INT8"),
                                     tiledb_attr("uint8",  type="UINT8"),
                                     tiledb_attr("int16",  type="INT16"),
                                     tiledb_attr("uint16", type="UINT16"),
                                     tiledb_attr("int32",  type="INT32"),
                                     tiledb_attr("uint32", type="UINT32"),
                                     tiledb_attr("int64",  type="INT64"),
                                     tiledb_attr("uint64", type="UINT64"),
                                     tiledb_attr("float64",type="FLOAT64")),
                           sparse = TRUE)
tiledb_array_create(tmp, sch)

## create an arrow 'record batch' with a number of (correcsponding) columns
rb <- record_batch("rows"   = Array$create(1:n, int32()),
                   "int8"   = Array$create(1:n, int8()),
                   "uint8"  = Array$create(1:n, uint8()),
                   "int16"  = Array$create(1:n, int16()),
                   "uint16" = Array$create(1:n, uint16()),
                   "int32"  = Array$create(1:n, int32()),
                   "uint32" = Array$create(1:n, uint32()),
                   "int64"  = Array$create(1:n, int64()),
                   "uint64" = Array$create(1:n, uint64()),
                   "float64"= Array$create(1:n, float64()) )
#print(rb)
#print(as.data.frame(rb))

arr <- tiledb_array(tmp)
qry <- tiledb_query(arr, "WRITE")

nms <- rb$names()
lst <- list()
for (nam in nms) {
    vec <- rb[[nam]]                    # can access by name
    aa <- tiledb_arrow_array_ptr()
    as <- tiledb_arrow_schema_ptr()
    arrow:::ExportArray(vec, aa, as)

    qry <- tiledb_query_import_buffer(qry, nam, list(aa, as))

    lst[[nam]] <- list(aa=aa, as=as)
}
tiledb_query_set_layout(qry, "UNORDERED")
tiledb_query_submit(qry)
tiledb_query_finalize(qry)

arr <- tiledb_array(tmp, return_as="data.frame")
df <- arr[]

for (i in 1:10) {
  l <- lst[[i]]
  tiledb_arrow_array_del(l[[1]])
  tiledb_arrow_schema_del(l[[2]])
}

expect_true(is(df, "data.frame"))
expect_equal(dim(df), c(n, 10))
for (i in c(1:7,9:10)) {
    expect_equivalent(df[,i], 1:10)
}
expect_equivalent(df[,8], as.integer64(1:10))


## test support for return_as="arrow"
if (!requireNamespace("palmerpenguins", quietly=TRUE)) exit_file("remainder needs 'palmerpenguins'")
library(palmerpenguins)
uri <- tempfile()
fromDataFrame(penguins, uri, col_index = c("species", "year"))
for (arg in c("arrow", "arrow_table")) {
    res <- tiledb_array(uri, return_as=arg)[]
    expect_true(inherits(res, "Table"))
    expect_true(inherits(res, "ArrowTabular"))
    expect_true(inherits(res, "ArrowObject"))
    expect_equal(res$num_rows, 344)
    expect_equal(res$num_columns, 8)
}

## test support for return as Date (GH Issue 533)
uri <- tempfile()
D <- data.frame(val = 100 + 0:4,
                dat = Sys.Date() + seq(-4,0))
fromDataFrame(D, uri, col_index = 1)
at <- tiledb_array(uri, return_as = "arrow")[]
expect_true(inherits(at, "Table"))
chk <- data.frame(at)
expect_equal(D, chk)

## detaching arrow should not be necessary as we generally do not need to unload
## packages but had been seen as beneficial in some instanced so there is now an option
if (Sys.getenv("R_TESTING_CLEANUP", "") == "yes") detach(package:arrow, unload=TRUE)
