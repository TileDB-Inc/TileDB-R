library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.2.0") exit_file("TileDB ArrowIO types required TileDB 2.2.* or greater")

if (!requireNamespace("arrow", quietly=TRUE)) exit_file("No 'arrow' package.")
suppressMessages(library(arrow))


## -- RecordBatch test (and TileDB does not yet export to / import from these)
df <- data.frame(one = c(-1, NA, 2.5),
                 two = c("foo", "bar", "baz"),
                 three = c(TRUE, FALSE, TRUE))
batch <- record_batch(df)
expect_true(is(batch, "RecordBatch"))
expect_true(is(as.data.frame(batch), "data.frame"))


## allocate two structures (and release at end)
aa <- tiledb:::.allocate_arrow_array_as_double()
as <- tiledb:::.allocate_arrow_schema_as_double()
arrow:::ExportRecordBatch(batch,aa,as)

newrb <- arrow:::ImportRecordBatch(aa, as)
expect_true(is(newrb, "RecordBatch"))
expect_true(is(as.data.frame(newrb), "data.frame"))
expect_equal(batch, newrb)

tiledb:::.delete_arrow_schema_from_double(as)
tiledb:::.delete_arrow_array_from_double(aa)


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
                           sparse = FALSE)
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

#arr <- tiledb_array(tmp, as.data.frame=TRUE)
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
v <- Array$create(arrow:::ImportArray(res[1], res[2]))
tiledb:::.delete_arrow_array_from_double(res[1])
tiledb:::.delete_arrow_schema_from_double(res[2])
expect_equal(v$as_vector(), 4:7)

for (col in c("int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "uint64", "float64")) {
    qry <- tiledb_query_set_buffer_ptr(qry, col, attrlst[[col]])
    res <- tiledb_query_export_buffer(qry, col)
    v <- Array$create(arrow:::ImportArray(res[1], res[2]))
    tiledb:::.delete_arrow_array_from_double(res[1])
    tiledb:::.delete_arrow_schema_from_double(res[2])

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
                           sparse = FALSE)
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
    aa <- tiledb:::.allocate_arrow_array_as_double()
    as <- tiledb:::.allocate_arrow_schema_as_double()
    arrow:::ExportArray(vec, aa, as)

    qry <- tiledb_query_import_buffer(qry, nam, c(aa, as))

    lst[[nam]] <- list(aa=aa, as=as)
}
tiledb_query_set_layout(qry, "UNORDERED")
tiledb_query_submit(qry)
tiledb_query_finalize(qry)

arr <- tiledb_array(tmp, as.data.frame=TRUE)
df <- arr[]

for (i in 1:10) {
  l <- lst[[i]]
  tiledb:::.delete_arrow_array_from_double(l[[1]])
  tiledb:::.delete_arrow_schema_from_double(l[[2]])
}

expect_true(is(df, "data.frame"))
expect_equal(dim(df), c(n, 10))
for (i in 1:10)
  expect_equal(df[,1], 1:10)
