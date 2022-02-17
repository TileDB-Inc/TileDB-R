library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)

#test_that("tiledb_attr constructor works", {
a1 <- tiledb_attr(type = "FLOAT64")
expect_true(is(a1, "tiledb_attr"))
#})

#test_that("tiledb_attr constructor defaults are correct", {
a1 <- tiledb_attr(type = "FLOAT64")
expect_equal(tiledb::name(a1), "")
expect_true(is.anonymous(a1))
expect_equal(tiledb::datatype(a1), "FLOAT64")
expect_equal(tiledb::cell_val_num(a1), 1)
#})

#test_that("tiledb_attr is.anonymous is correct", {
a1  <- tiledb_attr("", , type = "FLOAT64")
expect_true(is.anonymous(a1))
a2  <- tiledb_attr("foo", type = "FLOAT64")
expect_false(is.anonymous(a2))
#})

#test_that("tiledb_attr with compression", {
a1 <- tiledb_attr("foo", type = "FLOAT64", filter_list = tiledb_filter_list(c(tiledb_filter("GZIP"))))
filter_list <- tiledb::filter_list(a1)
expect_true(is(filter_list, "tiledb_filter_list"))
expect_equal(tiledb_filter_type(filter_list[0]), "GZIP")
expect_equal(tiledb_filter_get_option(filter_list[0], "COMPRESSION_LEVEL"), -1)

expect_error(tiledb_attr("foo", compressor = tiledb_compressor("UNKNOWN", -1)))
#})

#test_that("tiledb_attr throws an error with invalid ncells argument", {
a1 <- tiledb_attr("foo", type = "FLOAT64", ncells = 1)
expect_equal(tiledb::cell_val_num(a1), 1)
expect_error(tiledb_attr("foo", ncells = 0))
#})

#test_that("tiledb_attr set ncells", {
attrs <- tiledb_attr("a", type = "INT32", ncells = 1)
expect_equal(tiledb::cell_val_num(attrs), 1) # as created

tiledb:::libtiledb_attribute_set_cell_val_num(attrs@ptr, 2)
expect_equal(tiledb::cell_val_num(attrs), 2) # as created

tiledb:::libtiledb_attribute_set_cell_val_num(attrs@ptr, NA_integer_)
expect_true(is.na(tiledb::cell_val_num(attrs)))
#})

#test_that("tiledb_attr set fill", {
if (tiledb_version(TRUE) < "2.1.0") exit_file("Needs TileDB 2.1.* or later")
if (isOldWindows) exit_file("skip remainder of this file on old Windows releases")

## test for default
dom <- tiledb_domain(dims = tiledb_dim("rows", c(1L, 4L), 4L, "INT32"))
attr <- tiledb_attr("a", type = "INT32")
sch <- tiledb_array_schema(dom, attr)

uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
tiledb_array_create(uri, sch)

#arr <- tiledb_dense(uri)
#val <- arr[]
## when no value has been set, expect NA
##expect_equal(val, array(rep(NA, 4)))
#expect_true(length(val) == 4)
#expect_true(all(is.na(val)))

## test for value set
dom <- tiledb_domain(dims = tiledb_dim("rows", c(1L, 4L), 4L, "INT32"))
attr <- tiledb_attr("a", type = "INT32")
tiledb_attribute_set_fill_value(attr, 42L)
sch <- tiledb_array_schema(dom, attr)
uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
tiledb_array_create(uri, sch)
#arr <- tiledb_dense(uri)
#val <- arr[]
## when fill value has been set, expect value
#expect_equal(val, array(rep(42, 4)))
#expect_equal(tiledb_attribute_get_fill_value(attr), 42)

dom <- tiledb_domain(dims = tiledb_dim("rows", c(1L, 4L), 4L, "UINT32"))
attr <- tiledb_attr("a", type = "UINT32")
tiledb_attribute_set_fill_value(attr, 42L)
sch <- tiledb_array_schema(dom, attr)
uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
tiledb_array_create(uri, sch)
arr <- tiledb_array(uri, return_as="asis", extended=FALSE)
val <- arr[1:4][[1]]
## when fill value has been set, expect value
expect_equal(val, rep(42, 4))
expect_equal(tiledb_attribute_get_fill_value(attr), 42)

attr <- tiledb_attr("b", type = "CHAR", ncells = NA)
tiledb_attribute_set_fill_value(attr, "abc")
sch <- tiledb_array_schema(dom, attr)
uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
tiledb_array_create(uri, sch)
#arr <- tiledb_dense(uri)
#val <- arr[]
expect_equal(tiledb_attribute_get_fill_value(attr), "abc")

if (dir.exists(uri)) unlink(uri, recursive=TRUE)
#})


## datetimes test (cf ex_aggdatetimes)
suppressMessages({
  library(nanotime)
  library(bit64)
})
dimtype <- "INT32"
intmax <- .Machine$integer.max         # shorthand
uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)

tile <- 1000L
domain <- tiledb_domain(tiledb_dim("row", c(-intmax,intmax), tile, dimtype))
attrib <- c(tiledb_attr("year",  type = "DATETIME_YEAR"),  # year
            tiledb_attr("month", type = "DATETIME_MONTH"), # month
            tiledb_attr("week",  type = "DATETIME_WEEK"),  # week
            tiledb_attr("day",   type = "DATETIME_DAY"),   # date
            tiledb_attr("hr",    type = "DATETIME_HR"),    # hour
            tiledb_attr("min",   type = "DATETIME_MIN"),   # minute
            tiledb_attr("sec",   type = "DATETIME_SEC"),   # second
            tiledb_attr("ms",    type = "DATETIME_MS"),    # millisecond
            tiledb_attr("us",    type = "DATETIME_US"),    # microsecond
            tiledb_attr("ns",    type = "DATETIME_NS"),    # nanosecond
            tiledb_attr("ps",    type = "DATETIME_PS"),    # picosecond
            tiledb_attr("fs",    type = "DATETIME_FS"),    # femtosecond
            tiledb_attr("as",    type = "DATETIME_AS")     # attosecond
            )
schema <- tiledb_array_schema(domain, attrib, sparse=TRUE)
res <- tiledb_array_create(uri, schema)

arr <- tiledb_array(uri, as.data.frame=TRUE)

dvec <- 1:3
data <- data.frame(row = dvec,
                   year  = c(as.Date("2020-01-01"), as.Date("2021-01-01"), as.Date("2022-01-01")),
                   month = c(as.Date("2020-01-01"), as.Date("2020-02-01"), as.Date("2020-03-01")),
                   week  = c(as.Date("2020-01-01"), as.Date("2020-01-08"), as.Date("2020-01-15")),
                   day   = as.Date("2020-01-01") + 0:2,
                   hr    = as.POSIXct("2020-01-01 00:00:00") + (0:2)*3600,
                   min   = as.POSIXct("2020-01-01 00:00:00") + (0:2)*60,
                   sec   = as.POSIXct("2020-01-01 00:00:00") + (0:2),
                   ms    = as.POSIXct("2000-01-01 00:00:00") + (0:2)*3600 + rep(0.001,3),
                   us    = as.POSIXct("2000-01-01 00:00:00") + (0:2)*3600 + rep(0.000002,3),
                   ns    = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:2)*1e9,
                   ps    = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:2)*1e9,
                   fs    = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:2)*1e9,
                   as    = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:2)*1e9
                   )

arr[] <- data
arr2 <- tiledb_array(uri, as.data.frame=TRUE)
readdata <- arr2[]
expect_true(all.equal(data, readdata, check.attributes=FALSE))


if (tiledb_version(TRUE) < "2.2.0") exit_file("Needs TileDB 2.2.* or later")
attrib <- tiledb_attr("a",  type = "INT32")
tiledb_attribute_set_nullable(attrib, TRUE)
expect_true(tiledb_attribute_get_nullable(attrib))
tiledb_attribute_set_nullable(attrib, FALSE)
expect_false(tiledb_attribute_get_nullable(attrib))
expect_error(tiledb_attribute_set_nullable(attrib, 1L))
expect_error(tiledb_attribute_set_nullable(attrib, as.logical(NA)))

attrib <- tiledb_attr("a",  type = "FLOAT64", nullable=TRUE)
expect_true(tiledb_attribute_get_nullable(attrib))

attrib <- tiledb_attr("a",  type = "FLOAT64", nullable=FALSE)
expect_false(tiledb_attribute_get_nullable(attrib))


uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)

domain <- tiledb_domain(tiledb_dim("row", c(0L, 100L), 100L, "INT32"))
attrib <- c(tiledb_attr("int8",   type = "INT8",    nullable = TRUE),
            tiledb_attr("int16",  type = "INT16",   nullable = TRUE),
            tiledb_attr("int32",  type = "INT32",   nullable = TRUE),
            tiledb_attr("int64",  type = "INT64",   nullable = TRUE),
            tiledb_attr("float32",type = "FLOAT32", nullable = TRUE),
            tiledb_attr("float64",type = "FLOAT64", nullable = TRUE),
            tiledb_attr("uint8",  type = "UINT8",   nullable = TRUE),
            tiledb_attr("uint16", type = "UINT16",  nullable = TRUE),
            tiledb_attr("uint32", type = "UINT32",  nullable = TRUE),
            tiledb_attr("uint64", type = "UINT64",  nullable = TRUE))

schema <- tiledb_array_schema(domain, attrib, sparse=TRUE)
res <- tiledb_array_create(uri, schema)


df <- data.frame(row     =  1:10,
                 int8    =  10L*c(1:2, NA, 4:10),
                 int16   =  20L*c(1:3, NA, 5:10),
                 int32   =  30L*c(1:4, NA, 6:10),
                 int64   =  as.integer64(40L*c(1:5, NA, 7:10)),
                 float32 =  50*c(1:6, NA, 8:10),
                 float64 =  60*c(1:7, NA, 9:10),
                 uint8   =  10*c(1:8, NA, 10),
                 uint16  =  80*c(1:9, NA),
                 uint32  =  90*c(1:8, NA, 10),
                 uint64  = as.integer64(100*c(1:7, NA, 9:10)))

arr <- tiledb_array(uri)
arr[] <- df

newarr <- tiledb_array(uri, as.data.frame=TRUE)
chk <- newarr[]
expect_equal(df[,1:10], chk[,1:10])
expect_equivalent(as.numeric(df[,11]), chk[,11]) # we currently return uint64_t as numeric
