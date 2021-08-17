library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

#test_that("tiledb_dim default constructor", {
dim <- tiledb_dim("foo", c(1, 100))
expect_true(is(dim, "tiledb_dim"))
#})

#test_that("tiledb_dim throws an error on missing constructor argument", {
expect_error(tiledb_dim("foo"))
#})

#test_that("tiledb_dim throws an error on invalid domain", {
expect_error(tiledb_dim("foo", c(100L, 1L), type = "INT32"))
#})

#test_that("tiledb_dim throws an error on invalid type", {
expect_error(tiledb_dim("foo", c(1, 100), type = "INVALID"))
#})

#test_that("tiledb_dim default type is double", {
dim <- tiledb_dim("foo", c(1, 100))
expect_equal(tiledb::datatype(dim), "FLOAT64")
#})

#test_that("tiledb_dim default type is the domain type", {
dim <- tiledb_dim("foo", c(1.0, 100.0))
expect_equal(tiledb::datatype(dim), "FLOAT64")

dim <- tiledb_dim("foo", c(1L, 100L))
expect_equal(tiledb::datatype(dim), "INT32")
#})

#test_that("tiledb_dim name", {
dim <- tiledb_dim("foo", c(1L, 100L))
expect_equal(tiledb::name(dim), "foo")

dim <- tiledb_dim("", c(1L, 100L))
expect_equal(tiledb::name(dim), "")
#})

#test_that("tiledb_dim tile should equal constructor", {
dim <- tiledb_dim("foo", c(1L, 100L), tile=10L, type="INT32")
expect_equal(tiledb::tile(dim), 10L)
#})

#test_that("tiledb_dim default tile extent should span the whole domain", {

dim <- tiledb_dim("foo", c(1L, 100L), type = "INT32")
expect_equal(tiledb::tile(dim), 100L)

dim <- tiledb_dim("foo", c(1L, 1L), type = "INT32")
expect_equal(tiledb::tile(dim), 1L)

dim <- tiledb_dim("foo", c(1.1, 11.9), type = "FLOAT64")
expect_equal(tiledb::tile(dim), 11.9 - 1.1)
#})

#test_that("tiledb_dim empty name is anonymous", {

dim <- tiledb_dim("", c(1L, 100L))
expect_true(is.anonymous(dim))

dim <- tiledb_dim("foo", c(1L, 100L))
expect_false(is.anonymous(dim))
#})

#test_that("tiledb_dim tiledb::datatype()", {
dim <- tiledb_dim("", c(1L, 100L), type = "INT32")
expect_equal(tiledb::datatype(dim), "INT32")

dim <- tiledb_dim("", c(1, 100), type = "FLOAT64")
expect_equal(tiledb::datatype(dim), "FLOAT64")
#})

t#est_that("tiledb_dim dim() method", {
d <- tiledb_dim("", c(-1L, 100L))
expect_equal(dim(d), 102L)
#})

if (tiledb_version(TRUE) < "2.1.0") exit_file("Needs TileDB 2.1.* or later")

## test permissible types for dimension objects -- cf inst/examples/ex_dimensions.R
## quick check of various dimension types
suppressMessages({
  library(nanotime)
  library(bit64)
})
atttype <- "INT32"
intmax <- .Machine$integer.max         # shorthand
uri <- tempfile()
dimtypes <- c("ASCII",  		# Variable length string
              "INT8",   		# 8-bit integer
              "UINT8",  		# 8-bit unsigned integer
              "INT16",  		# 16-bit integer
              "UINT16", 		# 16-bit unsigned integer
              "INT32",  		# 32-bit integer
              "UINT32", 		# 32-bit unsigned integer
              "INT64",  		# 64-bit integer
              "UINT64", 		# 64-bit unsigned integer
              "FLOAT32",		# 32-bit floating point
              "FLOAT64",		# 64-bit floating point
              "DATETIME_YEAR",  # year
              "DATETIME_MONTH", # month
              "DATETIME_WEEK",  # week
              "DATETIME_DAY",   # date
              "DATETIME_HR",    # hour
              "DATETIME_MIN",   # minute
              "DATETIME_SEC",   # second
              "DATETIME_MS",    # millisecond
              "DATETIME_US",    # microsecond
              "DATETIME_NS",    # nanosecond
              "DATETIME_PS",    # picosecond
              "DATETIME_FS",    # femtosecond
              "DATETIME_AS"     # attosecond
              )
for (dtype in dimtypes) {
    if (tiledb_vfs_is_dir(uri)) {
        tiledb_vfs_remove_dir(uri)
    }
    dom <- switch(dtype,
                  "ASCII"          = NULL,
                  "INT8"           =,
                  "UINT8"          = c(1L, 100L),
                  "INT16"          =,
                  "UINT16"         =,
                  "UINT32"         =,
                  "INT32"          = c(1L, 10000L),
                  "INT64"          =,
                  "UINT64"         = c(as.integer64(1), as.integer64(1000)),
                  "FLOAT32"        =,
                  "FLOAT64"        = c(1, 1000),
                  "DATETIME_YEAR"  =, #c(as.Date("2000-01-01"), as.Date("2030-12-31")),
                  "DATETIME_MONTH" =, #c(as.Date("2000-01-01"), as.Date("2030-12-31")),
                  "DATETIME_WEEK"  =, #c(as.Date("2000-01-01"), as.Date("2030-12-31")),
                  "DATETIME_DAY"   = c(-intmax, intmax),#c(as.Date("2000-01-01"), as.Date("2030-12-31")),
                  "DATETIME_HR"    =,
                  "DATETIME_MIN"   =,
                  "DATETIME_SEC"   =,
                  "DATETIME_MS"    =,
                  "DATETIME_US"    =,
                  "DATETIME_NS"    =,
                  "DATETIME_PS"    =,
                  "DATETIME_FS"    =,
                  "DATETIME_AS"    = c(-5e18, 5e18)
                  )

    tile <- switch(dtype,
                   "ASCII" = NULL,
                   "UINT8" = ,
                   "INT8"  = 100L,
                   "INT32" = ,
                   "UINT32" = 1000L,
                   "UINT64" =,
                   "INT64" = as.integer64(1000),
                   1000)                    # default is 1000

    domain <- tiledb_domain(tiledb_dim("row", dom, tile, dtype))
    attrib <- tiledb_attr("attr", type = "INT32")
    schema <- tiledb_array_schema(domain, attrib, sparse=TRUE)
    tiledb_array_create(uri, schema)

    arr <- tiledb_array(uri, as.data.frame=TRUE)
    dvec <- switch(dtype,
                   "ASCII"   = LETTERS[1:5],
                   "INT8"    =,
                   "UINT8"   =,
                   "INT16"   =,
                   "UINT16"  =,
                   "UINT32"  =,
                   "INT32"   = 1:5,      # sequences are integers
                   "INT64"   =,
                   "UINT64"  = as.integer64(1:5),
                   "FLOAT32" =,
                   "FLOAT64" = as.numeric(1:5),
                   "DATETIME_YEAR" = c(as.Date("2020-01-01"), as.Date("2021-01-01"), as.Date("2022-01-01"), as.Date("2023-01-01"), as.Date("2024-01-01")),
                   "DATETIME_MONTH" = c(as.Date("2020-01-01"), as.Date("2020-02-01"), as.Date("2020-03-01"), as.Date("2020-04-01"), as.Date("2020-05-01")),
                   "DATETIME_WEEK" = c(as.Date("2020-01-01"), as.Date("2020-01-08"), as.Date("2020-01-15"), as.Date("2020-01-22"), as.Date("2020-01-29")),
                   "DATETIME_DAY" = as.Date("2020-01-01") + 0:4,
                   "DATETIME_HR"  = as.POSIXct("2020-01-01 00:00:00") + (0:4)*3600,
                   "DATETIME_MIN" = as.POSIXct("2020-01-01 00:00:00") + (0:4)*3600,
                   "DATETIME_SEC" = as.POSIXct("2020-01-01 00:00:00") + (0:4)*3600,
                   "DATETIME_MS"  = as.POSIXct("2000-01-01 00:00:00") + (0:4)*3600 + rep(0.001,5),
                   ## POSIXct can do a bit less than 1 microsec so we drop one level
                   "DATETIME_US"  = as.POSIXct("2000-01-01 00:00:00") + (0:4)*3600 + rep(0.00001,5),
                   "DATETIME_NS"  =,
                   "DATETIME_PS"  =,
                   "DATETIME_FS"  =,
                   "DATETIME_AS"  = as.nanotime("1970-01-01T00:00:00.000000001+00:00") + (0:4)*1e9
                   )
    avec <- 10^(1:5)
    data <- data.frame(row = dvec, attr = avec, stringsAsFactors=FALSE)
    arr[] <- data

    arr2 <- tiledb_array(uri, as.data.frame=TRUE)
    readdata <- arr2[]
    if (dtype == "ASCII" && getRversion() < '4.0.0') readdata$row <- as.character(readdata$row)
    if (dtype == "UINT64") readdata[,1] <- as.integer64(readdata[,1])  # return doubles here
    expect_equivalent(data, readdata)
    if (grepl("^DATETIME", dtype)) {
        ## check for default date(time) type
        expect_false(class(readdata) == "integer64")
        expect_false(datetimes_as_int64(arr2))

        ## set it to TRUE, and test again
        datetimes_as_int64(arr2) <- TRUE
        expect_true(datetimes_as_int64(arr2))
        expect_true(class(arr2[][,"row"]) == "integer64")
    }

    ## subset tests
    arr3 <- tiledb_array(uri, as.data.frame=TRUE)
    if (dtype %in% c("DATETIME_YEAR", "DATETIME_MONTH", "DATETIME_WEEK", "DATETIME_DAY")) {
        scaleDate <- function(val, dtype) {
            val <- switch(dtype,
                          "DATETIME_YEAR" = as.numeric(strftime(val, "%Y")) - 1970,
                          "DATETIME_MONTH" = 12*(as.numeric(strftime(val, "%Y")) - 1970) + as.numeric(strftime(val, "%m")) - 1,
                          "DATETIME_WEEK" = as.numeric(val)/7,
                          "DATETIME_DAY" = as.numeric(val))
        }
        selected_ranges(arr3) <- list(cbind(as.integer64(scaleDate(data[2, "row"], dtype)),
                                            as.integer64(scaleDate(data[4, "row"], dtype))))
    } else if (dtype %in% c("DATETIME_HR", "DATETIME_MIN", "DATETIME_SEC",
                            "DATETIME_MS", "DATETIME_US")) {
        scaleDatetime <- function(val, dtype) {
            val <- switch(dtype,
                          "DATETIME_HR" = as.numeric(val)/3600,
                          "DATETIME_MIN" = as.numeric(val)/60,
                          "DATETIME_SEC" = as.numeric(val),
                          "DATETIME_MS" = as.numeric(val) * 1e3,
                          "DATETIME_US" = as.numeric(val) * 1e6
                          )
        }
        selected_ranges(arr3) <- list(cbind(as.integer64(scaleDatetime(data[2, "row"], dtype)),
                                            as.integer64(scaleDatetime(data[4, "row"], dtype))))
    } else if (dtype %in% c("DATETIME_NS", "DATETIME_PS", "DATETIME_FS", "DATETIME_AS")) {
        scaleDatetime <- function(val, dtype) {
            val <- switch(dtype,
                          "DATETIME_NS" = as.integer64(val),
                          "DATETIME_PS" = as.integer64(val) * 1e3,
                          "DATETIME_FS" = as.integer64(val) * 1e6,
                          "DATETIME_AS" = as.integer64(val) * 1e9
                          )
        }
        selected_ranges(arr3) <- list(cbind(as.integer64(scaleDatetime(data[2, "row"], dtype)),
                                            as.integer64(scaleDatetime(data[4, "row"], dtype))))
    } else {
        selected_ranges(arr3) <- list(cbind(data[2, "row"], data[4, "row"]))
    }
    readdata <- arr3[]
    if (dtype == "ASCII" && getRversion() < '4.0.0') readdata$row <- as.character(readdata$row)
    if (dtype == "UINT64") readdata[,1] <- as.integer64(readdata[,1])  # return doubles here
    expect_equivalent(data[2:4,], readdata, info=dtype) # equivalent as not type consistent (int <-> numeric)
    expect_equal(NROW(readdata), 3L)
}
