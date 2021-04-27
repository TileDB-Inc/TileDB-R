library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.0.0") exit_file("TileDB Array types required TileDB 2.0.* or greater")

#test_that("test tiledb_array read/write sparse array with heterogenous date domains", {
op <- options()
options(stringsAsFactors=FALSE)       # accomodate R 3.*
dir.create(tmp <- tempfile())

d1  <- tiledb_dim("d1",
                  domain = c(as.Date("2001-01-02"), as.Date("2099-12-31")), tile=1L,
                  type="DATETIME_DAY")
d2  <- tiledb_dim("d2", domain = NULL, tile = NULL, type="ASCII")
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr("val", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(val), sparse=TRUE)
tiledb_array_create(tmp, sch)

N <- 10
dat <- matrix(rnorm(N), N, 1)
arr <- tiledb_array(tmp, is.sparse=TRUE, as.data.frame=TRUE)
I <- as.Date("2020-01-01") + seq_len(N)
J <- sample(c("ABC","DEF","GHI"), N, replace=TRUE)

df <- data.frame(d1=I, d2=J, val=dat)
arr[] <- df
expect_equal(arr[]$val, df[,"val"])

unlink(tmp, recursive = TRUE)
options(op)
#})

#test_that("test tiledb_array read/write sparse array with heterogenous msec domains", {
op <- options()
options(stringsAsFactors=FALSE)       # accomodate R 3.*
dir.create(tmp <- tempfile())

d1  <- tiledb_dim("d1", domain = c(0, 1e18), tile=1000L, type="DATETIME_MS")
d2  <- tiledb_dim("d2", domain = NULL, tile = NULL, type="ASCII")
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr("val", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(val), sparse=TRUE)
tiledb_array_create(tmp, sch)

N <- 10
dat <- matrix(rnorm(N), N, 1)
arr <- tiledb_array(tmp, is.sparse=TRUE, as.data.frame=TRUE)
I <- as.POSIXct("2020-01-01") + seq_len(N)*3600
J <- sample(c("ABC","DEF","GHI"), N, replace=TRUE)

df <- data.frame(d1=I, d2=J, val=dat)
arr[] <- df
expect_equal(arr[]$val, df[,"val"])

unlink(tmp, recursive = TRUE)
options(op)
#})


#test_that("test full write-read cycle on sample data using fromDataFrame", {
op <- options()
options(stringsAsFactors=FALSE)       # accomodate R 3.*

## -- download data and extract data set, sample a portion
## download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip",
##               "/tmp/bank.zip")
## datfull <- read.csv(unz("/tmp/bank.zip", "bank-full.csv"), sep=";")
## set.seed(123)
## dat <- datfull[sample(nrow(datfull), 1000, replace=FALSE),]
## saveRDS(dat, "bankSample.rds")

dat <- readRDS(system.file("sampledata", "bankSample.rds", package="tiledb"))

dir.create(tmpuri <- tempfile())
fromDataFrame(dat[,-1], tmpuri)

arr <- tiledb_array(tmpuri, as.data.frame=TRUE)
newdat <- arr[]
expect_equal(dat[,-1], newdat[,-1])

unlink(tmpuri, recursive = TRUE)
options(op)
#})

#test_that("test full write-read cycle on sample data using schema", {
op <- options()
options(stringsAsFactors=FALSE)       # accomodate R 3.*

## -- download data and extract data set, sample a portion
## download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip",
##               "/tmp/bank.zip")
## datfull <- read.csv(unz("/tmp/bank.zip", "bank-full.csv"), sep=";")
## set.seed(123)
## dat <- datfull[sample(nrow(datfull), 1000, replace=FALSE),]
## saveRDS(dat, "bankSample.rds")

dat <- readRDS(system.file("sampledata", "bankSample.rds", package="tiledb"))

dir.create(tmpuri <- tempfile())

n <- nrow(dat)
dim <- tiledb_dim("rows", domain=c(1L,n), type="INT32", tile=1L)
dom <- tiledb_domain(dim)
sch <- tiledb_array_schema(dom, attrs = c(tiledb_attr("age", type="INT32"),
                                          tiledb_attr("job", type="CHAR", ncells=NA),
                                          tiledb_attr("marital", type="CHAR", ncells=NA),
                                          tiledb_attr("education", type="CHAR", ncells=NA),
                                          tiledb_attr("default", type="CHAR", ncells=NA),
                                          tiledb_attr("balance", type="INT32"),
                                          tiledb_attr("housing", type="CHAR", ncells=NA),
                                          tiledb_attr("loan", type="CHAR", ncells=NA),
                                          tiledb_attr("contact", type="CHAR", ncells=NA),
                                          tiledb_attr("day", type="INT32"),
                                          tiledb_attr("month", type="CHAR", ncells=NA),
                                          tiledb_attr("duration", type="INT32"),
                                          tiledb_attr("campaign", type="INT32"),
                                          tiledb_attr("pdays", type="INT32"),
                                          tiledb_attr("previous", type="INT32"),
                                          tiledb_attr("poutcome", type="CHAR", ncells=NA),
                                          tiledb_attr("y", type="CHAR", ncells=NA)
                                          ),
                           sparse = TRUE)
tiledb_array_create(tmpuri, sch)

arr <- tiledb_array(tmpuri, as.data.frame=TRUE)
arr[] <- dat

newarr <- tiledb_array(tmpuri, as.data.frame=TRUE)
newdat <- newarr[]
expect_equal(dat, newdat)

unlink(tmpuri, recursive = TRUE)
options(op)
#})

#test_that("test extended flag on reading", {
op <- options()
options(stringsAsFactors=FALSE)       # accomodate R 3.*

## -- download data and extract data set, sample a portion
## download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip",
##               "/tmp/bank.zip")
## datfull <- read.csv(unz("/tmp/bank.zip", "bank-full.csv"), sep=";")
## set.seed(123)
## dat <- datfull[sample(nrow(datfull), 1000, replace=FALSE),]
## saveRDS(dat, "bankSample.rds")

dat <- readRDS(system.file("sampledata", "bankSample.rds", package="tiledb"))

dir.create(tmpuri <- tempfile())
fromDataFrame(dat[,-1], tmpuri)

arr1 <- tiledb_array(tmpuri, as.data.frame=TRUE)
dat1 <- arr1[]

arr2 <- tiledb_array(tmpuri, as.data.frame=TRUE, extended=FALSE)
dat2 <- arr2[]
## dat2 should have fewer as not extended
expect_true(ncol(dat1) > ncol(dat2))

## check values
expect_true(extended(arr1))
expect_false(extended(arr2))

## change value, check again
extended(arr2) <- TRUE
expect_true(extended(arr2))

## now dat2 should be equal to dat1
dat2 <- arr2[]
expect_equal(ncol(dat1), ncol(dat2))
expect_equal(dat1, dat2)

unlink(tmpuri, recursive = TRUE)
options(op)
#})


#test_that("test attrs column selection on reading", {
op <- options()
options(stringsAsFactors=FALSE)       # accomodate R 3.*

## -- download data and extract data set, sample a portion
## download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip",
##               "/tmp/bank.zip")
## datfull <- read.csv(unz("/tmp/bank.zip", "bank-full.csv"), sep=";")
## set.seed(123)
## dat <- datfull[sample(nrow(datfull), 1000, replace=FALSE),]
## saveRDS(dat, "bankSample.rds")

dat <- readRDS(system.file("sampledata", "bankSample.rds", package="tiledb"))

dir.create(tmpuri <- tempfile())
fromDataFrame(dat[,-1], tmpuri)

arr <- tiledb_array(tmpuri, as.data.frame=TRUE)
expect_true(length(attrs(arr)) == 0)

sels <-  c("age", "job", "education", "duration")
attrs(arr) <- sels
dat <- arr[]
expect_equal(colnames(dat), sels)
extended(arr) <- FALSE
dat <- arr[]
expect_equal(colnames(dat), sels)

unlink(tmpuri, recursive = TRUE)
options(op)
#})

#test_that("test range selection on reading", {

set.seed(100)
y <- matrix((1:10) + runif(10)/10, 10)

rc <- dir.create(tmpuri <- tempfile())
d1  <- tiledb_dim("d1", domain = c(1L, 25L), type="INT32", tile=1L)
d2  <- tiledb_dim("d2", domain = c(1L, 25L), type="INT32", tile=1L)
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr("val", type = "FLOAT64")
sch <- tiledb_array_schema(dom, val, sparse=TRUE)
rc <- tiledb_array_create(tmpuri, sch)

x <- tiledb_array(uri = tmpuri, as.data.frame=TRUE)
x[] <- list(d1=1:10, d2=1:10, val=y)

x <- tiledb_array(uri = tmpuri, as.data.frame=TRUE)

## intersection: 2 and 8 ... from 1 to 2 and 7 to 9, and 2 and 8
selected_ranges(x) <- list(matrix(c(1,2,7,9),2,byrow=TRUE),
                           matrix(c(2,2,8,8),2,byrow=TRUE))
val <- x[]
expect_equal(nrow(val), 2)
expect_equal(val[,"d1"], c(2L,8L))
expect_equal(val[,"d2"], c(2L,8L))

## intersection: 2 and 3
selected_ranges(x) <- list(matrix(c(1,3),1,byrow=TRUE),
                           matrix(c(2,3),1,byrow=TRUE))
val <- x[]
expect_equal(nrow(val), 2)
expect_equal(val[,"d1"], c(2L,3L))
expect_equal(val[,"d2"], c(2L,3L))

## NULL in pos 1 and range 2 to 4
selected_ranges(x) <- list(NULL,
                           matrix(c(2,4),1,byrow=TRUE))
val <- x[]
expect_equal(nrow(val), 3)
expect_equal(val[,"d1"], c(2L,3L,4L))
expect_equal(val[,"d2"], c(2L,3L,4L))

## NULL in pos 2 and range 2 to 4
selected_ranges(x) <- list(matrix(c(2,4),1,byrow=TRUE), NULL)
val <- x[]
expect_equal(nrow(val), 3)
expect_equal(val[,"d1"], c(2L,3L,4L))
expect_equal(val[,"d2"], c(2L,3L,4L))

## all ten
selected_ranges(x) <- list()
val <- x[]
expect_equal(nrow(val), 10)
expect_equal(val[,"d1"], val[,"d2"])

unlink(tmpuri, recursive = TRUE)
#})

#test_that("test range selection edge cases", {
tmp <- tempfile()
dir.create(tmp)

d1  <- tiledb_dim("d1", domain = c(1L, 10L))
d2  <- tiledb_dim("d2", domain = c(1L, 10L))
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr("val", type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(val), sparse=TRUE)
tiledb_array_create(tmp, sch)

x <- tiledb_array(uri = tmp, as.data.frame=TRUE)
df <- data.frame(d1=integer(0), d2=integer(0), val=numeric(0))
## cannot currently write (corner-case) zero-length data.frame via <-
#x[] <- df
#val <- x[]
#expect_equal(nrow(val), 0L)

x[] <- data.frame(d1=1, d2=1, val=1)
selected_ranges(x) <- list(cbind(2,2), cbind(2,2))
val <- x[]
expect_equal(nrow(val), 0L)

unlink(tmp, recursive = TRUE)
#})

#test_that("test range selection edge cases sparse", {
tmp <- tempfile()
dir.create(tmp)

d1  <- tiledb_dim("d1", domain = c(1, 100))
d2  <- tiledb_dim("d2", domain = c(1, 100))
dom <- tiledb_domain(c(d1, d2))
val <- tiledb_attr("val", type = "FLOAT64")
sch <- tiledb_array_schema(dom, val, sparse=TRUE)
tiledb_array_create(tmp, sch)

x <- tiledb_array(uri = tmp, as.data.frame=TRUE)

set.seed(100)
df <- data.frame(d1=sample(100, 10, replace=TRUE),
                 d2=sample(100, 10, replace=TRUE),
                 val=1:10)
x[] <- df
selected_ranges(x) <- list(cbind(10,10), cbind(10,100))
val <- x[]
expect_equal(nrow(val), 0L)

selected_ranges(x) <- list(cbind(1,21), cbind(10,100))
val <- x[]
expect_equal(nrow(val), 1L)

unlink(tmp, recursive = TRUE)
#})

#test_that("test range selection for multiple dimensions", {
tmp <- tempfile()
dir.create(tmp)

dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("d2", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("d3", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("d4", c(1L, 4L), 4L, "INT32")))
schema = tiledb_array_schema(dom, attrs=c(tiledb_attr("a", type = "FLOAT64")), sparse = TRUE)
tiledb_array_create(tmp, schema)

I <- c(1L, 2L, 4L)
J <- c(1L, 2L, 3L)
K <- c(1L, 2L, 4L)
L <- c(1L, 2L, 3L)
data <- c(1.0, 2.1, 3.3)
A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
A[] <- data.frame(d1=I, d2=J, d3=K, d4=L, a=data)

## constrain to one row
matlist <- list(cbind(2,2), cbind(1,2), cbind(2,3), cbind(1,2))
selected_ranges(A) <- matlist
expect_equal(nrow(A[]), 1L)

## constrain to two rows, use a NULL
matlist <- list(cbind(1,4), cbind(1,4), NULL, cbind(1,2))
selected_ranges(A) <- matlist
expect_equal(nrow(A[]), 2L)

unlink(tmp, recursive = TRUE)
#})

#test_that("test int64 dimension for sparse arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))


  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  ## We use
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", as.integer64(c(1,4)), as.integer64(4), "INT64"),
                                tiledb_dim("cols", as.integer64(c(1,4)), as.integer64(4), "INT64")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
  tiledb_array_create(uri = tmp, schema)

  A <- tiledb_array(uri = tmp)

  I <- as.integer64(c(1, 2, 2))
  J <- as.integer64(c(1, 4, 3))
  data <- c(1L, 2L, 3L)
  A[I,J] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[as.integer64(1:2), as.integer64(2:4)]
  expect_equal(newdata[,"a"], c(3L, 2L))
  expect_equal(newdata[,"rows"], as.integer64(c(2, 2)))
  expect_equal(newdata[,"cols"], as.integer64(c(3, 4)))

  unlink(tmp, recursive = TRUE)

  ## test for error on non integer64 arguments
  expect_error(tiledb_dim("rows", c(1L,4L), as.integer64(4), "INT64"))
  expect_error(tiledb_dim("rows", as.integer64(c(1,4)), 4L, "INT64"))
}

#test_that("test uint64 dimension for sparse arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  ## We use
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", as.integer64(c(1,4)), as.integer64(4), "UINT64"),
                                tiledb_dim("cols", as.integer64(c(1,4)), as.integer64(4), "UINT64")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
  tiledb_array_create(uri = tmp, schema)

  A <- tiledb_array(uri = tmp)

  I <- as.integer64(c(1, 2, 2))
  J <- as.integer64(c(1, 4, 3))
  data <- c(1L, 2L, 3L)
  A[I,J] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[as.integer64(1:2), as.integer64(2:4)]
  expect_equal(newdata[,"a"], c(3L, 2L))
  expect_equal(newdata[,"rows"], c(2L, 2L))
  expect_equal(newdata[,"cols"], c(3L, 4L))

  unlink(tmp, recursive = TRUE)

  ## test for error on non integer64 arguments
  expect_error(tiledb_dim("rows", c(1L,4L), as.integer64(4), "UINT64"))
  expect_error(tiledb_dim("rows", as.integer64(c(1,4)), 4L, "UINT64"))
}

#test_that("test uint32 dimension for sparse arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))
  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "UINT32"),
                                tiledb_dim("cols", c(1L,4L), 4L, "UINT32")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
  tiledb_array_create(uri = tmp, schema)

  A <- tiledb_array(uri = tmp)

  I <- c(1, 2, 2)
  J <- c(1, 4, 3)
  data <- c(1L, 2L, 3L)
  A[I,J] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:4]
  expect_equal(newdata[,"a"], c(3L, 2L))
  expect_equal(newdata[,"rows"], c(2L, 2L))
  expect_equal(newdata[,"cols"], c(3L, 4L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test int16 dimension for sparse arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "INT16"),
                                tiledb_dim("cols", c(1L,4L), 4L, "INT16")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
  tiledb_array_create(uri = tmp, schema)

  A <- tiledb_array(uri = tmp)

  I <- c(1, 2, 2)
  J <- c(1, 4, 3)
  data <- c(1L, 2L, 3L)
  A[I,J] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:4]
  expect_equal(newdata[,"a"], c(3L, 2L))
  expect_equal(newdata[,"rows"], c(2L, 2L))
  expect_equal(newdata[,"cols"], c(3L, 4L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test uint16 dimension for sparse arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "UINT16"),
                                tiledb_dim("cols", c(1L,4L), 4L, "UINT16")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
  tiledb_array_create(uri = tmp, schema)

  A <- tiledb_array(uri = tmp)

  I <- c(1, 2, 2)
  J <- c(1, 4, 3)
  data <- c(1L, 2L, 3L)
  A[I,J] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:4]
  expect_equal(newdata[,"a"], c(3L, 2L))
  expect_equal(newdata[,"rows"], c(2L, 2L))
  expect_equal(newdata[,"cols"], c(3L, 4L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test int8 dimension for sparse arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "INT8"),
                                tiledb_dim("cols", c(1L,4L), 4L, "INT8")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
  tiledb_array_create(uri = tmp, schema)

  A <- tiledb_array(uri = tmp)

  I <- c(1, 2, 2)
  J <- c(1, 4, 3)
  data <- c(1L, 2L, 3L)
  A[I,J] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:4]
  expect_equal(newdata[,"a"], c(3L, 2L))
  expect_equal(newdata[,"rows"], c(2L, 2L))
  expect_equal(newdata[,"cols"], c(3L, 4L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test uint8 dimension for sparse arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "UINT8"),
                                tiledb_dim("cols", c(1L,4L), 4L, "UINT8")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
  tiledb_array_create(uri = tmp, schema)

  A <- tiledb_array(uri = tmp)

  I <- c(1, 2, 2)
  J <- c(1, 4, 3)
  data <- c(1L, 2L, 3L)
  A[I,J] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:4]
  expect_equal(newdata[,"a"], c(3L, 2L))
  expect_equal(newdata[,"rows"], c(2L, 2L))
  expect_equal(newdata[,"cols"], c(3L, 4L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test int8 dimension for dense arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "INT8"),
                                tiledb_dim("cols", c(1L,4L), 4L, "INT8")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=FALSE)
  tiledb_array_create(uri = tmp, schema)
  #print(schema)
  A <- tiledb_array(uri = tmp)
  query_layout(A) <- "UNORDERED"

  data <- 1:16
  ## can write as data.frame
  A[] <- data.frame(rows=rep(1:4,each=4), cols=rep(1:4,4), a=data)
  ## or with indices
  #A[rep(1:4,each=4), rep(1:4,4)] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:3]
  expect_equal(newdata[,"a"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"rows"], c(1L, 1L, 2L, 2L))
  expect_equal(newdata[,"cols"], c(2L, 3L, 2L, 3L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test uint8 dimension for dense arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "UINT8"),
                                tiledb_dim("cols", c(1L,4L), 4L, "UINT8")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=FALSE)
  tiledb_array_create(uri = tmp, schema)
  #print(schema)
  A <- tiledb_array(uri = tmp)
  query_layout(A) <- "UNORDERED"

  data <- 1:16
  ## can write as data.frame
  A[] <- data.frame(rows=rep(1:4,each=4), cols=rep(1:4,4), a=data)
  ## or with indices
  #A[rep(1:4,each=4), rep(1:4,4)] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:3]
  expect_equal(newdata[,"a"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"rows"], c(1L, 1L, 2L, 2L))
  expect_equal(newdata[,"cols"], c(2L, 3L, 2L, 3L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test int16 dimension for dense arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "INT16"),
                                tiledb_dim("cols", c(1L,4L), 4L, "INT16")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=FALSE)
  tiledb_array_create(uri = tmp, schema)
  #print(schema)
  A <- tiledb_array(uri = tmp)
  query_layout(A) <- "UNORDERED"

  data <- 1:16
  ## can write as data.frame
  A[] <- data.frame(rows=rep(1:4,each=4), cols=rep(1:4,4), a=data)
  ## or with indices
  #A[rep(1:4,each=4), rep(1:4,4)] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:3]
  expect_equal(newdata[,"a"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"rows"], c(1L, 1L, 2L, 2L))
  expect_equal(newdata[,"cols"], c(2L, 3L, 2L, 3L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test uint16 dimension for dense arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "UINT16"),
                                tiledb_dim("cols", c(1L,4L), 4L, "UINT16")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=FALSE)
  tiledb_array_create(uri = tmp, schema)
  #print(schema)
  A <- tiledb_array(uri = tmp)
  query_layout(A) <- "UNORDERED"

  data <- 1:16
  ## can write as data.frame
  A[] <- data.frame(rows=rep(1:4,each=4), cols=rep(1:4,4), a=data)
  ## or with indices
  #A[rep(1:4,each=4), rep(1:4,4)] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:3]
  expect_equal(newdata[,"a"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"rows"], c(1L, 1L, 2L, 2L))
  expect_equal(newdata[,"cols"], c(2L, 3L, 2L, 3L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test int32 dimension for dense arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "INT32"),
                                tiledb_dim("cols", c(1L,4L), 4L, "INT32")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=FALSE)
  tiledb_array_create(uri = tmp, schema)
  #print(schema)
  A <- tiledb_array(uri = tmp)
  query_layout(A) <- "UNORDERED"

  data <- 1:16
  ## can write as data.frame
  A[] <- data.frame(rows=rep(1:4,each=4), cols=rep(1:4,4), a=data)
  ## or with indices
  #A[rep(1:4,each=4), rep(1:4,4)] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:3]
  expect_equal(newdata[,"a"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"rows"], c(1L, 1L, 2L, 2L))
  expect_equal(newdata[,"cols"], c(2L, 3L, 2L, 3L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test uint32 dimension for dense arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "UINT32"),
                                tiledb_dim("cols", c(1L,4L), 4L, "UINT32")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=FALSE)
  tiledb_array_create(uri = tmp, schema)
  #print(schema)
  A <- tiledb_array(uri = tmp)
  query_layout(A) <- "UNORDERED"

  data <- 1:16
  ## can write as data.frame
  A[] <- data.frame(rows=rep(1:4,each=4), cols=rep(1:4,4), a=data)
  ## or with indices
  #A[rep(1:4,each=4), rep(1:4,4)] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:3]
  expect_equal(newdata[,"a"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"rows"], c(1L, 1L, 2L, 2L))
  expect_equal(newdata[,"cols"], c(2L, 3L, 2L, 3L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test int64 dimension for dense arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", as.integer64(c(1,4)), as.integer64(4), "INT64"),
                                tiledb_dim("cols", as.integer64(c(1,4)), as.integer64(4), "INT64")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=FALSE)
  tiledb_array_create(uri = tmp, schema)
  #print(schema)
  A <- tiledb_array(uri = tmp)
  query_layout(A) <- "UNORDERED"

  data <- 1:16
  ## can write as data.frame
  A[] <- data.frame(rows=as.integer64(rep(1:4,each=4)), cols=as.integer64(rep(1:4,4)), a=data)
  ## or with indices
  #A[as.integer64(rep(1:4,each=4)), as.integer64(rep(1:4,4))] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[as.integer64(1:2), as.integer64(2:3)]
  expect_equal(newdata[,"a"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"rows"], as.integer64(c(1L, 1L, 2L, 2L)))
  expect_equal(newdata[,"cols"], as.integer64(c(2L, 3L, 2L, 3L)))


  unlink(tmp, recursive = TRUE)
}

#test_that("test uint64 dimension for dense arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", as.integer64(c(1,4)), as.integer64(4), "UINT64"),
                                tiledb_dim("cols", as.integer64(c(1,4)), as.integer64(4), "UINT64")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=FALSE)
  tiledb_array_create(uri = tmp, schema)
  #print(schema)
  A <- tiledb_array(uri = tmp)
  query_layout(A) <- "UNORDERED"

  data <- 1:16
  ## can write as data.frame
  A[] <- data.frame(rows=as.integer64(rep(1:4,each=4)), cols=as.integer64(rep(1:4,4)), a=data)
  ## or with indices
  #A[as.integer64(rep(1:4,each=4)), as.integer64(rep(1:4,4))] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[as.integer64(1:2), as.integer64(2:3)]
  expect_equal(newdata[,"a"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"rows"], c(1L, 1L, 2L, 2L))
  expect_equal(newdata[,"cols"], c(2L, 3L, 2L, 3L))


  unlink(tmp, recursive = TRUE)
}

#test_that("test all integer types as attributes for dense arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "INT32"),
                                tiledb_dim("cols", c(1L,4L), 4L, "INT32")))
  schema <- tiledb_array_schema(dom,
                                attrs = c(tiledb_attr("a1", type = "INT8"),
                                          tiledb_attr("a2", type = "UINT8"),
                                          tiledb_attr("a3", type = "INT16"),
                                          tiledb_attr("a4", type = "UINT16"),
                                          tiledb_attr("a5", type = "INT32"),
                                          tiledb_attr("a6", type = "UINT32"),
                                          tiledb_attr("a7", type = "INT64"),
                                          tiledb_attr("a8", type = "UINT64")
                                          ),
                                sparse=FALSE)
  tiledb_array_create(uri = tmp, schema)
  #print(schema)
  A <- tiledb_array(uri = tmp)
  query_layout(A) <- "UNORDERED"

  data <- data.frame(a1=1:16,
                     a2=1:16,
                     a3=1:16,
                     a4=1:16,
                     a5=1:16,
                     a6=1:16,
                     a7=as.integer64(1:16),
                     a8=as.integer64(1:16))
  ## can write as data.frame
  A[] <- data.frame(rows=rep(1:4,each=4),
                    cols=rep(1:4,4),
                    data)
  ## or with indices
  #A[rep(1:4,each=4), rep(1:4,4)] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:3]
  expect_equal(newdata[,"rows"], c(1L, 1L, 2L, 2L))
  expect_equal(newdata[,"cols"], c(2L, 3L, 2L, 3L))
  expect_equal(newdata[,"a1"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a2"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a3"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a4"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a5"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a6"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a7"], as.integer64(c(2L, 3L, 6L, 7L)))
  expect_equal(newdata[,"a8"], c(2L, 3L, 6L, 7L))

  unlink(tmp, recursive = TRUE)
}

#test_that("test all integer types as attributes for sparse arrays", {
if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))

  tmp <- tempfile()
  dir.create(tmp)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L,4L), 4L, "INT32"),
                                tiledb_dim("cols", c(1L,4L), 4L, "INT32")))
  schema <- tiledb_array_schema(dom,
                                attrs = c(tiledb_attr("a1", type = "INT8"),
                                          tiledb_attr("a2", type = "UINT8"),
                                          tiledb_attr("a3", type = "INT16"),
                                          tiledb_attr("a4", type = "UINT16"),
                                          tiledb_attr("a5", type = "INT32"),
                                          tiledb_attr("a6", type = "UINT32"),
                                          tiledb_attr("a7", type = "INT64"),
                                          tiledb_attr("a8", type = "UINT64")
                                          ),
                                sparse=TRUE)
  tiledb_array_create(uri = tmp, schema)
  #print(schema)
  A <- tiledb_array(uri = tmp)

  data <- data.frame(a1=1:16,
                     a2=1:16,
                     a3=1:16,
                     a4=1:16,
                     a5=1:16,
                     a6=1:16,
                     a7=as.integer64(1:16),
                     a8=as.integer64(1:16))
  ## can write as data.frame
  A[] <- data.frame(rows=rep(1:4,each=4),
                    cols=rep(1:4,4),
                    data)
  ## or with indices
  A[rep(1:4,each=4), rep(1:4,4)] <- data

  A <- tiledb_array(uri = tmp, as.data.frame=TRUE)
  newdata <- A[1:2, 2:3]
  expect_equal(newdata[,"rows"], c(1L, 1L, 2L, 2L))
  expect_equal(newdata[,"cols"], c(2L, 3L, 2L, 3L))
  expect_equal(newdata[,"a1"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a2"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a3"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a4"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a5"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a6"], c(2L, 3L, 6L, 7L))
  expect_equal(newdata[,"a7"], as.integer64(c(2L, 3L, 6L, 7L)))
  expect_equal(newdata[,"a8"], c(2L, 3L, 6L, 7L))

  unlink(tmp, recursive = TRUE)
}


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



## non-empty domain, var and plain
tmp <- tempfile()
dir.create(tmp)

## create 4x4 with single attribute
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("d2", NULL, NULL, "ASCII")))
schema <- tiledb_array_schema(dom, attrs=c(tiledb_attr("a", type = "INT32")), sparse = TRUE)
invisible( tiledb_array_create(tmp, schema) )

## write
I <- c(1L, 2L, 3L)
J <- letters[1:3]
data <- c(1L, 2L, 3L)
arr <- tiledb_array(uri = tmp)
arr[I, J] <- data

expect_equal(tiledb_array_get_non_empty_domain_from_index(arr, 1), c(1, 3))
expect_equal(tiledb_array_get_non_empty_domain_from_name(arr, "d1"), c(1, 3))
expect_equal(tiledb_array_get_non_empty_domain_from_index(arr, 2), c("a", "c"))
expect_equal(tiledb_array_get_non_empty_domain_from_name(arr, "d2"), c("a", "c"))

if (FALSE) {                            # this tests fine in isolation but croaks in bulk
    ## access schema from uri
    schema2 <- schema(tmp)
    expect_true(is(schema2, "tiledb_array_schema"))
    expect_equal(schema, schema2)

    ## access schema from array
    schema3 <- schema(arr)
    expect_true(is(schema3, "tiledb_array_schema"))
    expect_equal(schema, schema3)
}

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
A <- tiledb_array(uri = tmp, timestamp=now1)
A[I, J] <- data

Sys.sleep(1)

now2 <- Sys.time()
I <- c(8, 6, 9)
J <- c(5, 7, 8)
data <- c(11L, 22L, 33L)
A <- tiledb_array(uri = tmp, timestamp=now2)
A[I, J] <- data

A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp=now1 - 0.5)
expect_equal(nrow(A[]), 0)
A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp=now1 + 0.5)
expect_equal(nrow(A[]), 3)
A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp=now2 - 0.5)
expect_equal(nrow(A[]), 3)
A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp=now2 + 0.5)
expect_equal(nrow(A[]), 6)

## as.matrix
tmp <- tempfile()
dir.create(tmp)
## Generate a matrix
n <- 5L
k <- 4L
mat <- matrix(1:(n*k), nrow=n, ncol=k)
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, n), n, "INT32"),
                              tiledb_dim("cols", c(1L, k), k, "INT32")))
schema <- tiledb_array_schema(dom, attrs=tiledb_attr("vals", type="INT32"))
tiledb_array_create(tmp, schema)
arr <- tiledb_array(tmp)
query_layout(arr) <- "COL_MAJOR"    	# needed if we want column order
arr[] <- mat                        	# we can write directly

arr2 <- tiledb_array(tmp, as.matrix=TRUE)
mat2 <- arr2[]
expect_equal(mat, mat2)                 # check round-turn

## check no double selection
expect_error(tiledb_array(tmp, as.data.frame=TRUE, as.matrix=TRUE))
## check normal data.frame return when row col select
expect_true(is.data.frame(suppressMessages(arr2[1:2,])))
expect_true(is.data.frame(suppressMessages(arr2[,3])))

arr3 <- tiledb_array(tmp, as.data.frame=TRUE)
df1 <- arr3[]
df1$vals2 <- df1$vals * 10
tmp2 <- tempfile()
fromDataFrame(df1, tmp2)

## check selecting matrix out of multiple cols
arr4 <- tiledb_array(tmp2, attrs=c("rows", "cols", "vals2"), as.matrix=TRUE)
expect_equal(arr4[], 10*mat)
arr5 <- tiledb_array(tmp2, attrs=c("rows", "cols", "vals"), as.matrix=TRUE)
expect_equal(arr5[], mat)
arr6 <- tiledb_array(tmp2, attrs=c("rows", "cols", "vals", "vals2"), as.matrix=TRUE)
res <- arr6[]
expect_true(is.list(res))
expect_equal(length(res), 2L)
expect_equal(res$vals, mat)
expect_equal(res$vals2, 10*mat)
