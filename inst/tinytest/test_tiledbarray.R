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
expect_equivalent(dat, newdat)

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

arr <- tiledb_array(tmpuri, as.data.frame=TRUE, extended=FALSE)
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
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
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
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
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
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
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
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
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
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
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
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
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
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
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
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")), sparse=TRUE)
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
                                sparse=TRUE)
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

if (tiledb_version(TRUE) >= "2.8.0" && tiledb_version(TRUE) < "2.10.0") exit_file("2.8.* and 2.9.* skip remainder")

## FYI: 101 tests here
## test encrypted arrays via high-level accessor
## (lower-level tests in test_densearray and test_arrayschema)
if (tiledb_version(TRUE) > "2.4.0") {
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
}

## FYI: 105 tests here
## non-empty domain, var and plain
tmp <- tempfile()
dir.create(tmp)

## create 4x4 with single attribute
dom <- tiledb_domain(dims = c(tiledb_dim("d1", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("d2", NULL, NULL, "ASCII")))
schema <- tiledb_array_schema(dom, attrs=c(tiledb_attr("a", type = "INT32")), sparse = TRUE)
tiledb_array_create(tmp, schema)

## write
I <- c(1L, 2L, 3L)
J <- letters[1:3]
data <- c(1L, 2L, 3L)
arr <- tiledb_array(uri = tmp)
arr[I, J] <- data
expect_false(tiledb_array_is_open(arr))
arr <- tiledb_array_open(arr)
expect_true(tiledb_array_is_open(arr))
expect_equal(tiledb_array_get_non_empty_domain_from_index(arr, 1), c(1, 3))
expect_equal(tiledb_array_get_non_empty_domain_from_name(arr, "d1"), c(1, 3))
expect_equal(tiledb_array_get_non_empty_domain_from_index(arr, 2), c("a", "c"))
expect_equal(tiledb_array_get_non_empty_domain_from_name(arr, "d2"), c("a", "c"))

## access schema from uri
schema2 <- tiledb::schema(tmp)
expect_true(is(schema2, "tiledb_array_schema"))
expect_equal(schema, schema2)

## access schema from array
schema3 <- tiledb::schema(arr)
expect_true(is(schema3, "tiledb_array_schema"))
expect_equal(schema, schema3)

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
A <- tiledb_array(uri = tmp, timestamp_end=now1)
A[I, J] <- data

twot <- 1 + isMacOS*5
onet <- twot/2
Sys.sleep(twot)

now2 <- Sys.time()
I <- c(8, 6, 9)
J <- c(5, 7, 8)
data <- c(11L, 22L, 33L)
A <- tiledb_array(uri = tmp, timestamp_end=now2)
A[I, J] <- data

if (tiledb_version(TRUE) >= "2.10.0") {
    A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp_end=now1 - onet)
    expect_equal(nrow(A[]), 0)
    A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp_end=now1 + onet)
    expect_equal(nrow(A[]), 3)
    A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp_end=now2 - onet)
    expect_equal(nrow(A[]), 3)
    A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp_end=now2 + onet)
    expect_equal(nrow(A[]), 6)
} else if (tiledb_version(TRUE) >= "2.8.0") {
    A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp=now1 - onet)
    expect_equal(nrow(A[]), 0)
    A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp=now1 + onet)
    expect_equal(nrow(A[]), 3)
    A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp=now2 - onet)
    expect_equal(nrow(A[]), 3)
    A <- tiledb_array(uri = tmp, as.data.frame=TRUE, timestamp=now2 + onet)
    expect_equal(nrow(A[]), 6)
}

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
expect_true(all.equal(mat, mat2, check.attributes=FALSE)) # check round-turn

## check no double selection
expect_error(tiledb_array(tmp, as.data.frame=TRUE, as.matrix=TRUE))
## check matrix return and not data.frame return when row col select
expect_false(is.data.frame(suppressMessages(arr2[1:2,])))
expect_true(is.matrix(suppressMessages(arr2[1:2,])))
expect_false(is.data.frame(suppressMessages(arr2[,3])))
expect_true(is.matrix(suppressMessages(arr2[,3])))
## selections via i and j (and ignore attribute)
expect_equivalent(arr2[1:2,], cbind( c(1,2), c(6,7), c(11,12), c(16,17)))
expect_equivalent(arr2[,3], cbind(11:15) )
## more complex selection with holes via selected_ranges()
selected_ranges(arr2) <- list(cbind(c(1,4),c(2,5)), cbind(2,3))
expect_equivalent(arr2[], cbind(c(6,7,9,10), c(11,12,14,15)))
selected_ranges(arr2) <- list(cbind(c(1,4),c(2,5)), cbind(2,2))
expect_equivalent(arr2[], cbind(c(6,7,9,10)))


arr3 <- tiledb_array(tmp, as.data.frame=TRUE)
df1 <- arr3[]
df1$vals2 <- df1$vals * 10
tmp2 <- tempfile()
fromDataFrame(df1, tmp2)

## check selecting matrix out of multiple cols
arr4 <- tiledb_array(tmp2, attrs=c("rows", "cols", "vals2"), as.matrix=TRUE)
expect_equivalent(arr4[], 10*mat)
arr5 <- tiledb_array(tmp2, attrs=c("rows", "cols", "vals"), as.matrix=TRUE)
expect_equivalent(arr5[], mat)
arr6 <- tiledb_array(tmp2, attrs=c("rows", "cols", "vals", "vals2"), as.matrix=TRUE)
res <- arr6[]
expect_true(is.list(res))
expect_equal(length(res), 2L)
expect_equal(res$vals, mat)
expect_equal(res$vals2, 10*mat)

## PR #245 (variant of examples/ex_1.R)
uri <- tempfile()
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 10L), 10L, "INT32"),
                              tiledb_dim("cols", c(1L, 5L), 5L, "INT32")))
schema <- tiledb_array_schema(dom,
                              attrs = c(tiledb_attr("a", type = "INT32"),
                                        tiledb_attr("b", type = "FLOAT64"),
                                        tiledb_attr("c", type = "CHAR", ncells=NA_integer_)),
                              cell_order = "ROW_MAJOR", tile_order = "ROW_MAJOR")
tiledb_array_create(uri, schema)
data <- list(a=array(seq(1:50), dim = c(10,5)),
             b=array(as.double(seq(101,by=0.5,length=50)), dim = c(10,5)),
             c=array(c(letters[1:26], "brown", "fox", LETTERS[1:22]), dim = c(10,5)))
A <- tiledb_array(uri)
A[] <- data
obj <- tiledb_array(uri, attrs="a", as.data.frame=TRUE)
res <- obj[]
expect_equal(colnames(res), c("rows", "cols", "a")) 	# this was the PR issues
obj <- tiledb_array(uri, attrs="a", as.matrix=TRUE)     # this is the preferred accessor here
expect_equivalent(obj[], data[["a"]])
obj <- tiledb_array(uri, as.matrix=TRUE)     			# test all three matrices
res <- obj[]
expect_equal(res[["a"]], data[["a"]])
expect_equal(res[["b"]], data[["b"]])
expect_equal(res[["c"]], data[["c"]])

## PR #246
N <- 25L
K <- 4L
uri <- tempfile()
schema <- tiledb_array_schema(tiledb_domain(dims=c(tiledb_dim("d1", c(1L, N), tile=N, type="INT32"),
                                                   tiledb_dim("d2", c(1L, K), tile=K, type="INT32"))),
                              sparse=FALSE,
                              attrs=tiledb_attr("x", type="FLOAT64"))
tiledb_array_create(uri, schema)
obj <- tiledb_array(uri, attrs="x", query_type="WRITE")
M <- matrix(runif(N*K), N, K)
obj[] <- M                              # prior to #246 this write had a write data type
chk <- tiledb_array(uri, as.matrix=TRUE)
expect_equivalent(chk[], M)


## test for data.frame append
if (!requireNamespace("palmerpenguins", quietly=TRUE)) exit_file("remainder needs 'palmerpenguins'")
library(palmerpenguins)
uri <- tempfile()
fromDataFrame(penguins, uri, sparse = TRUE,
              col_index = c("species", "year"),
              tile_domain=list(year=c(1966L, 2021L)))
arr <- tiledb_array(uri, as.data.frame=TRUE)
## new data
newdf <- penguins[1:2,]
newdf$species <- c("Fred", "Ginger")
newdf$island <- c("Manhattan", "Staten Island")
newdf$year <- c(1966L, 1969L)                     # int is important
arr[] <- newdf
## check it
chk <- tiledb_array(uri, as.data.frame=TRUE)
res <- chk[]
expect_true(1966L %in% res$year)
expect_true(1969L %in% res$year)
expect_true("Manhattan" %in% res$island)
expect_true("Staten Island" %in% res$island)
expect_true("Fred" %in% res$species)
expect_true("Ginger" %in% res$species)
expect_equal(nrow(penguins) + 2, nrow(res))
expect_equal(ncol(penguins), ncol(res))

## test for both possible orders of selected_ranges
selected_ranges(arr) <- list(year=cbind(1966L, 1999L),
                             species=matrix(c("Fred", "Fred",
                                              "Ginger", "Ginger"),
                                            2, 2, byrow=TRUE))
res1 <- arr[]
expect_equal(nrow(res1), 2)
selected_ranges(arr) <- list(species=matrix(c("Fred", "Fred",
                                              "Ginger", "Ginger"),
                                            2, 2, byrow=TRUE),
                             year=cbind(1966L, 1999L))
res2 <- arr[]
expect_equal(nrow(res2), 2)
expect_equal(res1, res2)

## check for strings_as_factors
arr <- tiledb_array(uri, as.data.frame=TRUE)
res <- arr[]
expect_equal(class(res[,"species"]), "character")
expect_equal(class(res[,"sex"]), "character")
arr <- tiledb_array(uri, as.data.frame=TRUE, strings_as_factors=TRUE)
res <- arr[]
expect_equal(class(res[,"species"]), "factor")
expect_equal(class(res[,"sex"]), "factor")


## issue 255
uri <- tempfile()
## Generate a matrix
n <- 5L
k <- 4L
mat <- matrix(1:(n*k) * 0.12345, nrow=n, ncol=k)
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, n), n, "INT32"),
                              tiledb_dim("cols", c(1L, k), k, "INT32")))
schema <- tiledb_array_schema(dom, attrs=tiledb_attr("vals", type="FLOAT64"))
tiledb_array_create(uri, schema)
arr <- tiledb_array(uri, is.sparse=FALSE)
I <- c(1:3)
J <- c(3:4)
arr[I, J] <- mat[I, J]
I <- 4:5
J <- 1:4
arr[I,J] <- mat[I, J]
arr2 <- tiledb_array(uri, as.matrix=TRUE)
res <- arr2[]
expect_equal(dim(res), c(5,4))
expect_equal(sum(is.na(res[1:3,1:2])), 6) # arr[1:3,1:2] all NA
if (tiledb_version(TRUE) < "2.7.0" || Sys.Date() >= as.Date("2022-01-28")) expect_equal(res[1:3,3:4], mat[1:3,3:4])  ## Fix pending, cf SC-13735
expect_equal(res[4:5,1:4], mat[4:5,1:4])

## issue 259 dense array with n>2 dimensions
dom <- tiledb_domain(dims = list(tiledb_dim("rows", c(1L, 10L), 10L, "INT32"),
                                 tiledb_dim("cols", c(1L, 5L), 5L, "INT32"),
                                 tiledb_dim("time", c(1L, 2L), 2L, "INT32")))
schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32"),
                                             tiledb_attr("b", type = "FLOAT64"),
                                             tiledb_attr("c", type = "CHAR", ncells = NA_integer_)))

uri <- tempfile()
res <- tiledb_array_create(uri, schema)
data <- list(a=array(seq(1:100), dim = c(10,5, 2)),
             b=array(as.double(seq(101,by=0.5,length=100)), dim = c(10,5,2)),
             c=array(rep(c(letters[1:26], "brs", "asdf", LETTERS[1:22]), 2), dim = c(10,5,2)))
A <- tiledb_array(uri = uri, as.data.frame = TRUE, query_layout = "ROW_MAJOR")
A[] <- data
chk <- tiledb_array(uri = uri, as.data.frame=TRUE)
res <- chk[]
expect_equal(dim(res), c(100,6))
expect_equal(colnames(res), c("rows", "cols", "time", "a", "b", "c"))

## consolidate
expect_equal(array_consolidate(uri), NULL)
expect_error(array_consolidate(uri, start_time="abc")) # not a datetime
expect_error(array_consolidate(uri, end_time="def"))   # not a datetime
now <- Sys.time()
if (tiledb_version(TRUE) >= "2.3.0") expect_equal(array_consolidate(uri, start_time=now-60, end_time=now), NULL)

## vaccum
expect_equal(array_vacuum(uri), NULL)
expect_error(array_vacuum(uri, start_time="abc")) # not a datetime
expect_error(array_vacuum(uri, end_time="def"))   # not a datetime
if (tiledb_version(TRUE) >= "2.3.0") expect_equal(array_vacuum(uri, start_time=now-60, end_time=now), NULL)




## test return preference
uri <- tempfile()
fromDataFrame(penguins, uri, sparse = TRUE, col_index = c("species", "year"))

defaultConversion <- get_return_as_preference()
if (defaultConversion != "asis") {
    oldConversionValue <- defaultConversion
    set_return_as_preference("asis") 		# baseline value
} else {
    oldConversionValue <- "asis"
}

res <- tiledb_array(uri)[]
expect_equal(class(res), "list")

set_return_as_preference("data.frame")
res <- tiledb_array(uri)[]
expect_equal(class(res), "data.frame")

if (hasDataTable) {
    set_return_as_preference("data.table")
    res <- tiledb_array(uri)[]
    expect_true(inherits(res, "data.table"))
}

if (hasTibble) {
    set_return_as_preference("tibble")
    res <- tiledb_array(uri)[]
    expect_true(inherits(res, "tbl_df"))
    expect_true(inherits(res, "tbl"))
}

set_return_as_preference(oldConversionValue) 		# reset baseline value

res <- tiledb_array(uri, return_as="data.frame")[]
expect_equal(class(res), "data.frame")

if (hasDataTable) {
    res <- tiledb_array(uri, return_as="data.table")[]
    expect_true(inherits(res, "data.table"))
}

if (hasTibble) {
    res <- tiledb_array(uri, return_as="tibble")[]
    expect_true(inherits(res, "tbl_df"))
    expect_true(inherits(res, "tbl"))
}

## test return_as for array and matrix
uri <- tempfile()
dir.create(uri)
n <- 5L
k <- 5L
mat <- matrix(1:(n*k), nrow=n, ncol=k)
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, n), n, "INT32"),
                              tiledb_dim("cols", c(1L, k), k, "INT32")))
schema <- tiledb_array_schema(dom, attrs=tiledb_attr("vals", type="INT32"))
tiledb_array_create(uri, schema)
arr <- tiledb_array(uri)
query_layout(arr) <- "COL_MAJOR"    	# needed if we want column order
arr[] <- mat                        	# we can write directly

set_return_as_preference("array")
res <- tiledb_array(uri)[][[1]]
expect_true(inherits(res, "array"))

set_return_as_preference("matrix")
res <- tiledb_array(uri)[]
expect_true(inherits(res, "matrix"))

set_return_as_preference(oldConversionValue) 		# reset baseline value

## test query_statistics setter and getter
if (tiledb_version(TRUE) < "2.4.0") exit_file("TileDB Query stats requires TileDB 2.4.* or greater")
uri <- tempfile()
fromDataFrame(mtcars, uri)
arr <- tiledb_array(uri)
expect_false(query_statistics(arr))     # as not set
query_statistics(arr) <- TRUE
expect_true(query_statistics(arr))

## piped filtering and selection
uri <- tempfile()
fromDataFrame(penguins, uri, sparse = TRUE, col_index=1:2)
## see the equivalence via
## deparse(subsitute(
##     res <- tiledb_array(uri, return_as="data.frame") |>
##          tdb_filter(body_mass_g >= 5500) |>
##          tdb_filter(bill_length_mm > 50) |>
##          tdb_select(body_mass_g, bill_length_mm, year, sex) |>
##          tdb_collect()
## ))
## to what follows, but the following works for R < 4.1.0 too
arr <- tiledb_array(uri, return_as="data.frame")
arr <- tdb_filter(arr, body_mass_g >= 5500)
arr <- tdb_filter(arr, bill_length_mm > 50)
arr <- tdb_select(arr, body_mass_g, bill_length_mm, year, sex)
res <- tdb_collect(arr)
expect_equal(dim(res), c(14,6))
expect_true(min(res$body_mass_g) >= 5500)
expect_true(min(res$bill_length_mm) > 50)
expect_equal(colnames(res), c("species", "island", "body_mass_g", "bill_length_mm", "year", "sex"))


## new 3d index, and int64 domain conversion
uri <- tempfile()
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("cols", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("depth", c(1L, 4L), 4L, "INT32")))
schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")))
tiledb_array_create(uri, schema)
data <- array(1:64, dim = c(4,4,4))
A <- tiledb_array(uri = uri)
A[] <- data

A <- tiledb_array(uri = uri, return_as="data.frame")
res <- A[2,2,2]
expect_equal(res[, "a", drop=TRUE], 22)
res <- A[2,2:3,2]
expect_equal(res[, "a", drop=TRUE], c(22,26))
res <- A[2,]
expect_true(all(res[, "rows", drop=TRUE] == 2))
expect_equal(res[, "a", drop=TRUE], c(2L, 18L, 34L, 50L, 6L, 22L, 38L, 54L, 10L, 26L, 42L, 58L, 14L,
30L, 46L, 62L))
res <- A[,2]
expect_true(all(res[, "cols", drop=TRUE] == 2))
expect_equal(res[, "a", drop=TRUE], c(5L, 21L, 37L, 53L, 6L, 22L, 38L, 54L, 7L, 23L, 39L, 55L, 8L, 24L, 40L, 56L))
res <- A[,,2]
expect_true(all(res[, "depth", drop=TRUE] == 2))
expect_equal(res[, "a", drop=TRUE], c(17L, 21L, 25L, 29L, 18L, 22L, 26L, 30L, 19L, 23L, 27L, 31L, 20L, 24L, 28L, 32L))
selected_ranges(A) <- list(cbind(2,2), cbind(2,2), cbind(2,2))
res <- A[]
expect_equal(res[, "a", drop=TRUE], 22)
selected_ranges(A) <- list(cbind(2,2), cbind(2,3), cbind(2,2))
res <- A[]
expect_equal(res[, "a", drop=TRUE], c(22,26))

if (requireNamespace("bit64", quietly=TRUE)) {
  suppressMessages(library(bit64))
  uri <- tempfile()
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(as.integer64(1), as.integer64(4)), as.integer64(4), "INT64"),
                                tiledb_dim("cols", c(as.integer64(1), as.integer64(4)), as.integer64(4), "INT64"),
                                tiledb_dim("depth", c(as.integer64(1), as.integer64(4)), as.integer64(4), "INT64")))
  schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT64")))
  tiledb_array_create(uri, schema)
  data <- array(as.integer64(1:64), dim = c(4,4,4))
  A <- tiledb_array(uri = uri)
  A[] <- data

  A <- tiledb_array(uri = uri, return_as="data.frame")
  res <- A[2,2,2]
  expect_equal(res[, "a", drop=TRUE], as.integer64(22))
  res <- A[2,2:3,2]
  expect_equal(res[, "a", drop=TRUE], as.integer64(c(22,26)))
  selected_ranges(A) <- list(cbind(2,2), cbind(2,2), cbind(2,2))
  res <- A[]
  expect_equal(res[, "a", drop=TRUE], as.integer64(22))
  selected_ranges(A) <- list(cbind(2,2), cbind(2,3), cbind(2,2))
  res <- A[]
  expect_equal(res[, "a", drop=TRUE], as.integer64(c(22,26)))
}


## test for no attributes
library(palmerpenguins)
uri <- tempfile()
fromDataFrame(penguins, uri, sparse = TRUE, col_index = c("species", "year"))
arr <- tiledb_array(uri, as.data.frame = TRUE, attrs = NA_character_)
res <- arr[]
expect_equal(NCOL(res), 2)
expect_equal(colnames(res), c("species", "year"))

## check that we can specify no attributes with the setter
arr <- tiledb_array(uri)
expect_identical(attrs(arr), character(length = 0L))

attrs(arr) <- NA_character_
expect_true(is.na(attrs(arr)))


## check for incomplete status on unsuccessful query
set_allocation_size_preference(256)     # too low for penguins to return something
array <- tiledb_array(uri, as.data.frame=TRUE)
expect_warning(res <- array[])          # warning emitted
expect_equal(attr(res, "query_status"), "INCOMPLETE") # and query status reported


## check for batched operation
set_allocation_size_preference(1024)
arr <- tiledb_array(uri, as.data.frame=TRUE)
lst <- tiledb:::createBatched(arr)
res1 <- tiledb:::fetchBatched(arr, lst)
expect_false(completedBatched(lst))
res2 <- tiledb:::fetchBatched(arr, lst)
expect_false(completedBatched(lst))
res3 <- tiledb:::fetchBatched(arr, lst)
expect_false(completedBatched(lst))
res4 <- tiledb:::fetchBatched(arr, lst)
expect_true(completedBatched(lst))
expect_equal(nrow(res1) + nrow(res2) + nrow(res3) + nrow(res4), 344)
