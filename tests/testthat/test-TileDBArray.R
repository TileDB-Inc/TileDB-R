library(tiledb)
context("tiledb::TileDBArray")

test_that("test tiledb_array read/write sparse array with heterogenous date domains", {
  skip_if(tiledb_version(TRUE) < "2.0.0")
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
  #print(sch)

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
})

test_that("test tiledb_array read/write sparse array with heterogenous msec domains", {
  skip_if(tiledb_version(TRUE) < "2.0.0")
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
})


test_that("test full write-read cycle on sample data using fromDataFrame", {
  skip_if(tiledb_version(TRUE) < "2.0.0")
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

  arr <- tiledb_dense(tmpuri, as.data.frame=TRUE)
  newdat <- arr[]
  expect_equal(dat[,-1], newdat)

  unlink(tmpuri, recursive = TRUE)
  options(op)
})

test_that("test full write-read cycle on sample data using schema", {
  skip_if(tiledb_version(TRUE) < "2.0.0")
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
})

test_that("test extended flag on reading", {
  skip_if(tiledb_version(TRUE) < "2.0.0")
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
})


test_that("test attrs column selection on reading", {
  skip_if(tiledb_version(TRUE) < "2.0.0")
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
  expect_equal(colnames(dat), c("rows", sels))
  extended(arr) <- FALSE
  dat <- arr[]
  expect_equal(colnames(dat), sels)

  unlink(tmpuri, recursive = TRUE)
  options(op)
})
