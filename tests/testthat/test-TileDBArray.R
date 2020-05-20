library(tiledb)
context("tiledb::TileDBArray")

test_that("test tiledb_array read/write sparse array with heterogenous date domains", {
  skip_if(tiledb_version(TRUE) < "2.0.0")
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
})

test_that("test tiledb_array read/write sparse array with heterogenous msec domains", {
  skip_if(tiledb_version(TRUE) < "2.0.0")
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
})


test_that("test full write-read cycle on sample data using fromDataFrame", {
  skip_if(tiledb_version(TRUE) < "2.0.0")

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
})

test_that("test full write-read cycle on sample data using schema", {
  skip_if(tiledb_version(TRUE) < "2.0.0")

  ## -- download data and extract data set, sample a portion
  ## download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip",
  ##               "/tmp/bank.zip")
  ## datfull <- read.csv(unz("/tmp/bank.zip", "bank-full.csv"), sep=";")
  ## set.seed(123)
  ## dat <- datfull[sample(nrow(datfull), 1000, replace=FALSE),]
  ## saveRDS(dat, "bankSample.rds")

  dat <- readRDS(system.file("sampledata", "bankSample.rds", package="tiledb"))

  dir.create(tmpuri <- tempfile())

  ctx <- tiledb_ctx()
  n <- nrow(dat)
  dim <- new("tiledb_dim", ptr = tiledb:::libtiledb_dim(ctx@ptr, "rows", "INT32", c(1L,n), 1L))
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
  sch@ptr <- tiledb:::libtiledb_array_schema_set_allows_dups(sch@ptr, TRUE)
  tiledb_array_create(tmpuri, sch)

  arr <- tiledb_array(tmpuri, as.data.frame=TRUE)
  ## prefix rows index
  # dat <- cbind(rows=1:n, dat)
  arr[] <- dat


  newarr <- tiledb_array(tmpuri, as.data.frame=TRUE)
  newdat <- newarr[]
  expect_equal(dat, newdat)

  unlink(tmpuri, recursive = TRUE)

})
