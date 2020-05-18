library(tiledb)
context("tiledb::TileDBArray")

test_that("test tiledb_array read/write sparse array with heterogenous date domains", {
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
