library(tiledb)
context("tiledb_dataframe")

test_that("tiledb_fromdataframe", {
  uri <- tempfile()
  ## turn factor into character
  irisdf <- within(iris, Species <- as.character(Species))

  fromDataFrame(irisdf, uri)

  arr <- tiledb_dense(uri, as.data.frame=TRUE)
  newdf <- arr[]
  expect_equal(iris, newdf)
  expect_equal(dim(irisdf), dim(newdf))

  ## result comes back as factor by default
  newdf <- within(newdf, Species <- as.character(Species))
  expect_equal(irisdf, newdf)
})

test_that("tiledb_date_datetime_types", {
  uri <- tempfile()

  df <- data.frame(char=c("abc", "def", "g", "hijk"),
                   int=1:4,
                   dbl=sqrt(1:4),
                   date=Sys.Date() + 0:3,
                   stringsAsFactors = FALSE)

  fromDataFrame(df, uri)

  arr <- tiledb_dense(uri, as.data.frame=TRUE)
  newdf <- arr[]

  ## result comes back as factor by default
  newdf <- within(newdf, char <- as.character(char))

  expect_equal(df, newdf)
})
