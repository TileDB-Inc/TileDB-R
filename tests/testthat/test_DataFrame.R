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

  newdf <- within(newdf, Species <- as.character(Species))
  expect_equal(irisdf, newdf)

})

