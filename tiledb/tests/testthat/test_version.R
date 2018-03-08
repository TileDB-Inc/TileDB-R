library(tiledb)
context("tiledb_version")

test_that("version is valid", {
  expect_equal(tiledb_version(), c(1, 3, 0))
})
