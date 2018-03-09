library(tiledb)
context("tiledb_version")

test_that("version is valid", {
  ver = tiledb_version()
  expect_equal(length(ver), 3)
  expect_equal(ver[1], 1)
  expect_gte(ver[2], 0)
  expect_gte(ver[3], 0)
})
