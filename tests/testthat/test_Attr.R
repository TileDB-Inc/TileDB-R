library(tiledb)
context("tiledb_attr")

test_that("tiledb_attr constructor works", {
  a1 <- tiledb_attr(type = "FLOAT64")
  expect_is(a1, "tiledb_attr")
})

test_that("tiledb_attr constructor defaults are correct", {
  a1 <- tiledb_attr(type = "FLOAT64")
  expect_equal(tiledb::name(a1), "")
  expect_true(is.anonymous(a1))
  expect_equal(tiledb::datatype(a1), "FLOAT64")
  expect_equal(tiledb::ncells(a1), 1)
})

test_that("tiledb_attr is.anonymous is correct", {
  a1  <- tiledb_attr("", , type = "FLOAT64")
  expect_true(is.anonymous(a1))
  a2  <- tiledb_attr("foo", type = "FLOAT64")
  expect_false(is.anonymous(a2))
})

test_that("tiledb_attr with compression", {
  a1 <- tiledb_attr("foo", type = "FLOAT64", filter_list = tiledb_filter_list(c(tiledb_filter("GZIP"))))
  filter_list <- tiledb::filter_list(a1)
  expect_is(filter_list, "tiledb_filter_list")
  expect_equal(tiledb_filter_type(filter_list[0]), "GZIP")
  expect_equal(tiledb_filter_get_option(filter_list[0], "COMPRESSION_LEVEL"), -1)

  expect_error(tiledb_attr("foo", compressor = tiledb_compressor("UNKNOWN", -1)))
})

test_that("tiledb_attr throws an error with invalid ncells argument", {
  a1 <- tiledb_attr("foo", type = "FLOAT64", ncells = 1)
  expect_equal(tiledb::ncells(a1), 1)
  expect_error(tiledb_attr("foo", ncells = 0))
})

test_that("tiledb_attr set ncells", {
  attrs <- tiledb_attr("a", type = "INT32", ncells = 1)
  expect_equal(tiledb::ncells(attrs), 1) # as created

  tiledb:::libtiledb_attr_set_cell_val_num(attrs@ptr, 2)
  expect_equal(tiledb::ncells(attrs), 2) # as created

  tiledb:::libtiledb_attr_set_cell_val_num(attrs@ptr, NA_integer_)
  expect_true(is.na(tiledb::ncells(attrs)))
})
