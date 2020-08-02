library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

#test_that("tiledb_filter_list default constructor", {
flt <- tiledb_filter_list()
expect_true(is(flt, "tiledb_filter_list"))
#})

#test_that("tiledb_filter_list accepts filters in constructor", {
flt <- tiledb_filter()
filter_list = tiledb_filter_list(c(flt))
expect_equal(nfilters(filter_list), 1)
#})

#test_that("tiledb_filter_list can set and get max_chunk_size", {
flt <- tiledb_filter()
filter_list = tiledb_filter_list(c(flt))
expect_equal(nfilters(filter_list), 1)
set_max_chunk_size(filter_list, 10)
expect_equal(max_chunk_size(filter_list), 10)
#})
