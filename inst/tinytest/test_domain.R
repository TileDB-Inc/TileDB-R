library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

#test_that("tiledb_domain basic constructor", {
d1  <- tiledb_dim("d1", c(1L, 100L))
dom <- tiledb_domain(list(d1))
expect_true(is(dom, "tiledb_domain"))
#})

#test_that("tiledb_domain constructor works for multiple tiledb_dim's", {
d1  <- tiledb_dim("d1", c(1L, 100L))
d2  <- tiledb_dim("d2", c(1L, 100L))
d3  <- tiledb_dim("d3", c(1L, 100L))
dom <- tiledb_domain(list(d1, d2, d3))
expect_true(is(dom, "tiledb_domain"))
#})

#test_that("tiledb_domain constructor fails with no tiledb_dim", {
expect_error(tiledb_domain(list()))
#})

#test_that("tiledb_domain constructor failes with tiledb_dim of different dtypes", {
d1  <- tiledb_dim("d1", c(1L, 100L))
d2  <- tiledb_dim("d2", c(1.3, 2.8))
if (tiledb_version(compact=TRUE) < as.package_version("1.8.0")) {
  expect_error(tiledb_domain(list(d1, d2)))
} else {
  dom <- tiledb_domain(list(d1, d2))
  expect_true(is(dom, "tiledb_domain"))
}
#})

#test_that("tiledb_domain dimensions works", {
d1  <- tiledb_dim("d1", c(1L, 100L))
d2  <- tiledb_dim("d2", c(1L, 100L))
d3  <- tiledb_dim("d3", c(1L, 100L))
dom <- tiledb_domain(list(d1, d2, d3))
dims <- tiledb::dimensions(dom)
expect_equal(length(dims), 3L)
expect_true(all(as.logical(lapply(dims, function(o) is(o, "tiledb_dim")))))
#})

## more domain accessor tests
expect_equal(tiledb_ndim(dom), 3L)
expect_equal(tiledb_domain_get_dimension_from_index(dom, 0), d1)
expect_error(tiledb_domain_get_dimension_from_index(dom, 3)) # only three present
expect_equal(tiledb_domain_get_dimension_from_name(dom, "d2"), d2)
expect_error(tiledb_domain_get_dimension_from_name(dom, "doesnotexist")) # not present
expect_true(tiledb_domain_has_dimension(dom, "d3"))
expect_false(tiledb_domain_has_dimension(dom, "doesnotexist"))
