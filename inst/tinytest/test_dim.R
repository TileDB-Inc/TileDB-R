library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

#test_that("tiledb_dim default constructor", {
dim <- tiledb_dim("foo", c(1, 100))
expect_true(is(dim, "tiledb_dim"))
#})

#test_that("tiledb_dim throws an error on missing constructor argument", {
expect_error(tiledb_dim("foo"))
#})

#test_that("tiledb_dim throws an error on invalid domain", {
expect_error(tiledb_dim("foo", c(100L, 1L), type = "INT32"))
#})

#test_that("tiledb_dim throws an error on invalid type", {
expect_error(tiledb_dim("foo", c(1, 100), type = "INVALID"))
#})

#test_that("tiledb_dim default type is double", {
dim <- tiledb_dim("foo", c(1, 100))
expect_equal(tiledb::datatype(dim), "FLOAT64")
#})

#test_that("tiledb_dim default type is the domain type", {
dim <- tiledb_dim("foo", c(1.0, 100.0))
expect_equal(tiledb::datatype(dim), "FLOAT64")

dim <- tiledb_dim("foo", c(1L, 100L))
expect_equal(tiledb::datatype(dim), "INT32")
#})

#test_that("tiledb_dim name", {
dim <- tiledb_dim("foo", c(1L, 100L))
expect_equal(tiledb::name(dim), "foo")

dim <- tiledb_dim("", c(1L, 100L))
expect_equal(tiledb::name(dim), "")
#})

#test_that("tiledb_dim tile should equal constructor", {
dim <- tiledb_dim("foo", c(1L, 100L), tile=10L, type="INT32")
expect_equal(tiledb::tile(dim), 10L)
#})

#test_that("tiledb_dim default tile extent should span the whole domain", {

dim <- tiledb_dim("foo", c(1L, 100L), type = "INT32")
expect_equal(tiledb::tile(dim), 100L)

dim <- tiledb_dim("foo", c(1L, 1L), type = "INT32")
expect_equal(tiledb::tile(dim), 1L)

dim <- tiledb_dim("foo", c(1.1, 11.9), type = "FLOAT64")
expect_equal(tiledb::tile(dim), 11.9 - 1.1)
#})

#test_that("tiledb_dim empty name is anonymous", {

dim <- tiledb_dim("", c(1L, 100L))
expect_true(is.anonymous(dim))

dim <- tiledb_dim("foo", c(1L, 100L))
expect_false(is.anonymous(dim))
#})

#test_that("tiledb_dim tiledb::datatype()", {
dim <- tiledb_dim("", c(1L, 100L), type = "INT32")
expect_equal(tiledb::datatype(dim), "INT32")

dim <- tiledb_dim("", c(1, 100), type = "FLOAT64")
expect_equal(tiledb::datatype(dim), "FLOAT64")
#})

t#est_that("tiledb_dim dim() method", {
d <- tiledb_dim("", c(-1L, 100L))
expect_equal(dim(d), 102L)
#})
