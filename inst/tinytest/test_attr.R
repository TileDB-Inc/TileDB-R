library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

#test_that("tiledb_attr constructor works", {
a1 <- tiledb_attr(type = "FLOAT64")
expect_true(is(a1, "tiledb_attr"))
#})

#test_that("tiledb_attr constructor defaults are correct", {
a1 <- tiledb_attr(type = "FLOAT64")
expect_equal(tiledb::name(a1), "")
expect_true(is.anonymous(a1))
expect_equal(tiledb::datatype(a1), "FLOAT64")
expect_equal(tiledb::cell_val_num(a1), 1)
#})

#test_that("tiledb_attr is.anonymous is correct", {
a1  <- tiledb_attr("", , type = "FLOAT64")
expect_true(is.anonymous(a1))
a2  <- tiledb_attr("foo", type = "FLOAT64")
expect_false(is.anonymous(a2))
#})

#test_that("tiledb_attr with compression", {
a1 <- tiledb_attr("foo", type = "FLOAT64", filter_list = tiledb_filter_list(c(tiledb_filter("GZIP"))))
filter_list <- tiledb::filter_list(a1)
expect_true(is(filter_list, "tiledb_filter_list"))
expect_equal(tiledb_filter_type(filter_list[0]), "GZIP")
expect_equal(tiledb_filter_get_option(filter_list[0], "COMPRESSION_LEVEL"), -1)

expect_error(tiledb_attr("foo", compressor = tiledb_compressor("UNKNOWN", -1)))
#})

#test_that("tiledb_attr throws an error with invalid ncells argument", {
a1 <- tiledb_attr("foo", type = "FLOAT64", ncells = 1)
expect_equal(tiledb::cell_val_num(a1), 1)
expect_error(tiledb_attr("foo", ncells = 0))
#})

#test_that("tiledb_attr set ncells", {
attrs <- tiledb_attr("a", type = "INT32", ncells = 1)
expect_equal(tiledb::cell_val_num(attrs), 1) # as created

tiledb:::libtiledb_attribute_set_cell_val_num(attrs@ptr, 2)
expect_equal(tiledb::cell_val_num(attrs), 2) # as created

tiledb:::libtiledb_attribute_set_cell_val_num(attrs@ptr, NA_integer_)
expect_true(is.na(tiledb::cell_val_num(attrs)))
#})

t#est_that("tiledb_attr set fill", {
if (tiledb_version(TRUE) < "2.1.0") exit_file("Needs TileDB 2.1.* or later")

## test for default
dom <- tiledb_domain(dims = tiledb_dim("rows", c(1L, 4L), 4L, "INT32"))
attr <- tiledb_attr("a", type = "INT32")
sch <- tiledb_array_schema(dom, attr)

uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
tiledb_array_create(uri, sch)

arr <- tiledb_dense(uri)
val <- arr[]
## when no value has been set, expect NA
##expect_equal(val, array(rep(NA, 4)))
expect_true(length(val) == 4)
expect_true(all(is.na(val)))

## test for value set
dom <- tiledb_domain(dims = tiledb_dim("rows", c(1L, 4L), 4L, "INT32"))
attr <- tiledb_attr("a", type = "INT32")
tiledb:::libtiledb_attribute_set_fill_value(attr@ptr, 42L)
sch <- tiledb_array_schema(dom, attr)
uri <- tempfile()
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
tiledb_array_create(uri, sch)
arr <- tiledb_dense(uri)
val <- arr[]
## when value has been set, expect value
expect_equal(val, array(rep(42, 4)))

expect_equal(tiledb:::libtiledb_attribute_get_fill_value(attr@ptr), 42)

if (dir.exists(uri)) unlink(uri, recursive=TRUE)
#})
