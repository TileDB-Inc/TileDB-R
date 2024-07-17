library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

if (get_return_as_preference() != "asis") set_return_as_preference("asis") 		# baseline value

#test_that("tiledb_array_schema default constructor works", {
d1  <- tiledb_dim("d1", domain=c(1L, 100L))
dom <- tiledb_domain(c(d1))
a1  <- tiledb_attr(type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(a1))
expect_true(is(sch, "tiledb_array_schema"))
#})

#test_that("tiledb_array_schema default constructor arugment values are correct",  {
d1  <- tiledb_dim("d1", domain = c(1L, 100L))
d2  <- tiledb_dim("d2", domain = c(1L, 100L))
dom <- tiledb_domain(c(d1, d2))
a1  <- tiledb_attr(type = "FLOAT64")
sch <- tiledb_array_schema(dom, c(a1))

## test domain
expect_true(is(domain(sch), "tiledb_domain"))

## test dimensions
ds <- tiledb::dimensions(sch)
expect_equal(length(ds), 2)
expect_true(is(ds[[1]], "tiledb_dim"))
expect_true(is(ds[[2]], "tiledb_dim"))

## test attrs
as <- tiledb::attrs(sch)
expect_equal(length(as), 1)
expect_true(is(as[[1]], "tiledb_attr"))

## test that default R schema is COL_MAJOR
expect_equal(tiledb::cell_order(sch), "COL_MAJOR")
expect_equal(tiledb::tile_order(sch), "COL_MAJOR")

## test that the default R schema is dense
expect_false(is.sparse(sch))
#})

#test_that("tiledb_array_schema full constructor argument values are correct",  {
d1  <- tiledb_dim("d1", domain = c(1L, 100L))
d2  <- tiledb_dim("d2", domain = c(1L, 100L))
d3  <- tiledb_dim("d3", domain = c(1L, 100L))

dom <- tiledb_domain(c(d1, d2, d3))

a1  <- tiledb_attr("attribute1", type = "FLOAT64")
a2  <- tiledb_attr("attribute2", type = "INT32")

sch <- tiledb_array_schema(dom, c(a1, a2),
                           cell_order = "ROW_MAJOR",
                           tile_order = "ROW_MAJOR",
                           coords_filter_list = tiledb_filter_list(c(tiledb_filter("GZIP"))),
                           offsets_filter_list = tiledb_filter_list(c(tiledb_filter("ZSTD"))),
                           sparse = TRUE)

## test domain
expect_true(is(domain(sch), "tiledb_domain"))

## test dimensions
ds <- tiledb::dimensions(sch)
expect_equal(length(ds), 3)
expect_true(is(ds[[1]], "tiledb_dim"))
expect_true(is(ds[[2]], "tiledb_dim"))
expect_true(is(ds[[3]], "tiledb_dim"))

## test attrs
as <- tiledb::attrs(sch)
expect_equal(length(as), 2)
expect_equal(names(as), c("attribute1", "attribute2"))
expect_true(is(as[[1]], "tiledb_attr"))
expect_true(is(as[[2]], "tiledb_attr"))

expect_equal(tiledb::cell_order(sch), "ROW_MAJOR")
expect_equal(tiledb::tile_order(sch), "ROW_MAJOR")

filter_list <- tiledb::filter_list(sch)
expect_equal(tiledb_filter_type(filter_list[["coords"]][0]), "GZIP")
expect_equal(tiledb_filter_get_option(filter_list[["coords"]][0], "COMPRESSION_LEVEL"), -1)
expect_equal(tiledb_filter_type(filter_list[["offsets"]][0]), "ZSTD")
expect_equal(tiledb_filter_get_option(filter_list[["offsets"]][0], "COMPRESSION_LEVEL"), -1)

expect_true(is.sparse(sch))

tiledb:::libtiledb_array_schema_set_capacity(sch@ptr, 100000)
expect_equal(tiledb:::libtiledb_array_schema_get_capacity(sch@ptr), 100000)
expect_error(tiledb:::libtiledb_array_schema_set_capacity(sch@ptr, -10))

#})


#test_that("tiledb_array_schema created with encryption",  {
dir.create(uri <- tempfile())
key <- "0123456789abcdeF0123456789abcdeF"

oldconfig <- config <- tiledb_config()
config <- tiledb_config()
config["sm.encryption_type"] <- "AES_256_GCM";
config["sm.encryption_key"] <- key
ctx <- tiledb_ctx(config)

## quick and direct test write with encryption
expect_silent( fromDataFrame(palmerpenguins::penguins, uri) )

## check access, here just schema return and number of rows when read
expect_true(is(schema(uri), "tiledb_array_schema"))
expect_equal(nrow(tiledb_array(uri, return_as="data.frame")[]), 344)

unlink(uri, recursive=TRUE)

## previous test
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))
schema <- tiledb_array_schema(dom, attrs = c(tiledb_attr("a", type = "INT32")))
tiledb_array_create(uri, schema, key)
expect_true(is(schema(uri), "tiledb_array_schema"))

unlink(uri, recursive=TRUE)

ctx <- tiledb_ctx(oldconfig)  # reset to no encryption via previous config


#})

#test_that("tiledb_array_schema dups setter/getter",  {
dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                              tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))
sch <- tiledb_array_schema(dom,
                           attrs = c(tiledb_attr("a", type = "INT32")),
                           sparse = TRUE)

## false by default
expect_false(allows_dups(sch))

## true once set to true
allows_dups(sch) <- TRUE
expect_true(allows_dups(sch))
#})
