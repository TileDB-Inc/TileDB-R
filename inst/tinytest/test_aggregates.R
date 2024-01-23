library(tinytest)
library(tiledb)

if (tiledb_version(TRUE) < "2.18.0") exit_file("Needs TileDB 2.18.0 or later")
if (!requireNamespace("palmerpenguins", quietly=TRUE)) exit_file("Remainder needs 'palmerpenguins'")

tiledb_ctx(limitTileDBCores())

library(palmerpenguins)
uri <- tempfile()
expect_silent(fromDataFrame(penguins, uri, sparse=TRUE))

expect_silent(arr <- tiledb_array(uri, extended=FALSE))

if (Sys.getenv("CI", "") != "") { 		# error handler needs a correction so skipping until that is made
    expect_error(tiledb_array_apply_aggregate(uri, "body_mass_g", "Mean"))             # not an array
    expect_error(tiledb_array_apply_aggregate(arr, "does_not_exit", "Mean"))           # not an attribute
    expect_error(tiledb_array_apply_aggregate(arr, "body_mass_g", "UnknownFunction"))  # not an operator
}

expect_equal(tiledb_array_apply_aggregate(arr, "body_mass_g", "Count", FALSE), 344)
expect_equal(tiledb_array_apply_aggregate(arr, "body_mass_g", "NullCount"), 2)
expect_equal(tiledb_array_apply_aggregate(arr, "body_mass_g", "Min"), 2700)
expect_equal(tiledb_array_apply_aggregate(arr, "body_mass_g", "Max"), 6300)
expect_equal(tiledb_array_apply_aggregate(arr, "body_mass_g", "Sum"), 1437000)
expect_equal(tiledb_array_apply_aggregate(arr, "body_mass_g", "Mean"), 4201.7543869)

expect_equal(tiledb_array_apply_aggregate(arr, "year", "Count", FALSE), 344)
expect_error(tiledb_array_apply_aggregate(arr, "year", "NullCount"))  # no nullcount on non-nullable
expect_equal(tiledb_array_apply_aggregate(arr, "year", "Min", FALSE), 2007)
expect_equal(tiledb_array_apply_aggregate(arr, "year", "Max", FALSE), 2009)
expect_equal(tiledb_array_apply_aggregate(arr, "year", "Sum", FALSE), 690762)
expect_equal(tiledb_array_apply_aggregate(arr, "year", "Mean", FALSE), 2008.02906977)


## also test on dense array via query argument
if (dir.exists(uri)) unlink(uri, recursive=TRUE)
fromDataFrame(palmerpenguins::penguins, uri, sparse=FALSE)
arr <- tiledb_array(uri, extended=FALSE)
expect_error(tiledb_array_apply_aggregate(arr, "body_mass_g", "Min"))  # not via _array_ on dense
qry <- tiledb_query(arr, "READ")
qry <- tiledb_query_set_subarray(qry, c(1L, 344L)) 		# but with subarray via query
expect_equal(tiledb_query_apply_aggregate(qry, "body_mass_g", "Min"), 2700)
qry <- tiledb_query(arr, "READ")
qry <- tiledb_query_set_subarray(qry, c(1L, 344L)) 		# but with subarray via query
expect_equal(tiledb_query_apply_aggregate(qry, "body_mass_g", "Max"), 6300)
