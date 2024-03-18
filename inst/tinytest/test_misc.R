
library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

if (is.null(get0("tiledb_error_message"))) exit_file("No 'tiledb_error_message'")
expect_true(inherits(tiledb_error_message(), "character"))
expect_true(is.character(tiledb_error_message()))
