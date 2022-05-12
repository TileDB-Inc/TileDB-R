library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.9.0") exit_file("Needs TileDB 2.9.* or later")

res <- tiledb_filestore_schema_create()
expect_true(inherits(tiledb_filestore_schema_create(), "tiledb_array_schema"))
expect_error(tiledb_filestore_schema_create("does_not_exist"))
