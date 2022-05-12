library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.9.0") exit_file("Needs TileDB 2.9.* or later")

text_file <- system.file("tinytest", "testdata", "text.txt", package="tiledb")
expect_true(file.exists(text_file))
expect_true(file.info(text_file)$size > 0)

## check schema creation from no file or given file, and error from missing file
expect_true(inherits(tiledb_filestore_schema_create(), "tiledb_array_schema"))
expect_true(inherits(tiledb_filestore_schema_create(text_file), "tiledb_array_schema"))
expect_error(tiledb_filestore_schema_create("does_not_exist"))


tempuri <- tempfile()
res <- tiledb_filestore_schema_create(text_file) 					# schema from text_file
expect_silent( tiledb_array_create(tempuri, res) )                  # create array
expect_true( tiledb_filestore_uri_import(tempuri, text_file) ) 	# import text_file into array
newfile <- tempfile()
expect_true( tiledb_filestore_uri_export(newfile, tempuri) )

oldcntnt <- readLines(text_file)
newcntnt <- readLines(newfile)
expect_equal(oldcntnt, newcntnt)
