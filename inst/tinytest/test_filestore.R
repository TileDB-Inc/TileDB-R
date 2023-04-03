library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

isWindows <- Sys.info()[["sysname"]] == "Windows"

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.9.0") exit_file("Needs TileDB 2.9.* or later")

text_file <- tempfile()
writeLines(c("Simple text file.", "With two lines."), text_file)
expect_true(file.exists(text_file))
expect_true(file.info(text_file)$size > 0)

## check schema creation from no file or given file, and error from missing file
expect_true(inherits(tiledb_filestore_schema_create(), "tiledb_array_schema"))
expect_true(inherits(tiledb_filestore_schema_create(text_file), "tiledb_array_schema"))
expect_error(tiledb_filestore_schema_create("does_not_exist"))

if (isWindows) exit_file("Skip remainder as tests randomly fail")

tempuri <- tempfile()
res <- tiledb_filestore_schema_create(text_file) 					# schema from text_file
expect_silent( tiledb_array_create(tempuri, res) )                  # create array
expect_true(tiledb_filestore_uri_import(tempuri, text_file)) 		# import text_file into array
newfile <- tempfile()
expect_true(tiledb_filestore_uri_export(newfile, tempuri))

oldcntnt <- readLines(text_file)
newcntnt <- readLines(newfile)
expect_equal(oldcntnt, newcntnt)
unlink(newfile)

unlink(tempuri, recursive=TRUE)
res <- tiledb_filestore_schema_create() 							# default schema
expect_silent( tiledb_array_create(tempuri, res) )                  # create array
buf <- paste(newcntnt, collapse="\n")
expect_true(tiledb_filestore_buffer_import(tempuri, buf)) 	        # import from variable into array

expect_silent(chkbuf <- tiledb_filestore_buffer_export(tempuri))
expect_equal(chkbuf, buf)

expect_equal(tiledb_filestore_size(tempuri), nchar(buf))

## also test reading from filestore-create array via tiledb_array
n <- tiledb_filestore_size(tempuri)
arr <- tiledb_array(tempuri, return_as="asis")
reftxt <- paste(oldcntnt, collapse="\n") # collapse input file into one string
chk <- arr[0:(n-1)]$contents
expect_equal(reftxt, rawToChar(chk))
