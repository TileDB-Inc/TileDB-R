#uri <- tempfile()
library(tiledb)
array_name <- "array_metadata_array"
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)
if (dir.exists(uri)) unlink(uri, recursive=TRUE)

if (!dir.exists(uri)) {                 # if that example does not exist, create new one
  dim <- tiledb_dim("dim", domain = c(1L, 10L))
  dom <- tiledb_domain(c(dim))
  a1  <- tiledb_attr("a1", type = "FLOAT64")
  a2  <- tiledb_attr("a2", type = "FLOAT64")
  sch <- tiledb_array_schema(dom, c(a1, a2), sparse=TRUE)
  tiledb_array_create(uri, sch)

  arr <- tiledb_array(uri, as.data.frame=FALSE)
}

## old initial accessors
#tiledb:::get_metadata(uri, "aaa")
#tiledb:::get_metadata(uri, "bb")
#tiledb:::get_metadata(uri, "ccc")
#tiledb:::has_metadata_simple(uri, "aaa")

#uri <- "quickstart_dense_array"
cat("Assigning uri\n")
arrR <- tiledb::tiledb_array(uri)
arrW <- tiledb::tiledb_array(uri)

#arr <- tiledb:::libtiledb_array_reopen(arr@ptr)
cat("Reopening arr\n")
arrR@ptr <- tiledb:::libtiledb_array_open_with_ptr(arrR@ptr, "READ")
#print(str(arr))

cat("Testing for metadata\n")
tiledb_has_metadata(arrR, "aaa")

cat("Number of metadata objects\n")
tiledb_num_metadata(arrR)

cat("Write current time\n")
arrW@ptr <- tiledb:::libtiledb_array_open_with_ptr(arrW@ptr, "WRITE")
tiledb_put_metadata(arrW, "time_now", format(Sys.time()))
res <- tiledb_array_close(arrW)

cat("Get time\n")
arrR <- tiledb::tiledb_array(uri)
arrR@ptr <- tiledb:::libtiledb_array_open_with_ptr(arrR@ptr, "READ")
tiledb_get_metadata(arrR, "time_now")
