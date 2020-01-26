
library(tiledb)

uri <- tempfile()
#uri <- "array_metadata_array"

if (!dir.exists(uri)) {                 # if that example does not exist, create new one
  dim <- tiledb_dim(domain = c(1L, 10L))
  dom <- tiledb_domain(c(dim))
  a1  <- tiledb_attr("a1", type = "FLOAT64")
  a2  <- tiledb_attr("a2", type = "FLOAT64")
  sch <- tiledb_array_schema(dom, c(a1, a2), sparse=TRUE)
  tiledb_array_create(uri, sch)

  arr <- tiledb_sparse(uri, as.data.frame=FALSE)
}

arr <- tiledb_array_open(arr, "READ")

cat("Testing for metadata\n")
print(tiledb_has_metadata(arr, "aa"))

cat("Number of metadata objects\n")
tiledb_num_metadata(arr)

cat("Write current time\n")
arr <- tiledb_array_close(arr)
arr <- tiledb_array_open(arr, "WRITE")
tiledb_put_metadata(arr, "time_now", format(Sys.time()))

cat("Get time\n")
arr <- tiledb_array_open(arr, "READ")
tiledb_get_metadata(arr, "time_now")

unlink(uri, recursive=TRUE)
