#uri <- tempfile()
uri <- "array_metadata_array"

if (!dir.exists(uri)) {                 # if that example does not exist, create new one
  dim <- tiledb_dim("dim", domain = c(1L, 10L))
  dom <- tiledb_domain(c(dim))
  a1  <- tiledb_attr("a1", type = "FLOAT64")
  a2  <- tiledb_attr("a2", type = "FLOAT64")
  sch <- tiledb_array_schema(dom, c(a1, a2))
  tiledb_array_create(uri, sch)

  arr <- tiledb_dense(uri, as.data.frame=FALSE)
}

## old initial accessors
#tiledb:::get_metadata(uri, "aaa")
#tiledb:::get_metadata(uri, "bb")
#tiledb:::get_metadata(uri, "ccc")
#tiledb:::has_metadata_simple(uri, "aaa")

#uri <- "quickstart_dense_array"
cat("Assigning uri\n")
arrR <- tiledb::tiledb_sparse(uri)
arrW <- tiledb::tiledb_sparse(uri)

#arr <- tiledb:::libtiledb_array_reopen(arr@ptr)
cat("Reopening arr\n")
arrR <- tiledb:::libtiledb_array_open_with_ptr(arrR@ptr, "READ")
#print(str(arr))

cat("Testing for metadata\n")
tiledb:::has_metadata(arrR, "aaa")

cat("Number of metadata objects\n")
tiledb:::num_metadata(arrR)

cat("Write current time\n")
arrW <- tiledb:::libtiledb_array_open_with_ptr(arrW@ptr, "WRITE")
tiledb:::put_metadata(arrW, "time_now", format(Sys.time()))

cat("Get time\n")
tiledb:::get_metadata(arrR, "time_now")
