library(tiledb)

checkForMetadataAndAddSome <- function(arr) {
  arr <- tiledb_array_open(arr, "READ")

  cat("Testing for metadata (using key 'aaa'):",
      ifelse(tiledb_has_metadata(arr, "aaa"), "(yes)", "(no)"), "\n")

  cat("Number of metadata objects:", tiledb_num_metadata(arr), "\n")

  cat("Write current time\n")
  arr <- tiledb_array_close(arr)
  arr <- tiledb_array_open(arr, "WRITE")
  tiledb_put_metadata(arr, "time_now", format(Sys.time()))

  arr <- tiledb_array_close(arr)
}

tmp <- "/tmp/ex2"
darr <- tiledb_dense(uri=tmp)  		# takes URI, returns S4 obj with pointer
checkForMetadataAndAddSome(darr)

tmp <- "/tmp/ex3"
sarr <- tiledb_sparse(uri=tmp)   	# takes URI, returns S4 obj with pointer
checkForMetadataAndAddSome(sarr)
