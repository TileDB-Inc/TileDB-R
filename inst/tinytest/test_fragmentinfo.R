library(tinytest)
library(tiledb)

isMacOS <- (Sys.info()['sysname'] == "Darwin")

ctx <- tiledb_ctx(limitTileDBCores())

uri <- tempfile()
if (dir.exists(uri)) unlink(uri, TRUE)

## create simple array
set.seed(123)
D1 <- data.frame(keys = 1:10,
                 groups = replicate(10, paste0(sample(LETTERS[1:4], 3), collapse="")),
                 vals = sample(100, 10, TRUE))
fromDataFrame(D1, uri, col_index=1:2, sparse=TRUE, tile_domain=list(keys=c(1L, 1000L)))

fraginf <- tiledb_fragment_info(uri)

expect_true(isS4(fraginf))
expect_true(is(fraginf@ptr, "externalptr"))

furi <- tiledb_fragment_info_uri(fraginf, 0)
expect_true(is.character(furi))
#expect_true(tiledb_vfs_is_dir(furi))

ned <- tiledb_fragment_info_get_non_empty_domain_index(fraginf, 0, 0)
expect_equal(ned, c(1, 10))
ned <- tiledb_fragment_info_get_non_empty_domain_name(fraginf, 0, "keys")
expect_equal(ned, c(1, 10))
ned <- tiledb_fragment_info_get_non_empty_domain_index(fraginf, 0, 0)

ned <- tiledb_fragment_info_get_non_empty_domain_var_index(fraginf, 0, 1)
expect_true(is.character(ned))
expect_equal(length(ned), 2)

expect_equal(tiledb_fragment_info_get_num(fraginf), 1)
expect_true(tiledb_fragment_info_get_size(fraginf, 0) > 2000) # 2389 on my machine ... but may vary

expect_false(tiledb_fragment_info_dense(fraginf, 0))
expect_true(tiledb_fragment_info_sparse(fraginf, 0))

rng <- tiledb_fragment_info_get_timestamp_range(fraginf, 0)
expect_true(inherits(rng, "POSIXt"))
expect_equal(length(rng), 2)
expect_equal(as.Date(rng[1]), as.Date(as.POSIXlt(Sys.time(), tz="UTC")))  # very coarse :)
expect_true(as.numeric(difftime(Sys.time(), rng[1], "secs")) < 10) # ten is very conservative but when this runs under valgrind it can be slooooooooow
expect_equal(tiledb_fragment_info_get_cell_num(fraginf, 0), 10)

expect_true(tiledb_fragment_info_get_version(fraginf, 0) > 5) # we may test with older core libs

expect_false(tiledb_fragment_info_has_consolidated_metadata(fraginf, 0))

expect_equal(tiledb_fragment_info_get_to_vacuum_num(fraginf), 0)

D2 <- data.frame(keys = 11:20,
                 groups = replicate(10, paste0(sample(LETTERS[1:4], 3), collapse="")),
                 vals = sample(100, 10, TRUE))
arr <- tiledb_array(uri, "WRITE")
arr[] <- D2

array_consolidate(uri)                  # written twice so consolidate

rm(fraginf)
fraginf <- tiledb_fragment_info(uri)
expect_equal(tiledb_fragment_info_get_num(fraginf), 1)
expect_equal(tiledb_fragment_info_get_to_vacuum_num(fraginf), 2)
expect_true(nchar(tiledb_fragment_info_get_to_vacuum_uri(fraginf, 0)) > 20)
expect_true(nchar(tiledb_fragment_info_get_to_vacuum_uri(fraginf, 1)) > 20)
expect_false(tiledb_fragment_info_has_consolidated_metadata(fraginf, 0))
expect_equal(tiledb_fragment_info_get_unconsolidated_metadata_num(fraginf), 1)
