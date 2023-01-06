library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

ctx <- tiledb_ctx(limitTileDBCores())

#test_that("tiledb_filter default constructor", {
flt <- tiledb_filter()
expect_true(is(flt, "tiledb_filter"))
#})

#test_that("tiledb_filter defaults to no filter", {
flt <- tiledb_filter()
expect_equal(tiledb_filter_type(flt), "NONE")
#})

#test_that("tiledb_filter name is correct", {
name_list <- c("NONE",
               "GZIP",
               "ZSTD",
               "LZ4",
               "RLE",
               "BZIP2",
               "DOUBLE_DELTA",
               "BIT_WIDTH_REDUCTION",
               "BITSHUFFLE",
               "BYTESHUFFLE",
               "POSITIVE_DELTA",
               "CHECKSUM_MD5",
               "CHECKSUM_SHA256")
for (name in name_list) {
  flt <- tiledb_filter(name)
  expect_equal(tiledb_filter_type(flt), name)

}
name_list <- c("DICTIONARY_ENCODING")
for (name in name_list) {
    if (tiledb_version(TRUE) >= "2.9.0") {
        flt <- tiledb_filter(name)
        expect_equal(tiledb_filter_type(flt), name)
    } else {
        expect_error(tiledb_filter(name))
    }
}

expect_error(tiledb_filter("UNKNOWN"))
#})

#test_that("tiledb_filter set compression level", {
name_list <- c("GZIP",
               "ZSTD",
               "LZ4",
               "RLE",
               "BZIP2")
for (name in name_list) {
  flt <- tiledb_filter(name)
  expect_equal(tiledb_filter_type(flt), name)
  tiledb_filter_set_option(flt, "COMPRESSION_LEVEL", 10)
  expect_equal(tiledb_filter_get_option(flt, "COMPRESSION_LEVEL"), 10)
}
#})

#test_that("tiledb_filter set bit width max window", {
flt <- tiledb_filter("BIT_WIDTH_REDUCTION")
tiledb_filter_set_option(flt, "BIT_WIDTH_MAX_WINDOW", 10)
expect_equal(tiledb_filter_get_option(flt, "BIT_WIDTH_MAX_WINDOW"), 10)
#})

#test_that("tiledb_filter positive delta max window", {
flt <- tiledb_filter("POSITIVE_DELTA")
tiledb_filter_set_option(flt, "POSITIVE_DELTA_MAX_WINDOW", 10)
expect_equal(tiledb_filter_get_option(flt, "POSITIVE_DELTA_MAX_WINDOW"), 10)
#})

## add some bulk checking for filters
name_list <- c("NONE",
               "GZIP",
               "ZSTD",
               "LZ4",
               "RLE",
               "BZIP2",
               #"DOUBLE_DELTA",			# cannot be used with floating point data
               "BIT_WIDTH_REDUCTION",
               "BITSHUFFLE",
               "BYTESHUFFLE",
               "CHECKSUM_MD5",
               "CHECKSUM_SHA256"
               #"DICTIONARY_ENCODING",  # cannot be used here, see below for explicit block
               #"SCALE_FLOAT"           # cannot be used here, see below for explicit block
               )

if (!requireNamespace("palmerpenguins", quietly=TRUE)) exit_file("remainder needs 'palmerpenguins'")
dat <- palmerpenguins::penguins

## we have seen some test setups fail and suspect lack of AVX2
if (Sys.info()[["sysname"]]=="Linux" && isFALSE(any(grepl("avx2", readLines("/proc/cpuinfo")))))
    exit_file("Skipping remainder on Linux systems without AVX2")

vfs <- tiledb_vfs()                     # use an explicit VFS instance for the ops in loop over filters
for (name in name_list) {
    dat2 <- dat
    basepath <- file.path(tempdir())
    uri <- file.path(basepath, name)
    fromDataFrame(dat2, uri, filter=name)

    if (is.na(match(name, c("NONE", "BITSHUFFLE", "BYTESHUFFLE",
                            "CHECKSUM_MD5", "CHECKSUM_SHA256", "RLE")))) {
        size_none <- tiledb_vfs_dir_size(file.path(basepath, "NONE"), vfs)
        expect_true(size_none > 0)
        size_curr <- tiledb_vfs_dir_size(uri, vfs)
        expect_true(size_curr > 0)
        expect_true(size_curr < size_none)
        #message("None ", size_none, " vs ", name, " ", size_curr)
    }
}

## add some bulk checking for filters on character data
name_list <- c("NONE",
               "RLE",
               "DICTIONARY_ENCODING")
for (name in name_list) {
    dat3 <- data.frame(SP=as.character(dat$species),
                       IS=as.character(dat$island),
                       SX=as.character(dat$sex))
    if (tiledb_version(TRUE) < "2.8.0") next                 # skip if not 2.8.0 or later
    if (name == "DICTIONARY_ENCODING") {
        if (tiledb_version(TRUE) < "2.9.0") next             # skip if not 2.9.0 or later
        dat2 <- dat2[, sapply(dat2, class) == "character"]
    }
    basepath <- file.path(tempdir())
    uri <- file.path(basepath, name)
    if (dir.exists(uri)) unlink(uri, recursive=TRUE)
    fromDataFrame(dat3, uri, filter=name)

    if (is.na(match(name, c("NONE")))) {
        size_none <- tiledb_vfs_dir_size(file.path(basepath, "NONE"), vfs)
        expect_true(size_none > 0)
        size_curr <- tiledb_vfs_dir_size(uri, vfs)
        expect_true(size_curr > 0)
        expect_true(size_curr < size_none)
        #message("None ", size_none, " vs ", name, " ", size_curr)
    }
}

## add some bulk checking for filters on float/double
name_list <- c("NONE",
               "SCALE_FLOAT")
for (name in name_list) {
    dat4 <- data.frame(BL=dat$bill_length_mm,
                       BD=dat$bill_depth_mm,
                       FL=as.double(dat$flipper_length_mm),
                       BM=as.double(dat$body_mass_g))
    if (name == "SCALE_FLOAT") {
        if (tiledb_version(TRUE) < "2.11.0") next            # skip if not 2.11.0 or later
    }
    basepath <- file.path(tempdir())
    uri <- file.path(basepath, name)
    if (dir.exists(uri)) unlink(uri, recursive=TRUE)
    fromDataFrame(dat4, uri, filter=name)

    if (is.na(match(name, c("NONE")))) {
        size_none <- tiledb_vfs_dir_size(file.path(basepath, "NONE"), vfs)
        expect_true(size_none > 0)
        size_curr <- tiledb_vfs_dir_size(uri, vfs)
        expect_true(size_curr > 0)
        #expect_true(size_curr < size_none)
        #message("None ", size_none, " vs ", name, " ", size_curr)
    }
}

## scale_float with parameters (cf test in TileDB-Py PR #1195)
if (tiledb_version(TRUE) >= "2.11.0") {
    dat4 <- data.frame(BL=dat$bill_length_mm,
                       BD=dat$bill_depth_mm,
                       FL=as.double(dat$flipper_length_mm),
                       BM=as.double(dat$body_mass_g))
    pars <- expand.grid(factor=c(1.0,0.5,2.0),
                        offset=0.0,
                        bytewidth=bit64::as.integer64(c(1,8)))
    for (i in seq_len(nrow(pars))) {
        flt <- tiledb_filter("SCALE_FLOAT")
        tiledb_filter_set_option(flt, "SCALE_FLOAT_FACTOR", pars[i, "factor"])
        tiledb_filter_set_option(flt, "SCALE_FLOAT_OFFSET", pars[i, "offset"])
        tiledb_filter_set_option(flt, "SCALE_FLOAT_BYTEWIDTH", pars[i, "bytewidth"])

        uri <- file.path(tempdir())
        fromDataFrame(dat4, uri, filter_list=tiledb_filter_list(flt))
        res <- tiledb_array(uri, return_as="data.frame", extended=FALSE)[]
        expect_equivalent(dat4, res)
        if (dir.exists(uri)) unlink(uri, recursive=TRUE)
    }
    if (!dir.exists(tempdir())) dir.create(tempdir())
}

if (tiledb_version(TRUE) >= "2.12.0") {
    D <- data.frame(index=sample(100, 26, FALSE),
                    key=LETTERS,
                    value=cumsum(runif(26)))
    uri <- file.path(tempdir())
    fromDataFrame(D, uri, filter="FILTER_XOR")
    arr <- tiledb_array(uri, return_as="data.frame", extended=FALSE)[]
    expect_equal(D$value, arr$value)

    if (dir.exists(uri)) unlink(uri, recursive=TRUE)
    if (!dir.exists(tempdir())) dir.create(tempdir())
}

rm(vfs)
