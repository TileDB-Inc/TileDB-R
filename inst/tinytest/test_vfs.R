library(tinytest)
library(tiledb)

tiledb_ctx(limitTileDBCores())

#test_that("tiledb_vfs default constructor", {
vfs <- tiledb_vfs()
expect_true(is(vfs, "tiledb_vfs"))
#})

## create/remove/is/is_empty/empty bucket hard to test without credentials

#test_that("tiledb_vfs create / test / remove directory", {

# Create/copy/move/remove directory
vfs <- tiledb_vfs()
base_path <- tempdir()
dir_uri <- file.path(base_path, "dir_uri")

## Create directory
expect_equal(tiledb_vfs_create_dir(dir_uri), dir_uri)
### check directly
expect_true(dir.exists(dir_uri))
### check via VFS
expect_true(tiledb_vfs_is_dir(dir_uri))

## Check dir size
expect_equal(tiledb_vfs_dir_size(dir_uri), 0)

## Create a file in dir_uri to make sure move_dir and copy_dir work
file_name <- "test.txt"
file_uri <- file.path(dir_uri, file_name)
expect_equal(tiledb_vfs_touch(file_uri), file_uri)

# Windows doesn't support copying directories yet
if (tolower(Sys.info()["sysname"]) != "windows") {
    ## Copy directory
    copy_dir_uri <- file.path(base_path, "copy_dir_uri")
    expect_equal(tiledb_vfs_copy_dir(dir_uri, copy_dir_uri), copy_dir_uri)

    ### Check both directories exist
    expect_true(dir.exists(dir_uri))
    expect_true(dir.exists(copy_dir_uri))

    ### Check both files exist
    copy_file_uri <- file.path(copy_dir_uri, file_name)
    expect_true(file.exists(file_uri))
    expect_true(file.exists(copy_file_uri))
}

## Move directory
move_dir_uri <- file.path(base_path, "move_dir_uri")
expect_equal(tiledb_vfs_move_dir(dir_uri, move_dir_uri), move_dir_uri)

### Check only new directory exists
expect_false(dir.exists(dir_uri))

### Check only new file exists
move_file_uri <- file.path(move_dir_uri, file_name)
expect_false(file.exists(file_uri))
expect_true(file.exists(move_file_uri))

## Remove directory
expect_equal(tiledb_vfs_remove_dir(move_dir_uri), move_dir_uri)

### Check the moved file and directory no longer exist
expect_false(dir.exists(move_dir_uri))
expect_false(file.exists(move_file_uri))

#})

# Create/copy/move/remove file

#test_that("tiledb_vfs create / test / remove file", {
vfs <- tiledb_vfs()
file_uri <- tempfile()

## Touch file
expect_equal(tiledb_vfs_touch(file_uri), file_uri)
expect_true(file.exists(file_uri))
expect_true(tiledb_vfs_is_file(file_uri))

### Check file exists via VFS
expect_true(tiledb_vfs_is_file(file_uri))

### check file size via VFS
expect_equal(tiledb_vfs_file_size(file_uri), 0)

if (tolower(Sys.info()["sysname"]) != "windows") {
    ## Copy file
    copy_file_uri <- tempfile()
    expect_equal(tiledb_vfs_copy_file(file_uri, copy_file_uri), copy_file_uri)
    expect_true(file.exists(file_uri))
    expect_true(file.exists(copy_file_uri))
}

## Move file
move_file_uri <- tempfile()
expect_equal(tiledb_vfs_move_file(file_uri, move_file_uri), move_file_uri)
expect_false(file.exists(file_uri))
expect_true(file.exists(move_file_uri))

## Remove file
expect_equal(tiledb_vfs_remove_file(move_file_uri), move_file_uri)
expect_false(file.exists(move_file_uri))

#})

if (requireNamespace("palmerpenguins", quietly=TRUE)) {
    pp <- palmerpenguins::penguins
    urirds <- tempfile()

    saveRDS(pp, urirds)
    newrds <- tempfile()

    if (Sys.info()[["sysname"]] != "Windows") {
        ## check file copy
        expect_equal(tiledb_vfs_copy_file(urirds, newrds), newrds)
        expect_equal(pp, readRDS(newrds))
    }

    uriser <- tempfile()
    expect_equal(tiledb_vfs_serialize(pp, uriser), uriser)

    ## Check file has contents
    expect_false(tiledb_vfs_file_size(uriser) == 0)

    expect_equal(pp, tiledb_vfs_unserialize(uriser))
}

if (tiledb_version(TRUE) >= '2.22.0' && nzchar(Sys.getenv("AWS_ACCESS_KEY_ID"))) {
    expect_silent(dat <- tiledb::tiledb_vfs_ls_recursive("s3://tiledb-test-arrays/1.4/customer"))
    expect_true(inherits(dat, "data.frame"))
    expect_true(nrow(dat) > 400)
}
