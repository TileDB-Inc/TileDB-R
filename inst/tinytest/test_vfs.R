library(tinytest)
library(tiledb)

tiledb_ctx(limitTileDBCores())

#test_that("tiledb_vfs default constructor", {
vfs <- tiledb_vfs()
expect_true(is(vfs, "tiledb_vfs"))
#})

## create/remove/is/is_empty/empty bucket hard to test without credentials

#test_that("tiledb_vfs create / test / remove directory", {
vfs <- tiledb_vfs()
uri <- tempfile()

expect_equal(tiledb_vfs_create_dir(uri), uri)
## check directly
expect_true(dir.exists(uri))
## check via VFS
expect_true(tiledb_vfs_is_dir(uri))

newuri <- tempfile()
expect_equal(tiledb_vfs_move_dir(uri, newuri), newuri)
expect_equal(tiledb_vfs_remove_dir(newuri), newuri)
expect_false(dir.exists(newuri))
#})

#test_that("tiledb_vfs create / test / remove file", {
vfs <- tiledb_vfs()
uri <- tempfile()

expect_equal(tiledb_vfs_touch(uri), uri)
expect_true(tiledb_vfs_is_file(uri))

## check via VFS
expect_true(tiledb_vfs_is_file(uri))

## check via VFS
expect_equal(tiledb_vfs_file_size(uri), 0)

newuri <- tempfile()
expect_equal(tiledb_vfs_move_file(uri, newuri), newuri)
expect_equal(tiledb_vfs_remove_file(newuri), newuri)
expect_false(file.exists(newuri))

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

    expect_equal(pp, tiledb_vfs_unserialize(uriser))
}

if (tiledb_version(TRUE) >= '2.21.0' && nzchar(Sys.getenv("AWS_ACCESS_KEY_ID"))) {
    expect_silent(dat <- tiledb::tiledb_vfs_ls_recursive("s3://tiledb-test-arrays/1.4/customer"))
    expect_true(inherits(dat, "data.frame"))
    expect_true(nrow(dat) > 400)
}
