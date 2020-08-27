library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")

tiledb_ctx(limitTileDBCores())

#test_that("tiledb_vfs default constructor", {
vfs <- tiledb_vfs()
expect_true(is(vfs, "tiledb_vfs"))
#})

## create/remove/is/is_empty/empty bucket hard to test without credentials

#test_that("tiledb_vfs create / test / remove directory", {
vfs <- tiledb_vfs()
uri <- tempfile()

expect_equal(tiledb_vfs_create_dir(vfs, uri), uri)
## check directly
expect_true(dir.exists(uri))
## check via VFS
expect_true(tiledb_vfs_is_dir(vfs, uri))

newuri <- tempfile()
expect_equal(tiledb_vfs_move_dir(vfs, uri, newuri), newuri)
expect_equal(tiledb_vfs_remove_dir(vfs, newuri), newuri)
expect_false(dir.exists(newuri))
#})

#test_that("tiledb_vfs create / test / remove file", {
vfs <- tiledb_vfs()
uri <- tempfile()

expect_equal(tiledb_vfs_touch(vfs, uri), uri)
expect_true(tiledb_vfs_is_file(vfs, uri))

## check via VFS
expect_true(tiledb_vfs_is_file(vfs, uri))

## check via VFS
expect_equal(tiledb_vfs_file_size(vfs, uri), 0)

newuri <- tempfile()
expect_equal(tiledb_vfs_move_file(vfs, uri, newuri), newuri)
expect_equal(tiledb_vfs_remove_file(vfs, newuri), newuri)
expect_false(file.exists(newuri))
#})
