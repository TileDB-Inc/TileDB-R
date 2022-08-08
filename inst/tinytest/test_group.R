library(tinytest)
library(tiledb)

isOldWindows <- Sys.info()[["sysname"]] == "Windows" && grepl('Windows Server 2008', osVersion)
if (isOldWindows) exit_file("skip this file on old Windows releases")
isWindows <- Sys.info()[["sysname"]] == "Windows"
isMacOS <- (Sys.info()['sysname'] == "Darwin")

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.8.0") exit_file("TileDB Group requires TileDB 2.8.* or later")

uri <- tempfile()

chk <- tiledb_group_create(uri)
expect_equal(chk, uri)           # returns uri
expect_true(dir.exists(chk))

## instantiate and check properties
grp <- tiledb_group(uri)
expect_true(is(grp, "tiledb_group"))
expect_true(is(grp@ptr, "externalptr"))
expect_true(tiledb_group_is_open(grp))
if (!isWindows && !isMacOS) expect_true(grepl(uri, tiledb_group_uri(grp)))
expect_equal(tiledb_group_query_type(grp), "READ")

## do not reopen already opened
expect_error(grp <- tiledb_group_open(grp, "READ"))    # can't re-open what is open
expect_true(tiledb_group_is_open(grp))

cfg <- tiledb_group_get_config(grp)
expect_true(is(cfg, "tiledb_config"))
grp <- tiledb_group_set_config(grp, cfg)

## close, re-open to write
grp <- tiledb_group_close(grp)
expect_false(tiledb_group_is_open(grp))
grp <- tiledb_group_open(grp, "WRITE")
expect_true(tiledb_group_is_open(grp))
expect_equal(tiledb_group_query_type(grp), "WRITE")

## put metadata in
tiledb_group_put_metadata(grp, "mykey", 42L)
tiledb_group_put_metadata(grp, "otherkey", 1.2345)
tiledb_group_put_metadata(grp, "lastkey", "quick brown fox")

## close, re-open to read
grp <- tiledb_group_close(grp)
expect_false(tiledb_group_is_open(grp))
grp <- tiledb_group_open(grp, "READ")
expect_true(tiledb_group_is_open(grp))
expect_equal(tiledb_group_query_type(grp), "READ")

## check on metadata writes (and use 'equivalent' to drop the attribute with key we return)
expect_true(tiledb_group_has_metadata(grp, "mykey"))
expect_equivalent(tiledb_group_get_metadata(grp, "mykey"), 42L)
expect_equal(attr(tiledb_group_get_metadata(grp, "mykey"), "key"), "mykey")
expect_true(tiledb_group_has_metadata(grp, "otherkey"))
expect_equivalent(tiledb_group_get_metadata(grp, "otherkey"), 1.2345)
expect_equal(attr(tiledb_group_get_metadata(grp, "otherkey"), "key"), "otherkey")
expect_true(tiledb_group_has_metadata(grp, "lastkey"))
expect_equivalent(tiledb_group_get_metadata(grp, "lastkey"), "quick brown fox")
expect_equal(attr(tiledb_group_get_metadata(grp, "lastkey"), "key"), "lastkey")
expect_equal(tiledb_group_metadata_num(grp), 3)

expect_equivalent(tiledb_group_get_metadata_from_index(grp, 0), "quick brown fox")
expect_equivalent(tiledb_group_get_metadata_from_index(grp, 1), 42L)
expect_equivalent(tiledb_group_get_metadata_from_index(grp, 2), 1.2345)

lst <- tiledb_group_get_all_metadata(grp)
expect_equal(length(lst), 3)
expect_true("mykey" %in% names(lst))
expect_true("otherkey" %in% names(lst))
expect_true("lastkey" %in% names(lst))
expect_equivalent(lst[["mykey"]], 42L)
expect_equivalent(lst[["otherkey"]], 1.2345)
expect_equivalent(lst[["lastkey"]], "quick brown fox")

## close, re-open to write, delete one
grp <- tiledb_group_close(grp)
grp <- tiledb_group_open(grp, "WRITE")
grp <- tiledb_group_delete_metadata(grp, "otherkey")
grp <- tiledb_group_close(grp)

#grp2 <- tiledb_group(uri)
#expect_equal(tiledb_group_metadata_num(grp2), 2)
#expect_false(tiledb_group_has_metadata(grp2, "otherkey"))

## create some temp arrays to adds as groups
uri1 <- file.path(uri, "anny")
uri2 <- file.path(uri, "bob")
uri3 <- file.path(uri, "chloe")
uri4 <- file.path(uri, "dave")
df1 <- data.frame(val=seq(100, 200, by=10))
df2 <- data.frame(letters=letters)
df3 <- data.frame(nine=rep(9L, 9))
df4 <- data.frame(dat=c(1.1, 2.2, 3.3))
tiledb::fromDataFrame(df1, uri1)
tiledb::fromDataFrame(df2, uri2)
tiledb::fromDataFrame(df3, uri3)
tiledb::fromDataFrame(df4, uri4)

## add member
grp <- tiledb_group_open(grp, "WRITE")
grp <- tiledb_group_add_member(grp, uri1, FALSE) 					# use absolute URL
grp <- tiledb_group_close(grp)
grp <- tiledb_group_open(grp, "READ")
expect_equal(tiledb_group_member_count(grp), 1)

grp <- tiledb_group_close(grp)
grp <- tiledb_group_open(grp, "WRITE")
grp <- tiledb_group_add_member(grp, "bob", TRUE) 					# use relative URL
grp <- tiledb_group_add_member(grp, "chloe", TRUE, "name_is_chloe") # and an optional name
grp <- tiledb_group_add_member(grp, "dave", TRUE, "name_is_dave") 	# and an optional name
grp <- tiledb_group_close(grp)
grp <- tiledb_group_open(grp, "READ")
expect_equal(tiledb_group_member_count(grp), 4)

## adding a member that is not a group or an array errors
expect_error(tiledb_group_add_member(grp, "doesNotExist", TRUE))

grp <- tiledb_group_close(grp)
grp <- tiledb_group_open(grp, "WRITE")
grp <- tiledb_group_remove_member(grp, "bob") 						# remove by (rel.) uri
grp <- tiledb_group_remove_member(grp, "name_is_dave") 				# remove by name
expect_error(tiledb_remove_member(grp, "does_not_exists"))
grp <- tiledb_group_close(grp)
grp <- tiledb_group_open(grp, "READ")
expect_equal(tiledb_group_member_count(grp), 2)

## remainder fragile on Fedora
if (isFALSE(tiledb:::.isFedora())) {
    obj <- tiledb_group_member(grp, 0)
    expect_equal(length(obj), 3)
    expect_true(is.character(obj[1]))
    expect_equal(obj[1], "ARRAY")
    expect_true(is.character(obj[2]))
    expect_equal(obj[2], file.path(tiledb_group_uri(grp), "chloe"))
    expect_true(is.character(obj[3]))
    expect_equal(obj[3], "name_is_chloe")

    obj <- tiledb_group_member(grp, 1) 									# group member with no name
    expect_equal(obj[3], "")

    txt <- tiledb_group_member_dump(grp, TRUE)
    dat <- read.csv(text=txt, sep=' ', header=FALSE)
    expect_equal(nrow(dat), 1+2)              # one for header 'filename GROUP'
}
