
library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.25.0") exit_file("These tests needs TileDB 2.25.0 or later")

## INT32
expect_silent(intdim <- tiledb_dim("dim", c(1L, 100L), type = "INT32"))
expect_true(is(intdim, "tiledb_dim"))
expect_silent(intdom <- tiledb_domain(dim = intdim))
expect_true(is(intdom, "tiledb_domain"))

expect_error(ndr <- tiledb_ndrectangle(intdim))
expect_silent(ndr <- tiledb_ndrectangle(intdom))
expect_true(is(ndr, "tiledb_ndrectangle"))

expect_error(tiledb_ndrectangle_set_range(intdim, "dim", 1, 2)) # wrong type
expect_error(tiledb_ndrectangle_set_range(ndr, "notdim", 1, 2)) # wrong name
expect_error(tiledb_ndrectangle_set_range(ndr, "dim", 1, 2L))   # wrong type
expect_error(tiledb_ndrectangle_set_range(ndr, "dim", 1L, 2))   # wrong type
expect_silent(ndr <- tiledb_ndrectangle_set_range(ndr, "dim", 1L, 20L))

expect_error(tiledb_ndrectangle_get_range(intdim, "dim")) # wrong type
expect_error(tiledb_ndrectangle_set_range(ndr, "notdim")) # wrong name
expect_equal(tiledb_ndrectangle_get_range(ndr, "dim"), c(1L, 20L))


## ASCII
expect_silent(strdim <- tiledb_dim("strdim", c(NULL, NULL), NULL, type = "ASCII"))
expect_true(is(strdim, "tiledb_dim"))
expect_silent(strdom <- tiledb_domain(dim = strdim))
expect_true(is(strdom, "tiledb_domain"))

expect_silent(ndr <- tiledb_ndrectangle(strdom))
expect_true(is(ndr, "tiledb_ndrectangle"))

expect_error(tiledb_ndrectangle_set_range(ndr, "notdim", 1, 2)) # wrong name
expect_error(tiledb_ndrectangle_set_range(ndr, "strdim", 1, 2L))   # wrong type
expect_error(tiledb_ndrectangle_set_range(ndr, "strdim", 1L, 2))   # wrong type
expect_silent(ndr <- tiledb_ndrectangle_set_range(ndr, "strdim", "aa", "zz"))

expect_error(tiledb_ndrectangle_set_range(ndr, "notdim")) # wrong name
expect_equal(tiledb_ndrectangle_get_range(ndr, "strdim"), c("aa", "zz"))
