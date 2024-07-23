library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.25.0") exit_file("These tests needs TileDB 2.25.0 or later")

expect_silent(intdim <- tiledb_dim("dim", c(1L, 100L), type = "INT32"))
expect_silent(intdom <- tiledb_domain(dim = intdim))
expect_silent(ndr <- tiledb_ndrectangle(intdom))
expect_silent(tiledb_ndrectangle_set_range(ndr, "dim", 41L, 42L))

expect_silent(cd <- tiledb_current_domain())
expect_true(tiledb_current_domain_is_empty(cd))
expect_error(tiledb_current_domain_get_type(cd))

expect_silent(tiledb_current_domain_set_ndrectangle(cd, ndr))
expect_silent(newndr <- tiledb_current_domain_get_ndrectangle(cd))
expect_silent(newtp <- tiledb_current_domain_get_type(cd))
expect_true(is(newndr, "tiledb_ndrectangle"))
expect_equal(tiledb_ndrectangle_get_range(newndr, "dim"), c(41L, 42L))
