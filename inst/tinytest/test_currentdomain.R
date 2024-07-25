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

expect_silent(cd <- tiledb_current_domain_set_ndrectangle(cd, ndr))
expect_silent(newndr <- tiledb_current_domain_get_ndrectangle(cd))
expect_silent(newtp <- tiledb_current_domain_get_type(cd))
expect_true(is(newndr, "tiledb_ndrectangle"))
expect_equal(tiledb_ndrectangle_get_range(newndr, "dim"), c(41L, 42L))

## complete example with multiple dims and schema evolution
uri <- tempfile()
dim <- c(tiledb_dim("row", c(1L, 1000L), 50L, type = "INT32"),
         tiledb_dim("col", c(1L, 100000L), 500L, type = "INT64"),
         tiledb_dim("key", c(NULL, NULL), NULL, type = "ASCII"))
dom <- tiledb_domain(dim = dim)
attr <- tiledb_attr("a", type = "INT32") # unused
sch <- tiledb_array_schema(dom, attrs = attr, sparse = TRUE)
arr <- tiledb_array_create(uri, sch)
expandDomain <- function(uri, upperend=200L, upperkey="dd") {
    ase <- tiledb_array_schema_evolution()
    sch <- tiledb::schema(uri)
    dom <- tiledb::domain(sch)
    cd <- tiledb_current_domain()
    ndr <- tiledb_ndrectangle(dom)
    tiledb_ndrectangle_set_range(ndr, "row", 1L, upperend)
    tiledb_ndrectangle_set_range(ndr, "col", bit64::as.integer64(1L), bit64::as.integer64(10 * upperend))
    tiledb_ndrectangle_set_range(ndr, "key", "bb", upperkey)
    tiledb_current_domain_set_ndrectangle(cd, ndr)
    tiledb_array_schema_evolution_expand_current_domain(ase, cd)
    tiledb_array_schema_evolution_array_evolve(ase, uri)
    invisible(NULL)
}
expect_silent(expandDomain(uri))
sch <- tiledb::schema(uri)
cd <- tiledb_array_schema_get_current_domain(sch)
ndr <- tiledb_current_domain_get_ndrectangle(cd)
expect_equal(tiledb_ndrectangle_get_range(ndr, "row"), c(1, 200))
expect_equal(tiledb_ndrectangle_get_range(ndr, "col"), bit64::as.integer64(c(1, 2000)))
expect_equal(tiledb_ndrectangle_get_range(ndr, "key"), c("bb", "dd"))

expandDomain(uri, 300L, "ff")
sch <- tiledb::schema(uri)
cd <- tiledb_array_schema_get_current_domain(sch)
ndr <- tiledb_current_domain_get_ndrectangle(cd)
expect_equal(tiledb_ndrectangle_get_range(ndr, "row"), c(1, 300))
expect_equal(tiledb_ndrectangle_get_range(ndr, "col"), bit64::as.integer64(c(1, 3000)))
expect_equal(tiledb_ndrectangle_get_range(ndr, "key"), c("bb", "ff"))

expandDomain(uri, 400L, "kk")
sch <- tiledb::schema(uri)
cd <- tiledb_array_schema_get_current_domain(sch)
ndr <- tiledb_current_domain_get_ndrectangle(cd)
expect_equal(tiledb_ndrectangle_get_range(ndr, "row"), c(1, 400))
expect_equal(tiledb_ndrectangle_get_range(ndr, "col"), bit64::as.integer64(c(1, 4000)))
expect_equal(tiledb_ndrectangle_get_range(ndr, "key"), c("bb", "kk"))
