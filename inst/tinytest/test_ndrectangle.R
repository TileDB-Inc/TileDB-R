
library(tinytest)
library(tiledb)

ctx <- tiledb_ctx(limitTileDBCores())

if (tiledb_version(TRUE) < "2.26.0") exit_file("These tests needs TileDB 2.26.0 or later")

for (tp in c("INT32", "UINT32", "INT16", "UINT16", "INT64", "UINT64", "INT8", "UINT8", "FLOAT32", "FLOAT64")) {
    if (grepl("INT64", tp)) {
        expect_silent(dim <- tiledb_dim("dim", bit64::as.integer64(c(1L, 100L)), bit64::as.integer64(50L), type = tp))
    } else if (grepl("FLOAT", tp)) {
        expect_silent(dim <- tiledb_dim("dim", c(1, 100), 50, type = tp))
    } else {
        expect_silent(dim <- tiledb_dim("dim", c(1L, 100L), 50L, type = tp))
    }
    expect_true(is(dim, "tiledb_dim"))
    expect_silent(dom <- tiledb_domain(dim = dim))
    expect_true(is(dom, "tiledb_domain"))

    expect_error(ndr <- tiledb_ndrectangle(dim))
    expect_silent(ndr <- tiledb_ndrectangle(dom))
    expect_true(is(ndr, "tiledb_ndrectangle"))

    expect_error(tiledb_ndrectangle_set_range(dim, "dim", 1, 2)) # wrong type
    expect_error(tiledb_ndrectangle_set_range(ndr, "notdim", 1, 2)) # wrong name
    expect_error(tiledb_ndrectangle_set_range(ndr, "dim", 1, 2L))   # wrong type
    expect_error(tiledb_ndrectangle_set_range(ndr, "dim", 1L, 2))   # wrong type
    if (grepl("INT64", tp)) {
        expect_silent(ndr <- tiledb_ndrectangle_set_range(ndr, "dim",
                                                          bit64::as.integer64(1L),
                                                          bit64::as.integer64(20L)))
    } else if (grepl("FLOAT", tp)) {
        expect_silent(ndr <- tiledb_ndrectangle_set_range(ndr, "dim", 1, 20))
    } else {
        expect_silent(ndr <- tiledb_ndrectangle_set_range(ndr, "dim", 1L, 20L))
    }

    expect_error(tiledb_ndrectangle_get_range(dim, "dim")) # wrong type
    expect_error(tiledb_ndrectangle_set_range(ndr, "notdim")) # wrong name
    if (grepl("INT64", tp)) {
        expect_equal(tiledb_ndrectangle_get_range(ndr, "dim"), bit64::as.integer64(c(1L, 20L)))
    } else {
        expect_equal(tiledb_ndrectangle_get_range(ndr, "dim"), c(1L, 20L))
    }

    expect_equal(tiledb_ndrectangle_dim_num(ndr), 1)
    expect_equal(tiledb_ndrectangle_datatype(ndr, "dim"), tp)
    expect_error(tiledb_ndrectangle_datatype(ndr, "not_a_dim"))
    expect_equal(tiledb_ndrectangle_datatype_by_ind(ndr, 0), tp)
    expect_error(tiledb_ndrectangle_datatype_by_ind(ndr, 1))
}

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
