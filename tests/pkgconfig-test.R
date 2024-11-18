if (requireNamespace("tinytest", quietly = TRUE)) {
  tinytest::test_package(
    "tiledb",
    testdir = "pkgconfigtest",
    at_home = nzchar(Sys.getenv("_R_TILEDB_LIBTILEDB_"))
  )
}
