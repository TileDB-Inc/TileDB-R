
if (requireNamespace("tinytest", quietly=TRUE))  {
    if (R.Version()$minor >= "2.0" && Sys.getenv("MY_UNIVERSE", "") == "")
        tinytest::test_package("tiledb")
}
