library(testthat)
library(tiledb)

test_that( "Multi-attribute array input can be validated", {
    expect_error( assert_uniform_dimensions( list(1:4, 2:3, 4:5, 9:9 ) ) )
    expect_error( assert_uniform_dimensions( c(a = 1, b = 2, c = 4, d = 9) ) )
    expect_error( assert_uniform_dimensions(
        list(a = matrix(1:4, ncol = 2), b = matrix(1:4, ncol = 2), c = matrix(1:4, ncol = 2), d = matrix(1:8, ncol = 2))
    ))
    expect_true( assert_uniform_dimensions(
        list(a = matrix(1:6, ncol = 3), b = matrix(1:6, ncol = 3), c = matrix(1:6, ncol = 3), d = matrix(1:6, ncol = 3))
    ))
})

test_that( "Simple array creators work", {
    expect_s4_class( create_tiledb_array(tempfile(), c(2,3), "FLOAT64"), "tiledb_dense")
    expect_s4_class( create_tiledb_array(tempfile(), c(2,3), c("FLOAT64", "FLOAT64")), "tiledb_dense")
    expect_s4_class( create_tiledb_array(tempfile(), c(2,3), "FLOAT64", sparse = TRUE), "tiledb_sparse")
    x = matrix( c(1.3, 1, 2, 4), ncol = 2)
    expect_s4_class( tiledb_array( x, tempfile() ), "tiledb_dense")
})
