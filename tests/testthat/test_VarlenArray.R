library(testthat)
library(tiledb)

test_that("Utility functions for variable length arrays work", {
    expect_equal( varlen_offsets( c("bob", "joseph", "susan")), c(0,3,9) )
    expect_equal( varlen_offsets( matrix(c("bob", "joseph", "susan", "ted"), ncol = 2)), c(0,3,9,14) )
    x = matrix(list(1:4,2:3,100:104, 99:102), ncol = 2)
    expect_equal( varlen_offsets(x), c(0,16,24,44) )
    y = matrix(list(1:4 + 0.1, 2:3 + 0.1, 100:104 + 0.1, 99:102 + 0.1), ncol = 2)
    expect_equal( varlen_offsets(y), c(0,32,48,88) )
})
