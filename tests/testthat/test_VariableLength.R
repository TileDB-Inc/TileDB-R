library(tiledb)
context("tiledb_variable_length_arrays")

tmp <- tempfile()

unlink_and_create_simple <- function(tmp) {
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
  dir.create(tmp, recursive = TRUE)

  ## The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4].
  dom <- tiledb_domain(dims = c(tiledb_dim("rows", c(1L, 4L), 4L, "INT32"),
                                tiledb_dim("cols", c(1L, 4L), 4L, "INT32")))

  ## The array will be dense with a single attribute "a" so each (i,j) cell can store an integer.
  schema <- tiledb_array_schema(dom,
                                attrs = c(tiledb_attr("a1", type = "CHAR", is_var = TRUE),
                                          tiledb_attr("a2", type = "INT32", is_var = TRUE),
                                          tiledb_attr("a3", type = "FLOAT64", is_var = TRUE)))

  ## Create the (empty) array on disk.
  tiledb_array_create(tmp, schema)
}


## infrastructure
setup({
  # empty
})

teardown({
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE, force = TRUE)
})



test_that("Can write and read variable length array via helpers", {
  skip_if_not_installed("data.table")
  arr <- unlink_and_create_simple(tmp)

  a1 <- data.table::data.table(V1=list("a", "eee", "i", "m"),
                               V2=list("bb", "f", "jjj", "n"),
                               V3=list("ccc", "g", "kk", "oo"),
                               V4=list("dd", "hhh", "l", "qqqq"))
  a2 <- data.table::data.table(V1=list(c(1L, 1L), 5L, c(9L, 9L), 13L),
                               V2=list(c(2L,2L), c(6L,6L), 10L, c(14L,14L,14L)),
                               V3=list(3L, c(7L,7L), 11L, 15L),
                               V4=list(4L, c(8L,8L,8L), c(12L,12L), 16L))
  a3 <- data.table::data.table(V1=list(c(1.0, 1.1), 5,        c(9, 9),  13),
                               V2=list(c(2,2),      c(6,6),   10,       c(14.1,14.2,14.3)),
                               V3=list(3,           c(7,7),   11,       15),
                               V4=list(4,           c(8,8,8), c(12,12), 16.75))
  expect_true(write_variable_length(tmp, list(a1=a1, a2=a2, a3=a3)))

  expect_equal(read_variable_length(tmp, "a1", c(1,4,1,4)), a1)
  expect_equal(read_variable_length(tmp, "a1", c(1,2,1,4)), a1[1:2,])
  expect_equal(read_variable_length(tmp, "a1", c(1,4,1,2)), a1[,1:2])
  ## col names come back as 1 and 2. Fix?
  expect_equivalent(read_variable_length(tmp, "a1", c(3,4,3,4)), a1[3:4,3:4])

  expect_equal(read_variable_length(tmp, "a2", c(1,4,1,4)), a2)
  expect_equal(read_variable_length(tmp, "a2", c(1,2,1,4)), a2[1:2,])
  expect_equal(read_variable_length(tmp, "a2", c(1,4,1,2)), a2[,1:2])
  ## col names come back as 1 and 2. Fix?
  expect_equivalent(read_variable_length(tmp, "a2", c(3,4,3,4)), a2[3:4,3:4])

  expect_equal(read_variable_length(tmp, "a3", c(1,4,1,4)), a3)
  expect_equal(read_variable_length(tmp, "a3", c(1,2,1,4)), a3[1:2,])
  expect_equal(read_variable_length(tmp, "a3", c(1,4,1,2)), a3[,1:2])
  ## col names come back as 1 and 2. Fix?
  expect_equivalent(read_variable_length(tmp, "a3", c(3,4,3,4)), a3[3:4,3:4])

  expect_error(read_variable_length(tmp, "a1_not_a_key", c(1,4,1,4)))
  expect_error(read_variable_length(tmp, "a1", c(1,5,1,5)))

})

test_that("Can write and read variable length array via [ and [<-", {
  skip_if_not_installed("data.table")
  unlink_and_create_simple(tmp)
  arr <- tiledb_dense(tmp)
  a1 <- data.table::data.table(V1=list("a", "eee", "i", "m"),
                               V2=list("bb", "f", "jjj", "n"),
                               V3=list("ccc", "g", "kk", "oo"),
                               V4=list("dd", "hhh", "l", "qqqq"))
  a2 <- data.table::data.table(V1=list(c(1L, 1L), 5L, c(9L, 9L), 13L),
                               V2=list(c(2L,2L), c(6L,6L), 10L, c(14L,14L,14L)),
                               V3=list(3L, c(7L,7L), 11L, 15L),
                               V4=list(4L, c(8L,8L,8L), c(12L,12L), 16L))
  a3 <- data.table::data.table(V1=list(c(1.0, 1.1), 5,        c(9, 9),  13),
                               V2=list(c(2,2),      c(6,6),   10,       c(14.1,14.2,14.3)),
                               V3=list(3,           c(7,7),   11,       15),
                               V4=list(4,           c(8,8,8), c(12,12), 16.75))
  expect_true(write_variable_length(tmp, list(a1=a1, a2=a2, a3=a3)))

  expect_equal(arr[][["a1"]], a1)
  expect_equal(arr[][["a2"]], a2)
  expect_equal(arr[][["a3"]], a3)

  expect_equal(arr[1,][["a1"]], a1[1,])
  expect_equal(arr[1,][["a2"]], a2[1,])
  expect_equal(arr[1,][["a3"]], a3[1,])

  expect_equal(arr[,1][["a1"]], a1[,1])
  expect_equal(arr[,1][["a2"]], a2[,1])
  expect_equal(arr[,1][["a3"]], a3[,1])

  expect_equal(arr[1:3,][["a1"]], a1[1:3,])
  expect_equal(arr[1:3,][["a2"]], a2[1:3,])
  expect_equal(arr[1:3,][["a3"]], a3[1:3,])

  expect_equal(arr[,1:3][["a1"]], a1[,1:3])
  expect_equal(arr[,1:3][["a2"]], a2[,1:3])
  expect_equal(arr[,1:3][["a3"]], a3[,1:3])

  expect_equal(arr[2:3,1:2][["a1"]], a1[2:3,1:2])
  expect_equal(arr[2:3,1:2][["a2"]], a2[2:3,1:2])
  expect_equal(arr[2:3,1:2][["a3"]], a3[2:3,1:2])

})
