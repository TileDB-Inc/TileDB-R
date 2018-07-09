library(tiledb)
context("tiledb_config")

test_that("tiledb_config default constructor", {
  cfg <- tiledb_config()
  expect_is(cfg, "tiledb_config")
})

test_that("tiledb_config named vector constructor", {
  cfg <- tiledb_config(config = c(foo = "bar"))
  expect_is(cfg, "tiledb_config")
})

test_that("tiledb_config errors upon invalid vector argument", {
  expect_error(tiledb_config("foo"))
  # all values must have a non-empty name 
  expect_error(tiled_config(foo = "foo", "bar"))
  expect_error(tiledb_config(1))
  expect_error(tiledb_config(c(foo = 1))) 
})

test_that("tiledb_config indexing works", {
  cfg <- tiledb_config()
  cfg["foo"] <- "bar"
  expect_equal(cfg["foo"], c("foo" = "bar"))
  
  # can stringify basic types
  cfg["int"] <- 1L
  expect_equal(cfg["int"], c(int = "1"))
  
  cfg["bool"] <- FALSE
  expect_equal(cfg["bool"], c(bool = "false"))
  
  cfg["float"] <- 2.3
  expect_equal(cfg["float"], c(float = "2.3"))
  
  res <- cfg[c("int", "bool", "float")]
  expect_equal(res, c(int = "1", bool = "false", float = 2.3))
  
  # can only stringify basic types int / float / bools
  expect_error(cfg["error"] <- c(1, 2, 3))
})

test_that("tiledb_config can convert to R named vector", {
  cfg <- tiledb_config()
  v <- as.vector(cfg)
  expect_is(v, "character")
  keys <- names(v)
  values <- unname(v)
})