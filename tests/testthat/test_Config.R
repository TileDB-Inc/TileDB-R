library(tiledb)
context("tiledb::Config")

test_that("tiledb::Config default constructor", {
  cfg <- tiledb::Config()
  expect_is(cfg, "Config")
})

test_that("tiledb::Config named vector constructor", {
  cfg <- tiledb::Config(config = c(foo = "bar"))
  expect_is(cfg, "Config")
})

test_that("tiledb::Config errors upon invalid vector argument", {
  expect_error(tiledb::Config("foo"))
  # all values must have a non-empty name 
  expect_error(tiledb::Config(foo = "foo", "bar"))
  expect_error(tiledb::Config(1))
  expect_error(tiledb::Config(c(foo = 1))) 
})

test_that("tiledb::Config indexing works", {
  cfg <- tiledb::Config()
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

test_that("tiledb::Config can convert to R named vector", {
  cfg <- tiledb::Config()
  v <- as.vector(cfg)
  expect_is(v, "character")
  keys <- names(v)
  values <- unname(v)
  
   
})