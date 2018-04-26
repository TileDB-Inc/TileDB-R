library(tiledb)
context("tiledb::Array")

test_that("Can read / write a simple 1D vector", {
  tmp <- tempdir()
  setup({
   if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE)
    dir.create(tmp)
   } else {
    dir.create(tmp) 
   }
  })
  
  ctx <- tiledb::Ctx()
  dim <- tiledb::Dim(ctx, domain = c(1L, 10L)) 
  dom <- tiledb::Domain(ctx, c(dim))
  val <- tiledb::Attr(ctx) 
  sch <- tiledb::ArraySchema(ctx, dom, c(val)) 
  
  dat <- as.array(as.double(1:10))
  arr <- tiledb::Array(ctx, sch, tmp)
  
  arr[] <- dat
  expect_equal(arr[], dat)
  
  # explicit range enumeration
  expect_equal(arr[c(3,4,5,6,7)], dat[c(3,4,5,6,7)])
  
  # vector range syntax
  expect_equal(arr[3:7], dat[3:7])
  
  # vector range syntax (reversed)
  # TODO: find a way to efficiently do this
  # expect_equal(arr[7:3], dat[7:3]) 
    
  # scalar indexing
  expect_equal(arr[8], dat[8])
  
  teardown({
    unlink(tmp, recursive = TRUE)
  }) 
})

test_that("Can read / write a simple 2D matrix", {
  tmp <- tempdir()
  setup({
   if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE)
    dir.create(tmp)
   } else {
    dir.create(tmp) 
   }
  })
  
  ctx <- tiledb::Ctx()
  d1  <- tiledb::Dim(ctx, domain = c(1L, 5L)) 
  d2  <- tiledb::Dim(ctx, domain = c(1L, 5L)) 
  dom <- tiledb::Domain(ctx, c(d1, d2))
  val <- tiledb::Attr(ctx) 
  sch <- tiledb::ArraySchema(ctx, dom, c(val)) 
  
  dat <- matrix(rnorm(25), 5, 5)
  arr <- tiledb::Array(ctx, sch, tmp)
  
  arr[] <- dat
  expect_equal(arr[], dat)

  # explicit range enumeration
  expect_equal(arr[c(3,4,5), c(3,4,5)], 
               dat[c(3,4,5), c(3,4,5)])
  
  # vector range syntax
  expect_equal(arr[1:3, 1:3], dat[1:3, 1:3]) 
  
  # scalar indexing
  expect_equal(arr[3, 3], dat[3, 3])
  
  teardown({
    unlink(tmp, recursive = TRUE)
  }) 
})

test_that("Can read / write a simple 3D matrix", {
  tmp <- tempdir()
  setup({
   if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE)
    dir.create(tmp)
   } else {
    dir.create(tmp) 
   }
  })
  
  ctx <- tiledb::Ctx()
  d1  <- tiledb::Dim(ctx, domain = c(1L, 5L)) 
  d2  <- tiledb::Dim(ctx, domain = c(1L, 5L)) 
  d3  <- tiledb::Dim(ctx, domain = c(1L, 5L)) 
  dom <- tiledb::Domain(ctx, c(d1, d2, d3))
  val <- tiledb::Attr(ctx) 
  sch <- tiledb::ArraySchema(ctx, dom, c(val)) 
  
  dat <- array(rnorm(125), dim = c(5, 5, 5))
  arr <- tiledb::Array(ctx, sch, tmp)
  
  arr[] <- dat
  expect_equal(arr[], dat)

  # explicit range enumeration
  expect_equal(arr[c(3, 4, 5), c(3, 4, 5), c(1, 2)], 
               dat[c(3, 4, 5), c(3, 4, 5), c(1, 2)])
  
  # vector range syntax
  expect_equal(arr[1:3, 1:3, 1:2], dat[1:3, 1:3, 1:2]) 
  
  # scalar indexing
  expect_equal(arr[3, 3, 3], dat[3, 3, 3])
  
  teardown({
    unlink(tmp, recursive = TRUE)
  }) 
})