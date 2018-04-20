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
  
  teardown({
    unlink(tmp, recursive = TRUE)
  }) 
})