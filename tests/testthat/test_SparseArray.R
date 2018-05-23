library(tiledb)
context("tiledb::SparseArray")

unlink_and_create <- function(tmp) {
  if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE)
    dir.create(tmp)
  } else {
    dir.create(tmp)
  }
  return(tmp)
}

test_that("Can read / write simple 1D sparse vector", {
  tmp <- tempdir() 
  setup({
    unlink_and_create(tmp) 
  })
  
  ctx <- tiledb::Ctx()
  d1  <- tiledb::Dim(ctx, domain = c(1L, 10L)) 
  dom <- tiledb::Domain(ctx, c(d1))
  atr <- tiledb::Attr(ctx)
  sch <- tiledb::ArraySchema(ctx, dom, c(atr), sparse = TRUE)
  
  arr <- tiledb::SparseArray(ctx, sch, tmp) 
  expect_true(is.sparse(arr))   
   
  arr <- tiledb::SparseArray.load(ctx, tmp) 
  expect_true(is.sparse(arr))
  
  teardown(
    unlink(tmp, recursive = TRUE)
  ) 
})