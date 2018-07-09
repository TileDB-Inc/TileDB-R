library(tiledb)
context("tiledb::ArraySchema")

test_that("tiledb::ArraySchema default constructor works", {
  ctx <- tiledb_ctx()
  d1  <- tiledb_dim(ctx, domain=c(1L, 100L))
  dom <- tiledb::Domain(ctx, c(d1))
  a1  <- tiledb::Attr(ctx)
  sch <- tiledb::ArraySchema(ctx, dom, c(a1))
  expect_is(sch, "ArraySchema")
})

test_that("tiledb::ArraySchema default constructor arugment values are correct",  {
  ctx <- tiledb_ctx()
  d1  <- tiledb_dim(ctx, domain = c(1L, 100L))
  d2  <- tiledb_dim(ctx, domain = c(1L, 100L))
  dom <- tiledb::Domain(ctx, c(d1, d2))
  a1  <- tiledb::Attr(ctx)
  sch <- tiledb::ArraySchema(ctx, dom, c(a1)) 
  
  # test domain
  expect_is(tiledb::domain(sch), "Domain")
  
  # test dimensions
  ds <- tiledb::dimensions(sch)
  expect_equal(length(ds), 2)
  expect_is(ds[[1]], "tiledb_dim")
  expect_is(ds[[2]], "tiledb_dim")
  
  # test attrs
  as <- tiledb::attrs(sch) 
  expect_equal(length(as), 1)
  expect_is(as[[1]], "Attr") 
  
  # test that default R schema is COL_MAJOR
  expect_equal(tiledb::cell_order(sch), "COL_MAJOR")
  expect_equal(tiledb::tile_order(sch), "COL_MAJOR")
  
  # test that the default R schema is dense
  expect_false(is.sparse(sch))
})

test_that("tiledb::ArraySchema full constructor argument values are correct",  {
  ctx <- tiledb_ctx()
  
  d1  <- tiledb_dim(ctx, domain = c(1L, 100L))
  d2  <- tiledb_dim(ctx, domain = c(1L, 100L))
  d3  <- tiledb_dim(ctx, domain = c(1L, 100L))
  
  dom <- tiledb::Domain(ctx, c(d1, d2, d3))
  
  a1  <- tiledb::Attr(ctx, "attribute1", type = "FLOAT64")
  a2  <- tiledb::Attr(ctx, "attribute2", type = "INT32")
  
  sch <- tiledb::ArraySchema(ctx, dom, c(a1, a2), 
                             cell_order = "ROW_MAJOR", 
                             tile_order = "ROW_MAJOR",
                             coords_compressor = tiledb::Compressor("GZIP", 10),
                             offsets_compressor = tiledb::Compressor("ZSTD", 5),
                             sparse = TRUE)
  
  # test domain
  expect_is(tiledb::domain(sch), "Domain")
  
  # test dimensions
  ds <- tiledb::dimensions(sch)
  expect_equal(length(ds), 3)
  expect_is(ds[[1]], "tiledb_dim")
  expect_is(ds[[2]], "tiledb_dim")
  expect_is(ds[[3]], "tiledb_dim")
  
  # test attrs
  as <- tiledb::attrs(sch) 
  expect_equal(length(as), 2)
  expect_equal(names(as), c("attribute1", "attribute2"))
  expect_is(as[[1]], "Attr") 
  expect_is(as[[2]], "Attr") 
  
  expect_equal(tiledb::cell_order(sch), "ROW_MAJOR")
  expect_equal(tiledb::tile_order(sch), "ROW_MAJOR")
 
  compr <- tiledb::compressor(sch)
  expect_equal(tiledb::compressor_type(compr[["coords"]]), "GZIP")
  expect_equal(tiledb::compressor_level(compr[["coords"]]), 10)
  expect_equal(tiledb::compressor_type(compr[["offsets"]]), "ZSTD")
  expect_equal(tiledb::compressor_level(compr[["offsets"]]), 5)
  
  expect_true(is.sparse(sch))
})