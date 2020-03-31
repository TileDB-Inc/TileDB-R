
library(tiledb)

unlink_and_create <- function(tmp) {
  if (dir.exists(tmp)) {
    unlink(tmp, recursive = TRUE, force = TRUE)
    dir.create(tmp)
  } else {
    dir.create(tmp)
  }
  return(tmp)
}

# Name of the array to create.
array_name <- "/tmp/tiledb/fixed_length"

unlink_and_create(array_name)

d1  <- tiledb_dim("d1", domain = c(1L, 4L))
d2  <- tiledb_dim("d2", domain = c(1L, 4L))
dom <- tiledb_domain(c(d1, d2))

vec <- 1:32 * 10L
attr <- tiledb_attr("a", type = r_to_tiledb_type(vec))

tiledb:::libtiledb_attribute_set_cell_val_num(attr@ptr, 2)

sch <- tiledb_array_schema(dom, c(attr))

tiledb_array_create(array_name, sch)

ctx <- tiledb_ctx()
arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "WRITE")

subarr <- c(1L,4L, 1L,4L)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "WRITE")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", vec)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
res <- tiledb:::libtiledb_array_close(arrptr)

## written

arrptr <- tiledb:::libtiledb_array_open(ctx@ptr, array_name, "READ")
## subarray of rows 1,2 and cols 2,3,4
subarr <- c(1L,2L, 2L,4L)

qryptr <- tiledb:::libtiledb_query(ctx@ptr, arrptr, "READ")
qryptr <- tiledb:::libtiledb_query_set_subarray(qryptr, subarr)
qryptr <- tiledb:::libtiledb_query_set_layout(qryptr, "ROW_MAJOR")
v <- integer(12) ## == (2 x 3) x 2
qryptr <- tiledb:::libtiledb_query_set_buffer(qryptr, "a", v)
qryptr <- tiledb:::libtiledb_query_submit(qryptr)
print(v)         # unformed array, no coordinates

#expect_equal(v, c(20L, 30L, 40L, 60L, 70L, 80L))
res <- tiledb:::libtiledb_array_close(arrptr)
