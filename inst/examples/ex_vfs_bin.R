
suppressMessages({
  library(tiledb)
  library(bit64)
})

## Name of the array to create.
array_name <- "ex_vfs_bin"
## Path is either current directory, or a local config value is found
uri <- file.path(getOption("TileDB_Data_Path", "."), array_name)

low_level <- function() {
  ctx <- tiledb_ctx()
  vfsptr <- tiledb:::libtiledb_vfs(ctx@ptr)

  fhbuf <- tiledb:::libtiledb_vfs_open(ctx@ptr, vfsptr, uri, "WRITE")
  payload <- as.integer(serialize(list(int=153L, string="abcde"), NULL))
  tiledb:::libtiledb_vfs_write(ctx@ptr, fhbuf, payload)

  tiledb:::libtiledb_vfs_sync(ctx@ptr, fhbuf)
  tiledb:::libtiledb_vfs_close(ctx@ptr, fhbuf)

  fhbuf <- tiledb:::libtiledb_vfs_open(ctx@ptr, vfsptr, uri, "READ")
  vec <- tiledb:::libtiledb_vfs_read(ctx@ptr, fhbuf, as.integer64(0), as.integer64(122*4))
  tiledb:::libtiledb_vfs_close(ctx@ptr, fhbuf)
  print(str(unserialize(as.raw(vec))))
  invisible()
}

higher_level <- function() {
  fhbuf <- tiledb_vfs_open(uri, "WRITE")
  payload <- as.integer(serialize(list(int=153L, string="abcde"), NULL))
  tiledb_vfs_write(fhbuf, payload)
  tiledb_vfs_sync(fhbuf)
  tiledb_vfs_close(fhbuf)

  fhbuf <- tiledb_vfs_open(uri, "READ")
  vec <- tiledb_vfs_read(fhbuf, as.integer64(0), as.integer64(122*4))
  tiledb_vfs_close(fhbuf)
  print(str(unserialize(as.raw(vec))))
  invisible()
}

#low_level()
higher_level()
cat("Done.\n")
