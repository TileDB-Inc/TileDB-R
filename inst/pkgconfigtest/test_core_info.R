
if (!at_home()) {
  exit_file("'.pkg_config()' tests run at home")
}

libtype <- Sys.getenv("_R_TILEDB_LIBTILEDB_")
if (nzchar(libtype)) {
  expect_true(
    libtype %in% c("system", "vendored"),
    info = "`Sys.getenv('_R_TILEDB_LIBTILEDB_')` must be 'system' or 'vendored'"
  )
}

# Check .core_info()
expect_inherits(
  info <- .core_info(),
  class = "character",
  info = "'.core_info()' returns a character vector"
)
expect_length(info, length = 2L, info = "'.core_info()' returns a two-length vector")
expect_identical(
  names(info),
  target = c("version", "libtype"),
  info = "'.core_info()' returns a named vector with names of 'version' and 'libtype'"
)
expect_identical(
  info[["version"]],
  target = as.character(tiledb_version(compact = TRUE)),
  info = "'.core_info()' returns the correct core version"
)
libtarget <- ifelse(nzchar(libtype), yes = libtype, no = "unknown")
expect_identical(
  info[["libtype"]],
  target = libtarget,
  info = sprintf("'.core_info()' returns the correct libtype ('%s')", libtarget)
)

# Check .core_hash()
tmpfile <- tempfile(tmpdir = tempdir(check = TRUE))
writeLines(
  sprintf(
    "version:\t%s\nlibtype:\t%s",
    as.character(tiledb_version(compact = TRUE)),
    ifelse(nzchar(libtype), yes = libtype, no = "unknown")
  ),
  con = tmpfile
)
target <- unname(tools::md5sum(tmpfile))
hash <- .core_hash()
expect_inherits(
  hash,
  class = "character",
  info = "'.core_hash()' returns a character value"
)
expect_length(hash, length = 1L, info = "'.core_hash()' returns a single value")
expect_null(names(hash), info = "'.core_hash()' returns an unnamed value")
expect_identical(
  hash,
  target = target,
  info = "'.core_hash()' returns the correct hash value"
)
