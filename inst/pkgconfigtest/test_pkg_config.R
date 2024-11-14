
if (!at_home()) {
  exit_file("'.pkg_config()' tests run at home")
}

if (!nzchar(libtype <- Sys.getenv("_R_TILEDB_LIBTILEDB_"))) {
  exit_file("The libtiledb type is unset")
}

expect_true(
  libtype %in% c("system", "vendored"),
  info = "`Sys.getenv('_R_TILEDB_LIBTILEDB_')` must be 'system' or 'vendored'"
)

expect_error(.pkg_config('unknown'), info = "Invalid opts throw an error")
expect_true(
  nzchar(include <- .pkg_config("PKG_CXX_FLAGS")),
  info = "'PKG_CXX_FLAGS' are available"
)
expect_true(
  nzchar(libs <- .pkg_config("PKG_CXX_LIBS")),
  info = "'PKG_CXX_LIBS' are available"
)

if (libtype == "system") {
  # Check includes
  expect_true(
    grepl("^-I/", include),
    info = "'PKG_CXX_FLAGS' points to an include directory"
  )
  expect_length(
    unlist(strsplit(include, split = "[[:space:]]")),
    length = 1L,
    info = "'PKG_CXX_FLAGS' has only one include directory"
  )
  # Check libs
  expect_true(
    grepl("^-L", libs),
    info = "'PKG_CXX_LIBS' starts with a linking directory"
  )
  expect_true(
    grepl("[[:space:]]-ltiledb$", libs),
    info = "'PKG_CXX_LIBS' ends with the libtiledb directive"
  )
} else {
  pkgdir <- system.file(package = "tiledb")
  pattern <- paste0("^-%s", pkgdir)
  # Check includes
  include <- unlist(strsplit(include, split = "[[:space:]]"))
  expect_true(
    length(include) > 1L,
    info = "'PKG_CXX_FLAGS' includes multiple include directories"
  )
  for (i in seq_along(along.with = include)) {
    expect_true(
      grepl(sprintf(pattern, "I"), include[i]),
      info = sprintf(
        "All include directories are within the package directory (include %i)",
        i
      )
    )
  }
  # Check libs
  expect_true(
    grepl('^-ltiledb[[:space:]]', libs),
    info = "'PKG_CXX_LIBS' starts with the libtiledb directory"
  )
  libs <- unlist(strsplit(libs, split = "[[:space:]]"))
  expect_true(
    length(libs) > 2L,
    info = "'PKG_CXX_LIBS' includes multiple linking directories"
  )
  for (i in seq.int(2L, length(libs))) {
    expect_true(
      grepl(sprintf(pattern, "L"), libs[i]),
      info = sprintf(
        "All linking directories are within the package directory (link %i)",
        i - 1L
      )
    )
  }
}
