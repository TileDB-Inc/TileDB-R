# Compiler Arguments for Using `libtiledb`

Get compiler flags for using the core `libtiledb` install used by tiledb

## Usage

``` r
.pkg_config(opt = c("PKG_CXX_FLAGS", "PKG_CXX_LIBS"))
```

## Arguments

- opt:

  A single character value with the package configuration variable to
  fetch; choose from

  - “`PKG_CXX_FLAGS`”: compiler flags for `libtiledb`

  - “`PKG_CXX_LIBS`”: linking flags for `libtiledb`

## Value

A single string containing either the include directories or linking
directories for `libtiledb`

## Examples

``` r
.pkg_config()
#> [1] "-I/home/runner/work/_temp/Library/tiledb/include -I/home/runner/work/_temp/Library/tiledb/tiledb/include"
.pkg_config("PKG_CXX_LIBS")
#> [1] "-ltiledb -L/home/runner/work/_temp/Library/tiledb/lib -L/home/runner/work/_temp/Library/tiledb/tiledb/lib"
```
