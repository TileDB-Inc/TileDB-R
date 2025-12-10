# `libtiledb` Information

Get version and install information of the core `libtiledb` install

## Usage

``` r
.core_info()

.core_hash()
```

## Value

`.core_info()`: A named character vector with the following entries:

- “`version`”: `libtiledb` version

- “`libtype`”: type of `libtiledb` install; will be one of “`vendored`”,
  “`system`”, or “`unknown`”

`.core_hash()`: The [MD5 hash](https://rdrr.io/r/tools/md5sum.html) of
the core info

## Checking the `libtiledb` information in downstream packages

These functions are designed to make it easy to test if the core
`libtiledb` install has changed. This is accomplished by adding a
build-time constant to cache the version of `libtiledb` was built with.
For example, in `zzz.R`, put the following line to cache the `libtiledb`
information during package build

    .built_with <- list(libtiledb = tiledb::.core_hash())

Then, in the load hook, add the following check

    .onLoad <- function(libname, pkgname) {
      if (.built_with$libtiledb != tiledb::.core_hash()) {
        warning("Core libtiledb has changed, please reinstall ", pkgname)
      }
    }

This will throw a warning if tiledb, and therefore `libtiledb`, has
changed between downstream package install and load

## Examples

``` r
.core_info()
#>    version    libtype 
#>   "2.30.0" "vendored" 

.core_hash()
#> [1] "8685125d3829a8fbbce63aad0ebaed8d"
```
