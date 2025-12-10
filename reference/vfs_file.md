# Create a custom file connection

Create a custom file connection

## Usage

``` r
vfs_file(description, mode = "", verbosity = 0L)
```

## Arguments

- description:

  path to a filename; contrary to `rconnection` a connection object is
  not supported.

- mode:

  character string. A description of how to open the connection if it is
  to be opened upon creation e.g. "rb". Default "" (empty string) means
  to not open the connection on creation - user must still call
  [`open()`](https://rdrr.io/r/base/connections.html). Note: If an
  "open" string is provided, the user must still call
  [`close()`](https://rdrr.io/r/base/connections.html) otherwise the
  contents of the file aren't completely flushed until the connection is
  garbage collected.

- verbosity:

  integer value 0, 1, or 2. Default: 0. Set to `0` for no debugging
  messages, `1` for some high-level messages and `verbosity = 2` for all
  debugging messages.

## Details

This `vfs_file()` connection works like the
[`file()`](https://rdrr.io/r/base/connections.html) connection in R
itself.

This connection works with both ASCII and binary data, e.g. using
[`readLines()`](https://rdrr.io/r/base/readLines.html) and
[`readBin()`](https://rdrr.io/r/base/readBin.html).

## Examples

``` r
if (FALSE) { # \dontrun{
tmp <- tempfile()
dat <- as.raw(1:255)
writeBin(dat, vfs_file(tmp))
readBin(vfs_file(tmp),  raw(), 1000)
} # }
```
