<!--
%\VignetteIndexEntry{Installation Options}
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteEncoding{UTF-8}
-->
---
title: "Installation Options for the TileDB R Package"
date: "2022-01-25"
css: "water.css"
---

## Overview

This vignette discusses different options for installing the TileDB R package.

### CRAN

#### Binaries

The TileDB R package is released via the Comprehensive R Archive Network, or CRAN.
CRAN generally provides binaries for the two most widely operating systems,
Windows and macOS.  So issueing the command

```r
install.packages("tiledb")
```

will, on those two operating system, default to `type="binary"` and install a
ready-to-run binary package.

#### Source

On other operating systems such as Linux, or by explicitly selecting
`type="source" on Windows or macOS, an installation from source is
attempted when using `install.packages("tiledb")`.

On Linux or macOS, this invokes the standard R build process which will rely
on `configure`.  If `pkg-config` is found, it is used to query the system
about a possible TileDB installation.  If one is found, it used. Otherwise,
on all three main operating systems a prebuilt archive with the TileDB
Embedded library is downloaded from GitHub and unpacked.  Each build of the R
package will have one matching build of TileDB Embedded associated with it
via file the `tools/tiledbVersion.txt` specifying a version and release
commit sha1. These two values are used to find the corresponding binary
artifacts from a release at GitHub.

The location of an existing installed version of TileDB Embedded can also be
provided to the `configure` script which supplies an argument
`--with-tiledb=PATH`. So on the command-line, one could say

```sh
R CMD INSTALL --configure-args='--with-tiledb=/some/path' tiledb_*.tar.gz
```

to pass the `configure` argument on. From within R, the equivalent command is

```r
install.packages("tiledb", repos=NULL, configure.args="--with-tiledb=/some/path")
```

Lastly, one can also override the default download location (otherwise
inferred via `tools/tiledbVersion.txt`.  The `configure` script supports an
option `--with-download=URL` so the two calls become, respectively


```sh
R CMD INSTALL --configure-args='--with-download=https://some.where.net/file.tgz' tiledb_*.tar.gz
```

and

```r
install.packages("tiledb_0.11.0.tar.gz", repos=NULL,
                 configure.args="--with-download=https://some.where.net/file.tgz")
```

## Conda

Using for example the current [`condaforge/mambaforge`](https://hub.docker.com/r/condaforge/mambaforge) container
from the [conda-forge project](https://github.com/conda-forge/miniforge), we can do


```sh
mamba install -y r-tiledb				 # installs R, tiledb and deps without extra prompt
# ...several lines of installation detail omitted
R
# ... several lines of R startup output omitted
> library(tiledb)
```

which installs R, the tiledb package and all respective dependencies.

## Docker

Builds of the TileDB package could also be provided via Docker containers.
At present, no official TileDB R containers are provided by TileDB.
However, the discussion of installation from source above describes how to create such containers.

## Summary

This note described several installation options for the TileDB R package.
