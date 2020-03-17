<a href="https://tiledb.com"><img src="https://github.com/TileDB-Inc/TileDB/raw/dev/doc/source/_static/tiledb-logo_color_no_margin_@4x.png" alt="TileDB logo" width="400"></a>

[![Build Status](https://travis-ci.org/TileDB-Inc/TileDB-R.svg?branch=master)](https://travis-ci.org/TileDB-Inc/TileDB-R)

# TileDB-R

The TileDB R package offers an [R](https://www.r-project.org/) interface to the [storage
engine](https://github.com/TileDB-Inc/TileDB) of [TileDB](https://tiledb.com/).

Note that the R interface to TileDB is under development, and the API is subject to change.

## [Documentation](https://tiledb-inc.github.io/TileDB-R/)

## Quickstart

TileDB needs to be installed first (from a package or from source)
for the TileDB R package to build and link correctly:

    > install.packages("remotes")
    > library(remotes)
    > remotes::install_github("TileDB-Inc/TileDB-R")
    ...
    > library(tiledb)
    > tiledb_version()
    major minor patch
        1     7     5
    > help(package=tiledb)

## Installation

As the TileDB R package has not been published on [CRAN](https://cran.r-project.org/), it must be
installed from source.

The most recent released version can be installed from
[Github](https://github.com/TileDB-Inc/TileDB-R) using the package
[remotes](https://cran.r-project.org/package=remotes).

    install.packages("remotes")
    library(remotes)
    remotes::install_github("TileDB-Inc/TileDB-R")

If the TileDB library is installed in a custom location, you need to pass the explicit path:

    remotes::install_github("TileDB-Inc/TileDB-R",
        args="--configure-args='--with-tiledb=/path/to/tiledb'")

Note that the TileDB R package is developed and tested against the latest stable (`v1.7.x`) version
of TileDB, but should also build against the newest development version.

## Quick Links

- [Installation](https://docs.tiledb.com/developer/installation)
- [Quickstart](https://docs.tiledb.com/developer/quickstart)
- [Reference Docs](https://tiledb-inc.github.io/TileDB-R/)
- [Support](https://forum.tiledb.com/)

## Copyright

The TileDB R package is Copyright 2018-2020 TileDB, Inc

## License

MIT
