[![ci](https://github.com/TileDB-Inc/TileDB-R/actions/workflows/ci.yaml/badge.svg)](https://github.com/TileDB-Inc/TileDB-R/actions/workflows/ci.yaml)
[![windows](https://github.com/TileDB-Inc/TileDB-R/actions/workflows/windows.yaml/badge.svg)](https://github.com/TileDB-Inc/TileDB-R/actions/workflows/windows.yaml)
[![valgrind](https://github.com/TileDB-Inc/TileDB-R/actions/workflows/valgrind.yaml/badge.svg)](https://github.com/TileDB-Inc/TileDB-R/actions/workflows/valgrind.yaml)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/tiledb?color=brightgreen)](https://cran.r-project.org/package=tiledb)
[![CRAN](https://www.r-pkg.org/badges/version/tiledb)](https://cran.r-project.org/package=tiledb)
[![r-universe](https://tiledb-inc.r-universe.dev/badges/tiledb)](https://tiledb-inc.r-universe.dev/tiledb)
[![r-universe dev](https://eddelbuettel.r-universe.dev/badges/tiledb)](https://eddelbuettel.r-universe.dev/tiledb)

# <a href="https://tiledb.com/"><img src="https://github.com/TileDB-Inc/TileDB/raw/dev/doc/source/_static/tiledb-logo_color_no_margin_@4x.png" alt="TileDB logo" width="400"></a>

The TileDB R package offers an [R](https://www.r-project.org/) interface to
the [modern database](https://github.com/TileDB-Inc/TileDB) by [TileDB](https://tiledb.com/).


## Documentation

Documentation is available for [the R
package](https://tiledb-inc.github.io/TileDB-R/) as well as for the [TileDB
API](https://docs.tiledb.com/main/).


## Installation

TileDB Embedded needs to be present, and can be installed first (from a package or from source) for
the TileDB R package to build and link correctly. Alternatively, if no system installation is found,
a precompiled shared library is used during the installation of this R package.

The TileDB R package has been published on [CRAN](https://cran.r-project.org/) and be
installed directly via

    > install.packages("tiledb")

as usual.

The most recent released version can be installed from
[Github](https://github.com/TileDB-Inc/TileDB-R) using the package
[remotes](https://cran.r-project.org/package=remotes).

    > if (!requireNamespace("remotes",quietly=TRUE)) install.packages("remotes")
    > remotes::install_github("TileDB-Inc/TileDB-R")
    ...
    > library(tiledb)
    TileDB R 0.30.0 with TileDB Embedded 2.26.0 on Ubuntu 24.04.
    See https://tiledb.com for more information about TileDB.
    > help(package=tiledb)

If the TileDB library is installed in a custom location, you need to pass the explicit path:

    > remotes::install_github("TileDB-Inc/TileDB-R",
          args="--configure-args='--with-tiledb=/path/to/tiledb'")

Note that the TileDB R package is always developed and tested against the latest stable version
of TileDB, but should also build against the newest development version.


## Quick Links

- [TileDB Installation](https://docs.tiledb.com/main/how-to/installation/quick-install)
- [TileDB R Package Docs](https://tiledb-inc.github.io/TileDB-R/)
- [TileDB Support Forum](https://forum.tiledb.com/)


## Copyright

The TileDB R package is Copyright 2018-2024 TileDB, Inc

## License

MIT
