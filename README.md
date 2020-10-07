<a href="https://tiledb.com"><img src="https://github.com/TileDB-Inc/TileDB/raw/dev/doc/source/_static/tiledb-logo_color_no_margin_@4x.png" alt="TileDB logo" width="400"></a>

[![Build Status](https://img.shields.io/azure-devops/build/tiledb-inc/836549eb-f74a-4986-a18f-7fbba6bbb5f0/24/master?label=Azure%20Pipelines&logo=azure-pipelines&style=flat-square)](https://dev.azure.com/TileDB-Inc/CI/_build/latest?definitionId=24&branchName=master)
![R-CMD-check](https://github.com/TileDB-Inc/TileDB-R/workflows/R-CMD-check/badge.svg)
[![CRAN](https://www.r-pkg.org/badges/version/tiledb)](https://cran.r-project.org/package=tiledb)

# TileDB-R

The TileDB R package offers an [R](https://www.r-project.org/) interface to the [storage
engine](https://github.com/TileDB-Inc/TileDB) of [TileDB](https://tiledb.com/).

Note that the R interface to TileDB is under development, and the API is subject to change.

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
    > tiledb_version()
    major minor patch
        2     0     8
    > help(package=tiledb)

If the TileDB library is installed in a custom location, you need to pass the explicit path:

    > remotes::install_github("TileDB-Inc/TileDB-R",
          args="--configure-args='--with-tiledb=/path/to/tiledb'")

Note that the TileDB R package is developed and tested against the latest stable (`v2.0.x`) version
of TileDB, but should also build against the newest development version.

## Quick Links

- [TileDB Installation](https://docs.tiledb.com/main/solutions/tiledb-embedded/installation/quick-install)
- [TileDB R Package Docs](https://tiledb-inc.github.io/TileDB-R/)
- [Full documentation for all APIs and integrations](https://docs.tiledb.com/main/solutions/tiledb-embedded/api-usage)
- [TileDB Support Forum](https://forum.tiledb.com/)


## Copyright

The TileDB R package is Copyright 2018-2020 TileDB, Inc

## License

MIT
