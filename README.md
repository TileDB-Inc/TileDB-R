# TileDB-R

[![Build Status](https://travis-ci.org/TileDB-Inc/TileDB-R.svg?branch=master)](https://travis-ci.org/TileDB-Inc/TileDB-R)

`TileDB-R` is a [R](https://www.r-project.org/) interface to the [TileDB Storage Manager](http://tiledb.io). 

**Warning**: The R interface to TileDB is under development and the API is subject to change.

## [Documentation](https://tiledb-inc.github.io/TileDB-R/)

## Quickstart

TileDB needs to be installed beforehand (from a package or from source)
for the TileDB-R package to build and link correctly:
  
    > install.packages("devtools")
    > library(devtools)
    > devtools::install_github("TileDB-Inc/TileDB-R@latest")
    ...
    > library(tiledb)
    > tiledb_version()
    major minor patch
    1     4     2 
    > help(tiledb)

## Installation

The `TileDB-R`package has not been published on [CRAN](https://cran.r-project.org/)
and must be installed from source.

The most recent released version can be installed from [Github](https://github.com/TileDB-Inc/TileDB-R) using the package [devtools](https://github.com/r-lib/devtools).
    
    install.packages("devtools") 
    library(devtools)
    devtools::install_github("TileDB-Inc/TileDB-R@latest")

If the TileDB library is installed in a custom location, you need to pass the explicit path:
  
    devtools::install_github("TileDB-Inc/TileDB-R@latest",
        args="--configure-args='--with-tiledb=/path/to/tiledb'"")

To build the latest development version of TileDB-R:

    devtools::install_github("TileDB-Inc/TileDB-R")
    
**Note** The TileDB-R package is developed against latest stable (`v1.3.x`) version of TileDB 

### Conda

If you are using the TileDB Conda package, you may need to explicitly add the conda path
after activating the environment with `conda activate tiledb`.  More information on the
[TileDB conda installation documentation page](https://docs.tiledb.io/en/stable/installation.html#conda).

### Developer Documentation

Instructions for setting up a RStudio devlopment environment, building, and testing the TileDB-R package are located in the [developer documentation wiki](https://github.com/TileDB-Inc/TileDB-R/wiki).
