# TileDB-R

[![Build Status](https://travis-ci.org/TileDB-Inc/TileDB-R.svg?branch=master)](https://travis-ci.org/TileDB-Inc/TileDB-R)

`TileDB-R` is a [R](https://www.r-project.org/) interface to the [TileDB Storage Manager](http://tiledb.io). 

## Quickstart

TileDB needs to be installed beforehand (from a package or from source)
for the TileDB-R package to build and link correctly:
  
    > install.packages("devtools")
    > library(devtools)
    > devtools::install_github("TileDB-Inc/TileDB-R@latest")
    ...
    > library(tiledb)
    > tiledb::libtiledb_version()
    major minor patch
    1     3     0 
    > help(tiledb)

## Installation

The `TileDB-R`package has not been published on [CRAN](https://cran.r-project.org/web/packages/h5/index.html)
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
    
### Developer Documentation

Instructions for setting up a RStudio devlopment environment, building, and testing the TileDB-R package are located in the [developer documentation wiki](https://github.com/TileDB-Inc/TileDB-R/wiki).