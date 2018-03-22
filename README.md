# TileDB-R

[![Build Status](https://travis-ci.org/TileDB-Inc/TileDB-R.svg?branch=master)](https://travis-ci.org/TileDB-Inc/TileDB-R)

`TileDB-R` is a [R](https://www.r-project.org/) interface to the [TileDB Storage Manager](http://tiledb.io). 


## Installation

`TileDB-R` has not been published on [CRAN](https://cran.r-project.org/web/packages/h5/index.html)
so it must be installed from source.

The most recent devlopment version can be installed from [Github](https://github.com/TileDB-Inc/TileDB-R)
using the package [devtools](https://github.com/r-lib/devtools).

    library(devtools)
    devtools::install_github("TileDB-Inc/TileDB-R")
    
If the TileDB library is installed in a custom location, you need to pass the explicit path:
  
    devtools::install_github("TileDB-Inc/TileDB-R",
        args="--configure-args='--with-tiledb=/path/to/tiledb'"")
    
### Developer Documentation

Instructions for setting up a RStudio devlopment environment, building, and testing the TileDB-R 
package are located here: 

https://github.com/TileDB-Inc/TileDB-R/wiki