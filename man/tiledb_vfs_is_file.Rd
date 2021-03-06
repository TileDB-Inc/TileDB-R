% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VFS.R
\name{tiledb_vfs_is_file}
\alias{tiledb_vfs_is_file}
\title{Test for VFS File}
\usage{
tiledb_vfs_is_file(uri, vfs = tiledb_get_vfs())
}
\arguments{
\item{uri}{Character variable with a URI describing a file path}

\item{vfs}{A TileDB VFS object; default is to use a cached value.}
}
\value{
A boolean value indicating if it is a file
}
\description{
Test for VFS File
}
