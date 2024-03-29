% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Object.R
\name{tiledb_object_walk}
\alias{tiledb_object_walk}
\title{Recursively discover TileDB resources at a given root URI path}
\usage{
tiledb_object_walk(
  uri,
  order = c("PREORDER", "POSTORDER"),
  ctx = tiledb_get_context()
)
}
\arguments{
\item{uri}{root uri path to walk}

\item{order}{traversal order, one of "PREORDER" and "POSTORDER" (default "PREORDER")}

\item{ctx}{tiledb_ctx object (optional)}
}
\value{
a dataframe with object type, object uri string columns
}
\description{
Recursively discover TileDB resources at a given root URI path
}
