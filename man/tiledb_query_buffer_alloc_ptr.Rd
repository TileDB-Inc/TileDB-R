% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Query.R
\name{tiledb_query_buffer_alloc_ptr}
\alias{tiledb_query_buffer_alloc_ptr}
\title{Allocate a Query buffer for a given type}
\usage{
tiledb_query_buffer_alloc_ptr(query, datatype, ncells)
}
\arguments{
\item{query}{A TileDB Query object}

\item{datatype}{A character value containing the data type}

\item{ncells}{A number of elements (not bytes)}
}
\value{
An external pointer to the allocated buffer object
}
\description{
This function allocates a query buffer for the given data type.
}
