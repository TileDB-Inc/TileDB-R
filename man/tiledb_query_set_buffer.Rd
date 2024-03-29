% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Query.R
\name{tiledb_query_set_buffer}
\alias{tiledb_query_set_buffer}
\title{Set TileDB Query buffer}
\usage{
tiledb_query_set_buffer(query, attr, buffer)
}
\arguments{
\item{query}{A TileDB Query object}

\item{attr}{A character value containing the attribute}

\item{buffer}{A vector providing the query buffer}
}
\value{
The modified query object, invisisibly
}
\description{
This function allocates query buffers directly from R vectors in
case the types match: \code{integer}, \code{double}, \code{logical}. For more
general types see \code{tiledb_query_buffer_alloc_ptr} and
\code{tiledb_query_buffer_assign_ptr}
}
