% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ArraySchemaEvolution.R
\name{tiledb_array_schema_evolution_extend_enumeration}
\alias{tiledb_array_schema_evolution_extend_enumeration}
\title{Extend an Evolution via Array Schema Evolution}
\usage{
tiledb_array_schema_evolution_extend_enumeration(
  ase,
  array,
  enum_name,
  new_values,
  nullable = FALSE,
  ordered = FALSE,
  ctx = tiledb_get_context()
)
}
\arguments{
\item{ase}{An ArraySchemaEvolution object}

\item{array}{A TileDB Array object}

\item{enum_name}{A character value with the Enumeration name}

\item{new_values}{A character vector with the new Enumeration values}

\item{nullable}{A logical value indicating if the Enumeration can contain missing values
(with a default of \code{FALSE})}

\item{ordered}{A logical value indicating standard \code{factor} (when \code{FALSE}, the default)
or \code{ordered} (when \code{TRUE})}

\item{ctx}{Optional tiledb_ctx object}
}
\value{
The modified ArraySchemaEvolution object
}
\description{
Extend an Evolution via Array Schema Evolution
}
