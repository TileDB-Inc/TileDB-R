#include <tiledb/tiledb>
#include <Rcpp.h>

#ifndef __tiledb_version_h__
#define __tiledb_version_h__

Rcpp::NumericVector tiledb_version();
Rcpp::XPtr<tiledb::Context> tiledb_ctx();

#endif