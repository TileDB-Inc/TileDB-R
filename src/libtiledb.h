#include <tiledb/tiledb>

#define STRICT_R_HEADERS
#include <Rcpp.h>

// in inst/include so that Rcpp code generation can use the types for glue code
#include <tiledb.h>

#ifndef __libtiledb_h__
#define __libtiledb_h__

// Version
Rcpp::NumericVector tiledb_version();

// Context
Rcpp::XPtr<tiledb::Context> tiledb_ctx();

// Config
Rcpp::XPtr<tiledb::Config> tiledb_config();
Rcpp::XPtr<tiledb::Config> tiledb_config_set(Rcpp::XPtr<tiledb::Config> xconfig,
                                             std::string param,
                                             std::string value);
Rcpp::CharacterVector tiledb_config_get(Rcpp::XPtr<tiledb::Config> xconfig,
                                        std::string param);

void tiledb_config_dump(Rcpp::XPtr<tiledb::Config> config);

// Conversion helper
Rcpp::NumericVector makeNanotime(const std::vector<int64_t>& vec);

#endif
