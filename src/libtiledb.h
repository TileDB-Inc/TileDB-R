#include <tiledb/tiledb>
#include <Rcpp.h>

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

#endif