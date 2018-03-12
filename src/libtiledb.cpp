#include "libtiledb.h"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export(rng = false)]]
NumericVector tiledb_version() {
    auto ver = tiledb::Version::version();
    NumericVector Rver = NumericVector::create(ver.major(), ver.minor(), ver.patch()) ;
    return Rver;
}

// [[Rcpp::export(rng = false)]]
XPtr<tiledb::Context> tiledb_ctx() {
  try {
    auto ctx = new tiledb::Context();
    return XPtr<tiledb::Context>(ctx);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export(rng = false)]]
XPtr<tiledb::Config> tiledb_config() {
  try {
    auto config = new tiledb::Config();
    return XPtr<tiledb::Config>(config);
  } catch (tiledb::TileDBError& err) {
    throw  Rcpp::exception(err.what());
  }
}

// [[Rcpp::export(rng = false)]]
void tiledb_config_dump(XPtr<tiledb::Config> config) {
  try {
    std::cout << "Config settings:\n";
    for (auto& p : *config) {
      std::cout << "\"" << p.first << "\" : \"" << p.second << "\"\n";
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}