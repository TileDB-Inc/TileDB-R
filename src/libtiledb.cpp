#include "libtiledb.h"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
NumericVector tiledb_version() {
    auto ver = tiledb::Version::version();
    NumericVector Rver = NumericVector::create(ver.major(), ver.minor(), ver.patch()) ;
    return Rver;
}

// [[Rcpp::export]]
XPtr<tiledb::Context> tiledb_ctx() {
  try {
    auto ctx = new tiledb::Context();
    return XPtr<tiledb::Context>(ctx);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}


// [[Rcpp::export]]
XPtr<tiledb::Config> tiledb_config() {
  try {
    auto config = new tiledb::Config();
    return XPtr<tiledb::Config>(config);
  } catch (tiledb::TileDBError& err) {
    throw  Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Config> tiledb_config_set(XPtr<tiledb::Config> xconfig,
                                       std::string param,
                                       std::string value) {
  try {
    (*xconfig)[param] = value;
    return xconfig;
  } catch (tiledb::TileDBError& err)  {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
CharacterVector tiledb_config_get(XPtr<tiledb::Config> xconfig,
                                  std::string param) {
  try {
    return xconfig->get(param);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
void tiledb_config_dump(XPtr<tiledb::Config> xconfig) {
  try {
    std::cout << "Config settings:\n";
    for (auto& p : *xconfig) {
      std::cout << "\"" << p.first << "\" : \"" << p.second << "\"\n";
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}