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
XPtr<tiledb::Config> tiledb_config(Nullable<CharacterVector> config=R_NilValue) {
  try {
    XPtr<tiledb::Config> tiledb_config(new tiledb::Config(), true);
    if (config.isNotNull()) {
      CharacterVector config_vec(config);
      CharacterVector config_names = config_vec.names();
      for (auto &name : config_names) {
        std::string param = as<std::string>(name);
        std::string value = as<std::string>(config_vec[param]);
        tiledb_config->set(param, value);
      }
    }
    return tiledb_config;
  } catch (tiledb::TileDBError& err) {
    throw  Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Config> tiledb_config_set(XPtr<tiledb::Config> config,
                                       std::string param,
                                       std::string value) {
  try {
    (*config)[param] = value;
    return config;
  } catch (tiledb::TileDBError& err)  {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
CharacterVector tiledb_config_get(XPtr<tiledb::Config> config,
                                  std::string param) {
  try {
    return config->get(param);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
void tiledb_config_dump(XPtr<tiledb::Config> config) {
  try {
    Rcout << "Config settings:\n";
    for (auto& p : *config) {
      Rcout << "\"" << p.first << "\" : \"" << p.second << "\"\n";
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}