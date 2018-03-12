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
