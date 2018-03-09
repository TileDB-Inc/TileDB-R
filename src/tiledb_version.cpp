#include <tiledb/tiledb>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
NumericVector tiledb_version() {
    auto ver = tiledb::Version::version();
    NumericVector Rver = NumericVector::create( ver.major(), ver.minor(), ver.patch() ) ;
    return Rver;
}