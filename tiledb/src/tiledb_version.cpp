#include <tiledb/tiledb.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List tiledb_version() {
    int major = 0; 
    int minor = 0;
    int patch = 0;
    tiledb_version(&major, &minor, &patch);
    NumericVector ver = NumericVector::create( major, minor, patch ) ;
    return ver;
}
