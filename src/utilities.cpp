
#define STRICT_R_HEADERS
#include <Rcpp.h>

// Create a nanotime object
//
// Nanotime is an S4 class so we invoke an existing R macro along with
// proper class attributes. For more on this see e.g.
// https://gallery.rcpp.org/articles/creating-integer64-and-nanotime-vectors/
//
Rcpp::NumericVector makeNanotime(const std::vector<int64_t>& vec) {
  size_t n = vec.size();

  Rcpp::NumericVector num(n);
  std::memcpy(&(num[0]), vec.data(), n*sizeof(double));

  Rcpp::CharacterVector cl = Rcpp::CharacterVector::create("nanotime");
  cl.attr("package") = "nanotime";

  num.attr(".S3Class") = "integer64";
  num.attr("class") = cl;
  SET_S4_OBJECT(num);

  return(num);
}

// Create a integer64 object
//
// Integer64 is an S3 class. For more on this see e.g.
// https://gallery.rcpp.org/articles/creating-integer64-and-nanotime-vectors/
//
Rcpp::NumericVector makeInteger64(const std::vector<int64_t>& vec) {
  size_t n = vec.size();

  Rcpp::NumericVector num(n);
  std::memcpy(&(num[0]), vec.data(), n*sizeof(double));

  num.attr("class") = "integer64";
  return(num);
}


// Convert to a scalar int64_t
//
int64_t makeScalarInteger64(const double val) {
  int64_t newval;
  memcpy(&newval, &val, sizeof(double));
  return newval;
}
