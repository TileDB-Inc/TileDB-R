//  MIT License
//
//  Copyright (c) 2017-2020 TileDB Inc.
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

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

// Create a int64_t vector from a NumericVector
//
std::vector<int64_t> getInt64Vector(Rcpp::NumericVector vec) {
  size_t n = vec.size();
  std::vector<int64_t> num(n);
  std::memcpy(&(num[0]), &(vec[0]), n*sizeof(double));
  return num;
}

// Check for integer64 type -- which for R 'S3' object means checking the attributes
//
bool isInteger64(Rcpp::NumericVector v) {
  if (!v.hasAttribute("class")) {
    return FALSE;
  }
  std::string s = v.attr("class");
  return s == "integer64";
}
