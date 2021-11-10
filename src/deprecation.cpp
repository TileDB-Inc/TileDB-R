//  MIT License
//
//  Copyright (c) 2017-2021 TileDB Inc.
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

// compilation is noisy with the deprecation, we cannot use -Wno-deprecated-declarations
// as CRAN flags it as a non-portable compiler option, and we cannot (easily) remove the
// code (yet) so silencing it is for now, and regrouping the affected routines here which
// also minimizes the surface of code covered by this definition
#define TILEDB_DEPRECATED

#include "libtiledb.h"
#include "tiledb_version.h"

#include <fstream>
#include <unistd.h>

using namespace Rcpp;

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_coordinates(XPtr<tiledb::Query> query,
                                                    SEXP coords,
                                                    std::string dtype) {
  //printf("In qsc %s\n", dtype.c_str());
  if (dtype == "DATETIME_MS") {
    IntegerVector sub(coords);
    std::vector<int64_t> vec(sub.length());
    for (int i=0; i<sub.length(); i++) {
      vec[i] = sub[i];
      //Rprintf("%d %d %ld %lu\n", sub[i], vec[i],
      //        static_cast<int64_t>(sub[i]), static_cast<uint64_t>(sub[i]));
    }
    query->set_coordinates(vec.data(), vec.size());
    return query;
  } else if (TYPEOF(coords) == INTSXP) {
    IntegerVector sub(coords);
    query->set_coordinates(sub.begin(), sub.length());
    return query;
  } else if (TYPEOF(coords) == REALSXP) {
    NumericVector sub(coords);
    query->set_coordinates(sub.begin(), sub.length());
    return query;
  } else {
    Rcpp::stop("invalid subarray datatype");
  }
}


// [[Rcpp::export]]
std::string libtiledb_coords() {
  return tiledb_coords();
}
