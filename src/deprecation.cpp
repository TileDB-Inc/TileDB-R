//  MIT License
//
//  Copyright (c) 2017-2022 TileDB Inc.
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

using namespace Rcpp;

// Using add_range() on a Query object is deprecated in TileDB Core, and will be
// removed in a future release.  Until then it will remain accessible here for
// the convenience of R package users
//
// The new function libtiledb_subarray_add_range_with_type() has the same
// functionality, but uses a a Subarray external pointer instead of an Query
// external pointer.  User have to first create a Subarray pointer and then
// set it in the query object -- see R/TileDBArray.R for one example

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_add_range_with_type(XPtr<tiledb::Query> query,
                                                        int iidx,
                                                        std::string typestr,
                                                        SEXP starts, SEXP ends,
                                                        SEXP strides = R_NilValue) {

  check_xptr_tag<tiledb::Query>(query);
  spdl::debug("[libtiledb_query_add_range_with type] deprecated subarray setter for type {}", typestr);
  if (TYPEOF(starts) != TYPEOF(ends)) {
      Rcpp::stop("'start' and 'end' must be of identical types");
  }
  uint32_t uidx = static_cast<uint32_t>(iidx);

  if (typestr == "INT32") {
    int32_t start = as<int32_t>(starts);
    int32_t end = as<int32_t>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      int32_t stride = as<int32_t>(strides);
      query->add_range(uidx, start, end, stride);
    }
  } else if (typestr == "FLOAT64") {
    double start = as<double>(starts);
    double end = as<double>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      double stride = as<double>(strides);
      query->add_range(uidx, start, end, stride);
    }
  } else if (typestr == "INT64") {
    int64_t start = makeScalarInteger64(as<double>(starts));
    int64_t end = makeScalarInteger64(as<double>(ends));
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      int64_t stride = makeScalarInteger64(as<double>(strides));
      query->add_range(uidx, start, end, stride);
    }
  } else if (typestr == "UINT64") {
    uint64_t start = static_cast<uint64_t>(makeScalarInteger64(as<double>(starts)));
    uint64_t end = static_cast<uint64_t>(makeScalarInteger64(as<double>(ends)));
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      uint64_t stride = makeScalarInteger64(as<double>(strides));
      query->add_range(uidx, start, end, stride);
    }
  } else if (typestr == "UINT32") {
    uint32_t start = as<uint32_t>(starts);
    uint32_t end   = as<uint32_t>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      uint32_t stride = as<int32_t>(strides);
      query->add_range(uidx, start, end, stride);
    }
  } else if (typestr == "INT16") {
    int16_t start = as<int16_t>(starts);
    int16_t end   = as<int16_t>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      int16_t stride = as<int16_t>(strides);
      query->add_range(uidx, start, end, stride);
    }
  } else if (typestr == "UINT16") {
    uint16_t start = as<uint16_t>(starts);
    uint16_t end   = as<uint16_t>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      uint16_t stride = as<uint16_t>(strides);
      query->add_range(uidx, start, end, stride);
    }
  } else if (typestr == "INT8") {
    int8_t start = as<int16_t>(starts);
    int8_t end   = as<int16_t>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      int8_t stride = as<int16_t>(strides);
      query->add_range(uidx, start, end, stride);
    }
  } else if (typestr == "UINT8") {
    uint8_t start = as<uint16_t>(starts);
    uint8_t end   = as<uint16_t>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      uint8_t stride = as<uint16_t>(strides);
      query->add_range(uidx, start, end, stride);
    }
  } else if (typestr == "DATETIME_YEAR"  ||
             typestr == "DATETIME_MONTH" ||
             typestr == "DATETIME_WEEK"  ||
             typestr == "DATETIME_DAY"   ||
             typestr == "DATETIME_HR"    ||
             typestr == "DATETIME_MIN"   ||
             typestr == "DATETIME_SEC"   ||
             typestr == "DATETIME_MS"    ||
             typestr == "DATETIME_US"   ) {
    //int64_t start = date_to_int64(as<Date>(starts), _string_to_tiledb_datatype(typestr));
    int64_t start = makeScalarInteger64(as<double>(starts));
    //int64_t end = date_to_int64(as<Date>(ends), _string_to_tiledb_datatype(typestr));
    int64_t end = makeScalarInteger64(as<double>(ends));
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      int64_t stride = as<int64_t>(strides);
      query->add_range(uidx, start, end, stride);
    }
  } else if (
             typestr == "DATETIME_NS" ||
             typestr == "DATETIME_FS" ||
             typestr == "DATETIME_PS" ||
             typestr == "DATETIME_AS") {
    int64_t start = makeScalarInteger64(as<double>(starts));
    int64_t end = makeScalarInteger64(as<double>(ends));
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      int64_t stride = as<int64_t>(strides);
      query->add_range(uidx, start, end, stride);
    }
#if TILEDB_VERSION >= TileDB_Version(2,0,0)
  } else if (typestr == "ASCII" || typestr == "CHAR") {
    std::string start = as<std::string>(starts);
    std::string end = as<std::string>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      Rcpp::stop("Non-empty stride for string not supported yet.");
    }
    //query->set_subarray(sub);
#endif
  } else if (typestr == "FLOAT32") {
    float start = as<float>(starts);
    float end = as<float>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      float stride = as<float>(strides);
      query->add_range(uidx, start, end, stride);
    }
    //query->set_subarray(sub);
  } else {
    Rcpp::stop("Invalid data type for adding range to query: '%s'", Rcpp::type2name(starts));
  }
  return query;
}

// Older (simpler) version for just int32, double, string

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_add_range(XPtr<tiledb::Query> query, int iidx,
                                              SEXP starts, SEXP ends,
                                              SEXP strides = R_NilValue) {
    check_xptr_tag<tiledb::Query>(query);
    spdl::debug("[libtiledb_query_add_range] deprecated setting subarray");
    if (TYPEOF(starts) != TYPEOF(ends)) {
        Rcpp::stop("'start' and 'end' must be of identical types");
    }
    uint32_t uidx = static_cast<uint32_t>(iidx);
    if (TYPEOF(starts) == INTSXP) {
        int32_t start = as<int32_t>(starts);
        int32_t end = as<int32_t>(ends);
        int32_t stride = (strides == R_NilValue) ? 0 : Rcpp::as<int32_t>(strides);
        query->add_range(uidx, start, end, stride);
    } else if (TYPEOF(starts) == REALSXP) {
        double start = as<double>(starts);
        double end = as<double>(ends);
        double stride = (strides == R_NilValue) ? 0 : Rcpp::as<double_t>(strides);
        query->add_range(uidx, start, end, stride);
#if TILEDB_VERSION >= TileDB_Version(2,0,0)
    } else if (TYPEOF(starts) == STRSXP) {
        std::string start = as<std::string>(starts);
        std::string end = as<std::string>(ends);
        if (strides == R_NilValue) {
            query->add_range(uidx, start, end);
        } else {
            Rcpp::stop("Non-emoty stride for string not supported yet.");
        }
#endif
    } else {
        Rcpp::stop("Invalid data type for query range: '%s'", Rcpp::type2name(starts));
    }
    return query;
}
