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

// [[Rcpp::plugins(cpp11)]]


// declarations needed
XPtr<tiledb::ArraySchema> libtiledb_array_get_schema(XPtr<tiledb::Array> array);
XPtr<tiledb::Domain> libtiledb_array_schema_get_domain(XPtr<tiledb::ArraySchema> schema);
XPtr<tiledb::Attribute> libtiledb_array_schema_get_attribute_from_name(XPtr<tiledb::ArraySchema> schema, std::string name);
std::string libtiledb_attribute_get_type(XPtr<tiledb::Attribute> attr);



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
XPtr<vlc_buf_t> libtiledb_query_buffer_var_char_alloc(XPtr<tiledb::Array> array,
                                                      SEXP subarray, std::string attribute,
                                                      int szoffsets = 0, int szdata = 0) {
  XPtr<vlc_buf_t> buf = XPtr<vlc_buf_t>(new vlc_buf_t);
  if (TYPEOF(subarray) == INTSXP) {
    auto sub = as<std::vector<int32_t>>(subarray);
    auto max_elements = array->max_buffer_elements(sub);
    buf->offsets.resize(szoffsets <= 0 ? max_elements[attribute].first : szoffsets);
    buf->str.resize(szdata <= 0 ? max_elements[attribute].second : szdata);
    buf->rows = sub[1] - sub[0] + 1;
    if (sub.size() == 4) {
      buf->cols = sub[3] - sub[2] + 1;
    } else {
      buf->cols = 1;
    }
  } else if (TYPEOF(subarray) == REALSXP) {
    auto sub = as<std::vector<double>>(subarray);
    auto max_elements = array->max_buffer_elements(sub);
    buf->offsets.resize(szoffsets <= 0 ? max_elements[attribute].first : szoffsets);
    buf->str.resize(szdata <= 0 ? max_elements[attribute].second : szdata);
    buf->rows = sub[1] - sub[0] + 1;
    if (sub.size() == 4) {
      buf->cols = sub[3] - sub[2] + 1;
    } else {
      buf->cols = 1;
    }
  } else {
    Rcpp::stop("Invalid subarray buffer type for domain: '%s'", Rcpp::type2name(subarray));
  }
  return buf;
}


// In the following signature we cannot have a templated type as the return type so we have
// to bring the switch between types 'inside' and make it run-time dependent on the subarray
// type we already had
// [[Rcpp::export]]
XPtr<vlv_buf_t> libtiledb_query_buffer_var_vec_alloc(XPtr<tiledb::Array> array,
                                                     SEXP subarray, std::string attribute,
                                                     int szoffsets = 0, int szdata = 0) {

  XPtr<tiledb::ArraySchema> sch = libtiledb_array_get_schema(array);
  XPtr<tiledb::Domain> dom = libtiledb_array_schema_get_domain(sch);
  XPtr<tiledb::Attribute> attr = libtiledb_array_schema_get_attribute_from_name(sch, attribute);
  std::string typestr = libtiledb_attribute_get_type(attr);
  XPtr<vlv_buf_t> buf = XPtr<vlv_buf_t>(new vlv_buf_t);
  auto sub = as<std::vector<int32_t>>(subarray);
  auto max_elements = array->max_buffer_elements(sub);
  buf->offsets.resize(szoffsets <= 0 ? max_elements[attribute].first : szoffsets);
  if (typestr == "INT32") {
    buf->idata.resize(szdata <= 0 ? max_elements[attribute].second : szdata);
    buf->ddata.clear();
    buf->dtype = TILEDB_INT32;
  } else if (typestr == "FLOAT64") {
    buf->ddata.resize(szdata <= 0 ? max_elements[attribute].second : szdata);
    buf->idata.clear();
    buf->dtype = TILEDB_FLOAT64;
  } else {
    Rcpp::stop("Invalid type for buffer: '%s'", typestr.c_str());
  }
  return buf;
}


// [[Rcpp::export]]
std::string libtiledb_coords() {
  return tiledb_coords();
}


// using domain type information
// [[Rcpp::export]]
R_xlen_t libtiledb_array_max_buffer_elements_with_type(XPtr<tiledb::Array> array,
                                                       SEXP subarray,
                                                       std::string attribute,
                                                       std::string typestr) {
  if (typestr == "INT32") {
    auto sub = as<std::vector<int32_t>>(subarray);
    auto max_elements = array->max_buffer_elements(sub);
    return max_elements[attribute].second;
  } else if (typestr == "FLOAT64") {
    auto sub = as<std::vector<double>>(subarray);
    auto max_elements = array->max_buffer_elements(sub);
    return max_elements[attribute].second;
  } else if (typestr == "INT64" ||
             typestr == "UINT64" ||
             typestr == "UINT32" ||
             typestr == "DATETIME_DAY" ||
             typestr == "DATETIME_HR"  ||
             typestr == "DATETIME_MIN" ||
             typestr == "DATETIME_SEC" ||
             typestr == "DATETIME_MS" ||
             typestr == "DATETIME_US" ||
             typestr == "DATETIME_NS") {
    NumericVector svec(subarray);
    std::vector<int64_t> v(svec.size());
    for (int i=0; i<svec.size(); i++) {
      v[i] = static_cast<int64_t>(svec[i]);
    }
    auto max_elements = array->max_buffer_elements(v);
    return max_elements[attribute].second;
  } else {
    Rcpp::stop("Invalid subarray buffer type '%s' for domain: '%s'",
               typestr.c_str(), Rcpp::type2name(subarray));
  }
}


// [[Rcpp::export]]
R_xlen_t libtiledb_array_max_buffer_elements(XPtr<tiledb::Array> array,
                                             SEXP subarray,
                                             std::string attribute) {
  if (TYPEOF(subarray) == INTSXP) {
    auto sub = as<std::vector<int32_t>>(subarray);
    auto max_elements = array->max_buffer_elements(sub);
    return max_elements[attribute].second;
  } else if (TYPEOF(subarray) == REALSXP) {
    auto sub = as<std::vector<double>>(subarray);
    auto max_elements = array->max_buffer_elements(sub);
    return max_elements[attribute].second;
  } else {
    Rcpp::stop("Invalid subarray buffer type for domain: '%s'", Rcpp::type2name(subarray));
  }
}


// [[Rcpp::export]]
NumericVector libtiledb_array_max_buffer_elements_vec(XPtr<tiledb::Array> array,
                                                      SEXP subarray,
                                                      std::string attribute) {
  if (TYPEOF(subarray) == INTSXP) {
    auto sub = as<std::vector<int32_t>>(subarray);
    auto max_elements = array->max_buffer_elements(sub);
    return NumericVector::create(max_elements[attribute].first, max_elements[attribute].second);
  } else if (TYPEOF(subarray) == REALSXP) {
    auto sub = as<std::vector<double>>(subarray);
    auto max_elements = array->max_buffer_elements(sub);
    return NumericVector::create(max_elements[attribute].first, max_elements[attribute].second);
  } else {
    Rcpp::stop("Invalid subarray buffer type for domain %s", Rcpp::type2name(subarray));
  }
}
