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

#ifndef __libtiledb_h__
#define __libtiledb_h__

// in inst/include so that Rcpp code generation can use the types for glue code
#include "tiledb.h"

// Version
Rcpp::NumericVector tiledb_version();

// Context
Rcpp::XPtr<tiledb::Context> tiledb_ctx();

// Config
Rcpp::XPtr<tiledb::Config> tiledb_config();
Rcpp::XPtr<tiledb::Config> tiledb_config_set(Rcpp::XPtr<tiledb::Config> xconfig,
                                             std::string param,
                                             std::string value);
Rcpp::CharacterVector tiledb_config_get(Rcpp::XPtr<tiledb::Config> xconfig,
                                        std::string param);

void tiledb_config_dump(Rcpp::XPtr<tiledb::Config> config);

// Conversion helpers
Rcpp::NumericVector makeNanotime(const std::vector<int64_t>& vec);
Rcpp::NumericVector makeInteger64(const std::vector<int64_t>& vec);
int64_t makeScalarInteger64(const double val);
std::vector<int64_t> getInt64Vector(Rcpp::NumericVector vec);
bool isInteger64(Rcpp::NumericVector v);
bool is_datetime_column(const tiledb_datatype_t dtype);

// duration helpers
std::vector<int64_t> dates_to_int64(Rcpp::DateVector dv, tiledb_datatype_t dtype);
Rcpp::DateVector int64_to_dates(std::vector<int64_t>, tiledb_datatype_t dtype);
std::vector<int64_t> datetimes_to_int64(Rcpp::DatetimeVector dv, tiledb_datatype_t dtype);
Rcpp::DatetimeVector int64_to_datetimes(std::vector<int64_t> iv, tiledb_datatype_t dtype);
std::vector<int64_t> subnano_to_int64(NumericVector nv, tiledb_datatype_t dtype);
Rcpp::NumericVector int64_to_subnano(std::vector<int64_t> iv, tiledb_datatype_t dtype);



#endif
