//  MIT License
//
//  Copyright (c) 2021 TileDB Inc.
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

#include "libtiledb.h"

// In principle, the following functions could be templated. In practive it is a little
// harder as the NA values are not IEEE 754 generic (beyond double)

void getValidityMapFromInteger(Rcpp::IntegerVector & vec, std::vector<uint8_t> & map) {
    if (static_cast<size_t>(vec.size()) != map.size())
        Rcpp::stop("Unequal length between vector and map.");

    for (auto i=0; i < vec.size(); i++)
        map[i] = (vec[i] == R_NaInt) ? 0 : 1;
}

void setValidityMapForInteger(Rcpp::IntegerVector & vec, const std::vector<uint8_t> & map) {
    if (static_cast<size_t>(vec.size()) != map.size())
        Rcpp::stop("Unequal length between vector and map.");

    for (auto i=0; i < vec.size(); i++)
        if (map[i] == 0)
            vec[i] = R_NaInt;
}

void getValidityMapFromNumeric(Rcpp::NumericVector & vec, std::vector<uint8_t> & map) {
    if (static_cast<size_t>(vec.size()) != map.size())
        Rcpp::stop("Unequal length between vector and map.");

    for (auto i=0; i < vec.size(); i++) {
        // see R_ext/Arith.h: true for both NA and NaN
        map[i] = (R_isnancpp(vec[i])) ? 0 : 1;
    }
}

void setValidityMapForNumeric(Rcpp::NumericVector & vec, const std::vector<uint8_t> & map) {
    if (static_cast<size_t>(vec.size()) != map.size())
        Rcpp::stop("Unequal length between vector and map.");

    for (auto i=0; i < vec.size(); i++)
        if (map[i] == 0)
            vec[i] = R_NaReal;
}

// as defined in the bit64 package file integer64.h
#define NA_INTEGER64 LLONG_MIN
#define ISNA_INTEGER64(X)((X)==NA_INTEGER64)

void getValidityMapFromInt64(Rcpp::NumericVector & vec, std::vector<uint8_t> & map) {
    if (static_cast<size_t>(vec.size()) != map.size())
        Rcpp::stop("Unequal length between vector and map.");

    std::vector<int64_t> ivec = getInt64Vector(vec);

    for (auto i=0; i < vec.size(); i++) {
        map[i] = (ISNA_INTEGER64(ivec[i])) ? 0 : 1;
    }
}

void setValidityMapForInt64(std::vector<int64_t> & vec, const std::vector<uint8_t> & map) {
    if (static_cast<size_t>(vec.size()) != map.size())
        Rcpp::stop("Unequal length between vector and map.");

    for (size_t i=0; i < vec.size(); i++)
        if (map[i] == 0)
            vec[i] = NA_INTEGER64;
}
