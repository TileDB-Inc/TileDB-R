//  MIT License
//
//  Copyright (c) 2021-2023 TileDB Inc.
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

#include "libtiledb.h"

// In principle, the following functions could be templated. In practive it is a little
// harder as the NA values are not IEEE 754 generic (beyond double)

void getValidityMapFromInteger(Rcpp::IntegerVector & vec, std::vector<uint8_t> & map, const int32_t nc) {
    if (static_cast<size_t>(vec.size()) != nc * map.size())
        Rcpp::stop("Unequal length between vector (%d) and map * nc (%d) in int getter.", vec.size(), nc * map.size());

    for (auto i=0; i < vec.size(); i += nc) {
        uint8_t m = 1;  // default to no NA/NaN
        for (auto j=0; j<nc && m==1; j++) {
            if (vec[i + j] == R_NaInt) {
                m = 0;
            }
        }
        map[i / nc] = m;
    }
}

void setValidityMapForInteger(Rcpp::IntegerVector & vec, const std::vector<uint8_t> & map, const int32_t nc) {
    if (static_cast<size_t>(vec.size()) != nc * map.size())
        Rcpp::stop("Unequal length between vector (%d) and map * nc (%d) in int setter.", vec.size(), nc * map.size());

    for (auto i=0; i < vec.size(); i++)
        if (map[i/nc] == 0)
            vec[i] = R_NaInt;
}

void getValidityMapFromNumeric(Rcpp::NumericVector & vec, std::vector<uint8_t> & map, const int32_t nc) {
    if (static_cast<size_t>(vec.size()) != nc * map.size())
        Rcpp::stop("Unequal length between vector (%d) and map * nc (%d) in numeric getter.", vec.size(), nc * map.size());

    for (auto i=0; i < vec.size(); i += nc) {
        uint8_t m = 1;  // default to no NA/NaN
        for (auto j=0; j<nc && m==1; j++) {
            // see R_ext/Arith.h: true for both NA and NaN
            if (R_isnancpp(vec[i + j])) {
                m = 0;
            }
        }
        map[i / nc] = m;
        //Rprintf("getMap (%d) vec %f map %d\n", i, vec[i], map[i/nc]);
    }
}

void setValidityMapForNumeric(Rcpp::NumericVector & vec, const std::vector<uint8_t> & map, const int32_t nc) {
    if (static_cast<size_t>(vec.size()) != nc * map.size())
        Rcpp::stop("Unequal length between vector (%d) and map * nc (%d) in numeric setter.", vec.size(), nc * map.size());

    for (auto i=0; i < vec.size(); i++) {
        if (map[i/nc] == 0)
            vec[i] = R_NaReal;
        //Rprintf("setMap (%d) vec %f map %d\n", i, vec[i], map[i/nc]);
    }
}

// as defined in the bit64 package file integer64.h
#define NA_INTEGER64 LLONG_MIN
#define ISNA_INTEGER64(X)((X)==NA_INTEGER64)

void getValidityMapFromInt64(Rcpp::NumericVector & vec, std::vector<uint8_t> & map, const int32_t nc) {
    if (static_cast<size_t>(vec.size()) != nc * map.size())
        Rcpp::stop("Unequal length between vector (%d) and map * nc (%d) in int64 getter.", vec.size(), nc * map.size());

    std::vector<int64_t> ivec = fromInteger64(vec);

    for (auto i=0; i < vec.size(); i += nc) {
        uint8_t m = 1;  // default to no NA/NaN
        for (auto j=0; j<nc && m==1; j++) {
            if (ISNA_INTEGER64(ivec[i + j])) {
                m = 0;
            }
        }
        map[i / nc] = m;
    }
}

void setValidityMapForInt64(std::vector<int64_t> & vec, const std::vector<uint8_t> & map, const int32_t nc) {
    if (static_cast<size_t>(vec.size()) != nc * map.size())
        Rcpp::stop("Unequal length between vector (%d) and map * nc (%d) in int64 setter.", vec.size(), nc * map.size());

    for (size_t i=0; i < vec.size(); i++)
        if (map[i/nc] == 0)
            vec[i] = NA_INTEGER64;
}

void getValidityMapFromLogical(Rcpp::LogicalVector & vec, std::vector<uint8_t> & map, const int32_t nc) {
    if (static_cast<size_t>(vec.size()) != nc * map.size())
        Rcpp::stop("Unequal length between vector (%d) and map * nc (%d) in int getter.", vec.size(), nc * map.size());

    for (auto i=0; i < vec.size(); i += nc) {
        uint8_t m = 1;  // default to no NA/NaN
        for (auto j=0; j<nc && m==1; j++) {
            if (vec[i + j] == NA_LOGICAL) {
                m = 0;
            }
        }
        map[i / nc] = m;
    }
}

void setValidityMapForLogical(Rcpp::LogicalVector & vec, const std::vector<uint8_t> & map, const int32_t nc) {
    if (static_cast<size_t>(vec.size()) != nc * map.size())
        Rcpp::stop("Unequal length between vector (%d) and map (%d) in int setter.", vec.size(), nc * map.size());

    for (auto i=0; i < vec.size(); i++)
        if (map[i/nc] == 0)
            vec[i] = NA_LOGICAL;
}
