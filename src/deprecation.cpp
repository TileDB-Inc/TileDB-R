//  MIT License
//
//  Copyright (c) 2017-2024 TileDB Inc.
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

// Deprecated in Core April 2024
// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_submit_async(XPtr<tiledb::Query> query) {
    check_xptr_tag<tiledb::Query>(query);
    spdl::trace("[libtiledb_query_submit_async]");
    query->submit_async();
    return query;
}

// Helper for next function
tiledb_encryption_type_t _string_to_tiledb_encryption_type_t(std::string encstr) {
    tiledb_encryption_type_t enc;
    int rc = tiledb_encryption_type_from_str(encstr.c_str(), &enc);
    if (rc == TILEDB_OK)
        return enc;
    Rcpp::stop("Unknow TileDB encryption type '%s'", encstr.c_str());
}

// Deprecated in Core April 2024
// [[Rcpp::export]]
std::string libtiledb_array_create_with_key(std::string uri, XPtr<tiledb::ArraySchema> schema,
                                            std::string encryption_key) {
    check_xptr_tag<tiledb::ArraySchema>(schema);
    tiledb::Array::create(uri, *schema.get(),
                          _string_to_tiledb_encryption_type_t("AES_256_GCM"),
                          encryption_key);
    return uri;
}
