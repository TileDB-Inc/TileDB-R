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

#ifndef __libtiledb_h__
#define __libtiledb_h__

// in inst/include so that Rcpp code generation can use the types for glue code
#include "tiledb.h"

// logging support in RcppSpdlog namespace without making recourse to fmt::format
#include <tinyspdl.h>

// int64 conversion helpers from header package RcppInt64
// using narrower include of just functions.h
#include <rcppint64_bits/functions.h>

#include "nanoarrow.h"

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
//Rcpp::NumericVector makeNanotime(const std::vector<int64_t>& vec);
//Rcpp::NumericVector makeInteger64(const std::vector<int64_t>& vec);
//int64_t makeScalarInteger64(const double val);
//std::vector<int64_t> getInt64Vector(Rcpp::NumericVector vec);
//bool isInteger64(Rcpp::NumericVector v);
bool is_datetime_column(const tiledb_datatype_t dtype);

// duration helpers
std::vector<int64_t> dates_to_int64(Rcpp::DateVector dv, tiledb_datatype_t dtype);
Rcpp::DateVector int64_to_dates(std::vector<int64_t>, tiledb_datatype_t dtype);
std::vector<int64_t> datetimes_to_int64(Rcpp::DatetimeVector dv, tiledb_datatype_t dtype);
Rcpp::DatetimeVector int64_to_datetimes(std::vector<int64_t> iv, tiledb_datatype_t dtype);
std::vector<int64_t> subnano_to_int64(NumericVector nv, tiledb_datatype_t dtype);
Rcpp::NumericVector int64_to_subnano(std::vector<int64_t> iv, tiledb_datatype_t dtype);

// nullable helpers
void getValidityMapFromInteger(Rcpp::IntegerVector & vec, std::vector<uint8_t> & map, int32_t nc);
void setValidityMapForInteger(Rcpp::IntegerVector & vec, const std::vector<uint8_t> & map, int32_t nc);
void getValidityMapFromNumeric(Rcpp::NumericVector & vec, std::vector<uint8_t> & map, int32_t nc);
void setValidityMapForNumeric(Rcpp::NumericVector & vec, const std::vector<uint8_t> & map, int32_t nc);
void getValidityMapFromInt64(Rcpp::NumericVector & vec, std::vector<uint8_t> & map, int32_t nc);
void setValidityMapForInt64(std::vector<int64_t> & vec, const std::vector<uint8_t> & map, int32_t nc);
void getValidityMapFromLogical(Rcpp::LogicalVector & vec, std::vector<uint8_t> & map, int32_t nc);
void setValidityMapForLogical(Rcpp::LogicalVector & vec, const std::vector<uint8_t> & map, int32_t nc);

// type and size helper
tiledb_datatype_t _string_to_tiledb_datatype(std::string typestr);


// enum for TileDB XPtr Object type using int32_t payload (for R)
enum tiledb_xptr_object : int32_t {};
const tiledb_xptr_object tiledb_xptr_default                     { 0 };
const tiledb_xptr_object tiledb_xptr_object_array                { 10 };
const tiledb_xptr_object tiledb_xptr_object_arrayschema          { 20 };
const tiledb_xptr_object tiledb_xptr_object_arrayschemaevolution { 30 };
const tiledb_xptr_object tiledb_xptr_object_attribute            { 40 };
const tiledb_xptr_object tiledb_xptr_object_config               { 50 };
const tiledb_xptr_object tiledb_xptr_object_context              { 60 };
const tiledb_xptr_object tiledb_xptr_object_dimension            { 70 };
const tiledb_xptr_object tiledb_xptr_object_domain               { 80 };
const tiledb_xptr_object tiledb_xptr_object_filter               { 90 };
const tiledb_xptr_object tiledb_xptr_object_filterlist           { 100 };
const tiledb_xptr_object tiledb_xptr_object_fragmentinfo         { 110 };
const tiledb_xptr_object tiledb_xptr_object_group                { 120 };
const tiledb_xptr_object tiledb_xptr_object_query                { 130 };
const tiledb_xptr_object tiledb_xptr_object_querycondition       { 140 };
const tiledb_xptr_object tiledb_xptr_object_vfs                  { 150 };
const tiledb_xptr_object tiledb_xptr_vfs_fh_t              		 { 160 };
const tiledb_xptr_object tiledb_xptr_vlc_buf_t                   { 170 };
const tiledb_xptr_object tiledb_xptr_vlv_buf_t                   { 180 };
const tiledb_xptr_object tiledb_xptr_query_buf_t                 { 190 };
const tiledb_xptr_object tiledb_xptr_object_subarray             { 200 };
const tiledb_xptr_object tiledb_xptr_column_buffer               { 210 };
const tiledb_xptr_object tiledb_xptr_array_buffers               { 220 };
const tiledb_xptr_object tiledb_xptr_map_to_col_buf_t            { 230 };

// the definitions above are internal to tiledb-r but we need a new value here if we want tag the external pointer
const tiledb_xptr_object tiledb_arrow_array_t                    { 300 };
const tiledb_xptr_object tiledb_arrow_schema_t                   { 310 };

// templated checkers for external pointer tags
template <typename T> const int32_t XPtrTagType                            = tiledb_xptr_default; // clang++ wants a value
template <> inline const int32_t XPtrTagType<tiledb::Array>                = tiledb_xptr_object_array;
template <> inline const int32_t XPtrTagType<tiledb::ArraySchema>          = tiledb_xptr_object_arrayschema;
template <> inline const int32_t XPtrTagType<tiledb::ArraySchemaEvolution> = tiledb_xptr_object_arrayschemaevolution;
template <> inline const int32_t XPtrTagType<tiledb::Attribute>            = tiledb_xptr_object_attribute;
template <> inline const int32_t XPtrTagType<tiledb::Config>               = tiledb_xptr_object_config;
template <> inline const int32_t XPtrTagType<tiledb::Context>              = tiledb_xptr_object_context;
template <> inline const int32_t XPtrTagType<tiledb::Dimension>            = tiledb_xptr_object_dimension;
template <> inline const int32_t XPtrTagType<tiledb::Domain>               = tiledb_xptr_object_domain;
template <> inline const int32_t XPtrTagType<tiledb::Filter>               = tiledb_xptr_object_filter;
template <> inline const int32_t XPtrTagType<tiledb::FilterList>           = tiledb_xptr_object_filterlist;
template <> inline const int32_t XPtrTagType<tiledb::FragmentInfo>         = tiledb_xptr_object_fragmentinfo;
template <> inline const int32_t XPtrTagType<tiledb::Group>                = tiledb_xptr_object_group;
template <> inline const int32_t XPtrTagType<tiledb::Query>                = tiledb_xptr_object_query;
template <> inline const int32_t XPtrTagType<tiledb::QueryCondition>       = tiledb_xptr_object_query;
template <> inline const int32_t XPtrTagType<tiledb::VFS>                  = tiledb_xptr_object_vfs;
template <> inline const int32_t XPtrTagType<vfs_fh_t>                     = tiledb_xptr_vfs_fh_t;
template <> inline const int32_t XPtrTagType<vlc_buf_t>                    = tiledb_xptr_vlc_buf_t;
template <> inline const int32_t XPtrTagType<vlv_buf_t>                    = tiledb_xptr_vlv_buf_t;
template <> inline const int32_t XPtrTagType<query_buf_t>                  = tiledb_xptr_query_buf_t;
template <> inline const int32_t XPtrTagType<tiledb::Subarray>             = tiledb_xptr_object_subarray;
template <> inline const int32_t XPtrTagType<tiledb::ColumnBuffer>         = tiledb_xptr_column_buffer;
template <> inline const int32_t XPtrTagType<tiledb::ArrayBuffers>         = tiledb_xptr_array_buffers;
template <> inline const int32_t XPtrTagType<map_to_col_buf_t>             = tiledb_xptr_map_to_col_buf_t;

template <> inline const int32_t XPtrTagType<ArrowArray>             	   = tiledb_arrow_array_t;
template <> inline const int32_t XPtrTagType<ArrowSchema>             	   = tiledb_arrow_schema_t;

template <typename T> XPtr<T> make_xptr(T* p, bool finalize=true) {
    return XPtr<T>(p, finalize, Rcpp::wrap(XPtrTagType<T>), R_NilValue);
}

template <typename T> XPtr<T> make_xptr(SEXP p) {
    return XPtr<T>(p); 	// the default XPtr ctor with deleter on and tag and prot nil
}

template<typename T> void check_xptr_tag(XPtr<T> ptr) {
    spdl::trace("[check_xptr_tag]");
    if (R_ExternalPtrTag(ptr) == R_NilValue) {
        Rcpp::stop("External pointer without tag, expected tag %d\n", XPtrTagType<T>);
    }
    if (R_ExternalPtrTag(ptr) != R_NilValue) {
        int32_t tag = Rcpp::as<int32_t>(R_ExternalPtrTag(ptr));
        if (XPtrTagType<T> != tag) {
            Rcpp::stop("Wrong tag type: expected %d but received %d\n", XPtrTagType<T>, tag);
        }
    }
}


#endif
