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

#include "libtiledb.h"
#include "finalizers.h"
#include "tiledb_version.h"

#include <fstream>
#include <unistd.h>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

const char* _tiledb_datatype_to_string(tiledb_datatype_t dtype) {
  switch (dtype) {
    case TILEDB_INT8:
      return "INT8";
    case TILEDB_UINT8:
      return "UINT8";
    case TILEDB_INT16:
      return "INT16";
    case TILEDB_UINT16:
      return "UINT16";
    case TILEDB_INT32:
      return "INT32";
    case TILEDB_UINT32:
      return "UINT32";
    case TILEDB_INT64:
      return "INT64";
    case TILEDB_UINT64:
      return "UINT64";
    case TILEDB_FLOAT32:
      return "FLOAT32";
    case TILEDB_FLOAT64:
      return "FLOAT64";
    case TILEDB_CHAR:
      return "CHAR";
    case TILEDB_STRING_ASCII:
      return "ASCII";
    case TILEDB_STRING_UTF8:
      return "UTF8";
    case TILEDB_STRING_UTF16:
      return "UTF16";
    case TILEDB_STRING_UTF32:
      return "UTF32";
    case TILEDB_STRING_UCS2:
      return "UCS2";
    case TILEDB_STRING_UCS4:
      return "UCS4";
    case TILEDB_ANY:
      return "ANY";
    case TILEDB_DATETIME_YEAR:
      return "DATETIME_YEAR";
    case TILEDB_DATETIME_MONTH:
      return "DATETIME_MONTH";
    case TILEDB_DATETIME_WEEK:
      return "DATETIME_WEEK";
    case TILEDB_DATETIME_DAY:
      return "DATETIME_DAY";
    case TILEDB_DATETIME_HR:
      return "DATETIME_HR";
    case TILEDB_DATETIME_MIN:
      return "DATETIME_MIN";
    case TILEDB_DATETIME_SEC:
      return "DATETIME_SEC";
    case TILEDB_DATETIME_MS:
      return "DATETIME_MS";
    case TILEDB_DATETIME_US:
      return "DATETIME_US";
    case TILEDB_DATETIME_NS:
      return "DATETIME_NS";
    case TILEDB_DATETIME_PS:
      return "DATETIME_PS";
    case TILEDB_DATETIME_FS:
      return "DATETIME_FS";
    case TILEDB_DATETIME_AS:
      return "DATETIME_AS";
    default:
      Rcpp::stop("unknown tiledb_datatype_t (%d)", dtype);
  }
}

tiledb_datatype_t _string_to_tiledb_datatype(std::string typestr) {
  if (typestr == "FLOAT32")  {
    return TILEDB_FLOAT32;
  } else if (typestr == "FLOAT64") {
    return TILEDB_FLOAT64;
  } else if (typestr == "ASCII") {
    return TILEDB_STRING_ASCII;
  } else if (typestr == "CHAR") {
    return TILEDB_CHAR;
  } else if (typestr == "INT8") {
    return TILEDB_INT8;
  } else if (typestr == "UINT8") {
    return TILEDB_UINT8;
  } else if (typestr == "INT16") {
    return  TILEDB_INT16;
  } else if (typestr == "UINT16") {
    return TILEDB_UINT16;
  } else if (typestr == "INT32") {
    return TILEDB_INT32;
  } else if (typestr == "UINT32") {
    return TILEDB_UINT32;
  } else if (typestr == "INT64") {
    return TILEDB_INT64;
  } else if (typestr == "UINT64") {
    return TILEDB_UINT64;
  } else if (typestr == "DATETIME_YEAR") {
    return TILEDB_DATETIME_YEAR;
  } else if (typestr == "DATETIME_MONTH") {
    return TILEDB_DATETIME_MONTH;
  } else if (typestr == "DATETIME_WEEK") {
    return TILEDB_DATETIME_WEEK;
  } else if (typestr == "DATETIME_DAY") {
    return TILEDB_DATETIME_DAY;
  } else if (typestr == "DATETIME_HR") {
    return TILEDB_DATETIME_HR;
  } else if (typestr == "DATETIME_MIN") {
    return TILEDB_DATETIME_MIN;
  } else if (typestr == "DATETIME_SEC") {
    return TILEDB_DATETIME_SEC;
  } else if (typestr == "DATETIME_MS") {
    return TILEDB_DATETIME_MS;
  } else if (typestr == "DATETIME_US") {
    return TILEDB_DATETIME_US;
  } else if (typestr == "DATETIME_NS") {
    return TILEDB_DATETIME_NS;
  } else if (typestr == "DATETIME_PS") {
    return TILEDB_DATETIME_PS;
  } else if (typestr == "DATETIME_FS") {
    return TILEDB_DATETIME_FS;
  } else if (typestr == "DATETIME_AS") {
    return TILEDB_DATETIME_AS;
  } else if (typestr == "UTF8") {
    return TILEDB_STRING_UTF8;
  } else {
    Rcpp::stop("Unknown TileDB type '%s'", typestr.c_str());
  }
}

// [[Rcpp::export]]
std::string tiledb_datatype_R_type(std::string datatype) {
  tiledb_datatype_t dtype = _string_to_tiledb_datatype(datatype);
  switch (dtype) {
    case TILEDB_INT8:
    case TILEDB_UINT8:
    case TILEDB_INT16:
    case TILEDB_UINT16:
    case TILEDB_INT32:
    case TILEDB_UINT32:
    case TILEDB_INT64:
    case TILEDB_UINT64:
      return "integer";
    case TILEDB_FLOAT32:
    case TILEDB_FLOAT64:
      return "double";
    case TILEDB_CHAR:
      return "raw";
    case TILEDB_STRING_ASCII:
    case TILEDB_STRING_UTF8:
    case TILEDB_STRING_UTF16:
    case TILEDB_STRING_UTF32:
    case TILEDB_STRING_UCS2:
    case TILEDB_STRING_UCS4:
      return "character";
    case TILEDB_ANY:
      return "any";
    case TILEDB_DATETIME_DAY:
      return "DATETIME_DAY";
    case TILEDB_DATETIME_SEC:
      return "DATETIME_SEC";
    case TILEDB_DATETIME_MS:
      return "DATETIME_MS";
    case TILEDB_DATETIME_US:
      return "DATETIME_US";
    case TILEDB_DATETIME_NS:
      return "DATETIME_NS";
    default:
      Rcpp::stop("unknown tiledb_datatype_t (%d)", dtype);
  }
}

const char* _tiledb_layout_to_string(tiledb_layout_t layout) {
  switch(layout) {
    case TILEDB_ROW_MAJOR:
      return "ROW_MAJOR";
    case TILEDB_COL_MAJOR:
      return "COL_MAJOR";
    case TILEDB_GLOBAL_ORDER:
      return "GLOBAL_ORDER";
    case TILEDB_UNORDERED:
      return "UNORDERED";
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    case TILEDB_HILBERT:
      return "HILBERT";
#endif
    default:
      Rcpp::stop("unknown tiledb_layout_t (%d)", layout);
  }
}

tiledb_layout_t _string_to_tiledb_layout(std::string lstr) {
  if (lstr == "ROW_MAJOR")  {
    return TILEDB_ROW_MAJOR;
  } else if (lstr == "COL_MAJOR") {
    return TILEDB_COL_MAJOR;
  } else if (lstr == "GLOBAL_ORDER") {
    return TILEDB_GLOBAL_ORDER;
  } else if (lstr == "UNORDERED") {
    return TILEDB_UNORDERED;
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
  } else if (lstr == "HILBERT") {
    return TILEDB_HILBERT;
#endif
  } else {
    Rcpp::stop("Unknown TileDB layout '%s' ", lstr.c_str());
  }
}

tiledb_filter_type_t _string_to_tiledb_filter(std::string filter) {
  if (filter == "NONE") {
    return TILEDB_FILTER_NONE;
  } else if (filter == "GZIP") {
    return TILEDB_FILTER_GZIP;
  } else if (filter == "ZSTD") {
    return TILEDB_FILTER_ZSTD;
  } else if (filter == "LZ4") {
    return TILEDB_FILTER_LZ4;
  } else if (filter == "RLE") {
    return TILEDB_FILTER_RLE;
  } else if (filter == "BZIP2") {
    return TILEDB_FILTER_BZIP2;
  } else if (filter == "DOUBLE_DELTA") {
    return TILEDB_FILTER_DOUBLE_DELTA;
  } else if (filter == "BIT_WIDTH_REDUCTION") {
    return TILEDB_FILTER_BIT_WIDTH_REDUCTION;
  } else if (filter == "BITSHUFFLE") {
    return TILEDB_FILTER_BITSHUFFLE;
  } else if (filter == "BYTESHUFFLE") {
    return TILEDB_FILTER_BYTESHUFFLE;
  } else if (filter == "POSITIVE_DELTA") {
    return TILEDB_FILTER_POSITIVE_DELTA;
#if TILEDB_VERSION >= TileDB_Version(1,7,6)
  } else if (filter == "CHECKSUM_MD5") {
    return TILEDB_FILTER_CHECKSUM_MD5;
  } else if (filter == "CHECKSUM_SHA256") {
    return TILEDB_FILTER_CHECKSUM_SHA256;
#endif
  } else {
    Rcpp::stop("Unknown TileDB filter '%s'", filter.c_str());
  }
}

const char* _tiledb_filter_to_string(tiledb_filter_type_t filter) {
  switch(filter) {
    case TILEDB_FILTER_NONE:
     return "NONE";
    case TILEDB_FILTER_GZIP:
      return "GZIP";
    case TILEDB_FILTER_ZSTD:
      return "ZSTD";
    case TILEDB_FILTER_LZ4:
      return "LZ4";
    case TILEDB_FILTER_RLE:
      return "RLE";
    case TILEDB_FILTER_BZIP2:
      return "BZIP2";
    case TILEDB_FILTER_DOUBLE_DELTA:
      return "DOUBLE_DELTA";
    case TILEDB_FILTER_BIT_WIDTH_REDUCTION:
      return "BIT_WIDTH_REDUCTION";
    case TILEDB_FILTER_BITSHUFFLE:
      return "BITSHUFFLE";
    case TILEDB_FILTER_BYTESHUFFLE:
      return "BYTESHUFFLE";
    case TILEDB_FILTER_POSITIVE_DELTA:
      return "POSITIVE_DELTA";
#if TILEDB_VERSION >= TileDB_Version(1,7,6)
    case TILEDB_FILTER_CHECKSUM_MD5:
      return "CHECKSUM_MD5";
    case TILEDB_FILTER_CHECKSUM_SHA256:
      return "CHECKSUM_SHA256";
#endif
    default: {
      Rcpp::stop("unknown tiledb_filter_t (%d)", filter);
    }
  }
}

tiledb_filter_option_t _string_to_tiledb_filter_option(std::string filter_option) {
  if (filter_option == "COMPRESSION_LEVEL") {
    return TILEDB_COMPRESSION_LEVEL;
  } else if (filter_option == "BIT_WIDTH_MAX_WINDOW") {
    return TILEDB_BIT_WIDTH_MAX_WINDOW;
  } else if (filter_option == "POSITIVE_DELTA_MAX_WINDOW") {
    return TILEDB_POSITIVE_DELTA_MAX_WINDOW;
  } else {
    Rcpp::stop("Unknown TileDB filter option '%s'", filter_option.c_str());
  }
}

const char* _tiledb_filter_option_to_string(tiledb_filter_option_t filter_option) {
  switch(filter_option) {
  case TILEDB_COMPRESSION_LEVEL:
    return "COMPRESSION_LEVEL";
  case TILEDB_BIT_WIDTH_MAX_WINDOW:
    return "BIT_WIDTH_MAX_WINDOW";
  case TILEDB_POSITIVE_DELTA_MAX_WINDOW:
    return "POSITIVE_DELTA_MAX_WINDOW";
  default:
    Rcpp::stop("unknown tiledb_filter_option_t (%d)", filter_option);
  }
}

tiledb_query_type_t _string_to_tiledb_query_type(std::string qtstr) {
  if (qtstr == "READ") {
    return TILEDB_READ;
  } else if (qtstr == "WRITE") {
    return TILEDB_WRITE;
  } else {
    Rcpp::stop("Unknown TileDB query type '%s'", qtstr.c_str());
  }
}

std::string _tiledb_query_type_to_string(tiledb_query_type_t qtype) {
  switch (qtype) {
    case TILEDB_READ:
      return "READ";
    case TILEDB_WRITE:
      return "WRITE";
    default:
      Rcpp::stop("unknown tiledb_query_type_t (%d)", qtype);
  }
}

const char* _tiledb_array_type_to_string(tiledb_array_type_t atype) {
  switch (atype) {
    case TILEDB_DENSE:
      return "dense";
    case TILEDB_SPARSE:
      return "sparse";
    default:
      Rcpp::stop("Unknown tiledb_array_type_t");
  }
}

tiledb_array_type_t _string_to_tiledb_array_type(std::string tpstr) {
  if (tpstr == "DENSE") {
    return TILEDB_DENSE;
  } else if (tpstr == "SPARSE") {
    return TILEDB_SPARSE;
  } else {
    Rcpp::stop("Unknown tiledb_array_type_t");
  }
}

tiledb_vfs_mode_t _string_to_tiledb_vfs_mode_t(std::string modestr) {
  if (modestr == "READ") {
    return TILEDB_VFS_READ;
  } else if (modestr == "WRITE") {
    return TILEDB_VFS_WRITE;
  } else if (modestr == "APPEND") {
    return TILEDB_VFS_APPEND;
  } else {
    Rcpp::stop("Unknown TileDB VFS mode type '%s'", modestr.c_str());
  }
}

// NB Limited type coverage here as aimed to sizing R allocations of either int, double or char
// Also note that there is 'inline size_t type_size(tiledb_datatype_t type)' in core_interface.h
const size_t _tiledb_datatype_sizeof(const tiledb_datatype_t dtype) {
  switch(dtype) {
    case TILEDB_FLOAT64:
      return sizeof(double);
    case TILEDB_INT32:
      return sizeof(int32_t);
    case TILEDB_CHAR:
      return sizeof(char);
    default:
      Rcpp::stop("Unsupported tiledb_datatype_t '%s'", _tiledb_datatype_to_string(dtype));
  }
}


// [[Rcpp::export]]
NumericVector libtiledb_version() {
  auto ver = tiledb::version();
  return NumericVector::create(Named("major") = std::get<0>(ver),
                               Named("minor") = std::get<1>(ver),
                               Named("patch") = std::get<2>(ver));
}


/**
 * TileDB Context
 */

// [[Rcpp::export]]
XPtr<tiledb::Context> libtiledb_ctx(Nullable<XPtr<tiledb::Config>> config=R_NilValue) {
  if (config.isNull()) {
    auto ptr = XPtr<tiledb::Context>(new tiledb::Context(), false);
    registerXptrFinalizer(ptr, libtiledb_ctx_delete);
    return ptr;
  } else {
    XPtr<tiledb::Config> config_xptr(config);
    auto ptr = XPtr<tiledb::Context>(new tiledb::Context(*config_xptr.get()), false);
    registerXptrFinalizer(ptr, libtiledb_ctx_delete);
    return ptr;
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_ctx_config(XPtr<tiledb::Context> ctx) {
  auto ptr = XPtr<tiledb::Config>(new tiledb::Config(ctx.get()->config()), false);
  registerXptrFinalizer(ptr, libtiledb_config_delete);
  return ptr;
}

// [[Rcpp::export]]
bool libtiledb_ctx_is_supported_fs(XPtr<tiledb::Context> ctx, std::string scheme) {
  if (scheme == "file") {
    return true;
  } else if  (scheme == "s3") {
    return ctx->is_supported_fs(TILEDB_S3);
  } else if (scheme == "hdfs") {
    return ctx->is_supported_fs(TILEDB_HDFS);
  } else {
    Rcpp::stop("Unknown TileDB fs scheme: '%s'", scheme.c_str());
  }
}

// [[Rcpp::export]]
void libtiledb_ctx_set_tag(XPtr<tiledb::Context> ctx, std::string key, std::string value) {
  ctx->set_tag(key, value);
}

/**
 * TileDB Config
 */

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_config(Nullable<CharacterVector> config = R_NilValue) {
  XPtr<tiledb::Config> ptr(new tiledb::Config(), false);
  registerXptrFinalizer(ptr, libtiledb_config_delete);
  if (config.isNotNull()) {
    auto config_vec = config.as();
    auto config_names = as<CharacterVector>(config_vec.names());
    for (auto &name : config_names) {
      auto param = as<std::string>(name);
      auto value = as<std::string>(config_vec[param]);
      ptr->set(param, value);
    }
  }
  return ptr;
}

// [[Rcpp::export]]
std::string libtiledb_config_save_to_file(XPtr<tiledb::Config> config, std::string filename) {
  config->save_to_file(filename);
  return filename;
}

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_config_load_from_file(std::string filename) {
  XPtr<tiledb::Config> ptr(new tiledb::Config(filename), false);
  registerXptrFinalizer(ptr, libtiledb_config_delete);
  return ptr;
}

// [[Rcpp::export]]
CharacterVector libtiledb_config_vector(XPtr<tiledb::Config> config) {
  CharacterVector config_vec;
  for (auto& p : *config) {
    config_vec[p.first]  = p.second;
  }
  return config_vec;
}

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_config_set(XPtr<tiledb::Config> config,
                                          std::string param,
                                          std::string value) {
  (*config)[param] = value;
  return config;
}

// [[Rcpp::export]]
CharacterVector libtiledb_config_get(XPtr<tiledb::Config> config,
                                     CharacterVector params) {
  CharacterVector result;
  for (auto const& p : params) {
    auto param = as<std::string>(p);
    try {
      result.push_back(config->get(param), param);
    } catch (std::exception& err) {
      result.push_back(NA_STRING, as<std::string>(NA_STRING));
    }
  }
  return result;
}

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_config_unset(XPtr<tiledb::Config> config,
                                            std::string param) {
  config->unset(param);
  return config;
}

// [[Rcpp::export]]
void libtiledb_config_dump(XPtr<tiledb::Config> config) {
  Rcout << "Config settings:\n";
  for (auto& p : *config) {
    Rcout << "\"" << p.first << "\" : \"" << p.second << "\"\n";
  }
}


/**
 * TileDB Dimension
 */
// [[Rcpp::export]]
XPtr<tiledb::Dimension> libtiledb_dim(XPtr<tiledb::Context> ctx,
                                      std::string name,
                                      std::string type,
                                      SEXP domain,
                                      SEXP tile_extent) {
  // check that the dimension type is supported
  const tiledb_datatype_t dtype = _string_to_tiledb_datatype(type);
  if (dtype != TILEDB_INT8 &&
      dtype != TILEDB_INT16 &&
      dtype != TILEDB_INT32 &&
      dtype != TILEDB_INT64 &&
      dtype != TILEDB_UINT8 &&
      dtype != TILEDB_UINT16 &&
      dtype != TILEDB_UINT32 &&
      dtype != TILEDB_UINT64 &&
      dtype != TILEDB_FLOAT32 &&
      dtype != TILEDB_FLOAT64 &&
      dtype != TILEDB_DATETIME_YEAR &&
      dtype != TILEDB_DATETIME_MONTH &&
      dtype != TILEDB_DATETIME_WEEK &&
      dtype != TILEDB_DATETIME_DAY &&
      dtype != TILEDB_DATETIME_HR &&
      dtype != TILEDB_DATETIME_MIN &&
      dtype != TILEDB_DATETIME_SEC &&
      dtype != TILEDB_DATETIME_MS &&
      dtype != TILEDB_DATETIME_US &&
      dtype != TILEDB_DATETIME_NS &&
      dtype != TILEDB_DATETIME_PS &&
      dtype != TILEDB_DATETIME_FS &&
      dtype != TILEDB_DATETIME_AS &&
      dtype != TILEDB_STRING_ASCII) {
    Rcpp::stop("only integer ((U)INT{8,16,32,64}), real (FLOAT{32,64}), DATETIME_{YEAR,MONTH,WEEK,DAY,HR,MIN,SEC,MS,US,NS,PS,FS,AS}, STRING_ACII domains supported");
  }
  // check that the dimension type aligns with the domain and tiledb_extent type
  if (dtype == TILEDB_INT32 && (TYPEOF(domain) != INTSXP || TYPEOF(tile_extent) != INTSXP)) {
    Rcpp::stop("domain or tile_extent does not match dimension type");
  } else if (dtype == TILEDB_FLOAT64 && (TYPEOF(domain) != REALSXP || TYPEOF(tile_extent) != REALSXP)) {
    Rcpp::stop("domain or tile_extent does not match dimension type");
  }
  if (dtype == TILEDB_INT32) {
    using Dtype = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
    auto domain_vec = as<IntegerVector>(domain);
    if (domain_vec.length() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    auto tile_extent_vec = as<IntegerVector>(tile_extent);
    if (tile_extent_vec.length() != 1) {
      Rcpp::stop("tile_extent must be a scalar");
    }
    std::array<Dtype, 2> _domain = {domain_vec[0], domain_vec[1]};
    std::array<Dtype, 1> _tile_extent = {tile_extent_vec[0]};
    auto ptr = XPtr<tiledb::Dimension>(new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0])),false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_UINT32) {
    using Dtype = tiledb::impl::tiledb_to_type<TILEDB_UINT32>::type;
    Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
    if (domain_vec.length() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    Rcpp::IntegerVector extent_vec = Rcpp::IntegerVector(tile_extent);
    std::array<Dtype, 2> _domain = {static_cast<uint32_t>(domain_vec[0]), static_cast<uint32_t>(domain_vec[1])};
    std::array<Dtype, 1> _tile_extent = {static_cast<uint32_t>(extent_vec[0])};
    auto dim = new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0]));
    auto ptr = XPtr<tiledb::Dimension>(dim, false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_INT16) {
    using Dtype = tiledb::impl::tiledb_to_type<TILEDB_INT16>::type;
    Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
    if (domain_vec.length() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    Rcpp::IntegerVector extent_vec = Rcpp::IntegerVector(tile_extent);
    std::array<Dtype, 2> _domain = {static_cast<int16_t>(domain_vec[0]), static_cast<int16_t>(domain_vec[1])};
    std::array<Dtype, 1> _tile_extent = {static_cast<int16_t>(extent_vec[0])};
    auto dim = new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0]));
    auto ptr = XPtr<tiledb::Dimension>(dim, false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_UINT16) {
    using Dtype = tiledb::impl::tiledb_to_type<TILEDB_UINT16>::type;
    Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
    if (domain_vec.length() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    Rcpp::IntegerVector extent_vec = Rcpp::IntegerVector(tile_extent);
    std::array<Dtype, 2> _domain = {static_cast<uint16_t>(domain_vec[0]), static_cast<uint16_t>(domain_vec[1])};
    std::array<Dtype, 1> _tile_extent = {static_cast<uint16_t>(extent_vec[0])};
    auto dim = new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0]));
    auto ptr = XPtr<tiledb::Dimension>(dim, false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_INT8) {
    using Dtype = tiledb::impl::tiledb_to_type<TILEDB_INT8>::type;
    Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
    if (domain_vec.length() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    Rcpp::IntegerVector extent_vec = Rcpp::IntegerVector(tile_extent);
    std::array<Dtype, 2> _domain = {static_cast<int8_t>(domain_vec[0]), static_cast<int8_t>(domain_vec[1])};
    std::array<Dtype, 1> _tile_extent = {static_cast<int8_t>(extent_vec[0])};
    auto dim = new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0]));
    auto ptr = XPtr<tiledb::Dimension>(dim, false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_UINT8) {
    using Dtype = tiledb::impl::tiledb_to_type<TILEDB_UINT8>::type;
    Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
    if (domain_vec.length() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    Rcpp::IntegerVector extent_vec = Rcpp::IntegerVector(tile_extent);
    std::array<Dtype, 2> _domain = {static_cast<uint8_t>(domain_vec[0]), static_cast<uint8_t>(domain_vec[1])};
    std::array<Dtype, 1> _tile_extent = {static_cast<uint8_t>(extent_vec[0])};
    auto dim = new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0]));
    auto ptr = XPtr<tiledb::Dimension>(dim, false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_INT64) {
    // for int64 domains and extents we require integer64 types
    Rcpp::NumericVector dv(domain);
    if (!isInteger64(dv)) {
      Rcpp::stop("dimension domain for INT64 must be an integer64 type in R");
    }
    if (dv.size() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    std::vector<int64_t> domain_vec = getInt64Vector(Rcpp::NumericVector(dv));
    int64_t domain[] = {domain_vec[0], domain_vec[1]};
    Rcpp::NumericVector ext(tile_extent);
    if (!isInteger64(ext)) {
      Rcpp::stop("tile exent for INT64 domain must be an integer64 type in R");
    }
    int64_t extent = makeScalarInteger64(ext[0]);
    auto dim = new tiledb::Dimension(tiledb::Dimension::create(*ctx.get(), name, dtype, domain, &extent));
    auto ptr = XPtr<tiledb::Dimension>(dim, false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_UINT64) {
    // for uint64 domains and extents we require integer64 types
    Rcpp::NumericVector dv(domain);
    if (!isInteger64(dv)) {
      Rcpp::stop("dimension domain for UINT64 must be an integer64 type in R");
    }
    if (dv.size() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    std::vector<int64_t> domain_vec = getInt64Vector(Rcpp::NumericVector(dv));
    uint64_t domain[] = { static_cast<uint64_t>(domain_vec[0]), static_cast<uint64_t>(domain_vec[1])};
    Rcpp::NumericVector ext(tile_extent);
    if (!isInteger64(ext)) {
      Rcpp::stop("tile exent for UINT64 domain must be an integer64 type in R");
    }
    uint64_t extent = static_cast<uint64_t>(makeScalarInteger64(ext[0]));
    auto dim = new tiledb::Dimension(tiledb::Dimension::create(*ctx.get(), name, dtype, domain, &extent));
    auto ptr = XPtr<tiledb::Dimension>(dim, false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_FLOAT32) {
    using Dtype = tiledb::impl::tiledb_to_type<TILEDB_FLOAT32>::type;
    auto domain_vec = as<NumericVector>(domain);
    if (domain_vec.length() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    auto tile_extent_vec = as<NumericVector>(tile_extent);
    if (tile_extent_vec.length() != 1) {
      Rcpp::stop("tile_extent must be a scalar");
    }
    std::array<Dtype, 2> _domain = {static_cast<float>(domain_vec[0]), static_cast<float>(domain_vec[1])};
    std::array<Dtype, 1> _tile_extent = {static_cast<float>(tile_extent_vec[0])};
    auto d = new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0]));
    auto ptr = XPtr<tiledb::Dimension>(d, false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_FLOAT64) {
    using Dtype = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
    auto domain_vec = as<NumericVector>(domain);
    if (domain_vec.length() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    auto tile_extent_vec = as<NumericVector>(tile_extent);
    if (tile_extent_vec.length() != 1) {
      Rcpp::stop("tile_extent must be a scalar");
    }
    std::array<Dtype, 2> _domain = {domain_vec[0], domain_vec[1]};
    std::array<Dtype, 1> _tile_extent = {tile_extent_vec[0]};
    auto d = new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0]));
    auto ptr = XPtr<tiledb::Dimension>(d, false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_DATETIME_YEAR ||
             dtype == TILEDB_DATETIME_MONTH ||
             dtype == TILEDB_DATETIME_WEEK ||
             dtype == TILEDB_DATETIME_DAY ||
             dtype == TILEDB_DATETIME_HR  ||
             dtype == TILEDB_DATETIME_MIN ||
             dtype == TILEDB_DATETIME_SEC ||
             dtype == TILEDB_DATETIME_MS  ||
             dtype == TILEDB_DATETIME_US  ||
             dtype == TILEDB_DATETIME_NS  ||
             dtype == TILEDB_DATETIME_PS  ||
             dtype == TILEDB_DATETIME_FS  ||
             dtype == TILEDB_DATETIME_AS    ) {
    auto domain_vec = as<std::vector<int64_t>>(domain);
    if (domain_vec.size() != 2) {
      Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
    }
    int64_t domain[] = {domain_vec[0], domain_vec[1]};
    int64_t extent = Rcpp::as<int64_t>(tile_extent);
    auto dim = new tiledb::Dimension(tiledb::Dimension::create(*ctx.get(), name, dtype, domain, &extent));
    auto ptr = XPtr<tiledb::Dimension>(dim, false);
    registerXptrFinalizer(ptr, libtiledb_dimension_delete);
    return ptr;

  } else if (dtype == TILEDB_STRING_ASCII) {
    if (Rf_isNull(domain) && Rf_isNull(tile_extent)) {
      auto d = tiledb::Dimension::create(*ctx.get(), name, TILEDB_STRING_ASCII, nullptr, nullptr);
      auto dim = new tiledb::Dimension(d);
      auto ptr = XPtr<tiledb::Dimension>(dim, false);
      registerXptrFinalizer(ptr, libtiledb_dimension_delete);
      return ptr;

    } else {
      Rcpp::stop("Non-null domain or extend to be added.");
    }
  } else {
    Rcpp::stop("Unsupported tiledb type (%d) this should not happen!", dtype);
  }
}

// [[Rcpp::export]]
std::string libtiledb_dim_get_name(XPtr<tiledb::Dimension> dim) {
  return dim->name();
}

// [[Rcpp::export]]
SEXP libtiledb_dim_get_domain(XPtr<tiledb::Dimension> dim) {
  auto dim_type = dim->type();
  switch (dim_type) {
    case TILEDB_FLOAT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT32>::type;
      return NumericVector({dim->domain<DataType>().first,
                            dim->domain<DataType>().second});
    }
    case TILEDB_FLOAT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
      auto d1 = dim->domain<DataType>().first;
      auto d2 = dim->domain<DataType>().second;
      if (d1 == R_NaReal || d2 == R_NaReal) {
        Rcpp::stop("tiledb_dim domain FLOAT64 value not representable as an R double");
      }
      return NumericVector({d1, d2});
    }
    case TILEDB_INT8: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT8>::type;
      return IntegerVector({dim->domain<DataType>().first,
                            dim->domain<DataType>().second});
    }
    case TILEDB_UINT8: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT8>::type;
      return IntegerVector({dim->domain<DataType>().first,
                            dim->domain<DataType>().second});
    }
    case TILEDB_INT16: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT16>::type;
      return IntegerVector({dim->domain<DataType>().first,
                            dim->domain<DataType>().second});
    }
    case TILEDB_UINT16: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT16>::type;
      return IntegerVector({dim->domain<DataType>().first,
                            dim->domain<DataType>().second});
    }
    case TILEDB_INT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
      auto d1 = dim->domain<DataType>().first;
      auto d2 = dim->domain<DataType>().second;
      if (d1 == R_NaInt || d2 == R_NaInt) {
        Rcpp::stop("tiledb_dim domain INT32 value not representable as an R integer");
      }
      return IntegerVector({d1, d2});
    }
    case TILEDB_UINT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT32>::type;
      auto d1 = dim->domain<DataType>().first;
      auto d2 = dim->domain<DataType>().second;
      if (d1 > std::numeric_limits<int32_t>::max() ||
          d2 > std::numeric_limits<int32_t>::max()) {
        Rcpp::stop("tiledb_dim domain UINT32 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(d1),
                            static_cast<int32_t>(d2)});
    }
    case TILEDB_INT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT64>::type;
      auto d1 = dim->domain<DataType>().first;
      auto d2 = dim->domain<DataType>().second;
      if (d1 <= R_NaInt || d1 > std::numeric_limits<int32_t>::max() ||
          d2 <= R_NaInt || d2 > std::numeric_limits<int32_t>::max()) {
        Rcpp::stop("tiledb_dim domain INT64 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(d1),
                            static_cast<int32_t>(d2)});
    }
    case TILEDB_UINT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT64>::type;
      auto d1 = dim->domain<DataType>().first;
      auto d2 = dim->domain<DataType>().second;
      if (d1 > std::numeric_limits<int32_t>::max() ||
          d2 > std::numeric_limits<int32_t>::max()) {
        Rcpp::stop("tiledb_dim domain UINT64 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(d1),
                            static_cast<int32_t>(d2)});
    }
    case TILEDB_DATETIME_YEAR:
    case TILEDB_DATETIME_MONTH:
    case TILEDB_DATETIME_WEEK:
    case TILEDB_DATETIME_DAY:
    case TILEDB_DATETIME_HR:
    case TILEDB_DATETIME_MIN:
    case TILEDB_DATETIME_SEC:
    case TILEDB_DATETIME_MS:
    case TILEDB_DATETIME_US:
    case TILEDB_DATETIME_NS:
    case TILEDB_DATETIME_PS:
    case TILEDB_DATETIME_FS:
    case TILEDB_DATETIME_AS: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT64>::type;
      auto d1 = dim->domain<DataType>().first;
      auto d2 = dim->domain<DataType>().second;
      if (d1 <= R_NaInt || d1 > std::numeric_limits<int32_t>::max() ||
          d2 <= R_NaInt || d2 > std::numeric_limits<int32_t>::max()) {
        Rcpp::stop("tiledb_dim domain INT64 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(d1),
                            static_cast<int32_t>(d2)});
    }
    default:
      Rcpp::stop("invalid tiledb_dim domain type (%d)", dim_type);
  }
}

// [[Rcpp::export]]
SEXP libtiledb_dim_get_tile_extent(XPtr<tiledb::Dimension> dim) {
  auto dim_type = dim->type();
  switch (dim_type) {
    case TILEDB_FLOAT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT32>::type;
      return NumericVector({dim->tile_extent<DataType>(),});
    }
    case TILEDB_FLOAT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
      auto t = dim->tile_extent<DataType>();
      if (t == R_NaReal) {
        Rcpp::stop("tiledb_dim tile FLOAT64 value not representable as an R double");
      }
      return NumericVector({t});
    }
    case TILEDB_INT8: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT8>::type;
      return IntegerVector({dim->tile_extent<DataType>()});
    }
    case TILEDB_UINT8: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT8>::type;
      return IntegerVector({dim->tile_extent<DataType>()});
    }
    case TILEDB_INT16: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT16>::type;
      return IntegerVector({dim->tile_extent<DataType>()});
    }
    case TILEDB_UINT16: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT16>::type;
      return IntegerVector({dim->tile_extent<DataType>()});
    }
    case TILEDB_INT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
      auto t = dim->tile_extent<DataType>();
      if (t == R_NaInt) {
        Rcpp::stop("tiledb_dim tile INT32 value not representable as an R integer");
      }
      return IntegerVector({t,});
    }
    case TILEDB_UINT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT32>::type;
      auto t = dim->tile_extent<DataType>();
      if (t > std::numeric_limits<int32_t>::max()) {
        Rcpp::stop("tiledb_dim tile UINT32 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(t),});
    }
    case TILEDB_INT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT64>::type;
      auto t = dim->tile_extent<DataType>();
      if (t <= R_NaInt || t > std::numeric_limits<int32_t>::max()) {
        Rcpp::stop("tiledb_dim tile INT64 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(t),});
    }
    case TILEDB_UINT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT64>::type;
      auto t = dim->tile_extent<DataType>();
      if (t > std::numeric_limits<int32_t>::max()) {
        Rcpp::stop("tiledb_dim tile UINT64 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(t),});
    }
    default:
      Rcpp::stop("invalid tiledb_dim domain type");
  }
}

// [[Rcpp::export]]
std::string libtiledb_dim_get_datatype(XPtr<tiledb::Dimension> dim) {
  return _tiledb_datatype_to_string(dim->type());
}

// Computes the TileDB subarray for a given dimension domain
// [[Rcpp::export]]
NumericVector dim_domain_subarray(NumericVector domain, NumericVector subscript) {
  if (domain.length() != 2) {
    Rcpp::stop("invalid tiledb_dim domain");
  }
  double domain_lb = domain[0];
  double domain_ub = domain[1];
  auto sub0 = subscript[0];
  if (sub0 == R_NaReal) {
    Rcpp::stop("NA subscript not supported");
  }
  if (sub0 < domain_lb || sub0 > domain_ub) {
    Rcpp::stop("subscript out of domain bounds");
  }
  if (subscript.length() == 1) {
    return NumericVector({sub0, sub0});
  }
  // allocate
  auto sub = std::vector<double>();
  sub.push_back(sub0);
  R_xlen_t subscript_length = subscript.length();
  for (R_xlen_t i = 1; i < subscript_length; i++) {
    auto low = subscript[i - 1];
    auto high = subscript[i];
    if (high == R_NaReal) {
      Rcpp::stop("NA subscripting not supported");
    }
    if (high < domain_lb || high > domain_ub) {
      Rcpp::stop("subscript out of domain bounds: (at index: [%d] %f < %f",
                 i, high, domain_lb);
    }
    double diff = high - low;
    if (diff > 1.0 || diff < 1.0) {
      // end one subarray range
      sub.push_back(low);
      // begin another subarray range
      sub.push_back(high);
    }
  }
  // end final subarray range
  double end = subscript[subscript_length - 1];
  sub.push_back(end);
  return wrap(sub);
}

// [[Rcpp::export]]
int libtiledb_dim_get_cell_val_num(XPtr<tiledb::Dimension> dim) {
#if TILEDB_VERSION >= TileDB_Version(2,0,0)
  unsigned int ncells = dim->cell_val_num();
  if (ncells == TILEDB_VAR_NUM) {
    return R_NaInt;          // set to R's NA for integer
  } else if (ncells > std::numeric_limits<int32_t>::max()) {
    Rcpp::stop("tiledb_attr ncells value not representable as an R integer");
  }
  return static_cast<int32_t>(ncells);
#else
  return R_NaInt;
#endif
}

// [[Rcpp::export]]
XPtr<tiledb::FilterList> libtiledb_dimension_get_filter_list(XPtr<tiledb::Dimension> dim) {
  return XPtr<tiledb::FilterList>(new tiledb::FilterList(dim->filter_list()));
}

// [[Rcpp::export]]
XPtr<tiledb::Dimension> libtiledb_dimension_set_filter_list(XPtr<tiledb::Dimension> dim,
                                                            XPtr<tiledb::FilterList> filter_list) {
  dim->set_filter_list(*filter_list);
  return dim;
}


/**
 * TileDB Domain
 */
// [[Rcpp::export]]
XPtr<tiledb::Domain> libtiledb_domain(XPtr<tiledb::Context> ctx, List dims) {
  R_xlen_t ndims = dims.length();
  if (ndims == 0) {
    Rcpp::stop("domain must have one or more dimensions");
  }
  for (R_xlen_t i=0; i < ndims; i++) {
    SEXP d = dims[i];
    if (TYPEOF(d) != EXTPTRSXP) {
      Rcpp::stop("Invalid tiledb_dim object at index %d (type %s)", i, Rcpp::type2name(d));
    }
  }
  XPtr<tiledb::Domain> domain(new tiledb::Domain(*ctx.get()), false);
  for (auto& val : dims) {
    // TODO: we can't do much type checking for the cast here until we wrap EXTPTRSXP in S4 classes
    auto dim = as<XPtr<tiledb::Dimension>>(val);
    domain->add_dimension(*dim.get());
  }
  registerXptrFinalizer(domain, libtiledb_domain_delete);
  return domain;
}

// [[Rcpp::export]]
std::string libtiledb_domain_get_type(XPtr<tiledb::Domain> domain) {
  auto dtype = domain->type();
  return _tiledb_datatype_to_string(dtype);
}

// [[Rcpp::export]]
int libtiledb_domain_get_ndim(XPtr<tiledb::Domain> domain) {
  uint32_t rank = domain->ndim();
  if (rank > std::numeric_limits<int32_t>::max()) {
    Rcpp::stop("tiledb::Domain rank is not representable by an R integer");
  }
  return static_cast<int32_t>(rank);
}

// [[Rcpp::export]]
XPtr<tiledb::Dimension> libtiledb_domain_get_dimension_from_index(XPtr<tiledb::Domain> dom, int idx) {
  auto dim = dom->dimension(idx);
  auto ptr = XPtr<tiledb::Dimension>(new tiledb::Dimension(dim), false);
  registerXptrFinalizer(ptr, libtiledb_dimension_delete);
  return ptr;
}

// [[Rcpp::export]]
XPtr<tiledb::Dimension> libtiledb_domain_get_dimension_from_name(XPtr<tiledb::Domain> dom,
                                                                 std::string name) {
  auto dim = dom->dimension(name.c_str());
  auto ptr = XPtr<tiledb::Dimension>(new tiledb::Dimension(dim), false);
  registerXptrFinalizer(ptr, libtiledb_dimension_delete);
  return ptr;
}

// [[Rcpp::export]]
List libtiledb_domain_get_dimensions(XPtr<tiledb::Domain> domain) {
  List dimensions;
  for (auto& dim : domain->dimensions()) {
    dimensions.push_back(XPtr<tiledb::Dimension>(new tiledb::Dimension(dim)));
  }
  return dimensions;
}

// [[Rcpp::export]]
bool libtiledb_domain_has_dimension(XPtr<tiledb::Domain> domain, std::string name) {
  return domain->has_dimension(name.c_str());
}

// [[Rcpp::export]]
void libtiledb_domain_dump(XPtr<tiledb::Domain> domain) {
  domain->dump();
}

double _domain_datatype_time_scale_factor(tiledb_datatype_t dtype) {
  //Rcpp::Rcout << "In _domain_datatype_time_scale_factor " << dtype << std::endl;
  switch (dtype) {
  case TILEDB_INT8:
  case TILEDB_UINT8:
  case TILEDB_INT16:
  case TILEDB_UINT16:
  case TILEDB_INT32:
  case TILEDB_UINT32:
  case TILEDB_INT64:
  case TILEDB_UINT64:
  case TILEDB_FLOAT32:
  case TILEDB_FLOAT64:
  case TILEDB_CHAR:
  case TILEDB_STRING_ASCII:
  case TILEDB_STRING_UTF8:
  case TILEDB_STRING_UTF16:
  case TILEDB_STRING_UTF32:
  case TILEDB_STRING_UCS2:
  case TILEDB_STRING_UCS4:
  case TILEDB_ANY:
    return 1.0;               // fallback, could also error here
  case TILEDB_DATETIME_YEAR:
  case TILEDB_DATETIME_MONTH:
  case TILEDB_DATETIME_WEEK:
    return 1.0;                 // also fallback
  case TILEDB_DATETIME_DAY:
    return 24 * 60 * 60 * 1e9;
  case TILEDB_DATETIME_HR:
    return 60 * 60 * 1e9;
  case TILEDB_DATETIME_MIN:
    return 60 * 1e9;
  case TILEDB_DATETIME_SEC:
    return 1e9;
  case TILEDB_DATETIME_MS:
    return 1e6;
  case TILEDB_DATETIME_US:
    return 1e3;
  case TILEDB_DATETIME_NS:
    return 1;
  case TILEDB_DATETIME_PS:
    return 1e-3;
  case TILEDB_DATETIME_FS:
    return 1e-6;
  case TILEDB_DATETIME_AS:
    return 1e-9;
  default:
    Rcpp::stop("Unsupport datatype (%d)", dtype);
  }
  return R_NaReal; // not reached
}


/**
 * TileDB Filter
 */
//[[Rcpp::export]]
XPtr<tiledb::Filter> libtiledb_filter(XPtr<tiledb::Context> ctx, std::string filter) {
  tiledb_filter_type_t fltr = _string_to_tiledb_filter(filter);
  return XPtr<tiledb::Filter>(new tiledb::Filter(*ctx.get(), fltr));
}

//[[Rcpp::export]]
std::string libtiledb_filter_get_type(XPtr<tiledb::Filter> filter) {
  return _tiledb_filter_to_string(filter->filter_type());
}

//[[Rcpp::export]]
R_xlen_t libtiledb_filter_get_option(XPtr<tiledb::Filter> filter, std::string filter_option_str) {
  tiledb_filter_option_t filter_option = _string_to_tiledb_filter_option(filter_option_str);
  if (filter_option == TILEDB_BIT_WIDTH_MAX_WINDOW || filter_option == TILEDB_POSITIVE_DELTA_MAX_WINDOW) {
    uint32_t value;
    filter->get_option(filter_option, &value);
    return static_cast<R_xlen_t>(value);
  }
  int value;
  filter->get_option(filter_option, &value);
  return static_cast<R_xlen_t>(value);
}

//[[Rcpp::export]]
void libtiledb_filter_set_option(XPtr<tiledb::Filter> filter, std::string filter_option_str, int value) {
  tiledb_filter_option_t filter_option = _string_to_tiledb_filter_option(filter_option_str);
  filter->set_option(filter_option, &value);
  return;
}


/**
 * TileDB Filter List
 */
//[[Rcpp::export]]
XPtr<tiledb::FilterList> libtiledb_filter_list(XPtr<tiledb::Context> ctx, List filters) {
  XPtr<tiledb::FilterList> filter_list(new tiledb::FilterList(*ctx.get()));
  // check that external pointers are supported
  R_xlen_t nfilters = filters.length();
  if (nfilters > 0) {
    for (R_xlen_t i=0; i < nfilters; i++)  {
      SEXP filter = filters[i];
      if (TYPEOF(filter) != EXTPTRSXP) {
        Rcpp::stop("Invalid filter object at index %d (type %s)", i, Rcpp::type2name(filter));
      }
    }
    for (SEXP f : filters) {
      auto filter = as<XPtr<tiledb::Filter>>(f);
      filter_list->add_filter(*filter.get());
    }
  }
  return filter_list;
}

//[[Rcpp::export]]
void libtiledb_filter_list_set_max_chunk_size(XPtr<tiledb::FilterList> filterList, uint32_t max_chunk_sie) {
  filterList->set_max_chunk_size(max_chunk_sie);
}

//[[Rcpp::export]]
int libtiledb_filter_list_get_max_chunk_size(XPtr<tiledb::FilterList> filterList) {
  return filterList->max_chunk_size();
}

//[[Rcpp::export]]
int libtiledb_filter_list_get_nfilters(XPtr<tiledb::FilterList> filterList) {
  return filterList->nfilters();
}

//[[Rcpp::export]]
XPtr<tiledb::Filter> libtiledb_filter_list_get_filter_from_index(XPtr<tiledb::FilterList> filterList, uint32_t filter_index) {
  return XPtr<tiledb::Filter>(new tiledb::Filter(filterList->filter(filter_index)));
}



/**
 * TileDB Attribute
 */
//[[Rcpp::export]]
XPtr<tiledb::Attribute> libtiledb_attribute(XPtr<tiledb::Context> ctx,
                                            std::string name,
                                            std::string type,
                                            XPtr<tiledb::FilterList> filter_list,
                                            int ncells,
                                            bool nullable) {
    tiledb_datatype_t attr_dtype = _string_to_tiledb_datatype(type);
    if (ncells < 1 && ncells != R_NaInt) {
        Rcpp::stop("ncells must be >= 1 (or NA for variable cells)");
    }

    // placeholder, overwritten in all branches below
    XPtr<tiledb::Attribute> attr = XPtr<tiledb::Attribute>(static_cast<tiledb::Attribute*>(nullptr));

    if (attr_dtype == TILEDB_INT32) {
        using DType = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
        attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)), false);
    } else if (attr_dtype == TILEDB_FLOAT64) {
        using DType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
        attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)), false);
    } else if (attr_dtype == TILEDB_FLOAT32) {
        using DType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT32>::type;
        attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)), false);
    } else if (attr_dtype == TILEDB_CHAR || attr_dtype == TILEDB_STRING_ASCII) {
        using DType = tiledb::impl::tiledb_to_type<TILEDB_CHAR>::type;
        attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)), false);
        uint64_t num = static_cast<uint64_t>(ncells);
        if (ncells == R_NaInt) {
            num = TILEDB_VAR_NUM;           // R's NA is different from TileDB's NA
        }
        attr->set_cell_val_num(num);
    } else if (attr_dtype == TILEDB_DATETIME_YEAR ||
               attr_dtype == TILEDB_DATETIME_MONTH ||
               attr_dtype == TILEDB_DATETIME_WEEK ||
               attr_dtype == TILEDB_DATETIME_DAY ||
               attr_dtype == TILEDB_DATETIME_HR ||
               attr_dtype == TILEDB_DATETIME_MIN ||
               attr_dtype == TILEDB_DATETIME_SEC ||
               attr_dtype == TILEDB_DATETIME_MS  ||
               attr_dtype == TILEDB_DATETIME_US  ||
               attr_dtype == TILEDB_DATETIME_NS  ||
               attr_dtype == TILEDB_DATETIME_PS  ||
               attr_dtype == TILEDB_DATETIME_FS  ||
               attr_dtype == TILEDB_DATETIME_AS) {
        attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(*ctx.get(), name, attr_dtype), false);
    } else if (attr_dtype == TILEDB_INT64  ||
               attr_dtype == TILEDB_UINT64 ||
               attr_dtype == TILEDB_UINT32 ||
               attr_dtype == TILEDB_INT16  ||
               attr_dtype == TILEDB_UINT16 ||
               attr_dtype == TILEDB_INT8   ||
               attr_dtype == TILEDB_UINT8    ) {
        attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(*ctx.get(), name, attr_dtype), false);
    } else {
        Rcpp::Rcout << type << std::endl;
        Rcpp::stop("Only integer ((U)INT{8,16,32,64}), logical (INT32), real (FLOAT{32,64}), "
                   "Date (DATEIME_DAY), Datetime (DATETIME_{SEC,MS,US}), "
                   "nanotime (DATETIME_NS) and character (CHAR,ASCII) attributes "
                   "are supported");
    }
    attr->set_filter_list(*filter_list);
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    attr->set_nullable(nullable);
#endif
    registerXptrFinalizer(attr, libtiledb_attribute_delete);
    return attr;
}

// [[Rcpp::export]]
std::string libtiledb_attribute_get_name(XPtr<tiledb::Attribute> attr) {
  return attr->name();
}

// [[Rcpp::export]]
std::string libtiledb_attribute_get_type(XPtr<tiledb::Attribute> attr) {
  return _tiledb_datatype_to_string(attr->type());
}

// [[Rcpp::export]]
double libtiledb_attribute_get_cell_size(XPtr<tiledb::Attribute> attr) {
  uint64_t size = attr->cell_size();
  return static_cast<double>(size);
}

// [[Rcpp::export]]
XPtr<tiledb::FilterList> libtiledb_attribute_get_filter_list(XPtr<tiledb::Attribute> attr) {
  return XPtr<tiledb::FilterList>(new tiledb::FilterList(attr->filter_list()));
}

// [[Rcpp::export]]
XPtr<tiledb::Attribute> libtiledb_attribute_set_filter_list(XPtr<tiledb::Attribute> attr, XPtr<tiledb::FilterList> filter_list) {
  attr->set_filter_list(*filter_list);
  return attr;
}

// [[Rcpp::export]]
int libtiledb_attribute_get_cell_val_num(XPtr<tiledb::Attribute> attr) {
  unsigned int ncells = attr->cell_val_num();
  if (ncells == TILEDB_VAR_NUM) {
    return R_NaInt;          // set to R's NA for integer
  } else if (ncells > std::numeric_limits<int32_t>::max()) {
    Rcpp::stop("tiledb_attr ncells value not representable as an R integer");
  }
  return static_cast<int32_t>(ncells);
}

// [[Rcpp::export]]
void libtiledb_attribute_set_cell_val_num(XPtr<tiledb::Attribute> attr, int num) {
  uint64_t ncells = static_cast<uint64_t>(num);
  if (num == R_NaInt) {
    ncells = TILEDB_VAR_NUM;             // R's NA is different from TileDB's NA
  } else if (num <= 0) {
    Rcpp::stop("Variable cell number of '%d' not sensible", num);
  }
  attr->set_cell_val_num(ncells);        // returns reference to self so nothing for us to return
}

// [[Rcpp::export]]
bool libtiledb_attribute_is_variable_sized(XPtr<tiledb::Attribute> attr) {
  return attr->variable_sized();
}

// [[Rcpp::export]]
void libtiledb_attribute_dump(XPtr<tiledb::Attribute> attr) {
  attr->dump();
}

// [[Rcpp::export]]
void libtiledb_attribute_set_fill_value(XPtr<tiledb::Attribute> attr, SEXP val) {
#if TILEDB_VERSION >= TileDB_Version(2,1,0)
  tiledb_datatype_t dtype = attr->type();
  if (dtype == TILEDB_INT32) {
    IntegerVector v(val);
    if (v.size() > 1) Rcpp::stop("Setting fill values only supports scalar values for now.");
    attr->set_fill_value((void*) &(v[0]), static_cast<uint64_t>(sizeof(int32_t)));
  } else if (dtype == TILEDB_FLOAT64) {
    NumericVector v(val);
    if (v.size() > 1) Rcpp::stop("Setting fill values only supports scalar values for now.");
    attr->set_fill_value((void*) &(v[0]), static_cast<uint64_t>(sizeof(double)));
  } else if (dtype == TILEDB_STRING_ASCII || dtype == TILEDB_CHAR) {
    CharacterVector v(val);
    if (v.size() > 1) Rcpp::stop("Setting fill values only supports scalar values for now.");
    std::string s(v[0]);
    attr->set_fill_value((void*) s.c_str(), static_cast<uint64_t>(s.size()));
  } else {
    std::string typestr = _tiledb_datatype_to_string(dtype);
    Rcpp::stop("Type '%s' is not currently supported.", typestr.c_str());
  }
#else
  Rcpp::stop("libtiledb_attribute_set_fill_value_with_type only available with TileDB 2.1.0 or later");
#endif
}

// [[Rcpp::export]]
SEXP libtiledb_attribute_get_fill_value(XPtr<tiledb::Attribute> attr) {
#if TILEDB_VERSION >= TileDB_Version(2,1,0)
  tiledb_datatype_t dtype = attr->type();
  const void* valptr;
  uint64_t size = sizeof(dtype);
  attr->get_fill_value(&valptr, &size);
  if (dtype == TILEDB_INT32) {
    int32_t v = *(const int32_t*)valptr;
    return wrap(v);
  } else if (dtype == TILEDB_FLOAT64) {
    double v = *(const double*)valptr;
    return wrap(v);
  } else if (dtype == TILEDB_STRING_ASCII || dtype == TILEDB_CHAR) {
    std::string s(static_cast<const char*>(valptr), static_cast<size_t>(size));
    return wrap(s);
  } else {
    std::string typestr = _tiledb_datatype_to_string(dtype);
    Rcpp::stop("Type '%s' is not currently supported.", typestr.c_str());
  }
#else
  Rcpp::stop("libtiledb_attribute_get_fill_value_with_type only available with TileDB 2.1.0 or later");
  return R_NilValue; // not reached
#endif
}

// [[Rcpp::export]]
void libtiledb_attribute_set_nullable(XPtr<tiledb::Attribute> attr, const bool flag) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    attr->set_nullable(flag);
#else
    Rcpp::stop("Nullable attributes are only available with TileDB 2.2.0 or later");
#endif
}

// [[Rcpp::export]]
bool libtiledb_attribute_get_nullable(XPtr<tiledb::Attribute> attr) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    return attr->nullable();
#else
    return false;
#endif
}

/**
 * TileDB Array Schema
 */
//[[Rcpp::export]]
XPtr<tiledb::ArraySchema>
libtiledb_array_schema(XPtr<tiledb::Context> ctx,
                       XPtr<tiledb::Domain> domain,
                       List attributes,
                       std::string cell_order,
                       std::string tile_order,
                       Nullable<XPtr<tiledb::FilterList>> coords_filter_list = R_NilValue,
                       Nullable<XPtr<tiledb::FilterList>> offsets_filter_list = R_NilValue,
                       bool sparse = false) {
  // check that external pointers are supported
  R_xlen_t nattr = attributes.length();
  if (nattr == 0) {
    Rcpp::stop("libtiledb_array_schema requires one or more attributes");
  }
  for (R_xlen_t i=0; i < nattr; i++)  {
    SEXP attr = attributes[i];
    if (TYPEOF(attr) != EXTPTRSXP) {
      Rcpp::stop("Invalid attribute object at index %d (type %s)", i, Rcpp::type2name(attr));
    }
  }
  auto _cell_order = _string_to_tiledb_layout(cell_order);
  auto _tile_order = _string_to_tiledb_layout(tile_order);
  auto schptr = new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(),
                                                            sparse ? TILEDB_SPARSE : TILEDB_DENSE));
  auto schema = XPtr<tiledb::ArraySchema>(schptr, false);
  registerXptrFinalizer(schema, libtiledb_arrayschema_delete);
  schema->set_domain(*domain.get());
  for (SEXP a : attributes) {
    auto attr = as<XPtr<tiledb::Attribute>>(a);
    schema->add_attribute(*attr.get());
  }
  schema->set_cell_order(_cell_order);
  schema->set_tile_order(_tile_order);
  if (coords_filter_list.isNotNull()) {
    XPtr<tiledb::FilterList> xptr_coords(coords_filter_list);
    schema->set_coords_filter_list(*xptr_coords);
  }
  if (offsets_filter_list.isNotNull()) {
    XPtr<tiledb::FilterList> xptr_offsets(offsets_filter_list);
    schema->set_offsets_filter_list(*xptr_offsets);
  }
  schema->check();
  return schema;
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_create(XPtr<tiledb::Context> ctx, std::string atstr) {
  auto at = _string_to_tiledb_array_type(atstr);
  auto p = new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(), at));
  auto ptr = XPtr<tiledb::ArraySchema>(p, false);
  registerXptrFinalizer(ptr, libtiledb_arrayschema_delete);
  return ptr;
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_load(XPtr<tiledb::Context> ctx, std::string uri) {
  auto ptr = XPtr<tiledb::ArraySchema>(new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(),
                                                                                   uri)), false);
  registerXptrFinalizer(ptr, libtiledb_arrayschema_delete);
  return ptr;
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_load_with_key(XPtr<tiledb::Context> ctx,
                                                               std::string uri,
                                                               std::string key) {
  auto p = new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(), uri, TILEDB_AES_256_GCM,
                                                       key.data(), (uint32_t) key.size()));
  auto ptr = XPtr<tiledb::ArraySchema>(p, false);
  registerXptrFinalizer(ptr, libtiledb_arrayschema_delete);
  return ptr;
}

// [[Rcpp::export]]
void libtiledb_array_schema_set_domain(XPtr<tiledb::ArraySchema> schema,
                                       XPtr<tiledb::Domain> dom) {
  schema->set_domain(*dom);
}

// [[Rcpp::export]]
XPtr<tiledb::Domain> libtiledb_array_schema_get_domain(XPtr<tiledb::ArraySchema> schema) {
  auto ptr = XPtr<tiledb::Domain>(new tiledb::Domain(schema->domain()), false);
  registerXptrFinalizer(ptr, libtiledb_domain_delete);
  return ptr;
}

// [[Rcpp::export]]
void libtiledb_array_schema_add_attribute(XPtr<tiledb::ArraySchema> schema,
                                          XPtr<tiledb::Attribute> attr) {
  schema->add_attribute(*attr.get());
}

// [[Rcpp::export]]
List libtiledb_array_schema_attributes(XPtr<tiledb::ArraySchema> schema) {
  List result;
  int nattr = schema->attribute_num();
  for (auto i=0; i < nattr; i++) {
    auto attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(schema->attribute(i)));
    result[attr->name()] = attr;
  }
  return result;
}

// [[Rcpp::export]]
std::string libtiledb_array_schema_get_array_type(XPtr<tiledb::ArraySchema> schema) {
  auto type = schema->array_type();
  return _tiledb_array_type_to_string(type);
}

// [[Rcpp::export]]
void libtiledb_array_schema_set_cell_order(XPtr<tiledb::ArraySchema> schema, std::string ord) {
  tiledb_layout_t cellorder = _string_to_tiledb_layout(ord);
  schema->set_cell_order(cellorder);
}

// [[Rcpp::export]]
std::string libtiledb_array_schema_get_cell_order(XPtr<tiledb::ArraySchema> schema) {
  auto order = schema->cell_order();
  return _tiledb_layout_to_string(order);
}

// [[Rcpp::export]]
void libtiledb_array_schema_set_tile_order(XPtr<tiledb::ArraySchema> schema, std::string ord) {
  tiledb_layout_t tileorder = _string_to_tiledb_layout(ord);
  schema->set_cell_order(tileorder);
}

// [[Rcpp::export]]
std::string libtiledb_array_schema_get_tile_order(XPtr<tiledb::ArraySchema> schema) {
  auto order = schema->tile_order();
  return _tiledb_layout_to_string(order);
}

// [[Rcpp::export]]
void libtiledb_array_schema_set_capacity(XPtr<tiledb::ArraySchema> schema, int cap) {
  if (cap <= 0) {
    Rcpp::stop("Tile capacity of '%d' not sensible", cap);
  }
  uint64_t tilecap = static_cast<uint64_t>(cap);
  schema->set_capacity(tilecap);
}

// [[Rcpp::export]]
int libtiledb_array_schema_get_capacity(XPtr<tiledb::ArraySchema> schema) {
  // FIXME: we try to return a uint64_t as an int. Overflow possible
  uint64_t cap = schema->capacity();
  if (cap > std::numeric_limits<int32_t>::max()) {
    Rcpp::stop("Overflow on schema capcity at '%ld'", cap);
  }
  return static_cast<int>(cap);
}

// [[Rcpp::export]]
bool libtiledb_array_schema_get_allows_dups(XPtr<tiledb::ArraySchema> schema) {
#if TILEDB_VERSION >= TileDB_Version(2,0,0)
  return schema->allows_dups();
#else
  return true;
#endif
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_set_allows_dups(XPtr<tiledb::ArraySchema> schema,
                                                                 bool allows_dups) {
#if TILEDB_VERSION >= TileDB_Version(2,0,0)
  schema->set_allows_dups(allows_dups);
#endif
  return schema;
}

// [[Rcpp::export]]
XPtr<tiledb::FilterList>
libtiledb_array_schema_get_coords_filter_list(XPtr<tiledb::ArraySchema> schema) {
  return XPtr<tiledb::FilterList>(new tiledb::FilterList(schema->coords_filter_list()));
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema>
libtiledb_array_schema_set_coords_filter_list(XPtr<tiledb::ArraySchema> schema,
                                              XPtr<tiledb::FilterList> fltlst) {
  schema->set_coords_filter_list(*fltlst);
  return schema;
}

// [[Rcpp::export]]
XPtr<tiledb::FilterList>
libtiledb_array_schema_get_offsets_filter_list(XPtr<tiledb::ArraySchema> schema) {
  return XPtr<tiledb::FilterList>(new tiledb::FilterList(schema->offsets_filter_list()));
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema>
libtiledb_array_schema_set_offsets_filter_list(XPtr<tiledb::ArraySchema> schema,
                                               XPtr<tiledb::FilterList> fltlst) {
  schema->set_offsets_filter_list(*fltlst);
  return schema;
}


// [[Rcpp::export]]
int libtiledb_array_schema_get_attribute_num(XPtr<tiledb::ArraySchema> schema) {
  uint32_t attr_num = schema->attribute_num();
  if (attr_num >= std::numeric_limits<int32_t>::max()) {
    Rcpp::stop("Overflow retrieving attribute number.");
  }
  return static_cast<int32_t>(attr_num);
}

// [[Rcpp::export]]
XPtr<tiledb::Attribute> libtiledb_array_schema_get_attribute_from_index(XPtr<tiledb::ArraySchema> schema,
                                                                        int ind) {
  if (ind < 0) {
    Rcpp::stop("Index must be non-negative.");
  }
  uint32_t idx = static_cast<uint32_t>(ind);
  return XPtr<tiledb::Attribute>(new tiledb::Attribute(schema->attribute(idx)));
}

// [[Rcpp::export]]
XPtr<tiledb::Attribute> libtiledb_array_schema_get_attribute_from_name(XPtr<tiledb::ArraySchema> schema,
                                                                       std::string name) {
   return XPtr<tiledb::Attribute>(new tiledb::Attribute(schema->attribute(name)));
}

// [[Rcpp::export]]
bool libtiledb_array_schema_has_attribute(XPtr<tiledb::ArraySchema> schema, std::string name) {
  return schema->has_attribute(name);
}

// [[Rcpp::export]]
bool libtiledb_array_schema_sparse(XPtr<tiledb::ArraySchema> schema) {
  return (schema->array_type() == TILEDB_SPARSE);
}

// [[Rcpp::export]]
void libtiledb_array_schema_dump(XPtr<tiledb::ArraySchema> schema) {
  schema->dump();
}

// [[Rcpp::export]]
bool libtiledb_array_schema_check(XPtr<tiledb::ArraySchema> schema) {
  schema->check();   // throws, rather than returning bool
  return true;
}


/**
 * TileDB Array
 */
// [[Rcpp::export]]
std::string libtiledb_array_create(std::string uri, XPtr<tiledb::ArraySchema> schema) {
  tiledb::Array::create(uri, *schema.get());
  return uri;
}

// [[Rcpp::export]]
std::string libtiledb_array_create_with_key(std::string uri, XPtr<tiledb::ArraySchema> schema,
                                            std::string encryption_key) {
  tiledb::Array::create(uri, *schema.get(), TILEDB_AES_256_GCM,
                        encryption_key.c_str(), encryption_key.size());
  return uri;
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open(XPtr<tiledb::Context> ctx, std::string uri,
                                         std::string type) {
  auto query_type = _string_to_tiledb_query_type(type);
  auto array = XPtr<tiledb::Array>(new tiledb::Array(tiledb::Array(*ctx.get(), uri, query_type)));
  return array;
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open_at(XPtr<tiledb::Context> ctx, std::string uri,
                                            std::string type, Datetime tstamp) {
  auto query_type = _string_to_tiledb_query_type(type);
  // get timestamp as seconds since epoch (plus fractional seconds, returns double), scale to millisec
  uint64_t ts_ms = static_cast<uint64_t>(std::round(tstamp.getFractionalTimestamp() * 1000));
  auto array = XPtr<tiledb::Array>(new tiledb::Array(tiledb::Array(*ctx.get(), uri,
                                                                   query_type, ts_ms)));
  return array;
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open_with_key(XPtr<tiledb::Context> ctx, std::string uri,
                                                  std::string type,
                                                  std::string enc_key) {
  auto query_type = _string_to_tiledb_query_type(type);
  auto array = XPtr<tiledb::Array>(new tiledb::Array(tiledb::Array(*ctx.get(), uri, query_type,
                                                                   TILEDB_AES_256_GCM,
                                                                   enc_key.data(),
                                                                   (uint32_t)enc_key.size())));
  return array;
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open_at_with_key(XPtr<tiledb::Context> ctx, std::string uri,
                                                     std::string type, std::string enc_key,
                                                     Datetime tstamp) {
  auto query_type = _string_to_tiledb_query_type(type);
  uint64_t ts_ms = static_cast<uint64_t>(std::round(tstamp.getFractionalTimestamp() * 1000));
  auto array = XPtr<tiledb::Array>(new tiledb::Array(tiledb::Array(*ctx.get(), uri, query_type,
                                                                   TILEDB_AES_256_GCM,
                                                                   enc_key.data(),
                                                                   (uint32_t)enc_key.size(),
                                                                   ts_ms)));
  return array;
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open_with_ptr(XPtr<tiledb::Array> array, std::string query_type) {
  tiledb_query_type_t qtype = _string_to_tiledb_query_type(query_type);
  array->open(qtype);
  return array;
}

// [[Rcpp::export]]
bool libtiledb_array_is_open(XPtr<tiledb::Array> array) {
  return array->is_open();
}

// [[Rcpp::export]]
bool libtiledb_array_is_open_for_reading(XPtr<tiledb::Array> array) {
  return array->is_open() && array->query_type() == TILEDB_READ;
}

// [[Rcpp::export]]
bool libtiledb_array_is_open_for_writing(XPtr<tiledb::Array> array) {
  return array->is_open() && array->query_type() == TILEDB_WRITE;
}

// [[Rcpp::export]]
std::string libtiledb_array_get_uri(XPtr<tiledb::Array> array) {
  return array->uri();
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_get_schema(XPtr<tiledb::Array> array) {
  return XPtr<tiledb::ArraySchema>(new tiledb::ArraySchema(array->schema()));
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_reopen(XPtr<tiledb::Array> array) {
  array->reopen();
  return array;
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_close(XPtr<tiledb::Array> array) {
  array->close();
  return array;
}

// [[Rcpp::export]]
std::string libtiledb_array_query_type(XPtr<tiledb::Array> array) {
  tiledb_query_type_t qtype = array->query_type();
  return _tiledb_query_type_to_string(qtype);
}

// [[Rcpp::export]]
List libtiledb_array_get_non_empty_domain(XPtr<tiledb::Array> array) {
  List nonempty_domain;
  auto domain = array->schema().domain();
  if (domain.type() == TILEDB_INT32) {
    using DType = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
    auto res = array->non_empty_domain<DType>();
    for (auto& d: res) {
      auto dim_name = d.first;
      auto dim_domain = d.second;
      nonempty_domain[dim_name] = IntegerVector::create(dim_domain.first,
                                                        dim_domain.second);
    }
  } else if (domain.type() == TILEDB_FLOAT64) {
    using DType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
    auto res = array->non_empty_domain<DType>();
    for (auto& d: res) {
      auto dim_name = d.first;
      auto dim_domain = d.second;
      nonempty_domain[dim_name] = NumericVector::create(dim_domain.first,
                                                        dim_domain.second);
    }
  } else {
    Rcpp::stop("Invalid tiledb_schema domain type: '%s'", _tiledb_datatype_to_string(domain.type()));
  }
  return nonempty_domain;
}

// [[Rcpp::export]]
CharacterVector libtiledb_array_get_non_empty_domain_var_from_name(XPtr<tiledb::Array> array,
                                                                   std::string name) {
#if TILEDB_VERSION >= TileDB_Version(2,0,0)
  auto res = array->non_empty_domain_var(name);
  return CharacterVector::create(res.first, res.second);
#else
  return CharacterVector::create("NA", "NA");
#endif
}

// [[Rcpp::export]]
CharacterVector libtiledb_array_get_non_empty_domain_var_from_index(XPtr<tiledb::Array> array,
                                                                    int32_t idx,
                                                                    std::string typestr = "ASCII") {
#if TILEDB_VERSION >= TileDB_Version(2,0,0)
  //auto domain = array->schema().domain();
  if (typestr == "ASCII") {
    auto res = array->non_empty_domain_var(idx);
    return CharacterVector::create(res.first, res.second);
  } else {
    Rcpp::stop("Invalid tiledb_schema domain type: '%s'", typestr.c_str());
  }
  // not reached
  return CharacterVector::create("", "");
#else
  return CharacterVector::create("NA", "NA");
#endif
}

// [[Rcpp::export]]
NumericVector libtiledb_array_get_non_empty_domain_from_name(XPtr<tiledb::Array> array,
                                                             std::string name,
                                                             std::string typestr) {
#if TILEDB_VERSION >= TileDB_Version(2,0,0)
  if (typestr == "INT64") {
    auto p = array->non_empty_domain<int64_t>(name);
    std::vector<int64_t> v{p.first, p.second};
    return makeInteger64(v);
  } else if (typestr == "UINT64") {
    auto p = array->non_empty_domain<uint64_t>(name);
    std::vector<int64_t> v{ static_cast<int64_t>(p.first), static_cast<int64_t>(p.second) };
    return makeInteger64(v);
  } else if (typestr == "INT32") {
    auto p = array->non_empty_domain<int32_t>(name);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "UINT32") {
    auto p = array->non_empty_domain<uint32_t>(name);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "INT16") {
    auto p = array->non_empty_domain<int16_t>(name);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "UINT16") {
    auto p = array->non_empty_domain<uint16_t>(name);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "INT8") {
    auto p = array->non_empty_domain<int8_t>(name);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "UINT8") {
    auto p = array->non_empty_domain<uint8_t>(name);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "FLOAT64") {
    auto p = array->non_empty_domain<double>(name);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "FLOAT32") {
    auto p = array->non_empty_domain<float>(name);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "DATETIME_YEAR" ||
             typestr == "DATETIME_MONTH" ||
             typestr == "DATETIME_WEEK" ||
             typestr == "DATETIME_DAY" ||
             typestr == "DATETIME_HR"  ||
             typestr == "DATETIME_MIN" ||
             typestr == "DATETIME_SEC" ||
             typestr == "DATETIME_MS"  ||
             typestr == "DATETIME_US"  ||
             typestr == "DATETIME_PS"  ||
             typestr == "DATETIME_FS"  ||
             typestr == "DATETIME_AS"    ) {
    // type_check() from exception.h gets invoked and wants an int64_t
    auto p = array->non_empty_domain<int64_t>(name);
    std::vector<int64_t> v{p.first, p.second};
    return makeInteger64(v);
  } else if (typestr == "DATETIME_NS") {
    auto p = array->non_empty_domain<int64_t>(name);
    std::vector<int64_t> v{p.first, p.second};
    return makeNanotime(v);
  } else {
    Rcpp::stop("Currently unsupported tiledb domain type: '%s'", typestr.c_str());
    return NumericVector::create(NA_REAL, NA_REAL); // not reached
  }
#else
  return NumericVector::create(NA_REAL, NA_REAL);
#endif
}


// [[Rcpp::export]]
NumericVector libtiledb_array_get_non_empty_domain_from_index(XPtr<tiledb::Array> array,
                                                              int32_t idx,
                                                              std::string typestr) {
#if TILEDB_VERSION >= TileDB_Version(2,0,0)
  if (typestr == "INT64") {
    auto p = array->non_empty_domain<int64_t>(idx);
    std::vector<int64_t> v{p.first, p.second};
    return makeInteger64(v);
  } else if (typestr == "UINT64") {
    auto p = array->non_empty_domain<uint64_t>(idx);
    std::vector<int64_t> v{ static_cast<int64_t>(p.first), static_cast<int64_t>(p.second) };
    return makeInteger64(v);
  } else if (typestr == "INT32") {
    auto p = array->non_empty_domain<int32_t>(idx);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "UINT32") {
    auto p = array->non_empty_domain<uint32_t>(idx);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "INT16") {
    auto p = array->non_empty_domain<int16_t>(idx);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "UINT16") {
    auto p = array->non_empty_domain<uint16_t>(idx);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "INT8") {
    auto p = array->non_empty_domain<int8_t>(idx);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "UINT8") {
    auto p = array->non_empty_domain<uint8_t>(idx);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "FLOAT64") {
    auto p = array->non_empty_domain<double>(idx);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "FLOAT32") {
    auto p = array->non_empty_domain<float>(idx);
    return NumericVector::create(p.first, p.second);
  } else if (typestr == "DATETIME_YEAR" ||
             typestr == "DATETIME_MONTH" ||
             typestr == "DATETIME_WEEK" ||
             typestr == "DATETIME_DAY" ||
             typestr == "DATETIME_HR"  ||
             typestr == "DATETIME_MIN" ||
             typestr == "DATETIME_SEC" ||
             typestr == "DATETIME_MS"  ||
             typestr == "DATETIME_US"  ||
             typestr == "DATETIME_PS"  ||
             typestr == "DATETIME_FS"  ||
             typestr == "DATETIME_AS"    ) {
    // type_check() from exception.h gets invoked and wants an int64_t
    auto p = array->non_empty_domain<int64_t>(idx);
    std::vector<int64_t> v{p.first, p.second};
    return makeInteger64(v);
  } else if (typestr == "DATETIME_NS") {
    auto p = array->non_empty_domain<int64_t>(idx);
    std::vector<int64_t> v{p.first, p.second};
    return makeNanotime(v);
  } else {
    Rcpp::stop("Currently unsupported tiledb domain type: '%s'", typestr.c_str());
    return NumericVector::create(NA_REAL, NA_REAL); // not reached
  }
#else
  return NumericVector::create(NA_REAL, NA_REAL);
#endif
}


// [[Rcpp::export]]
void libtiledb_array_consolidate(XPtr<tiledb::Context> ctx,
                                 std::string uri,
                                 Nullable<XPtr<tiledb::Config>> cfgptr = R_NilValue) {

  if (cfgptr.isNotNull()) {
    XPtr<tiledb::Config> cfg(cfgptr);
    tiledb::Array::consolidate(*ctx.get(), uri, cfg);
  } else {
    tiledb::Array::consolidate(*ctx.get(), uri);
  }
}

// [[Rcpp::export]]
void libtiledb_array_vacuum(XPtr<tiledb::Context> ctx,
                            std::string uri,
                            Nullable<XPtr<tiledb::Config>> cfgptr = R_NilValue) {

  if (cfgptr.isNotNull()) {
    XPtr<tiledb::Config> cfg(cfgptr);
    tiledb::Array::vacuum(*ctx.get(), uri, cfg);
  } else {
    tiledb::Array::vacuum(*ctx.get(), uri);
  }
}


// [[Rcpp::export]]
bool libtiledb_array_put_metadata(XPtr<tiledb::Array> array,
                                  std::string key, SEXP obj) {

  // we implement a simpler interface here as the 'type' is given from
  // the supplied SEXP, as is the extend
  switch(TYPEOF(obj)) {
    case VECSXP: {
      Rcpp::stop("List objects are not supported.");
      break;// not reached
    }
    case REALSXP: {
      Rcpp::NumericVector v(obj);
      array->put_metadata(key.c_str(), TILEDB_FLOAT64, v.size(), v.begin());
      break;
    }
    case INTSXP: {
      Rcpp::IntegerVector v(obj);
      array->put_metadata(key.c_str(), TILEDB_INT32, v.size(), v.begin());
      break;
    }
    case STRSXP: {
      Rcpp::CharacterVector v(obj);
      std::string s(v[0]);
      // We use TILEDB_CHAR interchangeably with TILEDB_STRING_ASCII is this best string type?
      array->put_metadata(key.c_str(), TILEDB_CHAR, s.length(), s.c_str());
      break;
    }
    case LGLSXP: {              // experimental: map R logical (ie TRUE, FALSE, NA) to int8
      Rcpp::LogicalVector v(obj);
      size_t n = static_cast<size_t>(v.size());
      std::vector<int8_t> ints(n);
      for (size_t i=0; i<n; i++) ints[i] = static_cast<int8_t>(v[i]);
      array->put_metadata(key.c_str(), TILEDB_INT8, ints.size(), ints.data());
      break;
    }
    default: {
      Rcpp::stop("No support (yet) for type '%d'.", TYPEOF(obj));
      break; // not reached
    }
  }
  // Close array - Important so that the metadata get flushed
  // not here, array opening and closing responsibility of caller:  array.close();
  return true;
}

// [[Rcpp::export]]
R_xlen_t libtiledb_array_get_metadata_num(XPtr<tiledb::Array> array) {
  uint64_t num = array->metadata_num();
  return static_cast<R_xlen_t>(num);
}

// helper function to copy int vector
template <typename T>
Rcpp::IntegerVector copy_int_vector(const uint32_t v_num, const void* v) {
  // Strictly speaking a check for under/overflow would be needed here yet this for
  // metadata annotation (and not data payload) so extreme ranges are less likely
  Rcpp::IntegerVector vec(v_num);
  const T *ivec = static_cast<const T*>(v);
  size_t n = static_cast<size_t>(v_num);
  for (size_t i=0; i<n; i++) vec[i] = static_cast<int32_t>(ivec[i]);
  return(vec);
}

// helper function to convert_metadata
SEXP _metadata_to_sexp(const tiledb_datatype_t v_type, const uint32_t v_num, const void* v) {
  // This supports a limited set of basic types as the metadata
  // annotation is not meant to support complete serialization
  if (v_type == TILEDB_INT32) {
    Rcpp::IntegerVector vec(v_num);
    std::memcpy(vec.begin(), v, v_num*sizeof(int32_t));
    return(vec);
  } else if (v_type == TILEDB_FLOAT64) {
    Rcpp::NumericVector vec(v_num);
    std::memcpy(vec.begin(), v, v_num*sizeof(double));
    return(vec);
  } else if (v_type == TILEDB_FLOAT32) {
    Rcpp::NumericVector vec(v_num);
    const float *fvec = static_cast<const float*>(v);
    size_t n = static_cast<size_t>(v_num);
    for (size_t i=0; i<n; i++) vec[i] = static_cast<double>(fvec[i]);
    return(vec);
  } else if (v_type == TILEDB_CHAR || v_type == TILEDB_STRING_ASCII) {
    std::string s(static_cast<const char*>(v), v_num);
    return(Rcpp::wrap(s));
  } else if (v_type == TILEDB_INT8) {
    Rcpp::LogicalVector vec(v_num);
    const int8_t *ivec = static_cast<const int8_t*>(v);
    size_t n = static_cast<size_t>(v_num);
    for (size_t i=0; i<n; i++) vec[i] = static_cast<bool>(ivec[i]);
    return(vec);
  } else if (v_type == TILEDB_UINT8) {
    // Strictly speaking a check for under/overflow would be needed here (and below) yet this
    // is for metadata annotation (and not data payload) so extreme ranges are less likely
    return copy_int_vector<uint8_t>(v_num, v);
  } else if (v_type == TILEDB_INT16) {
    return copy_int_vector<int16_t>(v_num, v);
  } else if (v_type == TILEDB_UINT16) {
    return copy_int_vector<uint16_t>(v_num, v);
  } else if (v_type == TILEDB_UINT32) {
    return copy_int_vector<uint32_t>(v_num, v);
  } else if (v_type == TILEDB_INT64) {
    return copy_int_vector<int64_t>(v_num, v);
  } else if (v_type == TILEDB_UINT64) {
    return copy_int_vector<uint64_t>(v_num, v);
  } else {
    Rcpp::stop("No support yet for %s", _tiledb_datatype_to_string(v_type));
  }
}

// [[Rcpp::export]]
SEXP libtiledb_array_get_metadata_from_index(XPtr<tiledb::Array> array, int idx) {
  std::string key;
  tiledb_datatype_t v_type;
  uint32_t v_num;
  const void* v;
  array->get_metadata_from_index(static_cast<uint64_t>(idx), &key, &v_type, &v_num, &v);
  if (v == NULL) {
    return R_NilValue;
  }

  RObject vec = _metadata_to_sexp(v_type, v_num, v);
  vec.attr("names") = Rcpp::CharacterVector::create(key);
  return vec;
}

// [[Rcpp::export]]
SEXP libtiledb_array_get_metadata_list(XPtr<tiledb::Array> array) {
  uint64_t num = array->metadata_num();
  int n = static_cast<int>(num);
  Rcpp::List lst(n);
  Rcpp::CharacterVector names(n);
  for (auto i=0; i<n; i++) {
    // we trick this a little by having the returned object also carry an attribute
    // cleaner way (in a C++ pure sense) would be to return a pair of string and SEXP
    SEXP v = libtiledb_array_get_metadata_from_index(array, i);
    Rcpp::RObject obj(v);
    Rcpp::CharacterVector objnms = obj.attr("names");
    names(i) = objnms[0];
    obj.attr("names") = R_NilValue; // remove attribute from object
    lst(i) = obj;
  }
  lst.attr("names") = names;
  return lst;
}

// [[Rcpp::export]]
void libtiledb_array_delete_metadata(XPtr<tiledb::Array> array, std::string key) {
  array->delete_metadata(key.c_str());
}

/**
 * Query
 */
// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query(XPtr<tiledb::Context> ctx,
                                    XPtr<tiledb::Array> array,
                                    std::string type) {
  auto query_type = _string_to_tiledb_query_type(type);
  auto query = XPtr<tiledb::Query>(
    new tiledb::Query(tiledb::Query(*ctx.get(), *array.get(), query_type)));
  return query;
}

// [[Rcpp::export]]
std::string libtiledb_query_type(XPtr<tiledb::Query> query) {
  return _tiledb_query_type_to_string(query->query_type());
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_layout(XPtr<tiledb::Query> query,
                                               std::string layout) {
  auto _layout = _string_to_tiledb_layout(layout);
  query->set_layout(_layout);
  return query;
}

// [[Rcpp::export]]
std::string libtiledb_query_layout(XPtr<tiledb::Query> query) {
  auto layout = query->query_layout();
  return _tiledb_layout_to_string(layout);
}

// generalized version which switches
// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_subarray_with_type(XPtr<tiledb::Query> query,
                                                           SEXP subarray, std::string typestr) {
  if (typestr == "INT32") {
    IntegerVector sub(subarray);
    query->set_subarray(sub.begin(), sub.length());
  } else if (typestr == "FLOAT64") {
    NumericVector sub(subarray);
    query->set_subarray(sub.begin(), sub.length());
  } else if (typestr == "INT64" ||
             typestr == "UINT32" ||
             typestr == "DATETIME_NS") {
    NumericVector sub(subarray);
    std::vector<int64_t> v(sub.length());
    for (int i=0; i<sub.length(); i++)
      v[i] = static_cast<int64_t>(sub[i]);
    query->set_subarray(v);
  } else if (typestr == "DATETIME_YEAR"  ||
             typestr == "DATETIME_MONTH" ||
             typestr == "DATETIME_WEEK"  ||
             typestr == "DATETIME_DAY") {
    DateVector sub(subarray);
    std::vector<int64_t> v = dates_to_int64(sub, _string_to_tiledb_datatype(typestr));
    query->set_subarray(v);
  } else if (typestr == "DATETIME_HR"  ||
             typestr == "DATETIME_MIN" ||
             typestr == "DATETIME_SEC" ||
             typestr == "DATETIME_MS"  ||
             typestr == "DATETIME_US") {
    DatetimeVector sub(subarray);
    std::vector<int64_t> v = datetimes_to_int64(sub, _string_to_tiledb_datatype(typestr));
    query->set_subarray(v);
  } else if (typestr == "UINT64") {
    NumericVector sub(subarray);
    std::vector<uint64_t> v(sub.length());
    for (int i=0; i<sub.length(); i++)
      v[i] = static_cast<uint64_t>(sub[i]);
    query->set_subarray(v);
  } else {
    Rcpp::stop("currently unsupported subarray datatype '%s'", typestr.c_str());
  }
  return query;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_subarray(XPtr<tiledb::Query> query,
                                                 SEXP subarray) {
  if (TYPEOF(subarray) == INTSXP) {
    IntegerVector sub(subarray);
    query->set_subarray(sub.begin(), sub.length());
    return query;
  } else if (TYPEOF(subarray) == REALSXP) {
    NumericVector sub(subarray);
    query->set_subarray(sub.begin(), sub.length());
    return query;
  } else {
    Rcpp::stop("currently unsupported subarray datatype");
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_buffer(XPtr<tiledb::Query> query,
                                               std::string attr,
                                               SEXP buffer) {
  if (TYPEOF(buffer) == INTSXP) {
    IntegerVector vec(buffer);
    query->set_buffer(attr, vec.begin(), vec.length());
    return query;
  } else if (TYPEOF(buffer) == REALSXP) {
    NumericVector vec(buffer);
    query->set_buffer(attr, vec.begin(), vec.length());
    return query;
  } else if (TYPEOF(buffer) == LGLSXP) {
    LogicalVector vec(buffer);
    query->set_buffer(attr, vec.begin(), vec.length());
    return query;
  } else {
    Rcpp::stop("Invalid attribute buffer type for attribute '%s': %s",
               attr.c_str(), Rcpp::type2name(buffer));
  }
}

// -- vlc_buf_t functions below

// [[Rcpp::export]]
XPtr<vlc_buf_t> libtiledb_query_buffer_var_char_alloc_direct(int szoffsets, int szdata,
                                                             bool nullable) {
  XPtr<vlc_buf_t> buf = XPtr<vlc_buf_t>(new vlc_buf_t, false);
  registerXptrFinalizer(buf, libtiledb_vlc_buf_delete);
  buf->offsets.resize(szoffsets);
  buf->str.resize(szdata);
  buf->rows = szoffsets;           // guess for number of elements
  buf->cols = 1;
  buf->nullable = nullable;
  buf->validity_map.resize(szdata);
  return buf;
}

// helper function to turn a vector of strings
// [[Rcpp::export]]
std::string convertStringVectorIntoOffsetsAndString(Rcpp::CharacterVector vec,
                                                    Rcpp::IntegerVector offsets) {
  size_t n = vec.size();
  if (offsets.size() != (R_xlen_t)n) Rcpp::stop("offsets needs to be of same size as vec");
  std::string data = "";
  int cumlen = 0;
  for (size_t i=0; i<n; i++) {
    std::string s(vec[i]);
    offsets[i] = cumlen;
    data += s;
    cumlen += s.length();
  }
  return data;
}


// assigning (for a write) allocates
// [[Rcpp::export]]
XPtr<vlc_buf_t> libtiledb_query_buffer_var_char_create(IntegerVector intoffsets,
                                                       std::string data) {
  XPtr<vlc_buf_t> bufptr = XPtr<vlc_buf_t>(new vlc_buf_t);
  int n = intoffsets.size();
  bufptr->offsets.resize(n);
  for (int i=0; i<n; i++) {
    bufptr->offsets[i] = static_cast<uint64_t>(intoffsets[i]);
  }
  bufptr->str = data;
  bufptr->rows = bufptr->cols = 0; // signal unassigned for the write case
  bufptr->validity_map.resize(n);  // validity_map resized but not used
  bufptr->nullable = false;
  return(bufptr);
}

// assigning (for a write) allocates with nullable vector
// [[Rcpp::export]]
XPtr<vlc_buf_t> libtiledb_query_buffer_var_char_create_nullable(IntegerVector intoffsets,
                                                                std::string data,
                                                                bool nullable,
                                                                std::vector<bool> navec) {
  XPtr<vlc_buf_t> bufptr = XPtr<vlc_buf_t>(new vlc_buf_t);
  int n = intoffsets.size();
  bufptr->offsets.resize(n);
  for (int i=0; i<n; i++) {
    bufptr->offsets[i] = static_cast<uint64_t>(intoffsets[i]);
  }
  bufptr->str = data;
  bufptr->rows = bufptr->cols = 0; // signal unassigned for the write case
  if (nullable) {
      bufptr->validity_map.resize(n);
      for (int i=0; i<n; i++) {
          bufptr->validity_map[i] = (navec[i] ? 1 : 0);
      }
  }
  bufptr->nullable = nullable;
  return(bufptr);
}


// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_buffer_var_char(XPtr<tiledb::Query> query,
                                                        std::string attr,
                                                        XPtr<vlc_buf_t> bufptr) {

#if TILEDB_VERSION >= TileDB_Version(2,2,4)
    if (bufptr->nullable) {
        query->set_buffer_nullable(attr, bufptr->offsets, bufptr->str, bufptr->validity_map);
    } else {
        query->set_buffer(attr, bufptr->offsets, bufptr->str);
    }
    return query;
#else
    query->set_buffer(attr, bufptr->offsets, bufptr->str);
    return query;
#endif
}

// 'len' is the length of the query result set, i.e. buffer elements for standard columns
// 'nchar' is the length of the result set for the particular column, i.e. actual (ex-pos)
//    string length in bufptr (as opposed to ex-ante guess)
// [[Rcpp::export]]
CharacterMatrix libtiledb_query_get_buffer_var_char(XPtr<vlc_buf_t> bufptr,
                                                    int32_t len=0, int32_t nchar=0) {
  size_t n = (len==0 ? bufptr->offsets.size() : len);
  //Rprintf("n=%d, strsize=%d, row %d col %d, nchar %d\n",
  //        n, bufptr->str.size(), bufptr->rows, bufptr->cols, nchar);
  std::vector<uint64_t> str_sizes(n);
  for (size_t i = 0; i < n - 1; i++) {                          // all but last
    //Rprintf("%d %d %d\n", i, bufptr->offsets[i + 1] , bufptr->offsets[i]);
    str_sizes[i] = bufptr->offsets[i + 1] - bufptr->offsets[i];
  }                                                             // last is total size minus last start
  str_sizes[n-1] = (nchar==0 ? bufptr->str.size() * sizeof(char) : nchar) - bufptr->offsets[n-1];
  // Get the strings
  CharacterMatrix mat(bufptr->rows, bufptr->cols);
  for (size_t i = 0; i < n; i++) {
      if (bufptr->validity_map[i] == 0)
          mat[i] = std::string(&bufptr->str[bufptr->offsets[i]], str_sizes[i]);
      else
          mat[i] = R_NaString;
  }
  return(mat);
}

// more of a debugging helper
// [[Rcpp::export]]
std::string libtiledb_query_get_buffer_var_char_simple(XPtr<vlc_buf_t> bufptr) {
  return(bufptr->str);
}

// -- vlv_buf_t functions below

// assigning (for a write) allocates
// [[Rcpp::export]]
XPtr<vlv_buf_t> libtiledb_query_buffer_var_vec_create(IntegerVector intoffsets, SEXP data) {
  int n = intoffsets.size();
  XPtr<vlv_buf_t> bufptr = XPtr<vlv_buf_t>(new vlv_buf_t);
  bufptr->offsets.resize(n);
  for (int i=0; i<n; i++) {
    bufptr->offsets[i] = static_cast<uint64_t>(intoffsets[i]);
  }
  if (TYPEOF(data) == INTSXP) {
    bufptr->idata = Rcpp::as<std::vector<int32_t>>(data);
    bufptr->ddata.clear();
    bufptr->dtype = TILEDB_INT32;
  } else if (TYPEOF(data) == REALSXP) {
    bufptr->ddata = Rcpp::as<std::vector<double>>(data);
    bufptr->idata.clear();
    bufptr->dtype = TILEDB_FLOAT64;
    return(bufptr);
  } else {
    Rcpp::stop("Invalid data type for buffer: '%s'", Rcpp::type2name(data));
  }
  return(bufptr);
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_buffer_var_vec(XPtr<tiledb::Query> query,
                                                       std::string attr, XPtr<vlv_buf_t> buf) {
  if (buf->dtype == TILEDB_INT32) {
    query->set_buffer(attr, buf->offsets, buf->idata);
  } else if (buf->dtype == TILEDB_FLOAT64) {
    query->set_buffer(attr, buf->offsets, buf->ddata);
  } else {
    Rcpp::stop("Unsupported type '%s' for buffer", _tiledb_datatype_to_string(buf->dtype));
  }
  return query;
}

// [[Rcpp::export]]
List libtiledb_query_get_buffer_var_vec(XPtr<tiledb::Query> query, std::string attr,
                                        XPtr<vlv_buf_t> buf) {

  int n = buf->offsets.size();
  IntegerVector ivec(n);
  for (int i=0; i<n; i++) {
    ivec[i] = static_cast<int32_t>(buf->offsets[i]);
  }
  auto dims = query->result_buffer_elements()[attr]; // actual result dims post query
  n = dims.second;            // actual size, not allocated size
  if (buf->dtype == TILEDB_INT32) {
    IntegerVector dvec(n);
    for (int i=0; i<n; i++) {
      dvec[i] = static_cast<int32_t>(buf->idata[i]);
    }
    List rl = List::create(Rcpp::Named("offsets") = ivec,
                           Rcpp::Named("data")    = dvec);
    return(rl);
  } else if (buf->dtype == TILEDB_FLOAT64) {
    NumericVector dvec(n);
    for (int i=0; i<n; i++) {
      dvec[i] = static_cast<double>(buf->ddata[i]);
    }
    List rl = List::create(Rcpp::Named("offsets") = ivec,
                           Rcpp::Named("data")    = dvec);
    return(rl);
  } else {
    Rcpp::stop("Unsupported type '%s' for buffer", _tiledb_datatype_to_string(buf->dtype));
  }
  return Rcpp::as<Rcpp::List>(R_NilValue); // not reached
}

// -- query_buf_t aka sparse_coords_buf_t

// In the following signature we cannot have a templated type as the return type so we have
// to bring the switch between types 'inside' and make it run-time dependent on the subarray
// type we already had
// [[Rcpp::export]]
XPtr<query_buf_t> libtiledb_query_buffer_alloc_ptr(XPtr<tiledb::Array> array,
                                                   std::string domaintype,
                                                   R_xlen_t ncells,
                                                   bool nullable = false) {
  XPtr<query_buf_t> buf = XPtr<query_buf_t>(new query_buf_t, false);
  registerXptrFinalizer(buf, libtiledb_query_buf_delete);
  if (domaintype == "INT32"  || domaintype == "UINT32") {
     buf->size = sizeof(int32_t);
  } else if (domaintype == "INT16"  || domaintype == "UINT16") {
     buf->size = sizeof(int16_t);
  } else if (domaintype == "INT8"   || domaintype == "UINT8") {
     buf->size = sizeof(int8_t);
  } else if (domaintype == "INT64" ||
             domaintype == "UINT64" ||
             domaintype == "DATETIME_YEAR" ||
             domaintype == "DATETIME_MONTH" ||
             domaintype == "DATETIME_WEEK" ||
             domaintype == "DATETIME_DAY" ||
             domaintype == "DATETIME_HR" ||
             domaintype == "DATETIME_MIN" ||
             domaintype == "DATETIME_SEC" ||
             domaintype == "DATETIME_MS" ||
             domaintype == "DATETIME_US" ||
             domaintype == "DATETIME_NS" ||
             domaintype == "DATETIME_PS" ||
             domaintype == "DATETIME_FS" ||
             domaintype == "DATETIME_AS") {
     buf->size = sizeof(int64_t);
  } else if (domaintype == "FLOAT64") {
     buf->size = sizeof(double);
  } else if (domaintype == "FLOAT32") {
     buf->size = sizeof(float);
  } else {
     Rcpp::stop("Currently unsupported domain type '%s'", domaintype.c_str());
  }
  buf->dtype = _string_to_tiledb_datatype(domaintype);
  buf->ncells = ncells;
  buf->vec.resize(ncells * buf->size);
  if (nullable) buf->validity_map.resize(ncells);
  buf->nullable = nullable;
  return buf;
}

// [[Rcpp::export]]
XPtr<query_buf_t> libtiledb_query_buffer_assign_ptr(XPtr<query_buf_t> buf, std::string dtype,
                                                    SEXP vec, bool asint64 = false) {
  if (dtype == "INT32") {
    IntegerVector v(vec);
    std::memcpy(buf->vec.data(), &(v[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map);
    //if (buf->nullable) for (int i=0; i<buf->ncells; i++) Rprintf("%d : %d\n", i, buf->validity_map[i]);
  } else if (dtype == "FLOAT64") {
    NumericVector v(vec);
    std::memcpy(buf->vec.data(), &(v[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromNumeric(v, buf->validity_map);
  } else if (dtype == "INT64" ||
             (asint64 && is_datetime_column(buf->dtype))) {
    // integer64 from the bit64 package uses doubles, sees nanosecond
    NumericVector v(vec);
    std::memcpy(buf->vec.data(), &(v[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInt64(v, buf->validity_map);
  } else if (dtype == "DATETIME_YEAR" ||
             dtype == "DATETIME_MONTH" ||
             dtype == "DATETIME_WEEK" ||
             dtype == "DATETIME_DAY") {
    DateVector v(vec);
    std::vector<int64_t> tt = dates_to_int64(v, _string_to_tiledb_datatype(dtype));
    std::memcpy(buf->vec.data(), tt.data(), buf->ncells * buf->size);
  } else if (dtype == "DATETIME_MS" ||
             dtype == "DATETIME_US" ||
             dtype == "DATETIME_SEC"||
             dtype == "DATETIME_MIN"||
             dtype == "DATETIME_HR"   ) {
    DatetimeVector v(vec);
    std::vector<int64_t> tt = datetimes_to_int64(v, _string_to_tiledb_datatype(dtype));
    std::memcpy(buf->vec.data(), tt.data(), buf->ncells * buf->size);
  } else if (dtype == "DATETIME_NS") {
    // nanosecond time uses the nanotime package which uses the bit64 package
    // to store the int64_t 'payload' on 64-bit double, so memcpy does the trick
    NumericVector v(vec);
    std::memcpy(buf->vec.data(), &(v[0]), buf->ncells*buf->size);
  } else if (dtype == "DATETIME_PS" ||
             dtype == "DATETIME_FS" ||
             dtype == "DATETIME_AS") {
    NumericVector v(vec);
    std::vector<int64_t> tt = subnano_to_int64(v, _string_to_tiledb_datatype(dtype));
    std::memcpy(buf->vec.data(), tt.data(), buf->ncells * buf->size);
  } else if (dtype == "UINT64") {
    // R has no native uint64_t representation so this comes in as numeric; we then
    // use our int64_t <-> integer64 machinery for null maps but store as uint64_t
    NumericVector v(vec);
    std::vector<int64_t> iv = getInt64Vector(v);
    if (buf->nullable)
        getValidityMapFromInt64(v, buf->validity_map);
    auto n = v.length();
    std::vector<uint64_t> uiv(n);
    for (auto i=0; i<n; i++) {
      uiv[i] = static_cast<uint64_t>(iv[i]);
    }
    std::memcpy(buf->vec.data(), &(uiv[0]), buf->ncells*buf->size);
  } else if (dtype == "UINT32") {
    IntegerVector v(vec);
    auto n = v.length();
    std::vector<uint32_t> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<uint32_t>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map);
  } else if (dtype == "INT16") {
    IntegerVector v(vec);
    auto n = v.length();
    std::vector<int16_t> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<int16_t>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map);
  } else if (dtype == "UINT16") {
    IntegerVector v(vec);
    auto n = v.length();
    std::vector<uint16_t> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<uint16_t>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map);
  } else if (dtype == "INT8") {
    IntegerVector v(vec);
    auto n = v.length();
    std::vector<int8_t> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<int8_t>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map);
  } else if (dtype == "UINT8") {
    IntegerVector v(vec);
    auto n = v.length();
    std::vector<uint8_t> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<uint8_t>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map);
  } else if (dtype == "FLOAT32") {
    NumericVector v(vec);
    if (buf->nullable)
        getValidityMapFromNumeric(v, buf->validity_map);
    auto n = v.length();
    std::vector<float> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<float>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
  } else {
    Rcpp::stop("Assignment to '%s' currently unsupported.", dtype.c_str());
  }
  return buf;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_buffer_ptr(XPtr<tiledb::Query> query,
                                                   std::string attr,
                                                   XPtr<query_buf_t> buf) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    if (buf->nullable) {
        query->set_buffer_nullable(attr, static_cast<void*>(buf->vec.data()), buf->ncells,
                                   buf->validity_map.data(),
                                   static_cast<uint64_t>(buf->validity_map.size()));
    } else {
        query->set_buffer(attr, static_cast<void*>(buf->vec.data()), buf->ncells);
    }
    return query;
#else
    query->set_buffer(attr, static_cast<void*>(buf->vec.data()), buf->ncells);
    return query;
#endif
}


// [[Rcpp::export]]
RObject libtiledb_query_get_buffer_ptr(XPtr<query_buf_t> buf, bool asint64 = false) {
  std::string dtype = _tiledb_datatype_to_string(buf->dtype);
  if (dtype == "INT32") {
    IntegerVector v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    if (buf->nullable)
        setValidityMapForInteger(v, buf->validity_map);
    return v;
  } else if (dtype == "UINT32") {
    IntegerVector v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    if (buf->nullable)
        setValidityMapForInteger(v, buf->validity_map);
    return v;
  } else if (dtype == "FLOAT64") {
    NumericVector v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    if (buf->nullable)
        setValidityMapForNumeric(v, buf->validity_map);
    return v;
  } else if (dtype == "FLOAT32") {
    std::vector<float> v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    NumericVector w(wrap(v));
    if (buf->nullable)
        setValidityMapForNumeric(w, buf->validity_map);
    return w;
  } else if (dtype == "UINT64") {
    auto n = buf->ncells;
    std::vector<uint64_t> uv(n);
    std::memcpy(&(uv[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    std::vector<int64_t> iv(n);
    for (auto i=0; i<n; i++) {
      iv[i] = static_cast<int64_t>(uv[i]);
    }
    NumericVector res = wrap(iv);
    if (buf->nullable)
        setValidityMapForNumeric(res, buf->validity_map);
    //return makeInteger64(iv); // we could return as int64,
    return res;                 // but current 'contract' is return as NumericVector
  } else if (dtype == "INT64") {
    std::vector<int64_t> v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    if (buf->nullable)
        setValidityMapForInt64(v, buf->validity_map);
    return makeInteger64(v);
  } else if (asint64 && is_datetime_column(buf->dtype)) {
    std::vector<int64_t> v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    return makeInteger64(v);
  } else if (dtype == "DATETIME_FS" ||
             dtype == "DATETIME_PS" ||
             dtype == "DATETIME_AS") {
    std::vector<int64_t> v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    Rcpp::NumericVector dv = int64_to_subnano(v, _string_to_tiledb_datatype(dtype));
    return dv;
  } else if (dtype == "DATETIME_YEAR" ||
             dtype == "DATETIME_MONTH" ||
             dtype == "DATETIME_WEEK" ||
             dtype == "DATETIME_DAY") {
    std::vector<int64_t> v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    DateVector dv = int64_to_dates(v, _string_to_tiledb_datatype(dtype));
    return dv;
  } else if (dtype == "DATETIME_HR" ||
             dtype == "DATETIME_MIN" ||
             dtype == "DATETIME_SEC" ||
             dtype == "DATETIME_MS" ||
             dtype == "DATETIME_US") {
    std::vector<int64_t> v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    DatetimeVector dv = int64_to_datetimes(v, _string_to_tiledb_datatype(dtype));
    return dv;
  } else if (dtype == "DATETIME_NS") {
    int n = buf->ncells;
    std::vector<int64_t> vec(n);
    std::memcpy(vec.data(), buf->vec.data(), n*buf->size);
    return makeNanotime(vec);
  } else if (dtype == "INT16") {
    size_t n = buf->ncells;
    std::vector<int16_t> intvec(n);
    std::memcpy(intvec.data(), buf->vec.data(), n*buf->size);
    Rcpp::IntegerVector out(buf->ncells);
    for (size_t i=0; i<n; i++) {
      out[i] = static_cast<int32_t>(intvec[i]);
    }
    if (buf->nullable)
        setValidityMapForInteger(out, buf->validity_map);
    return out;
  } else if (dtype == "UINT16") {
    size_t n = buf->ncells;
    std::vector<uint16_t> intvec(n);
    std::memcpy(intvec.data(), buf->vec.data(), n*buf->size);
    Rcpp::IntegerVector out(buf->ncells);
    for (size_t i=0; i<n; i++) {
      out[i] = static_cast<int32_t>(intvec[i]);
    }
    if (buf->nullable)
        setValidityMapForInteger(out, buf->validity_map);
    return out;
  } else if (dtype == "INT8") {
    size_t n = buf->ncells;
    std::vector<int8_t> intvec(n);
    std::memcpy(intvec.data(), buf->vec.data(), n*buf->size);
    Rcpp::IntegerVector out(buf->ncells);
    for (size_t i=0; i<n; i++) {
      out[i] = static_cast<int32_t>(intvec[i]);
    }
    if (buf->nullable)
        setValidityMapForInteger(out, buf->validity_map);
    return out;
  } else if (dtype == "UINT8") {
    size_t n = buf->ncells;
    std::vector<uint8_t> uintvec(n);
    std::memcpy(uintvec.data(), buf->vec.data(), n*buf->size);
    Rcpp::IntegerVector out(buf->ncells);
    for (size_t i=0; i<n; i++) {
      out[i] = static_cast<int32_t>(uintvec[i]);
    }
    if (buf->nullable)
        setValidityMapForInteger(out, buf->validity_map);
    return out;
  } else {
    Rcpp::stop("Unsupported type '%s'", dtype.c_str());
  }
  return R_NilValue; // not reached
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_submit(XPtr<tiledb::Query> query) {
  query->submit();
  return query;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_submit_async(XPtr<tiledb::Query> query) {
  query->submit_async();
  return query;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_finalize(XPtr<tiledb::Query> query) {
  query->finalize();
  return query;
}

std::string _query_status_to_string(tiledb::Query::Status status) {
  switch (status) {
    case tiledb::Query::Status::COMPLETE:
      return "COMPLETE";
    case tiledb::Query::Status::FAILED:
      return "FAILED";
    case tiledb::Query::Status::INPROGRESS:
      return "INPROGRESS";
    case tiledb::Query::Status::INCOMPLETE:
      return "INCOMPLETE";
    case tiledb::Query::Status::UNINITIALIZED:
    default:
      return "UNINITIALIZED";
  }
}


// [[Rcpp::export]]
std::string libtiledb_query_status(XPtr<tiledb::Query> query) {
  tiledb::Query::Status status = query->query_status();
  return _query_status_to_string(status);
}

// [[Rcpp::export]]
R_xlen_t libtiledb_query_result_buffer_elements(XPtr<tiledb::Query> query, std::string attribute) {
  R_xlen_t nelem = query->result_buffer_elements()[attribute].second;
  return nelem;
}

// [[Rcpp::export]]
int libtiledb_query_get_fragment_num(XPtr<tiledb::Query> query) {
  if (query->query_type() != TILEDB_WRITE) {
    Rcpp::stop("Fragment number only applicable to 'write' queries.");
  }
  return query->fragment_num();
}

// [[Rcpp::export]]
std::string libtiledb_query_get_fragment_uri(XPtr<tiledb::Query> query, int idx) {
  if (query->query_type() != TILEDB_WRITE) {
    Rcpp::stop("Fragment URI only applicable to 'write' queries.");
  }
  uint32_t uidx = static_cast<uint32_t>(idx);
  return query->fragment_uri(uidx);
}

// [[Rcpp::export]]
Rcpp::DatetimeVector libtiledb_query_get_fragment_timestamp_range(XPtr<tiledb::Query> query, int idx) {
  if (query->query_type() != TILEDB_WRITE) {
    Rcpp::stop("Fragment URI only applicable to 'write' queries.");
  }
  uint32_t uidx = static_cast<uint32_t>(idx);
  std::pair<uint64_t, uint64_t> range = query->fragment_timestamp_range(uidx);
  return Rcpp::DatetimeVector::create(range.first/1000.0, range.second/1000.0);
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_add_range(XPtr<tiledb::Query> query, int iidx,
                                              SEXP starts, SEXP ends,
                                              SEXP strides = R_NilValue) {
  if (TYPEOF(starts) != TYPEOF(ends)) {
    Rcpp::stop("'start' and 'end' must be of identical types");
  }
  uint32_t uidx = static_cast<uint32_t>(iidx);
  if (TYPEOF(starts) == INTSXP) {
    int32_t start = as<int32_t>(starts);
    int32_t end = as<int32_t>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      int32_t stride = as<int32_t>(strides);
      query->add_range(uidx, start, end, stride);
    }
  } else if (TYPEOF(starts) == REALSXP) {
    double start = as<double>(starts);
    double end = as<double>(ends);
    if (strides == R_NilValue) {
      query->add_range(uidx, start, end);
    } else {
      double stride = as<double>(strides);
      query->add_range(uidx, start, end, stride);
    }
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

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_add_range_with_type(XPtr<tiledb::Query> query,
                                                        int iidx,
                                                        std::string typestr,
                                                        SEXP starts, SEXP ends,
                                                        SEXP strides = R_NilValue) {
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
      uint64_t stride = static_cast<uint64_t>(makeScalarInteger64(as<double>(strides)));
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
  } else if (typestr == "DATETIME_YEAR" ||
             typestr == "DATETIME_MONTH" ||
             typestr == "DATETIME_WEEK" ||
             typestr == "DATETIME_DAY" ||
             typestr == "DATETIME_HR"  ||
             typestr == "DATETIME_MIN" ||
             typestr == "DATETIME_SEC" ||
             typestr == "DATETIME_MS" ||
             typestr == "DATETIME_US" ||
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
      Rcpp::stop("Non-emoty stride for string not supported yet.");
    }
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
  } else {
    Rcpp::stop("Invalid data type for adding range to query: '%s'", Rcpp::type2name(starts));
  }
  return query;
}

// [[Rcpp::export]]
R_xlen_t libtiledb_query_get_est_result_size(XPtr<tiledb::Query> query, std::string attr) {
  uint64_t est = query->est_result_size(attr);
  return static_cast<R_xlen_t>(est);
}


// [[Rcpp::export]]
NumericVector libtiledb_query_get_est_result_size_nullable(XPtr<tiledb::Query> query, std::string attr) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    std::array<uint64_t, 2> est = query->est_result_size_nullable(attr);
    return Rcpp::NumericVector::create(static_cast<R_xlen_t>(est[0]),
                                       static_cast<R_xlen_t>(est[1]));
#else
    return Rcpp::NumericVector::create(R_NaReal,R_NaReal);
#endif
}


// [[Rcpp::export]]
NumericVector libtiledb_query_get_est_result_size_var(XPtr<tiledb::Query> query, std::string attr) {
#if TILEDB_VERSION < TileDB_Version(2,2,0)
  std::pair<uint64_t, uint64_t> est = query->est_result_size_var(attr);
  return NumericVector::create(static_cast<R_xlen_t>(est.first), static_cast<R_xlen_t>(est.second));
#else
  std::array<uint64_t, 2> est = query->est_result_size_var(attr);
  return NumericVector::create(static_cast<R_xlen_t>(est[0]), static_cast<R_xlen_t>(est[1]));
#endif
}

// [[Rcpp::export]]
NumericVector libtiledb_query_get_est_result_size_var_nullable(XPtr<tiledb::Query> query, std::string attr) {
#if TILEDB_VERSION < TileDB_Version(2,2,0)
    return Rcpp::NumericVector::create(R_NaReal,R_NaReal,R_NaReal);
#else
    std::array<uint64_t, 3> est = query->est_result_size_var_nullable(attr);
    return Rcpp::NumericVector::create(static_cast<R_xlen_t>(est[0]),
                                       static_cast<R_xlen_t>(est[1]),
                                       static_cast<R_xlen_t>(est[2]));
#endif
}

// [[Rcpp::export]]
double libtiledb_query_get_range_num(XPtr<tiledb::Query> query, int dim_idx) {

  uint64_t range_num = query->range_num(static_cast<unsigned int>(dim_idx));
  return static_cast<double>(range_num);
}

// [[Rcpp::export]]
IntegerVector libtiledb_query_get_range(XPtr<tiledb::Query> query, int dim_idx, int rng_idx) {
  std::array<int32_t, 3> rng = query->range<int32_t>(static_cast<unsigned int>(dim_idx),
                                                     static_cast<unsigned int>(rng_idx));
  return IntegerVector::create(rng[0], 	// start
                               rng[1],  // end
                               rng[2]); // stride
}

// [[Rcpp::export]]
CharacterVector libtiledb_query_get_range_var(XPtr<tiledb::Query> query, int dim_idx, int rng_idx) {
  std::array<std::string, 2> rng = query->range(static_cast<unsigned int>(dim_idx), static_cast<uint64_t>(rng_idx));
  return CharacterVector::create(rng[0], rng[1]);	 // start and end
}

/**
 * Array helper functions
 */
// [[Rcpp::export]]
NumericVector libtiledb_zip_coords_numeric(List coords, R_xlen_t coord_length) {
  auto ndim = coords.length();
  NumericVector result(ndim * coord_length);
  if (result.length() < 2) {
    return result;
  }
  for (R_xlen_t dim = 0; dim < ndim; dim++) {
    NumericVector cur_dim = coords[dim];
    R_xlen_t result_idx = dim;
    for (R_xlen_t i = 0; i < coord_length; i++) {
      result[result_idx] = cur_dim[i];
      result_idx += ndim;
    }
  }
  return result;
}

// [[Rcpp::export]]
IntegerVector libtiledb_zip_coords_integer(List coords, R_xlen_t coord_length) {
  auto ndim = coords.length();
  IntegerVector result(ndim * coord_length);
  if (result.length() < 2) {
    return result;
  }
  for (R_xlen_t dim = 0; dim < ndim; dim++) {
    IntegerVector cur_dim = coords[dim];
    R_xlen_t result_idx = dim;
    for (R_xlen_t i = 0; i < coord_length; i++) {
      result[result_idx] = cur_dim[i];
      result_idx += ndim;
    }
  }
  return result;
}


/**
 * Object functionality
 */
// [[Rcpp::export]]
std::string libtiledb_group_create(XPtr<tiledb::Context> ctx, std::string uri) {
  tiledb::create_group(*ctx.get(), uri);
  return uri;
}

std::string _object_type_to_string(tiledb::Object::Type otype) {
  switch (otype) {
    case tiledb::Object::Type::Array:
      return "ARRAY";
    case tiledb::Object::Type::Group:
      return "GROUP";
    case tiledb::Object::Type::Invalid:
    default:
      return "INVALID";
  }
}

// [[Rcpp::export]]
std::string libtiledb_object_type(XPtr<tiledb::Context> ctx, std::string uri) {
  auto obj = tiledb::Object::object(*ctx.get(), uri);
  return _object_type_to_string(obj.type());
}

// [[Rcpp::export]]
std::string libtiledb_object_remove(XPtr<tiledb::Context> ctx, std::string uri) {
  tiledb::Object::remove(*ctx.get(), uri);
  return uri;
}

// [[Rcpp::export]]
std::string libtiledb_object_move(XPtr<tiledb::Context> ctx, std::string old_uri, std::string new_uri) {
  tiledb::Object::move(*ctx.get(), old_uri, new_uri);
  return new_uri;
}

tiledb::Object::Type _string_to_object_type(std::string otype) {
  if (otype == "ARRAY") {
    return tiledb::Object::Type::Array;
  } else if (otype == "GROUP") {
    return tiledb::Object::Type::Group;
  } else {
    Rcpp::stop("invalid object type string");
  }
}

// [[Rcpp::export]]
DataFrame libtiledb_object_walk(XPtr<tiledb::Context> ctx,
                                std::string uri,
                                std::string order,
                                bool recursive = false) {
  tiledb_walk_order_t walk_order;
  if (recursive) {
    if (order == "PREORDER") {
      walk_order = TILEDB_PREORDER;
    } else if (order == "POSTORDER") {
      walk_order = TILEDB_POSTORDER;
    } else {
      Rcpp::stop("invalid recursive walk order, must be \"PREORDER\" or \"POSTORDER\"");
    }
  }
  std::vector<std::string> uris;
  std::vector<std::string> types;
  tiledb::ObjectIter obj_iter(*ctx.get(), uri);
  if (recursive) {
    obj_iter.set_recursive(walk_order);
  } else {
    obj_iter.set_non_recursive();
  }
  for (const auto& object : obj_iter) {
    uris.push_back(object.uri());
    types.push_back(_object_type_to_string(object.type()));
  }
  Rcpp::StringVector r_uris(uris.size());
  r_uris = uris;
  Rcpp::StringVector r_types(types.size());
  r_types = types;
  return Rcpp::DataFrame::create(_["TYPE"] = r_types, _["URI"] = r_uris);
}

/**
 * VFS functionality
 */
// [[Rcpp::export]]
XPtr<tiledb::VFS> libtiledb_vfs(XPtr<tiledb::Context> ctx,
                                Nullable<XPtr<tiledb::Config>> config=R_NilValue) {
  if (config.isNull()) {
    auto ptr = XPtr<tiledb::VFS>(new tiledb::VFS(*ctx.get()), false);
    registerXptrFinalizer(ptr, libtiledb_vfs_delete);
    return ptr;
  } else {
    XPtr<tiledb::Config> config_xptr(config);
    auto ptr = XPtr<tiledb::VFS>(new tiledb::VFS(*ctx.get(), *config_xptr.get()), false);
    registerXptrFinalizer(ptr, libtiledb_vfs_delete);
    return ptr;
  }
}

// [[Rcpp::export]]
std::string libtiledb_vfs_create_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->create_bucket(uri);
  return uri;
}

// [[Rcpp::export]]
std::string libtiledb_vfs_remove_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->remove_bucket(uri);
  return uri;
}

// [[Rcpp::export]]
bool libtiledb_vfs_is_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  return vfs->is_bucket(uri);
}

// [[Rcpp::export]]
bool libtiledb_vfs_is_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  return vfs->is_empty_bucket(uri);
}

// [[Rcpp::export]]
std::string libtiledb_vfs_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->empty_bucket(uri);
  return uri;
}

// [[Rcpp::export]]
std::string libtiledb_vfs_create_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->create_dir(uri);
  return uri;
}

// [[Rcpp::export]]
bool libtiledb_vfs_is_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  return vfs->is_dir(uri);
}

// [[Rcpp::export]]
std::string libtiledb_vfs_remove_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->remove_dir(uri);
  return uri;
}

// [[Rcpp::export]]
bool libtiledb_vfs_is_file(XPtr<tiledb::VFS> vfs, std::string uri) {
  return vfs->is_file(uri);
}

// [[Rcpp::export]]
std::string libtiledb_vfs_remove_file(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->remove_file(uri);
  return uri;
}

// [[Rcpp::export]]
R_xlen_t libtiledb_vfs_file_size(XPtr<tiledb::VFS> vfs, std::string uri) {
  uint64_t size = vfs->file_size(uri);
  if (size > std::numeric_limits<R_xlen_t>::max()) {
    Rcpp::stop("file size is greater than maximum R integer");
  }
  return static_cast<R_xlen_t>(size);
}

// [[Rcpp::export]]
std::string libtiledb_vfs_move_file(XPtr<tiledb::VFS> vfs,
                                    std::string old_uri,
                                    std::string new_uri) {
  vfs->move_file(old_uri, new_uri);
  return new_uri;
}

// [[Rcpp::export]]
std::string libtiledb_vfs_move_dir(XPtr<tiledb::VFS> vfs,
                                   std::string old_uri,
                                   std::string new_uri) {
  vfs->move_dir(old_uri, new_uri);
  return new_uri;
}

// [[Rcpp::export]]
std::string libtiledb_vfs_touch(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->touch(uri);
  return uri;
}


// [[Rcpp::export]]
XPtr<vfs_fh_t> libtiledb_vfs_open(XPtr<tiledb::Context> ctxxp, XPtr<tiledb::VFS> vfsxp,
                                  std::string uri, std::string mode) {
   std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
   std::shared_ptr<tiledb_vfs_t> vfs = vfsxp.get()->ptr();
   tiledb_vfs_fh_t *fh = nullptr;
   tiledb_vfs_mode_t vfsmode = _string_to_tiledb_vfs_mode_t(mode);
   tiledb_vfs_open(ctx.get(), vfs.get(), uri.c_str(), vfsmode, &fh);
   XPtr<vfs_fh_t> ptr = XPtr<vfs_fh_t>(new vfs_fh_t, false);
   registerXptrFinalizer(ptr, libtiledb_vfs_fh_delete);
   ptr->fh = static_cast<void*>(fh);
   return ptr;
}

// [[Rcpp::export]]
void libtiledb_vfs_close(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh) {
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  tiledb_vfs_close(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh));
}

// [[Rcpp::export]]
void libtiledb_vfs_write(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh,
                         Rcpp::IntegerVector vec) {
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  tiledb_vfs_write(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh),
                   &(vec[0]), vec.size()*sizeof(int));
}

// [[Rcpp::export]]
Rcpp::IntegerVector libtiledb_vfs_read(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh,
                                       double offset, double nbytes) {
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  std::int64_t offs = makeScalarInteger64(offset);
  std::int64_t nb = makeScalarInteger64(nbytes);
  Rcpp::IntegerVector buf(nb/4);
  tiledb_vfs_read(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh), offs, &(buf[0]), nb);
  return buf;
}

// [[Rcpp::export]]
void libtiledb_vfs_sync(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh) {
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  tiledb_vfs_sync(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh));
}


/**
 * Stats
 */

// [[Rcpp::export]]
void libtiledb_stats_enable() {
  tiledb::Stats::enable();
}

// [[Rcpp::export]]
void libtiledb_stats_disable() {
  tiledb::Stats::disable();
}

// [[Rcpp::export]]
void libtiledb_stats_reset() {
  tiledb::Stats::reset();
}

// [[Rcpp::export]]
void libtiledb_stats_dump(std::string path = "") {
  if (path == "") {
    tiledb::Stats::dump();
  } else {
    FILE* fptr = nullptr;
    fptr = fopen(path.c_str(), "w");
    if (fptr == nullptr) {
      Rcpp::stop("error opening stats dump file for writing");
    }
    tiledb::Stats::dump(fptr);
    fclose(fptr);
  }
}

// [[Rcpp::export]]
void libtiledb_stats_raw_dump(std::string path = "") {
#if TILEDB_VERSION < TileDB_Version(2,0,3)
  Rcpp::stop("This function requires TileDB Embedded 2.0.3 or later.");
#else
  if (path == "") {
    tiledb::Stats::raw_dump();
  } else {
    FILE* fptr = nullptr;
    fptr = fopen(path.c_str(), "w");
    if (fptr == nullptr) {
      Rcpp::stop("error opening stats dump file for writing");
    }
    tiledb::Stats::raw_dump(fptr);
    fclose(fptr);
  }
#endif
}

// [[Rcpp::export]]
std::string libtiledb_stats_raw_get() {
#if TILEDB_VERSION < TileDB_Version(2,0,3)
  Rcpp::stop("This function requires TileDB Embedded 2.0.3 or later.");
  return(std::string());//not reached
#else
  std::string result;
  tiledb::Stats::raw_dump(&result);
  return result;
#endif
}
