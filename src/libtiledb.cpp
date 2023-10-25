//  MIT License
//
//  Copyright (c) 2017-2023 TileDB Inc.
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
#include "tiledb_version.h"

#include <fstream>
#include <unistd.h>

using namespace Rcpp;

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
    case TILEDB_BLOB:
      return "BLOB";
#if TILEDB_VERSION >= TileDB_Version(2,10,0)
    case TILEDB_BOOL:
      return "BOOL";
#endif
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
  } else if (typestr == "BLOB") {
    return TILEDB_BLOB;
#if TILEDB_VERSION >= TileDB_Version(2,10,0)
  } else if (typestr == "BOOL") {
    return TILEDB_BOOL;
#endif
  } else {
    Rcpp::stop("Unknown TileDB type '%s'", typestr.c_str());
  }
}

// [[Rcpp::export]]
int32_t tiledb_datatype_string_to_sizeof(const std::string str) {
    return static_cast<int32_t>(tiledb_datatype_size(_string_to_tiledb_datatype(str)));
}

//' Map from TileDB type to R datatype
//'
//' This function maps from the TileDB types to the (fewer) key datatypes in R. This
//' can be lossy as TileDB integers range from (signed and unsigned) 8 to 64 bit whereas
//' R only has (signed) 32 bit values. Similarly, R only has 64 bit doubles whereas TileDB
//' has 32 and 64 bit floating point types. TileDB also has more character encodings, and the
//' full range of (NumPy) date and time types.
//'
//' @param datatype A string describing one TileDB datatype
//' @return A string describing the closest match for an R datatype
//' @export
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
#if TILEDB_VERSION >= TileDB_Version(2,10,0)
    case TILEDB_BOOL:
      return "BOOL";
#endif
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
    case TILEDB_HILBERT:
      return "HILBERT";
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
  } else if (lstr == "HILBERT") {
    return TILEDB_HILBERT;
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
  } else if (filter == "CHECKSUM_MD5") {
    return TILEDB_FILTER_CHECKSUM_MD5;
  } else if (filter == "CHECKSUM_SHA256") {
    return TILEDB_FILTER_CHECKSUM_SHA256;
#if TILEDB_VERSION >= TileDB_Version(2,9,0)
  } else if (filter == "DICTIONARY_ENCODING") {
    return TILEDB_FILTER_DICTIONARY;
#endif
#if TILEDB_VERSION >= TileDB_Version(2,11,0)
  } else if (filter == "SCALE_FLOAT") {
    return TILEDB_FILTER_SCALE_FLOAT;
#endif
#if TILEDB_VERSION >= TileDB_Version(2,12,0)
  } else if (filter == "FILTER_XOR") {
    return TILEDB_FILTER_XOR;
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
    case TILEDB_FILTER_CHECKSUM_MD5:
      return "CHECKSUM_MD5";
    case TILEDB_FILTER_CHECKSUM_SHA256:
      return "CHECKSUM_SHA256";
#if TILEDB_VERSION >= TileDB_Version(2,9,0)
    case TILEDB_FILTER_DICTIONARY:
      return "DICTIONARY_ENCODING";
#endif
#if TILEDB_VERSION >= TileDB_Version(2,11,0)
    case TILEDB_FILTER_SCALE_FLOAT:
      return "SCALE_FLOAT";
#endif
#if TILEDB_VERSION >= TileDB_Version(2,12,0)
  case TILEDB_FILTER_XOR:
    return "FILTER_XOR";
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
#if TILEDB_VERSION >= TileDB_Version(2,11,0)
  } else if (filter_option == "SCALE_FLOAT_BYTEWIDTH") {
    return TILEDB_SCALE_FLOAT_BYTEWIDTH;
  } else if (filter_option == "SCALE_FLOAT_FACTOR") {
    return TILEDB_SCALE_FLOAT_FACTOR;
  } else if (filter_option == "SCALE_FLOAT_OFFSET") {
    return TILEDB_SCALE_FLOAT_OFFSET;
#endif
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
#if TILEDB_VERSION >= TileDB_Version(2,11,0)
  case TILEDB_SCALE_FLOAT_BYTEWIDTH:
    return "SCALE_FLOAT_BYTEWIDTH";
  case TILEDB_SCALE_FLOAT_FACTOR:
    return "SCALE_FLOAT_FACTOR";
  case TILEDB_SCALE_FLOAT_OFFSET:
    return "SCALE_FLOAT_OFFSET";
#endif
  default:
    Rcpp::stop("unknown tiledb_filter_option_t (%d)", filter_option);
  }
}

tiledb_query_type_t _string_to_tiledb_query_type(std::string qtstr) {
  if (qtstr == "READ") {
    return TILEDB_READ;
  } else if (qtstr == "WRITE") {
    return TILEDB_WRITE;
#if TILEDB_VERSION >= TileDB_Version(2,12,0)
  } else if (qtstr == "MODIFY_EXCLUSIVE") {
    return TILEDB_MODIFY_EXCLUSIVE;
  } else if (qtstr == "DELETE") {
    return TILEDB_DELETE;
#endif
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
#if TILEDB_VERSION >= TileDB_Version(2,12,0)
    case TILEDB_DELETE:
      return "DELETE";
    case TILEDB_MODIFY_EXCLUSIVE:
      return "MODIFY_EXCLUSIVE";
#endif
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

tiledb_encryption_type_t _string_to_tiledb_encryption_type_t(std::string encstr) {
    tiledb_encryption_type_t enc;
    int rc = tiledb_encryption_type_from_str(encstr.c_str(), &enc);
    if (rc == TILEDB_OK)
        return enc;
    Rcpp::stop("Unknow TileDB encryption type '%s'", encstr.c_str());
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
        return make_xptr<tiledb::Context>(new tiledb::Context());
    } else {
        XPtr<tiledb::Config> config_xptr(config);
        return make_xptr<tiledb::Context>(new tiledb::Context(*config_xptr.get()));
    }
}

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_ctx_config(XPtr<tiledb::Context> ctx) {
    check_xptr_tag<tiledb::Context>(ctx);
    return make_xptr<tiledb::Config>(new tiledb::Config(ctx.get()->config()));
}

// [[Rcpp::export]]
bool libtiledb_ctx_is_supported_fs(XPtr<tiledb::Context> ctx, std::string scheme) {
  check_xptr_tag<tiledb::Context>(ctx);
  if (scheme == "file") {
    return true;
  } else if  (scheme == "s3") {
    return ctx->is_supported_fs(TILEDB_S3);
  } else if (scheme == "hdfs") {
    return ctx->is_supported_fs(TILEDB_HDFS);
  } else if (scheme == "azure") {
    return ctx->is_supported_fs(TILEDB_AZURE);
  } else if (scheme == "gcs") {
    return ctx->is_supported_fs(TILEDB_GCS);
  } else if (scheme == "memory") {
    return ctx->is_supported_fs(TILEDB_MEMFS);
  } else {
    Rcpp::stop("Unknown TileDB fs scheme: '%s'", scheme.c_str());
  }
}

// [[Rcpp::export]]
void libtiledb_ctx_set_tag(XPtr<tiledb::Context> ctx, std::string key, std::string value) {
  check_xptr_tag<tiledb::Context>(ctx);
  ctx->set_tag(key, value);
}

// [[Rcpp::export]]
std::string libtiledb_ctx_stats(XPtr<tiledb::Context> ctx) {
    check_xptr_tag<tiledb::Context>(ctx);
    return ctx->stats();
}

/**
 * TileDB Config
 */

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_config(Nullable<CharacterVector> config = R_NilValue) {
  XPtr<tiledb::Config> ptr = make_xptr<tiledb::Config>(new tiledb::Config());
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
  check_xptr_tag<tiledb::Config>(config);
  config->save_to_file(filename);
  return filename;
}

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_config_load_from_file(std::string filename) {
  XPtr<tiledb::Config> ptr = make_xptr<tiledb::Config>(new tiledb::Config(filename));
  return ptr;
}

// [[Rcpp::export]]
CharacterVector libtiledb_config_vector(XPtr<tiledb::Config> config) {
  check_xptr_tag<tiledb::Config>(config);
  CharacterVector config_vec;
  for (auto& p : *config) {
    config_vec[p.first]  = p.second;
  }
  return config_vec;
}

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_config_set(XPtr<tiledb::Config> config,
                                          std::string param, std::string value) {
  check_xptr_tag<tiledb::Config>(config);
  (*config)[param] = value;
  return config;
}

// [[Rcpp::export]]
CharacterVector libtiledb_config_get(XPtr<tiledb::Config> config, CharacterVector params) {
  check_xptr_tag<tiledb::Config>(config);
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
XPtr<tiledb::Config> libtiledb_config_unset(XPtr<tiledb::Config> config, std::string param) {
  check_xptr_tag<tiledb::Config>(config);
  config->unset(param);
  return config;
}

// [[Rcpp::export]]
void libtiledb_config_dump(XPtr<tiledb::Config> config) {
  check_xptr_tag<tiledb::Config>(config);
  Rcout << "Config settings:\n";
  for (auto& p : *config) {
    Rcout << "\"" << p.first << "\" : \"" << p.second << "\"\n";
  }
}

// Placed here as it relates to config. (New as of 2.17, uses experimental header)
// [[Rcpp::export]]
std::string libtiledb_as_built_dump() {
    std::string str = "";
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    str = tiledb::AsBuilt::dump();
#endif
    return str;
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
    check_xptr_tag<tiledb::Context>(ctx);
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
        Rcpp::stop("only integer ((U)INT{8,16,32,64}), real (FLOAT{32,64}), DATETIME_{YEAR,MONTH,WEEK,DAY,HR,MIN,SEC,MS,US,NS,PS,FS,AS}, STRING_ASCII domains supported");
    }
    // check that the dimension type aligns with the domain and tiledb_extent type
    if (dtype == TILEDB_INT32 && (TYPEOF(domain) != INTSXP || TYPEOF(tile_extent) != INTSXP)) {
        Rcpp::stop("domain or tile_extent does not match dimension type");
    } else if (dtype == TILEDB_FLOAT64 && (TYPEOF(domain) != REALSXP || TYPEOF(tile_extent) != REALSXP)) {
        Rcpp::stop("domain or tile_extent does not match dimension type");
    }
    if (dtype == TILEDB_INT32) {
        Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
        if (domain_vec.length() != 2) {
            Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
        }
        std::array<int32_t, 2> _domain = {domain_vec[0], domain_vec[1]};
        int32_t _tile_extent = Rcpp::as<int32_t>(tile_extent);
        auto dim = new tiledb::Dimension(tiledb::Dimension::create<int32_t>(*ctx.get(), name, _domain, _tile_extent));
        auto ptr = make_xptr<tiledb::Dimension>(dim);
        return ptr;

    } else if (dtype == TILEDB_UINT32) {
        Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
        if (domain_vec.length() != 2) {
            Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
        }
        std::array<uint32_t, 2> _domain = {static_cast<uint32_t>(domain_vec[0]), static_cast<uint32_t>(domain_vec[1])};
        uint32_t _tile_extent = Rcpp::as<uint32_t>(tile_extent);
        auto dim = new tiledb::Dimension(tiledb::Dimension::create<uint32_t>(*ctx.get(), name, _domain, _tile_extent));
        auto ptr = make_xptr<tiledb::Dimension>(dim);
        return ptr;

    } else if (dtype == TILEDB_INT16) {
        Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
        if (domain_vec.length() != 2) {
            Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
        }
        std::array<int16_t, 2> _domain = {static_cast<int16_t>(domain_vec[0]), static_cast<int16_t>(domain_vec[1])};
        int16_t _tile_extent = Rcpp::as<int16_t>(tile_extent);
        auto dim = new tiledb::Dimension(tiledb::Dimension::create<int16_t>(*ctx.get(), name, _domain, _tile_extent));
        auto ptr = make_xptr<tiledb::Dimension>(dim);
        return ptr;

    } else if (dtype == TILEDB_UINT16) {
        Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
        if (domain_vec.length() != 2) {
            Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
        }
        std::array<uint16_t, 2> _domain = {static_cast<uint16_t>(domain_vec[0]), static_cast<uint16_t>(domain_vec[1])};
        int16_t _tile_extent = Rcpp::as<int16_t>(tile_extent);
        auto dim = new tiledb::Dimension(tiledb::Dimension::create<uint16_t>(*ctx.get(), name, _domain, _tile_extent));
        auto ptr = make_xptr<tiledb::Dimension>(dim);
        return ptr;

    } else if (dtype == TILEDB_INT8) {
        Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
        if (domain_vec.length() != 2) {
            Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
        }
        std::array<int8_t, 2> _domain = {static_cast<int8_t>(domain_vec[0]), static_cast<int8_t>(domain_vec[1])};
        int8_t _tile_extent = static_cast<int8_t>(Rcpp::as<int16_t>(tile_extent));
        auto dim = new tiledb::Dimension(tiledb::Dimension::create<int8_t>(*ctx.get(), name, _domain, _tile_extent));
        auto ptr = make_xptr<tiledb::Dimension>(dim);
        return ptr;

  } else if (dtype == TILEDB_UINT8) {
        Rcpp::IntegerVector domain_vec = Rcpp::IntegerVector(domain);
        if (domain_vec.length() != 2) {
            Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
        }
        std::array<uint8_t, 2> _domain = {static_cast<uint8_t>(domain_vec[0]), static_cast<uint8_t>(domain_vec[1])};
        uint8_t _tile_extent = Rcpp::as<uint8_t>(tile_extent);
        auto dim = new tiledb::Dimension(tiledb::Dimension::create<uint8_t>(*ctx.get(), name, _domain, _tile_extent));
        auto ptr = make_xptr<tiledb::Dimension>(dim);
        return ptr;

  } else if (dtype == TILEDB_INT64) {
        // for int64 domains and extents we require integer64 types which are internally memmap'ed from double
        Rcpp::NumericVector dv(domain);
        if (!isInteger64(dv)) {
            Rcpp::stop("dimension domain for INT64 must be an integer64 type in R");
        }
        if (dv.size() != 2) {
            Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
        }
        std::vector<int64_t> domain_vec = fromInteger64(Rcpp::NumericVector(dv));
        std::array<int64_t, 2> _domain = {domain_vec[0], domain_vec[1]};
        Rcpp::NumericVector ext(tile_extent);
        if (!isInteger64(ext)) {
            Rcpp::stop("tile exent for INT64 domain must be an integer64 type in R");
        }
        int64_t _tile_extent = fromInteger64(ext[0]);
        auto dim = new tiledb::Dimension(tiledb::Dimension::create<int64_t>(*ctx.get(), name, _domain, _tile_extent));
        auto ptr = make_xptr<tiledb::Dimension>(dim);
        return ptr;

    } else if (dtype == TILEDB_UINT64) {
        // for uint64 domains and extents we require integer64 types which are internally memmap'ed from double
        Rcpp::NumericVector dv(domain);
        if (!isInteger64(dv)) {
            Rcpp::stop("dimension domain for UINT64 must be an integer64 type in R");
        }
        if (dv.size() != 2) {
            Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
        }
        std::vector<int64_t> domain_vec = fromInteger64(Rcpp::NumericVector(dv));
        std::array<uint64_t, 2> _domain = { static_cast<uint64_t>(domain_vec[0]), static_cast<uint64_t>(domain_vec[1])};
        Rcpp::NumericVector ext(tile_extent);
        if (!isInteger64(ext)) {
            Rcpp::stop("tile exent for UINT64 domain must be an integer64 type in R");
        }
        uint64_t _tile_extent = static_cast<uint64_t>(fromInteger64(ext[0]));
        auto dim = new tiledb::Dimension(tiledb::Dimension::create<uint64_t>(*ctx.get(), name, _domain, _tile_extent));
        auto ptr = make_xptr<tiledb::Dimension>(dim);
        return ptr;

    } else if (dtype == TILEDB_FLOAT32) {
        Rcpp::NumericVector domain_vec = Rcpp::NumericVector(domain);
        if (domain_vec.length() != 2) {
            Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
        }
        std::array<float, 2> _domain = {static_cast<float>(domain_vec[0]), static_cast<float>(domain_vec[1])};
        float_t _tile_extent = Rcpp::as<float>(tile_extent);
        auto d = new tiledb::Dimension(tiledb::Dimension::create<float>(*ctx.get(), name, _domain, _tile_extent));
        auto ptr = make_xptr<tiledb::Dimension>(d);
        return ptr;

    } else if (dtype == TILEDB_FLOAT64) {
        Rcpp::NumericVector domain_vec = Rcpp::NumericVector(domain);
        if (domain_vec.length() != 2) {
            Rcpp::stop("dimension domain must be a c(lower bound, upper bound) pair");
        }
        std::array<double, 2> _domain = {static_cast<double>(domain_vec[0]), static_cast<double>(domain_vec[1])};
        double_t _tile_extent = Rcpp::as<double>(tile_extent);
        auto d = new tiledb::Dimension(tiledb::Dimension::create<double>(*ctx.get(), name, _domain, _tile_extent));
        auto ptr = make_xptr<tiledb::Dimension>(d);
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
        auto ptr = make_xptr<tiledb::Dimension>(dim);
        return ptr;

    } else if (dtype == TILEDB_STRING_ASCII) {
        if (Rf_isNull(domain) && Rf_isNull(tile_extent)) {
            auto d = tiledb::Dimension::create(*ctx.get(), name, TILEDB_STRING_ASCII, nullptr, nullptr);
            auto dim = new tiledb::Dimension(d);
            auto ptr = make_xptr<tiledb::Dimension>(dim);
            return ptr;

        } else {
            Rcpp::stop("Non-null domain or extent to be added.");
        }
    } else {
        Rcpp::stop("Unsupported tiledb type (%d) this should not happen!", dtype);
    }
}

// [[Rcpp::export]]
std::string libtiledb_dim_get_name(XPtr<tiledb::Dimension> dim) {
  check_xptr_tag<tiledb::Dimension>(dim);
  return dim->name();
}

// [[Rcpp::export]]
SEXP libtiledb_dim_get_domain(XPtr<tiledb::Dimension> dim) {
  check_xptr_tag<tiledb::Dimension>(dim);
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
      if (d1 > std::numeric_limits<uint32_t>::max() ||
          d2 > std::numeric_limits<uint32_t>::max()) {
        Rcpp::stop("tiledb_dim domain UINT32 value not representable as an R integer64 type");
      }
      std::vector<int64_t> v = { static_cast<int64_t>(d1), static_cast<int64_t>(d2) };
      return toInteger64(v);
    }
    case TILEDB_INT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT64>::type;
      auto d1 = dim->domain<DataType>().first;
      auto d2 = dim->domain<DataType>().second;
      if (d1 > std::numeric_limits<int64_t>::max() || d2 > std::numeric_limits<int64_t>::max()) {
          return NumericVector({static_cast<double>(d1), static_cast<double>(d2)});
      }
      std::vector<int64_t> v = { d1, d2 };
      return toInteger64(v);
    }
    case TILEDB_UINT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT64>::type;
      auto d1 = dim->domain<DataType>().first;
      auto d2 = dim->domain<DataType>().second;
      if (d1 > std::numeric_limits<int64_t>::max() || d2 > std::numeric_limits<int64_t>::max()) {
          return NumericVector({static_cast<double>(d1), static_cast<double>(d2)});
      }
      std::vector<int64_t> v = { static_cast<int64_t>(d1), static_cast<int64_t>(d2) };
      return toInteger64(v);
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
          std::vector<int64_t> v{d1, d2};     // return as int64
          return toInteger64(v);              // which 'travels' as a double
      }
      return IntegerVector({static_cast<int32_t>(d1), static_cast<int32_t>(d2)});
    }
    default:
      Rcpp::stop("invalid tiledb_dim domain type (%s)", _tiledb_datatype_to_string(dim_type));
  }
}

// [[Rcpp::export]]
SEXP libtiledb_dim_get_tile_extent(XPtr<tiledb::Dimension> dim) {
  check_xptr_tag<tiledb::Dimension>(dim);
  auto dim_type = dim->type();
  switch (dim_type) {
    case TILEDB_FLOAT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT32>::type;
      return Rcpp::wrap(static_cast<double>(dim->tile_extent<DataType>()));
    }
    case TILEDB_FLOAT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
      auto t = dim->tile_extent<DataType>();
      if (t == R_NaReal) {
        Rcpp::stop("tiledb_dim tile FLOAT64 value not representable as an R double");
      }
      return Rcpp::wrap(static_cast<double>(dim->tile_extent<DataType>()));
    }
    case TILEDB_INT8: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT8>::type;
      return Rcpp::wrap(static_cast<int32_t>(dim->tile_extent<DataType>()));
    }
    case TILEDB_UINT8: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT8>::type;
      return Rcpp::wrap(static_cast<int32_t>(dim->tile_extent<DataType>()));
    }
    case TILEDB_INT16: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT16>::type;
      return Rcpp::wrap(static_cast<int32_t>(dim->tile_extent<DataType>()));
    }
    case TILEDB_UINT16: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT16>::type;
      return Rcpp::wrap(static_cast<int32_t>(dim->tile_extent<DataType>()));
    }
    case TILEDB_INT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
      auto t = dim->tile_extent<DataType>();
      if (t == R_NaInt) {
        Rcpp::stop("tiledb_dim tile INT32 value not representable as an R integer");
      }
      return Rcpp::wrap(static_cast<int32_t>(t));
    }
    case TILEDB_UINT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT32>::type;
      auto t = dim->tile_extent<DataType>();
      if (t > std::numeric_limits<int32_t>::max()) {
        Rcpp::warning("tiledb_dim tile UINT32 value not representable as an R integer, returning double");
        return Rcpp::wrap(static_cast<double>(t));
      } else {
        return Rcpp::wrap(static_cast<int32_t>(t));
      }
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
    case TILEDB_DATETIME_AS:
    case TILEDB_INT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT64>::type;
      auto t = dim->tile_extent<DataType>();
      if (t <= R_NaInt || t > std::numeric_limits<int32_t>::max()) {
          std::vector<int64_t> v{t};         // return as int64
          return toInteger64(v);             // which 'travels' as a double
      }
      // 'else' i.e. default cast to int32
      return Rcpp::wrap(static_cast<int32_t>(t));
    }
    case TILEDB_UINT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT64>::type;
      auto t = dim->tile_extent<DataType>();
      if (t > std::numeric_limits<int64_t>::max()) {
          Rcpp::warning("tiledb_dim tile UINT32 value not representable as an INT64, returning double");
          return Rcpp::wrap(static_cast<double>(t));
      }
      if (t > std::numeric_limits<int32_t>::max()) {
          auto tt = static_cast<int64_t>(t); // avoids a 'narrowing' watnings
          std::vector<int64_t> v{ tt };      // return as int64
          return toInteger64(v);             // which 'travels' as a double
      }
      return Rcpp::wrap(static_cast<int32_t>(t));
    }
    default:
      Rcpp::stop("invalid tiledb_dim domain type (%s)", _tiledb_datatype_to_string(dim_type));
  }
}

// [[Rcpp::export]]
std::string libtiledb_dim_get_datatype(XPtr<tiledb::Dimension> dim) {
  check_xptr_tag<tiledb::Dimension>(dim);
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
  check_xptr_tag<tiledb::Dimension>(dim);
  unsigned int ncells = dim->cell_val_num();
  if (ncells == TILEDB_VAR_NUM) {
    return R_NaInt;          // set to R's NA for integer
  } else if (ncells > std::numeric_limits<int32_t>::max()) {
    Rcpp::stop("tiledb_attr ncells value not representable as an R integer");
  }
  return static_cast<int32_t>(ncells);
}

// [[Rcpp::export]]
XPtr<tiledb::FilterList> libtiledb_dimension_get_filter_list(XPtr<tiledb::Dimension> dim) {
  check_xptr_tag<tiledb::Dimension>(dim);
  return make_xptr<tiledb::FilterList>(new tiledb::FilterList(dim->filter_list()));
}

// [[Rcpp::export]]
XPtr<tiledb::Dimension> libtiledb_dimension_set_filter_list(XPtr<tiledb::Dimension> dim,
                                                            XPtr<tiledb::FilterList> fltrlst) {
  check_xptr_tag<tiledb::Dimension>(dim);
  check_xptr_tag<tiledb::FilterList>(fltrlst);
  dim->set_filter_list(*fltrlst);
  return dim;
}


/**
 * TileDB Domain
 */
// [[Rcpp::export]]
XPtr<tiledb::Domain> libtiledb_domain(XPtr<tiledb::Context> ctx, List dims) {
  check_xptr_tag<tiledb::Context>(ctx);
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
  XPtr<tiledb::Domain> domain = make_xptr<tiledb::Domain>(new tiledb::Domain(*ctx.get()));
  for (auto& val : dims) {
    // TODO: we can't do much type checking for the cast here until we wrap EXTPTRSXP in S4 classes
    auto dim = as<XPtr<tiledb::Dimension>>(val);
    check_xptr_tag<tiledb::Dimension>(dim);
    domain->add_dimension(*dim.get());
  }
  return domain;
}

// [[Rcpp::export]]
std::string libtiledb_domain_get_type(XPtr<tiledb::Domain> domain) {
  check_xptr_tag<tiledb::Domain>(domain);
  auto dtype = domain->type();
  return _tiledb_datatype_to_string(dtype);
}

// [[Rcpp::export]]
int libtiledb_domain_get_ndim(XPtr<tiledb::Domain> domain) {
  check_xptr_tag<tiledb::Domain>(domain);
  uint32_t rank = domain->ndim();
  if (rank > std::numeric_limits<int32_t>::max()) {
    Rcpp::stop("tiledb::Domain rank is not representable by an R integer");
  }
  return static_cast<int32_t>(rank);
}

// [[Rcpp::export]]
XPtr<tiledb::Dimension> libtiledb_domain_get_dimension_from_index(XPtr<tiledb::Domain> dom, int idx) {
  check_xptr_tag<tiledb::Domain>(dom);
  auto dim = dom->dimension(idx);
  auto ptr = make_xptr<tiledb::Dimension>(new tiledb::Dimension(dim));
  return ptr;
}

// [[Rcpp::export]]
XPtr<tiledb::Dimension> libtiledb_domain_get_dimension_from_name(XPtr<tiledb::Domain> dom,
                                                                 std::string name) {
  check_xptr_tag<tiledb::Domain>(dom);
  auto dim = dom->dimension(name.c_str());
  auto ptr = make_xptr<tiledb::Dimension>(new tiledb::Dimension(dim));
  return ptr;
}

// [[Rcpp::export]]
List libtiledb_domain_get_dimensions(XPtr<tiledb::Domain> domain) {
  check_xptr_tag<tiledb::Domain>(domain);
  List dimensions;
  for (auto& dim : domain->dimensions()) {
    dimensions.push_back(make_xptr<tiledb::Dimension>(new tiledb::Dimension(dim)));
  }
  return dimensions;
}

// [[Rcpp::export]]
bool libtiledb_domain_has_dimension(XPtr<tiledb::Domain> domain, std::string name) {
  check_xptr_tag<tiledb::Domain>(domain);
  return domain->has_dimension(name.c_str());
}

// [[Rcpp::export]]
void libtiledb_domain_dump(XPtr<tiledb::Domain> domain) {
  check_xptr_tag<tiledb::Domain>(domain);
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
  check_xptr_tag<tiledb::Context>(ctx);
  tiledb_filter_type_t fltr = _string_to_tiledb_filter(filter);
  return make_xptr<tiledb::Filter>(new tiledb::Filter(*ctx.get(), fltr));
}

//[[Rcpp::export]]
std::string libtiledb_filter_get_type(XPtr<tiledb::Filter> filter) {
  check_xptr_tag<tiledb::Filter>(filter);
  return _tiledb_filter_to_string(filter->filter_type());
}

//[[Rcpp::export]]
R_xlen_t libtiledb_filter_get_option(XPtr<tiledb::Filter> filter, std::string filter_option_str) {
  check_xptr_tag<tiledb::Filter>(filter);
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
XPtr<tiledb::Filter> libtiledb_filter_set_option(XPtr<tiledb::Filter> filter, std::string filter_option_str, SEXP valuesxp) {
    check_xptr_tag<tiledb::Filter>(filter);
    tiledb_filter_option_t filter_option = _string_to_tiledb_filter_option(filter_option_str);
#if TILEDB_VERSION >= TileDB_Version(2,11,0)
    // For scale_float filters we need either a double, or an
    if (filter_option == TILEDB_SCALE_FLOAT_FACTOR || filter_option == TILEDB_SCALE_FLOAT_OFFSET) {
        double value = Rcpp::as<double>(valuesxp);
        spdl::debug(tfm::format("[libtiledb_filter_set_option] setting %s to %f", filter_option_str, value));
        filter->set_option(filter_option, &value);
        return filter;
    } else if (filter_option == TILEDB_SCALE_FLOAT_BYTEWIDTH) {
        double dblval = Rcpp::as<double>(valuesxp);
        int64_t int64val = fromInteger64(dblval);
        uint64_t value = static_cast<uint64_t>(int64val);
        spdl::debug(tfm::format("[libtiledb_filter_set_option] setting %s to %ld", filter_option_str, value));
        filter->set_option(filter_option, &value);
        return filter;
    }
#endif
    // all others set an int value
    int32_t value = Rcpp::as<int32_t>(valuesxp);
    filter->set_option(filter_option, &value);
    return filter;
}


/**
 * TileDB Filter List
 */
//[[Rcpp::export]]
XPtr<tiledb::FilterList> libtiledb_filter_list(XPtr<tiledb::Context> ctx, List filters) {
  check_xptr_tag<tiledb::Context>(ctx);
  XPtr<tiledb::FilterList> fltrlst = make_xptr<tiledb::FilterList>(new tiledb::FilterList(*ctx.get()));
  // check that external pointers are supported
  R_xlen_t nfilters = filters.length();
  if (nfilters > 0) {
    for (SEXP f : filters) {
      auto filter = as<XPtr<tiledb::Filter>>(f);
      check_xptr_tag<tiledb::Filter>(filter);
      fltrlst->add_filter(*filter.get());
    }
  }
  return fltrlst;
}

//[[Rcpp::export]]
void libtiledb_filter_list_set_max_chunk_size(XPtr<tiledb::FilterList> filterList, uint32_t max_chunk_size) {
  check_xptr_tag<tiledb::FilterList>(filterList);
  filterList->set_max_chunk_size(max_chunk_size);
}

//[[Rcpp::export]]
int libtiledb_filter_list_get_max_chunk_size(XPtr<tiledb::FilterList> filterList) {
  check_xptr_tag<tiledb::FilterList>(filterList);
  return filterList->max_chunk_size();
}

//[[Rcpp::export]]
int libtiledb_filter_list_get_nfilters(XPtr<tiledb::FilterList> filterList) {
  check_xptr_tag<tiledb::FilterList>(filterList);
  return filterList->nfilters();
}

//[[Rcpp::export]]
XPtr<tiledb::Filter> libtiledb_filter_list_get_filter_from_index(XPtr<tiledb::FilterList> filterList, uint32_t filter_index) {
  check_xptr_tag<tiledb::FilterList>(filterList);
  return make_xptr<tiledb::Filter>(new tiledb::Filter(filterList->filter(filter_index)));
}



/**
 * TileDB Attribute
 */
//[[Rcpp::export]]
XPtr<tiledb::Attribute> libtiledb_attribute(XPtr<tiledb::Context> ctx,
                                            std::string name,
                                            std::string type,
                                            XPtr<tiledb::FilterList> fltrlst,
                                            int ncells,
                                            bool nullable) {
    check_xptr_tag<tiledb::Context>(ctx);
    tiledb_datatype_t attr_dtype = _string_to_tiledb_datatype(type);
    if (ncells < 1 && ncells != R_NaInt) {
        Rcpp::stop("ncells must be >= 1 (or NA for variable cells)");
    }

    // placeholder, overwritten in all branches below
    XPtr<tiledb::Attribute> attr = XPtr<tiledb::Attribute>(static_cast<tiledb::Attribute*>(nullptr));

    if (attr_dtype == TILEDB_INT32 ||
        attr_dtype == TILEDB_UINT32 ||
        attr_dtype == TILEDB_FLOAT64 ||
        attr_dtype == TILEDB_FLOAT32 ||
        attr_dtype == TILEDB_INT64  ||
        attr_dtype == TILEDB_UINT64 ||
        attr_dtype == TILEDB_UINT32 ||
        attr_dtype == TILEDB_INT16  ||
        attr_dtype == TILEDB_UINT16 ||
        attr_dtype == TILEDB_INT8   ||
        attr_dtype == TILEDB_UINT8  ||
        attr_dtype == TILEDB_DATETIME_YEAR ||
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
        attr = make_xptr<tiledb::Attribute>(new tiledb::Attribute(*ctx.get(), name, attr_dtype));
        attr->set_cell_val_num(static_cast<uint64_t>(ncells));
    } else if (attr_dtype == TILEDB_CHAR ||
               attr_dtype == TILEDB_STRING_ASCII ||
               attr_dtype == TILEDB_STRING_UTF8) {
        attr = make_xptr<tiledb::Attribute>(new tiledb::Attribute(*ctx.get(), name, attr_dtype));
        uint64_t num = static_cast<uint64_t>(ncells);
        if (ncells == R_NaInt) {
            num = TILEDB_VAR_NUM;           // R's NA is different from TileDB's NA
        }
        attr->set_cell_val_num(num);
#if TILEDB_VERSION >= TileDB_Version(2,10,0)
    } else if (attr_dtype == TILEDB_BOOL) {
        attr = make_xptr<tiledb::Attribute>(new tiledb::Attribute(*ctx.get(), name, attr_dtype));
#endif
    } else {
        Rcpp::stop("Only integer ((U)INT{8,16,32,64}), logical (INT32), real (FLOAT{32,64}), "
                   "Date (DATEIME_DAY), Datetime (DATETIME_{SEC,MS,US}), nanotime (DATETIME_NS), "
#if TILEDB_VERSION >= TileDB_Version(2,10,0)
                   "logical (BOOL), "
#endif
                   "and character (CHAR,ASCII,UTF8) attributes are supported "
                   "-- seeing %s which is not", type.c_str());
    }
    attr->set_filter_list(*fltrlst);
    attr->set_nullable(nullable);
    return attr;
}

// [[Rcpp::export]]
std::string libtiledb_attribute_get_name(XPtr<tiledb::Attribute> attr) {
  check_xptr_tag<tiledb::Attribute>(attr);
  return attr->name();
}

// [[Rcpp::export]]
std::string libtiledb_attribute_get_type(XPtr<tiledb::Attribute> attr) {
  check_xptr_tag<tiledb::Attribute>(attr);
  return _tiledb_datatype_to_string(attr->type());
}

// [[Rcpp::export]]
double libtiledb_attribute_get_cell_size(XPtr<tiledb::Attribute> attr) {
  check_xptr_tag<tiledb::Attribute>(attr);
  uint64_t size = attr->cell_size();
  return static_cast<double>(size);
}

// [[Rcpp::export]]
XPtr<tiledb::FilterList> libtiledb_attribute_get_filter_list(XPtr<tiledb::Attribute> attr) {
  check_xptr_tag<tiledb::Attribute>(attr);
  return make_xptr<tiledb::FilterList>(new tiledb::FilterList(attr->filter_list()));
}

// [[Rcpp::export]]
XPtr<tiledb::Attribute> libtiledb_attribute_set_filter_list(XPtr<tiledb::Attribute> attr, XPtr<tiledb::FilterList> fltrlst) {
  check_xptr_tag<tiledb::Attribute>(attr);
  check_xptr_tag<tiledb::FilterList>(fltrlst);
  attr->set_filter_list(*fltrlst);
  return attr;
}

// [[Rcpp::export]]
int libtiledb_attribute_get_cell_val_num(XPtr<tiledb::Attribute> attr) {
  check_xptr_tag<tiledb::Attribute>(attr);
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
  check_xptr_tag<tiledb::Attribute>(attr);
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
  check_xptr_tag<tiledb::Attribute>(attr);
  return attr->variable_sized();
}

// [[Rcpp::export]]
void libtiledb_attribute_dump(XPtr<tiledb::Attribute> attr) {
  check_xptr_tag<tiledb::Attribute>(attr);
  attr->dump();
}

// [[Rcpp::export]]
void libtiledb_attribute_set_fill_value(XPtr<tiledb::Attribute> attr, SEXP val) {
  tiledb_datatype_t dtype = attr->type();
  check_xptr_tag<tiledb::Attribute>(attr);
  if (dtype == TILEDB_INT32) {
    IntegerVector v(val);
    if (v.size() > 1) Rcpp::stop("Setting fill values only supports scalar values for now.");
    attr->set_fill_value((void*) &(v[0]), static_cast<uint64_t>(sizeof(int32_t)));
  } else if (dtype == TILEDB_UINT32) {
    IntegerVector v(val);
    if (v.size() > 1) Rcpp::stop("Setting fill values only supports scalar values for now.");
    attr->set_fill_value((void*) &(v[0]), static_cast<uint64_t>(sizeof(uint32_t)));
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
}

// [[Rcpp::export]]
SEXP libtiledb_attribute_get_fill_value(XPtr<tiledb::Attribute> attr) {
  check_xptr_tag<tiledb::Attribute>(attr);
  tiledb_datatype_t dtype = attr->type();
  const void* valptr;
  uint64_t size = sizeof(dtype);
  attr->get_fill_value(&valptr, &size);
  if (dtype == TILEDB_INT32) {
    int32_t v = *(const int32_t*)valptr;
    return wrap(v);
  } else if (dtype == TILEDB_UINT32) {
    uint32_t v = *(const uint32_t*)valptr;
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
}

// [[Rcpp::export]]
void libtiledb_attribute_set_nullable(XPtr<tiledb::Attribute> attr, const bool flag) {
    check_xptr_tag<tiledb::Attribute>(attr);
    attr->set_nullable(flag);
}

// [[Rcpp::export]]
bool libtiledb_attribute_get_nullable(XPtr<tiledb::Attribute> attr) {
    check_xptr_tag<tiledb::Attribute>(attr);
    return attr->nullable();
}

// [[Rcpp::export]]
bool libtiledb_attribute_has_enumeration(XPtr<tiledb::Context> ctx,
                                         XPtr<tiledb::Attribute> attr) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Attribute>(attr);
    bool res = false;
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    auto enmr = tiledb::AttributeExperimental::get_enumeration_name(*ctx.get(), *attr.get());
    if (enmr != std::nullopt) {
        res = true;
    }
#endif
    return res;
}

// [[Rcpp::export]]
std::vector<std::string> libtiledb_attribute_get_enumeration(XPtr<tiledb::Context> ctx,
                                                             XPtr<tiledb::Attribute> attr,
                                                             XPtr<tiledb::Array> arr) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Attribute>(attr);
    check_xptr_tag<tiledb::Array>(arr);
    std::vector<std::string> res;
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    auto enmrname = tiledb::AttributeExperimental::get_enumeration_name(*ctx.get(), *attr.get());
    if (enmrname == std::nullopt) {
        Rcpp::stop("No enumeration name for attribute");
    }
    auto enmr = tiledb::ArrayExperimental::get_enumeration(*ctx.get(), *arr.get(), enmrname.value());
    if (enmr.ptr() == nullptr) {
        Rcpp::stop("No enumeration for given attribute.");
    }
    res = enmr.as_vector<std::string>();
#endif
    return res;
}

// [[Rcpp::export]]
XPtr<tiledb::Attribute> libtiledb_attribute_set_enumeration(XPtr<tiledb::Context> ctx,
                                                            XPtr<tiledb::Attribute> attr,
                                                            const std::string &enum_name) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Attribute>(attr);
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    tiledb::AttributeExperimental::set_enumeration_name(*ctx.get(), *attr.get(), enum_name);
#endif
    return attr;
}

// [[Rcpp::export]]
bool libtiledb_attribute_is_ordered_enumeration(XPtr<tiledb::Context> ctx,
                                                XPtr<tiledb::Attribute> attr,
                                                XPtr<tiledb::Array> arr) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Attribute>(attr);
    check_xptr_tag<tiledb::Array>(arr);
    bool res = false;
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    auto enmrname = tiledb::AttributeExperimental::get_enumeration_name(*ctx.get(), *attr.get());
    if (enmrname == std::nullopt) {
        Rcpp::stop("No enumeration name for attribute");
    }
    auto enmr = tiledb::ArrayExperimental::get_enumeration(*ctx.get(), *arr.get(), enmrname.value());
    if (enmr.ptr() != nullptr) {
        res = enmr.ordered();
    }
#endif
    return res;
}


/**
 * TileDB Array Schema
 */

// forward declaration
XPtr<tiledb::ArraySchema> libtiledb_array_schema_set_enumeration(XPtr<tiledb::Context> ctx,
                                                                 XPtr<tiledb::ArraySchema> schema,
                                                                 XPtr<tiledb::Attribute> attr,
                                                                 const std::string enum_name,
                                                                 std::vector<std::string> values,
                                                                 bool nullable,
                                                                 bool ordered);

//[[Rcpp::export]]
XPtr<tiledb::ArraySchema>
libtiledb_array_schema(XPtr<tiledb::Context> ctx,
                       XPtr<tiledb::Domain> domain,
                       List attributes,
                       std::string cell_order,
                       std::string tile_order,
                       Nullable<XPtr<tiledb::FilterList>> coords_filter_list = R_NilValue,
                       Nullable<XPtr<tiledb::FilterList>> offsets_filter_list = R_NilValue,
                       Nullable<XPtr<tiledb::FilterList>> validity_filter_list = R_NilValue,
                       bool sparse = false,
                       Nullable<List> enumerations_list = R_NilValue) {
    // check that external pointers are supported
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Domain>(domain);
    R_xlen_t nattr = attributes.length();
    if (nattr > 0) {
        for (R_xlen_t i=0; i < nattr; i++)  {
            XPtr<tiledb::Attribute> attr = as<XPtr<tiledb::Attribute>>(attributes[i]);
            check_xptr_tag<tiledb::Attribute>(attr);
        }
    }
    auto _cell_order = _string_to_tiledb_layout(cell_order);
    auto _tile_order = _string_to_tiledb_layout(tile_order);
    auto schptr = new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(), sparse ? TILEDB_SPARSE : TILEDB_DENSE));
    auto schema = make_xptr<tiledb::ArraySchema>(schptr);
    schema->set_domain(*domain.get());

    // Deal with enumerations before attributes (that use them) are added
    if (enumerations_list.isNotNull()) {
        List enumerations(enumerations_list); // instantiate from now known non-null Nullable<List> wrapper
        CharacterVector enumnames = enumerations.names();
        R_xlen_t nenum = enumerations.length();
        for (R_xlen_t i=0; i < nenum; i++)  {
            bool nn = enumerations[i] == R_NilValue;
            if (nn == false) {
                XPtr<tiledb::Attribute> attr = as<XPtr<tiledb::Attribute>>(attributes[i]);
                std::vector<std::string> enums = as<std::vector<std::string>>(enumerations[i]);
                std::string enum_name = std::string(enumnames[i]);
                bool is_ordered = false; // default
                // 'ordered' is an attribute off the CharacterVector
                CharacterVector enumvect = enumerations[i];
                if (enumvect.hasAttribute("ordered")) {
                    is_ordered = (as<bool>(enumvect.attr("ordered")) == true);
                }
                libtiledb_array_schema_set_enumeration(ctx, schema, attr, enum_name, enums,
                                                       false, is_ordered);
            }
        }
    }

    // Now add attributes
    if (nattr > 0) {
        for (SEXP a : attributes) {
            auto attr = as<XPtr<tiledb::Attribute>>(a);
            spdl::debug(tfm::format("[libtiledb_array_schema] About to attr %s", libtiledb_attribute_get_name(attr)));
            schema->add_attribute(*attr.get());
        }
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
    if (validity_filter_list.isNotNull()) {
        XPtr<tiledb::FilterList> xptr_validity(validity_filter_list);
        schema->set_validity_filter_list(*xptr_validity);
    }
    schema->check();
    return schema;
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_create(XPtr<tiledb::Context> ctx, std::string atstr) {
  check_xptr_tag<tiledb::Context>(ctx);
  auto at = _string_to_tiledb_array_type(atstr);
  auto ptr = new tiledb::ArraySchema(*ctx.get(), at);
  return make_xptr<tiledb::ArraySchema>(ptr);
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_load(XPtr<tiledb::Context> ctx, std::string uri) {
    check_xptr_tag<tiledb::Context>(ctx);
    auto ptr = new tiledb::ArraySchema(*ctx.get(), uri);
    return make_xptr<tiledb::ArraySchema>(ptr);
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_load_with_key(XPtr<tiledb::Context> ctx,
                                                               std::string uri,
                                                               std::string key) {
    check_xptr_tag<tiledb::Context>(ctx);
    spdl::debug("[libtiledb_array_schema_load_with_key] function is deprecated");
    XPtr<tiledb::Config> cfg = libtiledb_ctx_config(ctx);
    cfg = libtiledb_config_set(cfg, "sm.encryption_key", key);
    cfg = libtiledb_config_set(cfg, "sm.encryption_type", "AES_256_GCM");
    XPtr<tiledb::Context> newctx = libtiledb_ctx(cfg);
    return libtiledb_array_schema_load(newctx, uri);
}

// [[Rcpp::export]]
void libtiledb_array_schema_set_domain(XPtr<tiledb::ArraySchema> schema,
                                       XPtr<tiledb::Domain> dom) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  schema->set_domain(*dom);
}

// [[Rcpp::export]]
XPtr<tiledb::Domain> libtiledb_array_schema_get_domain(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  auto ptr = make_xptr<tiledb::Domain>(new tiledb::Domain(schema->domain()));
  return ptr;
}

// [[Rcpp::export]]
void libtiledb_array_schema_add_attribute(XPtr<tiledb::ArraySchema> schema,
                                          XPtr<tiledb::Attribute> attr) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  check_xptr_tag<tiledb::Attribute>(attr);
  schema->add_attribute(*attr.get());
}

// [[Rcpp::export]]
List libtiledb_array_schema_attributes(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  List result;
  int nattr = schema->attribute_num();
  for (auto i=0; i < nattr; i++) {
    auto attr = make_xptr<tiledb::Attribute>(new tiledb::Attribute(schema->attribute(i)));
    result[attr->name()] = attr;
  }
  return result;
}

// [[Rcpp::export]]
std::string libtiledb_array_schema_get_array_type(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  auto type = schema->array_type();
  return _tiledb_array_type_to_string(type);
}

// [[Rcpp::export]]
void libtiledb_array_schema_set_cell_order(XPtr<tiledb::ArraySchema> schema, std::string ord) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  tiledb_layout_t cellorder = _string_to_tiledb_layout(ord);
  schema->set_cell_order(cellorder);
}

// [[Rcpp::export]]
std::string libtiledb_array_schema_get_cell_order(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  auto order = schema->cell_order();
  return _tiledb_layout_to_string(order);
}

// [[Rcpp::export]]
void libtiledb_array_schema_set_tile_order(XPtr<tiledb::ArraySchema> schema, std::string ord) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  tiledb_layout_t tileorder = _string_to_tiledb_layout(ord);
  schema->set_cell_order(tileorder);
}

// [[Rcpp::export]]
std::string libtiledb_array_schema_get_tile_order(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  auto order = schema->tile_order();
  return _tiledb_layout_to_string(order);
}

// [[Rcpp::export]]
void libtiledb_array_schema_set_capacity(XPtr<tiledb::ArraySchema> schema, int cap) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  if (cap <= 0) {
    Rcpp::stop("Tile capacity of '%d' not sensible", cap);
  }
  uint64_t tilecap = static_cast<uint64_t>(cap);
  schema->set_capacity(tilecap);
}

// [[Rcpp::export]]
int libtiledb_array_schema_get_capacity(XPtr<tiledb::ArraySchema> schema) {
  // FIXME: we try to return a uint64_t as an int. Overflow possible
  check_xptr_tag<tiledb::ArraySchema>(schema);
  uint64_t cap = schema->capacity();
  if (cap > std::numeric_limits<int32_t>::max()) {
    Rcpp::stop("Overflow on schema capcity at '%ld'", cap);
  }
  return static_cast<int>(cap);
}

// [[Rcpp::export]]
bool libtiledb_array_schema_get_allows_dups(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  return schema->allows_dups();
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_set_allows_dups(XPtr<tiledb::ArraySchema> schema,
                                                                 bool allows_dups) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  schema->set_allows_dups(allows_dups);
  return schema;
}

// [[Rcpp::export]]
XPtr<tiledb::FilterList>
libtiledb_array_schema_get_coords_filter_list(XPtr<tiledb::ArraySchema> schema) {
  return make_xptr<tiledb::FilterList>(new tiledb::FilterList(schema->coords_filter_list()));
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema>
libtiledb_array_schema_set_coords_filter_list(XPtr<tiledb::ArraySchema> schema,
                                              XPtr<tiledb::FilterList> fltrlst) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  check_xptr_tag<tiledb::FilterList>(fltrlst);
  schema->set_coords_filter_list(*fltrlst);
  return schema;
}

// [[Rcpp::export]]
XPtr<tiledb::FilterList>
libtiledb_array_schema_get_offsets_filter_list(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  return make_xptr<tiledb::FilterList>(new tiledb::FilterList(schema->offsets_filter_list()));
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema>
libtiledb_array_schema_set_offsets_filter_list(XPtr<tiledb::ArraySchema> schema,
                                               XPtr<tiledb::FilterList> fltrlst) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  check_xptr_tag<tiledb::FilterList>(fltrlst);
  schema->set_offsets_filter_list(*fltrlst);
  return schema;
}

// [[Rcpp::export]]
XPtr<tiledb::FilterList>
libtiledb_array_schema_get_validity_filter_list(XPtr<tiledb::ArraySchema> schema) {
    check_xptr_tag<tiledb::ArraySchema>(schema);
    return make_xptr<tiledb::FilterList>(new tiledb::FilterList(schema->validity_filter_list()));
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema>
libtiledb_array_schema_set_validity_filter_list(XPtr<tiledb::ArraySchema> schema,
                                                XPtr<tiledb::FilterList> fltrlst) {
    check_xptr_tag<tiledb::ArraySchema>(schema);
    check_xptr_tag<tiledb::FilterList>(fltrlst);
    schema->set_validity_filter_list(*fltrlst);
    return schema;
}


// [[Rcpp::export]]
int libtiledb_array_schema_get_attribute_num(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  uint32_t attr_num = schema->attribute_num();
  if (attr_num >= std::numeric_limits<int32_t>::max()) {
    Rcpp::stop("Overflow retrieving attribute number.");
  }
  return static_cast<int32_t>(attr_num);
}

// [[Rcpp::export]]
XPtr<tiledb::Attribute> libtiledb_array_schema_get_attribute_from_index(XPtr<tiledb::ArraySchema> schema,
                                                                        int ind) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  if (ind < 0) {
    Rcpp::stop("Index must be non-negative.");
  }
  uint32_t idx = static_cast<uint32_t>(ind);
  return make_xptr<tiledb::Attribute>(new tiledb::Attribute(schema->attribute(idx)));
}

// [[Rcpp::export]]
XPtr<tiledb::Attribute> libtiledb_array_schema_get_attribute_from_name(XPtr<tiledb::ArraySchema> schema,
                                                                       std::string name) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  return make_xptr<tiledb::Attribute>(new tiledb::Attribute(schema->attribute(name)));
}

// [[Rcpp::export]]
bool libtiledb_array_schema_has_attribute(XPtr<tiledb::ArraySchema> schema, std::string name) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  return schema->has_attribute(name);
}

// [[Rcpp::export]]
bool libtiledb_array_schema_sparse(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  return (schema->array_type() == TILEDB_SPARSE);
}

// [[Rcpp::export]]
void libtiledb_array_schema_dump(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  schema->dump();
}

// [[Rcpp::export]]
bool libtiledb_array_schema_check(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  schema->check();   // throws, rather than returning bool
  return true;
}

// [[Rcpp::export]]
int libtiledb_array_schema_version(XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  return static_cast<int32_t>(schema->version());
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_set_enumeration(XPtr<tiledb::Context> ctx,
                                                                 XPtr<tiledb::ArraySchema> schema,
                                                                 XPtr<tiledb::Attribute> attr,
                                                                 const std::string enum_name,
                                                                 std::vector<std::string> values,
                                                                 bool nullable = false,
                                                                 bool ordered = false) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::ArraySchema>(schema);
    check_xptr_tag<tiledb::Attribute>(attr);
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    auto enumeration = tiledb::Enumeration::create(*ctx.get(), enum_name, values, ordered);
    tiledb::ArraySchemaExperimental::add_enumeration(*ctx.get(), *schema.get(), enumeration);
    tiledb::AttributeExperimental::set_enumeration_name(*ctx.get(), *attr.get(), enum_name);
#endif
    return schema;
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema>
libtiledb_array_schema_set_enumeration_empty(XPtr<tiledb::Context> ctx,
                                             XPtr<tiledb::ArraySchema> schema,
                                             XPtr<tiledb::Attribute> attr,
                                             const std::string enum_name,
                                             const std::string type_str,
                                             int cell_val_num,
                                             bool ordered) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::ArraySchema>(schema);
    check_xptr_tag<tiledb::Attribute>(attr);
#if TILEDB_VERSION >= TileDB_Version(2,17,3)
    tiledb_datatype_t type = _string_to_tiledb_datatype(type_str);
    uint32_t num = static_cast<uint64_t>(cell_val_num);
    if (cell_val_num == R_NaInt) {
        num = TILEDB_VAR_NUM;           // R's NA is different from TileDB's NA
    }
    auto enumeration = tiledb::Enumeration::create_empty(*ctx.get(), enum_name, type, num, ordered);
    tiledb::ArraySchemaExperimental::add_enumeration(*ctx.get(), *schema.get(), enumeration);
    tiledb::AttributeExperimental::set_enumeration_name(*ctx.get(), *attr.get(), enum_name);
#endif
    return schema;
}


/**
 * TileDB Array Schema Evolution
 */
//[[Rcpp::export]]
XPtr<tiledb::ArraySchemaEvolution>
libtiledb_array_schema_evolution(XPtr<tiledb::Context> ctx) {
    check_xptr_tag<tiledb::Context>(ctx);
    auto p = new tiledb::ArraySchemaEvolution(*ctx.get());
    auto ptr = make_xptr<tiledb::ArraySchemaEvolution>(p);
    return ptr;
}

//[[Rcpp::export]]
XPtr<tiledb::ArraySchemaEvolution>
libtiledb_array_schema_evolution_add_attribute(XPtr<tiledb::ArraySchemaEvolution> ase,
                                               XPtr<tiledb::Attribute> attr) {
    check_xptr_tag<tiledb::ArraySchemaEvolution>(ase);
    check_xptr_tag<tiledb::Attribute>(attr);
    tiledb::ArraySchemaEvolution res = ase->add_attribute(*attr.get());
    auto ptr = new tiledb::ArraySchemaEvolution(res);
    return make_xptr<tiledb::ArraySchemaEvolution>(ptr);
}

//[[Rcpp::export]]
XPtr<tiledb::ArraySchemaEvolution>
libtiledb_array_schema_evolution_drop_attribute(XPtr<tiledb::ArraySchemaEvolution> ase,
                                                const std::string & attrname) {
    check_xptr_tag<tiledb::ArraySchemaEvolution>(ase);
    tiledb::ArraySchemaEvolution res = ase->drop_attribute(attrname);
    auto ptr = new tiledb::ArraySchemaEvolution(res);
    return make_xptr<tiledb::ArraySchemaEvolution>(ptr);
}

//[[Rcpp::export]]
XPtr<tiledb::ArraySchemaEvolution>
libtiledb_array_schema_evolution_array_evolve(XPtr<tiledb::ArraySchemaEvolution> ase,
                                              const std::string & uri) {
    check_xptr_tag<tiledb::ArraySchemaEvolution>(ase);
    tiledb::ArraySchemaEvolution res = ase->array_evolve(uri);
    auto ptr = new tiledb::ArraySchemaEvolution(res);
    return make_xptr<tiledb::ArraySchemaEvolution>(ptr);
}

//[[Rcpp::export]]
XPtr<tiledb::ArraySchemaEvolution>
libtiledb_array_schema_evolution_add_enumeration(XPtr<tiledb::Context> ctx,
                                                 XPtr<tiledb::ArraySchemaEvolution> ase,
                                                 const std::string & enum_name,
                                                 std::vector<std::string> values,
                                                 bool nullable = false,
                                                 bool ordered = false) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::ArraySchemaEvolution>(ase);
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    auto enumeration = tiledb::Enumeration::create(*ctx.get(), enum_name, values, ordered);
    tiledb::ArraySchemaEvolution res = ase->add_enumeration(enumeration);
    auto ptr = new tiledb::ArraySchemaEvolution(res);
    return make_xptr<tiledb::ArraySchemaEvolution>(ptr);
#endif
    return ase;
}

//[[Rcpp::export]]
XPtr<tiledb::ArraySchemaEvolution>
libtiledb_array_schema_evolution_add_enumeration_empty(XPtr<tiledb::Context> ctx,
                                                       XPtr<tiledb::ArraySchemaEvolution> ase,
                                                       const std::string & enum_name,
                                                       const std::string type_str,
                                                       int cell_val_num,
                                                       bool ordered = false) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::ArraySchemaEvolution>(ase);
#if TILEDB_VERSION >= TileDB_Version(2,17,3)
    tiledb_datatype_t type = _string_to_tiledb_datatype(type_str);
    uint32_t num = static_cast<uint32_t>(cell_val_num);
    auto enumeration = tiledb::Enumeration::create_empty(*ctx.get(), enum_name, type, num, ordered);
    tiledb::ArraySchemaEvolution res = ase->add_enumeration(enumeration);
    auto ptr = new tiledb::ArraySchemaEvolution(res);
    return make_xptr<tiledb::ArraySchemaEvolution>(ptr);
#endif
    return ase;
}


//[[Rcpp::export]]
XPtr<tiledb::ArraySchemaEvolution>
libtiledb_array_schema_evolution_drop_enumeration(XPtr<tiledb::ArraySchemaEvolution> ase,
                                                  const std::string & attrname) {
    check_xptr_tag<tiledb::ArraySchemaEvolution>(ase);
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    tiledb::ArraySchemaEvolution res = ase->drop_attribute(attrname);
    auto ptr = new tiledb::ArraySchemaEvolution(res);
    return make_xptr<tiledb::ArraySchemaEvolution>(ptr);
#endif
    return ase;
}

//[[Rcpp::export]]
XPtr<tiledb::ArraySchemaEvolution>
libtiledb_array_schema_evolution_extend_enumeration(XPtr<tiledb::Context> ctx,
                                                    XPtr<tiledb::ArraySchemaEvolution> ase,
                                                    XPtr<tiledb::Array> array,
                                                    const std::string & enum_name,
                                                    std::vector<std::string> new_values,
                                                    bool nullable = false,
                                                    bool ordered = false) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::ArraySchemaEvolution>(ase);
    check_xptr_tag<tiledb::Array>(array);
#if TILEDB_VERSION >= TileDB_Version(2,17,3)
    auto old_enumeration = tiledb::ArrayExperimental::get_enumeration(*ctx.get(), *array.get(), enum_name);
    auto new_enumeration = old_enumeration.extend(new_values);
    tiledb::ArraySchemaEvolution res = ase->extend_enumeration(new_enumeration);
    auto ptr = new tiledb::ArraySchemaEvolution(res);
    return make_xptr<tiledb::ArraySchemaEvolution>(ptr);
#endif
    return ase;
}


/**
 * TileDB Array
 */
// [[Rcpp::export]]
std::string libtiledb_array_create(std::string uri, XPtr<tiledb::ArraySchema> schema) {
  check_xptr_tag<tiledb::ArraySchema>(schema);
  tiledb::Array::create(uri, *schema.get());
  return uri;
}

// [[Rcpp::export]]
std::string libtiledb_array_create_with_key(std::string uri, XPtr<tiledb::ArraySchema> schema,
                                            std::string encryption_key) {
    check_xptr_tag<tiledb::ArraySchema>(schema);
    tiledb::Array::create(uri, *schema.get(),
                          _string_to_tiledb_encryption_type_t("AES_256_GCM"),
                          encryption_key);
    return uri;
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open(XPtr<tiledb::Context> ctx, std::string uri,
                                         std::string type) {
  check_xptr_tag<tiledb::Context>(ctx);
  auto query_type = _string_to_tiledb_query_type(type);
  return make_xptr<tiledb::Array>(new tiledb::Array(*ctx.get(), uri, query_type));
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open_at(XPtr<tiledb::Context> ctx, std::string uri,
                                            std::string type, Datetime tstamp) {
    check_xptr_tag<tiledb::Context>(ctx);
    auto query_type = _string_to_tiledb_query_type(type);
    // get timestamp as seconds since epoch (plus fractional seconds, returns double), scale to millisec
    uint64_t ts_ms = static_cast<uint64_t>(std::round(tstamp.getFractionalTimestamp() * 1000));
#if TILEDB_VERSION >= TileDB_Version(2,15,0)
    auto ptr = new tiledb::Array(*ctx.get(), uri, query_type, tiledb::TemporalPolicy(tiledb::TimeTravel, ts_ms));
    ptr->set_open_timestamp_end(ts_ms);
#else
    auto ptr = new tiledb::Array(*ctx.get(), uri, query_type);
    ptr->set_open_timestamp_end(ts_ms);
#endif
    return make_xptr<tiledb::Array>(ptr);
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open_with_key(XPtr<tiledb::Context> ctx, std::string uri,
                                                  std::string type,
                                                  std::string enc_key) {
    check_xptr_tag<tiledb::Context>(ctx);
    spdl::debug("[libtiledb_array_open_with_key] function is deprecated");
    XPtr<tiledb::Config> cfg = libtiledb_ctx_config(ctx);
    cfg = libtiledb_config_set(cfg, "sm.encryption_key", enc_key);
    cfg = libtiledb_config_set(cfg, "sm.encryption_type", "AES_256_GCM");
    XPtr<tiledb::Context> newctx = libtiledb_ctx(cfg);
    return libtiledb_array_open(newctx, uri, type);
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open_at_with_key(XPtr<tiledb::Context> ctx, std::string uri,
                                                     std::string type, std::string enc_key,
                                                     Datetime tstamp) {
    check_xptr_tag<tiledb::Context>(ctx);
    spdl::debug("[libtiledb_array_open_at_with_key] function is deprecated");
    auto query_type = _string_to_tiledb_query_type(type);
    uint64_t ts_ms = static_cast<uint64_t>(std::round(tstamp.getFractionalTimestamp() * 1000));
    XPtr<tiledb::Array> ptr = libtiledb_array_open_with_key(ctx, uri, type, enc_key);
    ptr->close();               // close to reopen at timestamp, this should be revisited
    ptr->open(query_type, TILEDB_AES_256_GCM, enc_key, ts_ms);
    return ptr;
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open_with_ptr(XPtr<tiledb::Array> array, std::string query_type) {
  check_xptr_tag<tiledb::Array>(array);
  tiledb_query_type_t qtype = _string_to_tiledb_query_type(query_type);
  array->open(qtype);
  return array;
}

// [[Rcpp::export]]
bool libtiledb_array_is_open(XPtr<tiledb::Array> array) {
  check_xptr_tag<tiledb::Array>(array);
  return array->is_open();
}

// [[Rcpp::export]]
bool libtiledb_array_is_open_for_reading(XPtr<tiledb::Array> array) {
  check_xptr_tag<tiledb::Array>(array);
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
  check_xptr_tag<tiledb::Array>(array);
  return make_xptr<tiledb::ArraySchema>(new tiledb::ArraySchema(array->schema()));
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_reopen(XPtr<tiledb::Array> array) {
  check_xptr_tag<tiledb::Array>(array);
  array->reopen();
  return array;
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_close(XPtr<tiledb::Array> array) {
  check_xptr_tag<tiledb::Array>(array);
  array->close();
  return array;
}

// [[Rcpp::export]]
std::string libtiledb_array_query_type(XPtr<tiledb::Array> array) {
  check_xptr_tag<tiledb::Array>(array);
  tiledb_query_type_t qtype = array->query_type();
  return _tiledb_query_type_to_string(qtype);
}

// [[Rcpp::export]]
List libtiledb_array_get_non_empty_domain(XPtr<tiledb::Array> array) {
  check_xptr_tag<tiledb::Array>(array);
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
  check_xptr_tag<tiledb::Array>(array);
  auto res = array->non_empty_domain_var(name);
  return CharacterVector::create(res.first, res.second);
}

// [[Rcpp::export]]
CharacterVector libtiledb_array_get_non_empty_domain_var_from_index(XPtr<tiledb::Array> array,
                                                                    int32_t idx,
                                                                    std::string typestr = "ASCII") {
  check_xptr_tag<tiledb::Array>(array);
  if (typestr == "ASCII") {
    auto res = array->non_empty_domain_var(idx);
    return CharacterVector::create(res.first, res.second);
  } else {
    Rcpp::stop("Invalid tiledb_schema domain type: '%s'", typestr.c_str());
  }
  // not reached
  return CharacterVector::create("", "");
}

// [[Rcpp::export]]
NumericVector libtiledb_array_get_non_empty_domain_from_name(XPtr<tiledb::Array> array,
                                                             std::string name,
                                                             std::string typestr) {
  check_xptr_tag<tiledb::Array>(array);
  if (typestr == "INT64") {
    auto p = array->non_empty_domain<int64_t>(name);
    std::vector<int64_t> v{p.first, p.second};
    return toInteger64(v);
  } else if (typestr == "UINT64") {
    auto p = array->non_empty_domain<uint64_t>(name);
    std::vector<int64_t> v{ static_cast<int64_t>(p.first), static_cast<int64_t>(p.second) };
    return toInteger64(v);
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
    return toInteger64(v);
  } else if (typestr == "DATETIME_NS") {
    auto p = array->non_empty_domain<int64_t>(name);
    std::vector<int64_t> v{p.first, p.second};
    return toNanotime(v);
  } else {
    Rcpp::stop("Currently unsupported tiledb domain type: '%s'", typestr.c_str());
    return NumericVector::create(NA_REAL, NA_REAL); // not reached
  }
}


// [[Rcpp::export]]
NumericVector libtiledb_array_get_non_empty_domain_from_index(XPtr<tiledb::Array> array,
                                                              int32_t idx,
                                                              std::string typestr) {
  check_xptr_tag<tiledb::Array>(array);
  if (typestr == "INT64") {
    auto p = array->non_empty_domain<int64_t>(idx);
    std::vector<int64_t> v{p.first, p.second};
    return toInteger64(v);
  } else if (typestr == "UINT64") {
    auto p = array->non_empty_domain<uint64_t>(idx);
    std::vector<int64_t> v{ static_cast<int64_t>(p.first), static_cast<int64_t>(p.second) };
    return toInteger64(v);
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
    return toInteger64(v);
  } else if (typestr == "DATETIME_NS") {
    auto p = array->non_empty_domain<int64_t>(idx);
    std::vector<int64_t> v{p.first, p.second};
    return toNanotime(v);
  } else {
    Rcpp::stop("Currently unsupported tiledb domain type: '%s'", typestr.c_str());
    return NumericVector::create(NA_REAL, NA_REAL); // not reached
  }
}


// [[Rcpp::export]]
void libtiledb_array_consolidate(XPtr<tiledb::Context> ctx,
                                 std::string uri,
                                 Nullable<XPtr<tiledb::Config>> cfgptr = R_NilValue) {
  check_xptr_tag<tiledb::Context>(ctx);
  if (cfgptr.isNotNull()) {
    XPtr<tiledb::Config> cfg(cfgptr);
    check_xptr_tag<tiledb::Config>(cfg);
    tiledb::Array::consolidate(*ctx.get(), uri, cfg);
  } else {
    tiledb::Array::consolidate(*ctx.get(), uri);
  }
}

// [[Rcpp::export]]
void libtiledb_array_vacuum(XPtr<tiledb::Context> ctx,
                            std::string uri,
                            Nullable<XPtr<tiledb::Config>> cfgptr = R_NilValue) {
  check_xptr_tag<tiledb::Context>(ctx);
  if (cfgptr.isNotNull()) {
    XPtr<tiledb::Config> cfg(cfgptr);
    check_xptr_tag<tiledb::Config>(cfg);
    tiledb::Array::vacuum(*ctx.get(), uri, cfg);
  } else {
    tiledb::Array::vacuum(*ctx.get(), uri);
  }
}


// [[Rcpp::export]]
bool libtiledb_array_put_metadata(XPtr<tiledb::Array> array,
                                  std::string key, SEXP obj) {
  check_xptr_tag<tiledb::Array>(array);
  // we implement a simpler interface here as the 'type' is given from
  // the supplied SEXP, as is the extent
  switch(TYPEOF(obj)) {
    case VECSXP: {
      Rcpp::stop("List objects are not supported.");
      break;// not reached
    }
    case REALSXP: {
      Rcpp::NumericVector v(obj);
      if (isInteger64(v)) {
          std::vector<int64_t> iv = fromInteger64(v);
          array->put_metadata(key.c_str(), TILEDB_INT64, iv.size(), (void*) &iv[0]);
      } else {
          array->put_metadata(key.c_str(), TILEDB_FLOAT64, v.size(), v.begin());
      }
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
      array->put_metadata(key.c_str(), TILEDB_STRING_ASCII, s.length(), s.c_str());
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
  check_xptr_tag<tiledb::Array>(array);
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
  } else if (v_type == TILEDB_CHAR || v_type == TILEDB_STRING_ASCII || v_type == TILEDB_STRING_UTF8) {
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
    std::vector<int64_t> iv(v_num);
    std::memcpy(&(iv[0]), v, v_num*sizeof(int64_t));
    return toInteger64(iv);
  } else if (v_type == TILEDB_UINT64) {
    return copy_int_vector<uint64_t>(v_num, v);
  } else {
    Rcpp::stop("No support yet for %s", _tiledb_datatype_to_string(v_type));
  }
}

// [[Rcpp::export]]
SEXP libtiledb_array_get_metadata_from_index(XPtr<tiledb::Array> array, int idx) {
  check_xptr_tag<tiledb::Array>(array);
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
  check_xptr_tag<tiledb::Array>(array);
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
  check_xptr_tag<tiledb::Array>(array);
  array->delete_metadata(key.c_str());
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_set_open_timestamp_start(XPtr<tiledb::Array> array, Rcpp::Datetime tstamp) {
    check_xptr_tag<tiledb::Array>(array);
    uint64_t ts_ms = static_cast<uint64_t>(std::round(tstamp.getFractionalTimestamp() * 1000));
    array->set_open_timestamp_start(ts_ms);
    return array;
}

// [[Rcpp::export]]
Rcpp::Datetime libtiledb_array_open_timestamp_start(XPtr<tiledb::Array> array) {
    check_xptr_tag<tiledb::Array>(array);
    uint64_t ts_ms = array->open_timestamp_start();
    double ts = ts_ms / 1000.0;
    return(Rcpp::Datetime(ts));
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_set_open_timestamp_end(XPtr<tiledb::Array> array, Rcpp::Datetime tstamp) {
    check_xptr_tag<tiledb::Array>(array);
    uint64_t ts_ms = static_cast<uint64_t>(std::round(tstamp.getFractionalTimestamp() * 1000));
    array->set_open_timestamp_end(ts_ms);
    return array;
}

// [[Rcpp::export]]
Rcpp::Datetime libtiledb_array_open_timestamp_end(XPtr<tiledb::Array> array) {
    check_xptr_tag<tiledb::Array>(array);
    uint64_t ts_ms = array->open_timestamp_end();
    double ts = ts_ms / 1000.0;
    return(Rcpp::Datetime(ts));
}

// [[Rcpp::export]]
void libtiledb_array_delete_fragments(XPtr<tiledb::Context> ctx, XPtr<tiledb::Array> array,
                                      Rcpp::Datetime tstamp_start, Rcpp::Datetime tstamp_end) {
#if TILEDB_VERSION >= TileDB_Version(2,12,0)
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Array>(array);
    const std::string uri = array->uri();
    uint64_t ts_ms_st = static_cast<uint64_t>(std::round(tstamp_start.getFractionalTimestamp() * 1000));
    uint64_t ts_ms_en = static_cast<uint64_t>(std::round(tstamp_end.getFractionalTimestamp() * 1000));
#if TILEDB_VERSION >= TileDB_Version(2,18,0)
    tiledb::Array::delete_fragments(*ctx.get(), uri, ts_ms_st, ts_ms_en);
#else
    array->delete_fragments(uri, ts_ms_st, ts_ms_en);
#endif
#endif
}

// [[Rcpp::export]]
bool libtiledb_array_has_enumeration(XPtr<tiledb::Context> ctx,
                                     XPtr<tiledb::Array> arr,
                                     const std::string name) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Array>(arr);
    bool res = false;
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    auto enmr = tiledb::ArrayExperimental::get_enumeration(*ctx.get(), *arr.get(), name);
    if (enmr.ptr() != nullptr) {
        res = true;
    }
#endif
    return res;
}

// [[Rcpp::export]]
std::vector<std::string> libtiledb_array_get_enumeration(XPtr<tiledb::Context> ctx,
                                                         XPtr<tiledb::Array> arr,
                                                         const std::string name) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Array>(arr);
    std::vector<std::string> res;
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    auto enmr = tiledb::ArrayExperimental::get_enumeration(*ctx.get(), *arr.get(), name);
    if (enmr.ptr() == nullptr) {
        Rcpp::stop("No enumeration named '%s' in array.");
    }
    res = enmr.as_vector<std::string>();
#endif
    return res;
}

// quick and dirty checker for an entire array
// [[Rcpp::export]]
Rcpp::LogicalVector libtiledb_array_has_enumeration_vector(XPtr<tiledb::Context> ctx, XPtr<tiledb::Array> array) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Array>(array);
    Rcpp::XPtr<tiledb::ArraySchema> arrsch = libtiledb_array_get_schema(array);
    Rcpp::List attrs = libtiledb_array_schema_attributes(arrsch);
    size_t n = attrs.size();
    Rcpp::LogicalVector has_enum = Rcpp::LogicalVector(n);
    Rcpp::CharacterVector attr_names(n);
    for (size_t i = 0; i < n; i++) {
        has_enum[i] = libtiledb_attribute_has_enumeration(ctx, attrs[i]);
        attr_names[i] = libtiledb_attribute_get_name(attrs[i]);
    }
    has_enum.attr("names") = attr_names;
    return has_enum;
}


/**
 * Query
 */
// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query(XPtr<tiledb::Context> ctx,
                                    XPtr<tiledb::Array> array,
                                    std::string type) {
  check_xptr_tag<tiledb::Context>(ctx);
  check_xptr_tag<tiledb::Array>(array);
  auto query_type = _string_to_tiledb_query_type(type);
  auto ptr = new tiledb::Query(*ctx.get(), *array.get(), query_type);
  return make_xptr<tiledb::Query>(ptr);
}

// [[Rcpp::export]]
std::string libtiledb_query_type(XPtr<tiledb::Query> query) {
  check_xptr_tag<tiledb::Query>(query);
  return _tiledb_query_type_to_string(query->query_type());
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_layout(XPtr<tiledb::Query> query,
                                               std::string layout) {
  check_xptr_tag<tiledb::Query>(query);
  auto _layout = _string_to_tiledb_layout(layout);
  query->set_layout(_layout);
  return query;
}

// [[Rcpp::export]]
std::string libtiledb_query_layout(XPtr<tiledb::Query> query) {
  check_xptr_tag<tiledb::Query>(query);
  auto layout = query->query_layout();
  return _tiledb_layout_to_string(layout);
}

// NB Function XPtr<tiledb::Array> libtiledb_query_get_array(XPtr<tiledb::Query> query,
//                                                           XPtr<tiledb::Context> ctx) {
// is below along with schema getter


// generalized version which switches
// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_subarray_with_type(XPtr<tiledb::Query> query,
                                                           SEXP subarray, std::string typestr) {
    check_xptr_tag<tiledb::Query>(query);
    spdl::debug(tfm::format("libtiledb_query_set_subarray_with_type] setting subarray for type %s", typestr));
    tiledb::Subarray subarr(query->ctx(), query->array());
    if (typestr == "INT32") {
        IntegerVector vec(subarray);
        subarr.set_subarray(vec.begin(), vec.length());
    } else if (typestr == "FLOAT64") {
        NumericVector sub(subarray);
        subarr.set_subarray(sub.begin(), sub.length());
    } else if (typestr == "INT64" ||
               typestr == "UINT32" ||
               typestr == "DATETIME_NS") {
        NumericVector sub(subarray);
        std::vector<int64_t> v(sub.length());
        for (int i=0; i<sub.length(); i++)
            v[i] = static_cast<int64_t>(sub[i]);
        subarr.set_subarray(v);
    } else if (typestr == "DATETIME_YEAR"  ||
               typestr == "DATETIME_MONTH" ||
               typestr == "DATETIME_WEEK"  ||
               typestr == "DATETIME_DAY") {
        DateVector sub(subarray);
        std::vector<int64_t> v = dates_to_int64(sub, _string_to_tiledb_datatype(typestr));
        subarr.set_subarray(v);
    } else if (typestr == "DATETIME_HR"  ||
               typestr == "DATETIME_MIN" ||
               typestr == "DATETIME_SEC" ||
               typestr == "DATETIME_MS"  ||
               typestr == "DATETIME_US") {
        DatetimeVector sub(subarray);
        std::vector<int64_t> v = datetimes_to_int64(sub, _string_to_tiledb_datatype(typestr));
        subarr.set_subarray(v);
    } else if (typestr == "UINT64") {
        NumericVector sub(subarray);
        std::vector<uint64_t> v(sub.length());
        for (int i=0; i<sub.length(); i++)
            v[i] = static_cast<uint64_t>(sub[i]);
        subarr.set_subarray(v);
    } else {
        Rcpp::stop("currently unsupported subarray datatype '%s'", typestr.c_str());
    }
    query->set_subarray(subarr);
    return query;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_subarray(XPtr<tiledb::Query> query,
                                                 SEXP subarray) {
    check_xptr_tag<tiledb::Query>(query);
    spdl::debug(tfm::format("libtiledb_query_set_subarray] setting subarray for type %s", Rf_type2char(TYPEOF(subarray))));
    tiledb::Subarray subarr(query->ctx(), query->array());
    if (TYPEOF(subarray) == INTSXP) {
        IntegerVector vec(subarray);
        subarr.set_subarray(vec.begin(), vec.length());
    } else if (TYPEOF(subarray) == REALSXP) {
        NumericVector vec(subarray);
        subarr.set_subarray(vec.begin(), vec.length());
    } else {
        Rcpp::stop("currently unsupported subarray datatype");
    }
    query->set_subarray(subarr);
    return query;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_buffer(XPtr<tiledb::Query> query,
                                               std::string attr,
                                               SEXP buffer) {
  check_xptr_tag<tiledb::Query>(query);
  if (TYPEOF(buffer) == INTSXP) {
    IntegerVector vec(buffer);
    query->set_data_buffer(attr, vec.begin(), vec.length());
    return query;
  } else if (TYPEOF(buffer) == REALSXP) {
    NumericVector vec(buffer);
    query->set_data_buffer(attr, vec.begin(), vec.length());
    return query;
  } else if (TYPEOF(buffer) == LGLSXP) {
    LogicalVector vec(buffer);  // note that it is really an int at the element storage
    query->set_data_buffer(attr, vec.begin(), vec.length());
    return query;
  } else {
    Rcpp::stop("Invalid attribute buffer type for attribute '%s': %s",
               attr.c_str(), Rcpp::type2name(buffer));
  }
}

// -- vlc_buf_t functions below

// [[Rcpp::export]]
XPtr<vlc_buf_t> libtiledb_query_buffer_var_char_alloc_direct(double szoffsets, double szdata,
                                                             bool nullable, int cols=1) {
    XPtr<vlc_buf_t> buf = make_xptr<vlc_buf_t>(new vlc_buf_t);
    buf->offsets.resize(static_cast<size_t>(szoffsets));
    buf->str.resize(static_cast<size_t>(szdata));
    buf->rows = std::round(szoffsets/cols);           // guess for number of elements
    buf->cols = cols;
    buf->nullable = nullable;
    buf->validity_map.resize(static_cast<size_t>(szdata));
    buf->legacy_validity = false; 		              // for legacy validity mode
    return buf;
}

// [[Rcpp::export]]
bool libtiledb_query_buffer_var_char_get_legacy_validity_value(XPtr<tiledb::Context> ctx,
                                                               bool validity_override = false) {
    check_xptr_tag<tiledb::Context>(ctx);
    XPtr<tiledb::Config> cfg = libtiledb_ctx_config(ctx);
    Rcpp::CharacterVector vec = libtiledb_config_get(cfg, "r.legacy_validity_mode");
    bool legacy_validity = std::string("true") == std::string(vec[0]) || validity_override;
    return legacy_validity;
}

// [[Rcpp::export]]
XPtr<vlc_buf_t> libtiledb_query_buffer_var_char_legacy_validity_mode(XPtr<tiledb::Context> ctx,
                                                                     XPtr<vlc_buf_t> buf,
                                                                     bool validity_override = false) {
    buf->legacy_validity = libtiledb_query_buffer_var_char_get_legacy_validity_value(ctx,
                                                                                     validity_override);
    spdl::debug(tfm::format("[libtiledb_query_buffer_var_char_legacy_validity_mode] "
                            "legacy_validity set to %s", buf->legacy_validity ? "true" : "false"));
    return buf;
}

// assigning (for a write) allocates
// [[Rcpp::export]]
XPtr<vlc_buf_t> libtiledb_query_buffer_var_char_create(CharacterVector vec, bool nullable,
                                                       bool legacy_validity = false) {
    size_t n = vec.size();
    XPtr<vlc_buf_t> bufptr = make_xptr<vlc_buf_t>(new vlc_buf_t);
    bufptr->offsets.resize(n);
    bufptr->validity_map.resize(n);
    bufptr->nullable = nullable;
    bufptr->legacy_validity = legacy_validity;
    bufptr->str = "";
    uint64_t cumlen = 0;
    for (size_t i=0; i<n; i++) {
        std::string s(vec[i]);
        bufptr->offsets[i] = cumlen;
        bufptr->str += s;
        cumlen += s.length();
        if (nullable) {
            if (legacy_validity) {
                bufptr->validity_map[i] = vec[i] == R_NaString;
            } else {
                bufptr->validity_map[i] = vec[i] != R_NaString;
            }
        }
    }
    bufptr->rows = bufptr->cols = 0; // signal unassigned for the write case
    return(bufptr);
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_buffer_var_char(XPtr<tiledb::Query> query,
                                                        std::string attr,
                                                        XPtr<vlc_buf_t> bufptr) {
    check_xptr_tag<tiledb::Query>(query);
    check_xptr_tag<vlc_buf_t>(bufptr);
    if (bufptr->nullable) {
        query->set_validity_buffer(attr, bufptr->validity_map);
    }
    query->set_data_buffer(attr, bufptr->str);
    query->set_offsets_buffer(attr, bufptr->offsets);
    return query;
}

// 'len' is the length of the query result set, i.e. buffer elements for standard columns
// 'nchar' is the length of the result set for the particular column, i.e. actual (ex-post)
//    string length in bufptr (as opposed to ex-ante guess)
// [[Rcpp::export]]
CharacterMatrix libtiledb_query_get_buffer_var_char(XPtr<vlc_buf_t> bufptr,
                                                    int32_t len=0, int32_t nchar=0) {
  check_xptr_tag<vlc_buf_t>(bufptr);
  size_t n = (len==0 ? bufptr->offsets.size() : len);
  //Rprintf("n=%d, strsize=%d, row %d col %d, nchar %d, nullable %d len=%d nchar=%d\n",
  //        n, bufptr->str.size(), bufptr->rows, bufptr->cols, nchar, bufptr->nullable, len, nchar);
  std::vector<uint64_t> str_sizes(n);
  for (size_t i = 0; i < n - 1; i++) {                          // all but last
      //  Rprintf("%d %d %d\n", i, bufptr->offsets[i + 1] , bufptr->offsets[i]);
      str_sizes[i] = bufptr->offsets[i + 1] - bufptr->offsets[i];
  }                                                             // last is total size minus last start
  str_sizes[n-1] = (nchar==0 ? bufptr->str.size() * sizeof(char) : nchar) - bufptr->offsets[n-1];
  // Get the strings
  CharacterMatrix mat(bufptr->rows, bufptr->cols);
  for (size_t i = 0; i < n; i++) {
      if (bufptr->nullable) {
          if (bufptr->legacy_validity) {
              if (bufptr->validity_map[i] == 0)
                  mat[i] = std::string(&bufptr->str[bufptr->offsets[i]], str_sizes[i]);
              else
                  mat[i] = R_NaString;
          } else {
              if (bufptr->validity_map[i] != 0)
                  mat[i] = std::string(&bufptr->str[bufptr->offsets[i]], str_sizes[i]);
              else
                  mat[i] = R_NaString;
          }
      } else {
          mat[i] = std::string(&bufptr->str[bufptr->offsets[i]], str_sizes[i]);
      }
  }
  return(mat);
}

// more of a debugging helper
// [[Rcpp::export]]
std::string libtiledb_query_get_buffer_var_char_simple(XPtr<vlc_buf_t> bufptr) {
  check_xptr_tag<vlc_buf_t>(bufptr);
  return(bufptr->str);
}

// -- vlv_buf_t functions below

// assigning (for a write) allocates
// [[Rcpp::export]]
XPtr<vlv_buf_t> libtiledb_query_buffer_var_vec_create(IntegerVector intoffsets, SEXP data) {
  int n = intoffsets.size();
  XPtr<vlv_buf_t> bufptr = make_xptr<vlv_buf_t>(new vlv_buf_t);
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
  check_xptr_tag<vlv_buf_t>(buf);
  if (buf->dtype == TILEDB_INT32) {
      query->set_data_buffer(attr, buf->idata);
      query->set_offsets_buffer(attr, buf->offsets);
  } else if (buf->dtype == TILEDB_FLOAT64) {
      query->set_data_buffer(attr, buf->ddata);
      query->set_offsets_buffer(attr, buf->offsets);
  } else {
      Rcpp::stop("Unsupported type '%s' for buffer", _tiledb_datatype_to_string(buf->dtype));
  }
  return query;
}

// [[Rcpp::export]]
List libtiledb_query_get_buffer_var_vec(XPtr<tiledb::Query> query, std::string attr,
                                        XPtr<vlv_buf_t> buf) {

  check_xptr_tag<tiledb::Query>(query);
  check_xptr_tag<vlv_buf_t>(buf);
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
XPtr<query_buf_t> libtiledb_query_buffer_alloc_ptr(std::string domaintype,
                                                   R_xlen_t ncells,
                                                   bool nullable = false,
                                                   int32_t numvar = 1) {
  XPtr<query_buf_t> buf = make_xptr<query_buf_t>(new query_buf_t);
  if (domaintype == "INT32"  || domaintype == "UINT32") {
     buf->size = sizeof(int32_t);
  } else if (domaintype == "INT16" || domaintype == "UINT16") {
     buf->size = sizeof(int16_t);
  } else if (domaintype == "INT8" || domaintype == "UINT8") {
     buf->size = sizeof(int8_t);
  } else if (domaintype == "BLOB") {
     buf->size = sizeof(int8_t);
#if TILEDB_VERSION >= TileDB_Version(2,10,0)
  } else if (domaintype == "BOOL") {
     buf->size = sizeof(uint8_t);
#endif
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
  if (nullable) buf->validity_map.resize(ncells/numvar);
  buf->numvar = numvar;
  buf->nullable = nullable;
  return buf;
}

// [[Rcpp::export]]
XPtr<query_buf_t> libtiledb_query_buffer_assign_ptr(XPtr<query_buf_t> buf, std::string dtype,
                                                    SEXP vec, bool asint64 = false) {
  check_xptr_tag<query_buf_t>(buf);
  if (dtype == "INT32") {
    IntegerVector v(vec);
    std::memcpy(buf->vec.data(), &(v[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map, buf->numvar);
  } else if (dtype == "FLOAT64") {
    NumericVector v(vec);
    std::memcpy(buf->vec.data(), &(v[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromNumeric(v, buf->validity_map, buf->numvar);
  } else if (dtype == "INT64" ||
             (asint64 && is_datetime_column(buf->dtype))) {
    // integer64 from the bit64 package uses doubles, sees nanosecond
    NumericVector v(vec);
    std::memcpy(buf->vec.data(), &(v[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInt64(v, buf->validity_map, buf->numvar);
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
    std::vector<int64_t> iv = fromInteger64(v);
    if (buf->nullable)
        getValidityMapFromInt64(v, buf->validity_map, buf->numvar);
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
        getValidityMapFromInteger(v, buf->validity_map, buf->numvar);
  } else if (dtype == "INT16") {
    IntegerVector v(vec);
    auto n = v.length();
    std::vector<int16_t> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<int16_t>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map, buf->numvar);
  } else if (dtype == "UINT16") {
    IntegerVector v(vec);
    auto n = v.length();
    std::vector<uint16_t> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<uint16_t>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map, buf->numvar);
  } else if (dtype == "INT8") {
    IntegerVector v(vec);
    auto n = v.length();
    std::vector<int8_t> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<int8_t>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map, buf->numvar);
  } else if (dtype == "UINT8") {
    IntegerVector v(vec);
    auto n = v.length();
    std::vector<uint8_t> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<uint8_t>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromInteger(v, buf->validity_map, buf->numvar);
  } else if (dtype == "FLOAT32") {
    NumericVector v(vec);
    if (buf->nullable)
        getValidityMapFromNumeric(v, buf->validity_map, buf->numvar);
    auto n = v.length();
    std::vector<float> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<float>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
#if TILEDB_VERSION >= TileDB_Version(2,10,0)
  } else if (dtype == "BOOL") {
    LogicalVector v(vec);
    auto n = v.length();
    std::vector<uint8_t> x(n);
    for (auto i=0; i<n; i++) {
      x[i] = static_cast<uint8_t>(v[i]);
    }
    std::memcpy(buf->vec.data(), &(x[0]), buf->ncells*buf->size);
    if (buf->nullable)
        getValidityMapFromLogical(v, buf->validity_map, buf->numvar);
#endif
  } else {
    Rcpp::stop("Assignment to '%s' currently unsupported.", dtype.c_str());
  }
  return buf;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_buffer_ptr(XPtr<tiledb::Query> query,
                                                   std::string attr,
                                                   XPtr<query_buf_t> buf) {
    check_xptr_tag<tiledb::Query>(query);
    if (buf->nullable) {
        query->set_validity_buffer(attr, buf->validity_map);
    }
    query->set_data_buffer(attr, static_cast<void*>(buf->vec.data()), buf->ncells);
    return query;
}

// [[Rcpp::export]]
IntegerVector length_from_vlcbuf(XPtr<vlc_buf_t> buf) {
    check_xptr_tag<vlc_buf_t>(buf);
    IntegerVector v = IntegerVector::create(buf->offsets.size(), buf->str.length());
    return v;
}

// [[Rcpp::export]]
RObject libtiledb_query_get_buffer_ptr(XPtr<query_buf_t> buf, bool asint64 = false) {
  spdl::debug("[libtiledb_query_get_buffer_ptr] before buf type check");
  check_xptr_tag<query_buf_t>(buf);
  std::string dtype = _tiledb_datatype_to_string(buf->dtype);
  //Rcpp::Rcout << dtype << " " << buf->ncells << " " << buf->size << " " << buf->nullable << std::endl;
  spdl::debug(tfm::format("[libtiledb_query_get_buffer_ptr] type %s", dtype));
  if (dtype == "INT32") {
    IntegerVector v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    if (buf->nullable)
        setValidityMapForInteger(v, buf->validity_map, buf->numvar);
    return v;
  } else if (dtype == "UINT32") {
    IntegerVector v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    if (buf->nullable)
        setValidityMapForInteger(v, buf->validity_map, buf->numvar);
    return v;
  } else if (dtype == "FLOAT64") {
    NumericVector v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    if (buf->nullable)
        setValidityMapForNumeric(v, buf->validity_map, buf->numvar);
    return v;
  } else if (dtype == "FLOAT32") {
    std::vector<float> v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    NumericVector w(wrap(v));
    if (buf->nullable)
        setValidityMapForNumeric(w, buf->validity_map, buf->numvar);
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
        setValidityMapForNumeric(res, buf->validity_map, buf->numvar);
    //return toInteger64(iv); // we could return as int64,
    return res;                 // but current 'contract' is return as NumericVector
  } else if (dtype == "INT64") {
    std::vector<int64_t> v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    if (buf->nullable)
        setValidityMapForInt64(v, buf->validity_map, buf->numvar);
    return toInteger64(v);
  } else if (asint64 && is_datetime_column(buf->dtype)) {
    std::vector<int64_t> v(buf->ncells);
    std::memcpy(&(v[0]), (void*) buf->vec.data(), buf->ncells * buf->size);
    return toInteger64(v);
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
    return toNanotime(vec);
  } else if (dtype == "INT16") {
    size_t n = buf->ncells;
    std::vector<int16_t> intvec(n);
    std::memcpy(intvec.data(), buf->vec.data(), n*buf->size);
    Rcpp::IntegerVector out(buf->ncells);
    for (size_t i=0; i<n; i++) {
      out[i] = static_cast<int32_t>(intvec[i]);
    }
    if (buf->nullable)
        setValidityMapForInteger(out, buf->validity_map, buf->numvar);
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
        setValidityMapForInteger(out, buf->validity_map, buf->numvar);
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
        setValidityMapForInteger(out, buf->validity_map, buf->numvar);
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
        setValidityMapForInteger(out, buf->validity_map, buf->numvar);
    return out;
  } else if (dtype == "BLOB") {
    size_t n = buf->ncells;
    Rcpp::RawVector out(n);
    std::memcpy(out.begin(), buf->vec.data(), n*buf->size);
    // -- raw has no NA type so no mapping possible here
    // if (buf->nullable)
    //    setValidityMapForRaw(out, buf->validity_map);
    return out;
#if TILEDB_VERSION >= TileDB_Version(2,10,0)
  } else if (dtype == "BOOL") {
    size_t n = buf->ncells;
    std::vector<uint8_t> uintvec(n);
    std::memcpy(uintvec.data(), buf->vec.data(), n*buf->size);
    Rcpp::LogicalVector out(buf->ncells);
    for (size_t i=0; i<n; i++) {
      out[i] = static_cast<int32_t>(uintvec[i]); // logical is int32_t internally
    }
    if (buf->nullable)
        setValidityMapForLogical(out, buf->validity_map, buf->numvar);
    return out;
#endif
  } else {
    Rcpp::stop("Unsupported type '%s'", dtype.c_str());
  }
  return R_NilValue; // not reached
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_submit(XPtr<tiledb::Query> query) {
  check_xptr_tag<tiledb::Query>(query);
  spdl::trace("[libtiledb_query_submit]");
  query->submit();
  return query;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_submit_async(XPtr<tiledb::Query> query) {
  check_xptr_tag<tiledb::Query>(query);
  spdl::trace("[libtiledb_query_submit_async]");
  query->submit_async();
  return query;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_finalize(XPtr<tiledb::Query> query) {
  check_xptr_tag<tiledb::Query>(query);
  spdl::trace("[libtiledb_query_finalize]");
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
  check_xptr_tag<tiledb::Query>(query);
  tiledb::Query::Status status = query->query_status();
  std::string status_text = _query_status_to_string(status);
  spdl::debug(tfm::format("[libtiledb_query_status] status = %s", status_text.c_str()));
  return status_text;
}

// [[Rcpp::export]]
R_xlen_t libtiledb_query_result_buffer_elements(XPtr<tiledb::Query> query,
                                                std::string attribute,
                                                int32_t which = 1) {
    check_xptr_tag<tiledb::Query>(query);
    auto nelements = query->result_buffer_elements()[attribute];
    spdl::debug(tfm::format("[libtiledb_query_result_buffer_elements] attribute %s : (%d,%d)",
                            attribute, nelements.first, nelements.second));
    R_xlen_t nelem = (which == 0 ? nelements.first : nelements.second);
    return nelem;
}

// [[Rcpp::export]]
NumericVector libtiledb_query_result_buffer_elements_vec(XPtr<tiledb::Query> query,
                                                         std::string attribute,
                                                         bool nullable = false) {
    check_xptr_tag<tiledb::Query>(query);
    if (nullable) {
        auto nelem = query->result_buffer_elements_nullable()[attribute];
        auto vec = NumericVector( {
                static_cast<double>(std::get<0>(nelem)),
                static_cast<double>(std::get<1>(nelem)),
                static_cast<double>(std::get<2>(nelem))
            });
        return vec;
    } else {
        auto nelem = query->result_buffer_elements()[attribute];
        return NumericVector( { static_cast<double>(nelem.first), static_cast<double>(nelem.second) });
    }
}

// [[Rcpp::export]]
int libtiledb_query_get_fragment_num(XPtr<tiledb::Query> query) {
  check_xptr_tag<tiledb::Query>(query);
  if (query->query_type() != TILEDB_WRITE) {
    Rcpp::stop("Fragment number only applicable to 'write' queries.");
  }
  return query->fragment_num();
}

// [[Rcpp::export]]
std::string libtiledb_query_get_fragment_uri(XPtr<tiledb::Query> query, int idx) {
  check_xptr_tag<tiledb::Query>(query);
  if (query->query_type() != TILEDB_WRITE) {
    Rcpp::stop("Fragment URI only applicable to 'write' queries.");
  }
  uint32_t uidx = static_cast<uint32_t>(idx);
  return query->fragment_uri(uidx);
}

// [[Rcpp::export]]
Rcpp::DatetimeVector libtiledb_query_get_fragment_timestamp_range(XPtr<tiledb::Query> query, int idx) {
  check_xptr_tag<tiledb::Query>(query);
  if (query->query_type() != TILEDB_WRITE) {
    Rcpp::stop("Fragment URI only applicable to 'write' queries.");
  }
  uint32_t uidx = static_cast<uint32_t>(idx);
  std::pair<uint64_t, uint64_t> range = query->fragment_timestamp_range(uidx);
  return Rcpp::DatetimeVector::create(range.first/1000.0, range.second/1000.0);
}

// Subarray functions replacing old Query functionality
// (This breaks the alphabetical + chronlogical sorting but belongs here

// [[Rcpp::export]]
XPtr<tiledb::Subarray> libtiledb_subarray(XPtr<tiledb::Query> query) {
    return make_xptr<tiledb::Subarray>(new tiledb::Subarray(query->ctx(), query->array()));
}

// [[Rcpp::export]]
XPtr<tiledb::Subarray> libtiledb_subarray_add_range(XPtr<tiledb::Subarray> subarr,
                                                    int iidx, SEXP starts, SEXP ends,
                                                    SEXP strides = R_NilValue) {
    check_xptr_tag<tiledb::Subarray>(subarr);
    spdl::debug("libtiledb_query_add_range] setting subarray");
    if (TYPEOF(starts) != TYPEOF(ends)) {
        Rcpp::stop("'start' and 'end' must be of identical types");
    }
    uint32_t uidx = static_cast<uint32_t>(iidx);
    if (TYPEOF(starts) == INTSXP) {
        int32_t start = as<int32_t>(starts);
        int32_t end = as<int32_t>(ends);
        int32_t stride = (strides == R_NilValue) ? 0 : Rcpp::as<int32_t>(strides);
        subarr->add_range(uidx, start, end, stride);
    } else if (TYPEOF(starts) == REALSXP) {
        double start = as<double>(starts);
        double end = as<double>(ends);
        double stride = (strides == R_NilValue) ? 0 : Rcpp::as<double_t>(strides);
        subarr->add_range(uidx, start, end, stride);
    } else if (TYPEOF(starts) == STRSXP) {
        std::string start = as<std::string>(starts);
        std::string end = as<std::string>(ends);
        if (strides == R_NilValue) {
            subarr->add_range(uidx, start, end);
        } else {
            Rcpp::stop("Non-emoty stride for string not supported yet.");
        }
    } else {
        Rcpp::stop("Invalid data type for query range: '%s'", Rcpp::type2name(starts));
    }
    return subarr;
}

// [[Rcpp::export]]
XPtr<tiledb::Subarray> libtiledb_subarray_add_range_with_type(XPtr<tiledb::Subarray> subarr,
                                                              int iidx,
                                                              std::string typestr,
                                                              SEXP starts, SEXP ends,
                                                              SEXP strides = R_NilValue) {

    check_xptr_tag<tiledb::Subarray>(subarr);
    if (TYPEOF(starts) != TYPEOF(ends)) {
        Rcpp::stop("'start' and 'end' must be of identical types");
    }
    uint32_t uidx = static_cast<uint32_t>(iidx);

    if (typestr == "INT32") {
        int32_t start = as<int32_t>(starts);
        int32_t end = as<int32_t>(ends);
        int32_t stride = (strides == R_NilValue) ? 0 : Rcpp::as<int32_t>(strides);
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %d to %d by %d", typestr, uidx, start, end, stride));
    } else if (typestr == "FLOAT64") {
        double start = as<double>(starts);
        double end = as<double>(ends);
        double stride = (strides == R_NilValue) ? 0 : Rcpp::as<double_t>(strides);
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %f to %f by %f", typestr, uidx, start, end, stride));
    } else if (typestr == "INT64") {
        int64_t start = fromInteger64(as<double>(starts));
        int64_t end = fromInteger64(as<double>(ends));
        int64_t stride = (strides == R_NilValue) ? 0 : fromInteger64(as<double>(strides));
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %d to %d by %d", typestr, uidx, start, end, stride));
    } else if (typestr == "UINT64") {
        uint64_t start = static_cast<uint64_t>(fromInteger64(as<double>(starts)));
        uint64_t end = static_cast<uint64_t>(fromInteger64(as<double>(ends)));
        uint64_t stride = (strides == R_NilValue) ? 0 : fromInteger64(as<double>(strides));
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %d to %d by %d", typestr, uidx, start, end, stride));
    } else if (typestr == "UINT32") {
        uint32_t start = as<uint32_t>(starts);
        uint32_t end   = as<uint32_t>(ends);
        uint32_t stride = (strides == R_NilValue) ? 0 : as<int32_t>(strides);
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %d to %d by %d", typestr, uidx, start, end, stride));
    } else if (typestr == "INT16") {
        int16_t start = as<int16_t>(starts);
        int16_t end   = as<int16_t>(ends);
        int16_t stride = (strides == R_NilValue) ? 0 : as<int16_t>(strides);
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %d to %d by %d", typestr, uidx, start, end, stride));
    } else if (typestr == "UINT16") {
        uint16_t start = as<uint16_t>(starts);
        uint16_t end   = as<uint16_t>(ends);
        uint16_t stride = (strides == R_NilValue) ? 0 : as<uint16_t>(strides);
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %d to %d by %d", typestr, uidx, start, end, stride));
    } else if (typestr == "INT8") {
        int8_t start = as<int16_t>(starts);
        int8_t end   = as<int16_t>(ends);
        int8_t stride = (strides == R_NilValue) ? 0 : as<int16_t>(strides);
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %d to %d by %d", typestr, uidx, start, end, stride));
    } else if (typestr == "UINT8") {
        uint8_t start = as<uint16_t>(starts);
        uint8_t end   = as<uint16_t>(ends);
        uint8_t stride = (strides == R_NilValue) ? 0 : as<uint16_t>(strides);
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %d to %d by %d", typestr, uidx, start, end, stride));
    } else if (typestr == "DATETIME_YEAR"  ||
               typestr == "DATETIME_MONTH" ||
               typestr == "DATETIME_WEEK"  ||
               typestr == "DATETIME_DAY"   ||
               typestr == "DATETIME_HR"    ||
               typestr == "DATETIME_MIN"   ||
               typestr == "DATETIME_SEC"   ||
               typestr == "DATETIME_MS"    ||
               typestr == "DATETIME_US"    ||
               typestr == "DATETIME_NS"    ||
               typestr == "DATETIME_FS"    ||
               typestr == "DATETIME_PS"    ||
               typestr == "DATETIME_AS") {
        int64_t start = fromInteger64(as<double>(starts));
        int64_t end = fromInteger64(as<double>(ends));
        int64_t stride = (strides == R_NilValue) ? 0 : fromInteger64(as<double>(strides));
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %d to %d by %d", typestr, uidx, start, end, stride));
    } else if (typestr == "ASCII" || typestr == "CHAR") {
        std::string start = as<std::string>(starts);
        std::string end = as<std::string>(ends);
        if (strides != R_NilValue) {
            Rcpp::stop("Non-empty stride for string not supported yet.");
        }
        subarr->add_range(uidx, start, end);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %s to %s", typestr, uidx, start, end));
    } else if (typestr == "FLOAT32") {
        float start = as<float>(starts);
        float end = as<float>(ends);
        float stride = (strides == R_NilValue) ? 0 : Rcpp::as<float>(strides);
        subarr->add_range(uidx, start, end, stride);
        spdl::debug(tfm::format("[libtiledb_subarry_add_range_with type] %s dim %d added %f to %f by %f", typestr, uidx, start, end, stride));
    }
    return subarr;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_subarray_object(XPtr<tiledb::Query> query, XPtr<tiledb::Subarray> subarr) {
    check_xptr_tag<tiledb::Query>(query);
    check_xptr_tag<tiledb::Subarray>(subarr);
    query->set_subarray(*subarr.get());
    return query;
}


// [[Rcpp::export]]
R_xlen_t libtiledb_query_get_est_result_size(XPtr<tiledb::Query> query, std::string attr) {
  check_xptr_tag<tiledb::Query>(query);
  uint64_t est = query->est_result_size(attr);
  return static_cast<R_xlen_t>(est);
}


// [[Rcpp::export]]
NumericVector libtiledb_query_get_est_result_size_nullable(XPtr<tiledb::Query> query, std::string attr) {
    check_xptr_tag<tiledb::Query>(query);
    std::array<uint64_t, 2> est = query->est_result_size_nullable(attr);
    return Rcpp::NumericVector::create(static_cast<R_xlen_t>(est[0]),
                                       static_cast<R_xlen_t>(est[1]));
}


// [[Rcpp::export]]
NumericVector libtiledb_query_get_est_result_size_var(XPtr<tiledb::Query> query, std::string attr) {
  check_xptr_tag<tiledb::Query>(query);
  std::array<uint64_t, 2> est = query->est_result_size_var(attr);
  return NumericVector::create(static_cast<R_xlen_t>(est[0]), static_cast<R_xlen_t>(est[1]));
}

// [[Rcpp::export]]
NumericVector libtiledb_query_get_est_result_size_var_nullable(XPtr<tiledb::Query> query, std::string attr) {
    check_xptr_tag<tiledb::Query>(query);
    std::array<uint64_t, 3> est = query->est_result_size_var_nullable(attr);
    return Rcpp::NumericVector::create(static_cast<R_xlen_t>(est[0]),
                                       static_cast<R_xlen_t>(est[1]),
                                       static_cast<R_xlen_t>(est[2]));
}

// [[Rcpp::export]]
double libtiledb_query_get_range_num(XPtr<tiledb::Query> query, int dim_idx) {
  check_xptr_tag<tiledb::Query>(query);
  tiledb::Array arr = query->array();
  tiledb::Context ctx = query->ctx();
  tiledb::Subarray sub(ctx, arr);
  query->update_subarray_from_query(&sub);
  uint64_t range_num = sub.range_num(static_cast<unsigned int>(dim_idx));
  return static_cast<double>(range_num);
}

// [[Rcpp::export]]
IntegerVector libtiledb_query_get_range(XPtr<tiledb::Query> query, int dim_idx, int rng_idx) {
  check_xptr_tag<tiledb::Query>(query);
  tiledb::Array arr = query->array();
  tiledb::Context ctx = query->ctx();
  tiledb::Subarray sub(ctx, arr);
  query->update_subarray_from_query(&sub);
  std::array<int32_t, 3> rng = sub.range<int32_t>(static_cast<unsigned int>(dim_idx),
                                                  static_cast<unsigned int>(rng_idx));
  return IntegerVector::create(rng[0], 	// start
                               rng[1],  // end
                               rng[2]); // stride
}

// [[Rcpp::export]]
CharacterVector libtiledb_query_get_range_var(XPtr<tiledb::Query> query, int dim_idx, int rng_idx) {
  check_xptr_tag<tiledb::Query>(query);
  tiledb::Array arr = query->array();
  tiledb::Context ctx = query->ctx();
  tiledb::Subarray sub(ctx, arr);
  query->update_subarray_from_query(&sub);
  std::array<std::string, 2> rng = sub.range(static_cast<unsigned int>(dim_idx), static_cast<uint64_t>(rng_idx));
  return CharacterVector::create(rng[0], rng[1]);	 // start and end
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_condition(XPtr<tiledb::Query> query,
                                                  XPtr<tiledb::QueryCondition> query_cond) {
  check_xptr_tag<tiledb::Query>(query);
  query->set_condition(*query_cond.get());
  return query;
}

// Note that the Array pointer returned here from a Query object is not owned and will not
// outlive the query object
//
// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_query_get_array(XPtr<tiledb::Query> query, XPtr<tiledb::Context> ctx) {
    check_xptr_tag<tiledb::Query>(query);
    check_xptr_tag<tiledb::Context>(ctx);
    auto arr = query->array();
    auto cptr = arr.ptr().get();
    return make_xptr<tiledb::Array>(new tiledb::Array(*ctx.get(), cptr, false));
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_query_get_schema(XPtr<tiledb::Query> query,
                                                     XPtr<tiledb::Context> ctx) {
    check_xptr_tag<tiledb::Query>(query);
    auto arr = query->array();
    return libtiledb_array_schema_load(ctx, arr.uri()); // returns an XPtr<tiledb::ArraySchema>
}

// [[Rcpp::export]]
std::string libtiledb_query_stats(XPtr<tiledb::Query> query) {
    check_xptr_tag<tiledb::Query>(query);
    return query->stats();
}

// [[Rcpp::export]]
XPtr<tiledb::Context> libtiledb_query_get_ctx(XPtr<tiledb::Query> query) {
    check_xptr_tag<tiledb::Query>(query);
    auto ctx = query->ctx();
    return make_xptr<tiledb::Context>(new tiledb::Context(ctx));
}


/**
 * Query Condition
 */
const char* _tiledb_query_condition_op_to_string(tiledb_query_condition_op_t op) {
    switch (op) {
    case TILEDB_LT:
        return "LT";
    case TILEDB_LE:
        return "LE";
    case TILEDB_GT:
        return "GT";
    case TILEDB_GE:
        return "GE";
    case TILEDB_EQ:
        return "EQ";
    case TILEDB_NE:
        return "NE";
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    case TILEDB_IN:
        return "IN";
    case TILEDB_NOT_IN:
        return "NOT_IN";
#endif
    default:
        Rcpp::stop("Unknown condition op (%d)", op);
    }
}

tiledb_query_condition_op_t _tiledb_query_string_to_condition_op(const std::string& opstr) {
    if (opstr == "LT") {
        return TILEDB_LT;
    } else if (opstr == "LE") {
        return TILEDB_LE;
    } else if (opstr == "GT") {
        return TILEDB_GT;
    } else if (opstr == "GE") {
        return TILEDB_GE;
    } else if (opstr == "EQ") {
        return TILEDB_EQ;
    } else if (opstr == "NE") {
        return TILEDB_NE;
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    } else if (opstr == "IN") {
        return TILEDB_IN;
    } else if (opstr == "NOT_IN") {
        return TILEDB_NOT_IN;
#endif
    } else {
        Rcpp::stop("Unknown TileDB op string '%s'", opstr.c_str());
    }
}

const char* _tiledb_query_condition_combination_op_to_string(tiledb_query_condition_combination_op_t op) {
    switch (op) {
    case TILEDB_AND:
        return "AND";
    case TILEDB_OR:
        return "OR";
    case TILEDB_NOT:
        return "NOT";
    default:
        Rcpp::stop("Unknown condition combination op (%d)", op);
    }
}

tiledb_query_condition_combination_op_t _tiledb_query_string_to_condition_combination_op(const std::string& opstr) {
    if (opstr == "AND") {
        return TILEDB_AND;
    } else if (opstr == "OR") {
        return TILEDB_OR;
    } else if (opstr == "NOT") {
        return TILEDB_NOT;
    } else {
        Rcpp::stop("Unknown TileDB combination op string '%s'", opstr.c_str());
    }
}


// [[Rcpp::export]]
XPtr<tiledb::QueryCondition> libtiledb_query_condition(XPtr<tiledb::Context> ctx) {
    check_xptr_tag<tiledb::Context>(ctx);
    auto ptr = make_xptr<tiledb::QueryCondition>(new tiledb::QueryCondition(*ctx.get()));
    return ptr;
}

// [[Rcpp::export]]
void libtiledb_query_condition_init(XPtr<tiledb::QueryCondition> query_cond,
                                    const std::string& attr_name,
                                    SEXP condition_value,
                                    const std::string& cond_val_type,
                                    const std::string& cond_op_string) {
    check_xptr_tag<tiledb::QueryCondition>(query_cond);
    tiledb_query_condition_op_t op = _tiledb_query_string_to_condition_op(cond_op_string);
    if (cond_val_type == "INT32"  || cond_val_type == "UINT32") {
        int v = as<int>(condition_value);
        uint64_t cond_val_size = sizeof(int);
        query_cond->init(attr_name, (void*) &v, cond_val_size, op);
    } else if (cond_val_type == "FLOAT64") {
        double v = as<double>(condition_value);
        uint64_t cond_val_size = sizeof(double);
        query_cond->init(attr_name, (void*) &v, cond_val_size, op);
    } else if (cond_val_type == "INT64" || cond_val_type == "UINT64") {
        int64_t v = fromInteger64( as<double>(condition_value) );
        uint64_t cond_val_size = sizeof(int64_t);
        query_cond->init(attr_name, (void*) &v, cond_val_size, op);
    } else if (cond_val_type == "INT8" || cond_val_type == "UINT8") {
        int v = as<int>(condition_value);
        uint64_t cond_val_size = sizeof(int8_t);
        query_cond->init(attr_name, (void*) &v, cond_val_size, op);
    } else if (cond_val_type == "INT16" || cond_val_type == "UINT16") {
        int v = as<int>(condition_value);
        uint64_t cond_val_size = sizeof(int16_t);
        query_cond->init(attr_name, (void*) &v, cond_val_size, op);
    } else if (cond_val_type == "FLOAT32") {
        float v = static_cast<float>(as<double>(condition_value));
        uint64_t cond_val_size = sizeof(float);
        query_cond->init(attr_name, (void*) &v, cond_val_size, op);
    } else if (cond_val_type == "ASCII" || cond_val_type == "UTF8") {
        std::string v = as<std::string>(condition_value);
        query_cond->init(attr_name, v, op);
    } else if (cond_val_type == "BOOL") {
        bool v = as<bool>(condition_value);
        uint64_t cond_val_size = sizeof(bool);
        query_cond->init(attr_name, (void*) &v, cond_val_size, op);
    } else if (cond_val_type == "DATETIME_MS") {
        int64_t v = static_cast<int64_t>(as<double>(condition_value) * 1000);
        uint64_t cond_val_size = sizeof(int64_t);
        query_cond->init(attr_name, (void*) &v, cond_val_size, op);
    } else if (cond_val_type == "DATETIME_DAY") {
        int64_t v = static_cast<int64_t>(as<double>(condition_value));
        uint64_t cond_val_size = sizeof(int64_t);
        query_cond->init(attr_name, (void*) &v, cond_val_size, op);
    } else {
        Rcpp::stop("Currently unsupported type: %s", cond_val_type);
    }
}

// [[Rcpp::export]]
XPtr<tiledb::QueryCondition> libtiledb_query_condition_combine(XPtr<tiledb::QueryCondition> lhs,
                                                               XPtr<tiledb::QueryCondition> rhs,
                                                               const std::string& str) {
    check_xptr_tag<tiledb::QueryCondition>(lhs);
    check_xptr_tag<tiledb::QueryCondition>(lhs);
    tiledb_query_condition_combination_op_t op = _tiledb_query_string_to_condition_combination_op(str);
    tiledb::QueryCondition res = lhs->combine(*rhs.get(), op);
    auto query_cond = make_xptr<tiledb::QueryCondition>(new tiledb::QueryCondition(res));
    return query_cond;
}

// [[Rcpp::export]]
void libtiledb_query_condition_set_use_enumeration(XPtr<tiledb::Context> ctx,
                                                   XPtr<tiledb::QueryCondition> cond,
                                                   bool use_enumeration) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::QueryCondition>(cond);
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    tiledb::QueryConditionExperimental::set_use_enumeration(*ctx.get(), *cond.get(), use_enumeration);
#endif
}

// [[Rcpp::export]]
XPtr<tiledb::QueryCondition>
libtiledb_query_condition_create(XPtr<tiledb::Context> ctx, const std::string& name,
                                 SEXP vec, const std::string& cond_op_string) {
    check_xptr_tag<tiledb::Context>(ctx);
#if TILEDB_VERSION >= TileDB_Version(2,17,0)
    tiledb_query_condition_op_t op = _tiledb_query_string_to_condition_op(cond_op_string);
    // consider three cases of 'vec' based on R types:  int, double and int64-as-double
    if (TYPEOF(vec) == INTSXP) {
        std::vector<int32_t> iv = Rcpp::as<std::vector<int32_t>>(vec);
        auto qc = tiledb::QueryConditionExperimental::create<int32_t>(*ctx.get(), name, iv, op);
        return make_xptr<tiledb::QueryCondition>(new tiledb::QueryCondition(qc));
    } else if (TYPEOF(vec) == REALSXP) {
        if (Rcpp::isInteger64(vec)) {
            std::vector<int64_t> dv = Rcpp::fromInteger64(Rcpp::NumericVector(vec));
            auto qc = tiledb::QueryConditionExperimental::create<int64_t>(*ctx.get(), name, dv, op);
            return make_xptr<tiledb::QueryCondition>(new tiledb::QueryCondition(qc));
        } else {
            std::vector<double> dv = Rcpp::as<std::vector<double>>(vec);
            auto qc = tiledb::QueryConditionExperimental::create<double>(*ctx.get(), name, dv, op);
            return make_xptr<tiledb::QueryCondition>(new tiledb::QueryCondition(qc));
        }
    } else if (TYPEOF(vec) == STRSXP) {
        std::vector<std::string> sv = Rcpp::as<std::vector<std::string>>(vec);
        auto qc = tiledb::QueryConditionExperimental::create(*ctx.get(), name, sv, op);
        return make_xptr<tiledb::QueryCondition>(new tiledb::QueryCondition(qc));
    } else {
        Rcpp::stop("No support (yet) for type '%s'.", Rcpp::type2name(vec));
    }
#endif
    return make_xptr<tiledb::QueryCondition>(R_NilValue);
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
std::string libtiledb_create_group(XPtr<tiledb::Context> ctx, std::string uri) {
  check_xptr_tag<tiledb::Context>(ctx);
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
  check_xptr_tag<tiledb::Context>(ctx);
  auto obj = tiledb::Object::object(*ctx.get(), uri);
  return _object_type_to_string(obj.type());
}

// [[Rcpp::export]]
std::string libtiledb_object_remove(XPtr<tiledb::Context> ctx, std::string uri) {
  check_xptr_tag<tiledb::Context>(ctx);
  tiledb::Object::remove(*ctx.get(), uri);
  return uri;
}

// [[Rcpp::export]]
std::string libtiledb_object_move(XPtr<tiledb::Context> ctx, std::string old_uri, std::string new_uri) {
  check_xptr_tag<tiledb::Context>(ctx);
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
                                std::string uri, std::string order, bool recursive = false) {
  check_xptr_tag<tiledb::Context>(ctx);
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
  return Rcpp::DataFrame::create(Rcpp::Named("TYPE") = r_types,
                                 Rcpp::Named("URI") = r_uris);
}

/**
 * VFS functionality
 */
// [[Rcpp::export]]
XPtr<tiledb::VFS> libtiledb_vfs(XPtr<tiledb::Context> ctx,
                                Nullable<XPtr<tiledb::Config>> config=R_NilValue) {
  check_xptr_tag<tiledb::Context>(ctx);
  if (config.isNull()) {
    return make_xptr<tiledb::VFS>(new tiledb::VFS(*ctx.get()));
  } else {
    XPtr<tiledb::Config> config_xptr(config);
    return make_xptr<tiledb::VFS>(new tiledb::VFS(*ctx.get(), *config_xptr.get()));
  }
}

// [[Rcpp::export]]
std::string libtiledb_vfs_create_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  vfs->create_bucket(uri);
  return uri;
}

// [[Rcpp::export]]
std::string libtiledb_vfs_remove_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  vfs->remove_bucket(uri);
  return uri;
}

// [[Rcpp::export]]
bool libtiledb_vfs_is_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  return vfs->is_bucket(uri);
}

// [[Rcpp::export]]
bool libtiledb_vfs_is_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  return vfs->is_empty_bucket(uri);
}

// [[Rcpp::export]]
std::string libtiledb_vfs_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  vfs->empty_bucket(uri);
  return uri;
}

// [[Rcpp::export]]
std::string libtiledb_vfs_create_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  vfs->create_dir(uri);
  return uri;
}

// [[Rcpp::export]]
bool libtiledb_vfs_is_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
    check_xptr_tag<tiledb::VFS>(vfs);
    auto ptr = vfs.get();
    return ptr->is_dir(uri);
}

// [[Rcpp::export]]
std::string libtiledb_vfs_remove_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  vfs->remove_dir(uri);
  return uri;
}

// [[Rcpp::export]]
bool libtiledb_vfs_is_file(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  return vfs->is_file(uri);
}

// [[Rcpp::export]]
std::string libtiledb_vfs_remove_file(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  vfs->remove_file(uri);
  return uri;
}

// [[Rcpp::export]]
R_xlen_t libtiledb_vfs_file_size(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  uint64_t size = vfs->file_size(uri);
  if (size > std::numeric_limits<R_xlen_t>::max()) {
    Rcpp::stop("file size is greater than maximum R integer");
  }
  return static_cast<R_xlen_t>(size);
}

// [[Rcpp::export]]
std::string libtiledb_vfs_move_file(XPtr<tiledb::VFS> vfs, std::string old_uri, std::string new_uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  vfs->move_file(old_uri, new_uri);
  return new_uri;
}

// [[Rcpp::export]]
std::string libtiledb_vfs_move_dir(XPtr<tiledb::VFS> vfs, std::string old_uri, std::string new_uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  vfs->move_dir(old_uri, new_uri);
  return new_uri;
}

// [[Rcpp::export]]
std::string libtiledb_vfs_touch(XPtr<tiledb::VFS> vfs, std::string uri) {
  check_xptr_tag<tiledb::VFS>(vfs);
  vfs->touch(uri);
  return uri;
}


// [[Rcpp::export]]
XPtr<vfs_fh_t> libtiledb_vfs_open(XPtr<tiledb::Context> ctxxp, XPtr<tiledb::VFS> vfsxp,
                                  std::string uri, std::string mode) {
  check_xptr_tag<tiledb::Context>(ctxxp);
  check_xptr_tag<tiledb::VFS>(vfsxp);
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  std::shared_ptr<tiledb_vfs_t> vfs = vfsxp.get()->ptr();
  tiledb_vfs_fh_t *fh = nullptr;
  tiledb_vfs_mode_t vfsmode = _string_to_tiledb_vfs_mode_t(mode);
  tiledb_vfs_open(ctx.get(), vfs.get(), uri.c_str(), vfsmode, &fh);
  XPtr<vfs_fh_t> ptr = make_xptr<vfs_fh_t>(new vfs_fh_t);
  ptr->fh = static_cast<void*>(fh);
  return ptr;
}

// [[Rcpp::export]]
void libtiledb_vfs_close(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh) {
  check_xptr_tag<tiledb::Context>(ctxxp);
  check_xptr_tag<vfs_fh_t>(fh);
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  tiledb_vfs_close(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh));
}

// [[Rcpp::export]]
void libtiledb_vfs_write(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh,
                         Rcpp::IntegerVector vec) {
  check_xptr_tag<tiledb::Context>(ctxxp);
  check_xptr_tag<vfs_fh_t>(fh);
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  tiledb_vfs_write(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh),
                   &(vec[0]), vec.size()*sizeof(int));
}

// [[Rcpp::export]]
Rcpp::IntegerVector libtiledb_vfs_read(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh,
                                       double offset, double nbytes) {
  check_xptr_tag<tiledb::Context>(ctxxp);
  check_xptr_tag<vfs_fh_t>(fh);
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  std::int64_t offs = fromInteger64(offset);
  std::int64_t nb = fromInteger64(nbytes);
  Rcpp::IntegerVector buf(nb/4);
  tiledb_vfs_read(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh), offs, &(buf[0]), nb);
  return buf;
}

// [[Rcpp::export]]
void libtiledb_vfs_sync(XPtr<tiledb::Context> ctxxp, XPtr<vfs_fh_t> fh) {
  check_xptr_tag<tiledb::Context>(ctxxp);
  check_xptr_tag<vfs_fh_t>(fh);
  std::shared_ptr<tiledb_ctx_t> ctx = ctxxp.get()->ptr();
  tiledb_vfs_sync(ctx.get(), static_cast<tiledb_vfs_fh_t*>(fh->fh));
}

// [[Rcpp::export]]
double libtiledb_vfs_dir_size(XPtr<tiledb::VFS> vfs, std::string uri) {
    check_xptr_tag<tiledb::VFS>(vfs);
    return static_cast<double>(vfs->dir_size(uri)); // uint64_t to double for R
}

// [[Rcpp::export]]
std::vector<std::string> libtiledb_vfs_ls(XPtr<tiledb::VFS> vfs, std::string uri) {
    check_xptr_tag<tiledb::VFS>(vfs);
    return vfs->ls(uri);
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
std::string libtiledb_stats_raw_dump() {
    std::string txt;
    tiledb::Stats::raw_dump(&txt);
    return txt;
}

// [[Rcpp::export]]
std::string libtiledb_stats_raw_get() {
    Rcpp::message(Rcpp::wrap("This function is deprecated, please use 'libtiledb_stats_raw_dump'."));
    return libtiledb_stats_raw_dump();
}

/**
 * FragmentInfo
 */
// [[Rcpp::export]]
XPtr<tiledb::FragmentInfo> libtiledb_fragment_info(XPtr<tiledb::Context> ctx,
                                                   const std::string& uri) {
    auto ptr = make_xptr<tiledb::FragmentInfo>(new tiledb::FragmentInfo(*ctx.get(), uri));
    ptr->load();                // also load
    return ptr;
}

// [[Rcpp::export]]
std::string libtiledb_fragment_info_uri(XPtr<tiledb::FragmentInfo> fi, int32_t fid) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return fi->fragment_uri(static_cast<uint32_t>(fid));
}

// [[Rcpp::export]]
Rcpp::NumericVector libtiledb_fragment_info_get_non_empty_domain_index(XPtr<tiledb::FragmentInfo> fi,
                                                                       int32_t fid, int32_t did,
                                                                       const std::string& typestr) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    if (typestr == "INT64") {
        std::vector<int64_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return toInteger64(non_empty_dom);
    } else if (typestr == "UINT64") {
        std::vector<uint64_t> ned(2);
        fi->get_non_empty_domain(fid, did, &ned[0]);
        std::vector<int64_t> v{ static_cast<int64_t>(ned[0]), static_cast<int64_t>(ned[1]) };
        return toInteger64(v);
    } else if (typestr == "INT32") {
        std::vector<int32_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
    } else if (typestr == "UINT32") {
        std::vector<uint32_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
    } else if (typestr == "INT16") {
        std::vector<int16_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
    } else if (typestr == "UINT16") {
        std::vector<uint16_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
    } else if (typestr == "INT8") {
        std::vector<int8_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
    } else if (typestr == "UINT8") {
        std::vector<uint8_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
    } else if (typestr == "FLOAT64") {
        std::vector<double> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
    } else if (typestr == "FLOAT32") {
        std::vector<float> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
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
        std::vector<int64_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return toInteger64(non_empty_dom);
    } else if (typestr == "DATETIME_NS") {
        std::vector<int64_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, did, &non_empty_dom[0]);
        return toNanotime(non_empty_dom);
    } else {
        Rcpp::stop("Currently unsupported tiledb domain type: '%s'", typestr.c_str());
        return NumericVector::create(NA_REAL, NA_REAL); // not reached
    }
}

// [[Rcpp::export]]
Rcpp::NumericVector libtiledb_fragment_info_get_non_empty_domain_name(XPtr<tiledb::FragmentInfo> fi,
                                                                      int32_t fid,
                                                                      const std::string& dim_name,
                                                                      const std::string& typestr) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    if (typestr == "INT64") {
        std::vector<int64_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return toInteger64(non_empty_dom);
    } else if (typestr == "UINT64") {
        std::vector<uint64_t> ned(2);
        fi->get_non_empty_domain(fid, dim_name, &ned[0]);
        std::vector<int64_t> v{ static_cast<int64_t>(ned[0]), static_cast<int64_t>(ned[1]) };
        return toInteger64(v);
    } else if (typestr == "INT32") {
        std::vector<int32_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
    } else if (typestr == "UINT32") {
        std::vector<uint32_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
    } else if (typestr == "INT16") {
        std::vector<int16_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
    } else if (typestr == "UINT16") {
        std::vector<uint16_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
  } else if (typestr == "INT8") {
        std::vector<int8_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
  } else if (typestr == "UINT8") {
        std::vector<uint8_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
  } else if (typestr == "FLOAT64") {
        std::vector<double> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
  } else if (typestr == "FLOAT32") {
        std::vector<float> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return NumericVector::create(non_empty_dom[0], non_empty_dom[1]);
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
        std::vector<int64_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return toInteger64(non_empty_dom);
    } else if (typestr == "DATETIME_NS") {
        std::vector<int64_t> non_empty_dom(2);
        fi->get_non_empty_domain(fid, dim_name, &non_empty_dom[0]);
        return toNanotime(non_empty_dom);
    } else {
        Rcpp::stop("Currently unsupported tiledb domain type: '%s'", typestr.c_str());
        return NumericVector::create(NA_REAL, NA_REAL); // not reached
    }
}

// [[Rcpp::export]]
Rcpp::CharacterVector
libtiledb_fragment_info_get_non_empty_domain_var_index(XPtr<tiledb::FragmentInfo> fi,
                                                       int32_t fid, int32_t did) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    auto sp = fi->non_empty_domain_var(static_cast<uint32_t>(fid), static_cast<uint32_t>(did));
    return CharacterVector::create(sp.first, sp.second);
}

// [[Rcpp::export]]
Rcpp::CharacterVector
libtiledb_fragment_info_get_non_empty_domain_var_name(XPtr<tiledb::FragmentInfo> fi,
                                                      int32_t fid,
                                                      const std::string& dim_name) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    auto sp = fi->non_empty_domain_var(static_cast<uint32_t>(fid), dim_name);
    return CharacterVector::create(sp.first, sp.second);
}

// [[Rcpp::export]]
double libtiledb_fragment_info_num(XPtr<tiledb::FragmentInfo> fi) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return static_cast<double>(fi->fragment_num());
}

// [[Rcpp::export]]
double libtiledb_fragment_info_size(XPtr<tiledb::FragmentInfo> fi, int32_t fid) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return static_cast<double>(fi->fragment_size(static_cast<uint32_t>(fid)));
}

// [[Rcpp::export]]
bool libtiledb_fragment_info_dense(XPtr<tiledb::FragmentInfo> fi, int32_t fid) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return fi->dense(static_cast<uint32_t>(fid));
}

// [[Rcpp::export]]
bool libtiledb_fragment_info_sparse(XPtr<tiledb::FragmentInfo> fi, int32_t fid) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return fi->sparse(static_cast<uint32_t>(fid));
}

// [[Rcpp::export]]
Rcpp::DatetimeVector
libtiledb_fragment_info_timestamp_range(XPtr<tiledb::FragmentInfo> fi, int32_t fid) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    auto range = fi->timestamp_range(static_cast<uint32_t>(fid));
    return Rcpp::DatetimeVector::create(range.first/1000.0, range.second/1000.0);
}

// [[Rcpp::export]]
double libtiledb_fragment_info_cell_num(XPtr<tiledb::FragmentInfo> fi, int32_t fid) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return static_cast<double>(fi->cell_num(static_cast<uint32_t>(fid)));
}

// [[Rcpp::export]]
int libtiledb_fragment_info_version(XPtr<tiledb::FragmentInfo> fi, int32_t fid) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return static_cast<int>(fi->version(static_cast<uint32_t>(fid)));
}

// [[Rcpp::export]]
bool libtiledb_fragment_info_has_consolidated_metadata(XPtr<tiledb::FragmentInfo> fi, int32_t fid) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return fi->has_consolidated_metadata(static_cast<uint32_t>(fid));
}

// [[Rcpp::export]]
double libtiledb_fragment_info_unconsolidated_metadata_num(XPtr<tiledb::FragmentInfo> fi) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return static_cast<double>(fi->unconsolidated_metadata_num());
}

// [[Rcpp::export]]
double libtiledb_fragment_info_to_vacuum_num(XPtr<tiledb::FragmentInfo> fi) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return static_cast<double>(fi->to_vacuum_num());
}

// [[Rcpp::export]]
std::string libtiledb_fragment_info_to_vacuum_uri(XPtr<tiledb::FragmentInfo> fi, int32_t fid) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return fi->to_vacuum_uri(static_cast<uint32_t>(fid));
}

// [[Rcpp::export]]
void libtiledb_fragment_info_dump(XPtr<tiledb::FragmentInfo> fi) {
    check_xptr_tag<tiledb::FragmentInfo>(fi);
    return fi->dump();
}

// [[Rcpp::export]]
std::string libtiledb_error_message(XPtr<tiledb::Context> ctx) {
    check_xptr_tag<tiledb::Context>(ctx);
    tiledb::Error error(*ctx.get());
    std::string txt(error.error_message());
    return txt;
}



/**
 * Groups
 */
// [[Rcpp::export]]
XPtr<tiledb::Group> libtiledb_group(XPtr<tiledb::Context> ctx,
                                    const std::string& uri,
                                    const std::string& querytypestr) {
    check_xptr_tag<tiledb::Context>(ctx);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    tiledb_query_type_t querytype = _string_to_tiledb_query_type(querytypestr);
    auto p = new tiledb::Group(*ctx.get(), uri, querytype);
    XPtr<tiledb::Group> ptr = make_xptr<tiledb::Group>(p);
#else
    XPtr<tiledb::Group> ptr(new tiledb::Group()); // placeholder
#endif
    return ptr;
}

// Interfaces to R are C-based so we cannot overload just on signature
// [[Rcpp::export]]
XPtr<tiledb::Group> libtiledb_group_with_config(XPtr<tiledb::Context> ctx,
                                                const std::string& uri,
                                                const std::string& querytypestr,
                                                XPtr<tiledb::Config> cfg) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Config>(cfg);
    tiledb_query_type_t querytype = _string_to_tiledb_query_type(querytypestr);
#if TILEDB_VERSION >= TileDB_Version(2,15,1)
    auto p = new tiledb::Group(*ctx.get(), uri, querytype, *cfg.get());
    XPtr<tiledb::Group> ptr = make_xptr<tiledb::Group>(p);
#elif TILEDB_VERSION >= TileDB_Version(2,8,0)
    Rcpp::warning("libtiledb_group_with_config should only called with TileDB 2.15.1 or later");
    auto p = new tiledb::Group(*ctx.get(), uri, querytype); // placeholder
    XPtr<tiledb::Group> ptr = make_xptr<tiledb::Group>(p);
#else
    XPtr<tiledb::Group> ptr(new tiledb::Group()); // placeholder
#endif
    return ptr;
}


// [[Rcpp::export]]
XPtr<tiledb::Group> libtiledb_group_open(XPtr<tiledb::Group> grp,
                                         const std::string& querytypestr) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    tiledb_query_type_t querytype = _string_to_tiledb_query_type(querytypestr);
    grp->open(querytype);
#endif
    return grp;
}

// [[Rcpp::export]]
XPtr<tiledb::Group> libtiledb_group_set_config(XPtr<tiledb::Group> grp, XPtr<tiledb::Config> cfg) {
    check_xptr_tag<tiledb::Group>(grp);
    check_xptr_tag<tiledb::Config>(cfg);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    grp->set_config(*cfg.get());
#endif
    return grp;
}

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_group_get_config(XPtr<tiledb::Group> grp) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    auto ptr = make_xptr<tiledb::Config>(new tiledb::Config(grp.get()->config()));
    return ptr;
#else
    return make_xptr<tiledb::Config>(new tiledb::Config());
#endif
}

// [[Rcpp::export]]
XPtr<tiledb::Group> libtiledb_group_close(XPtr<tiledb::Group> grp) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    grp->close();
#endif
    return grp;
}

// [[Rcpp::export]]
std::string libtiledb_group_create(XPtr<tiledb::Context> ctx, const std::string& uri) {
    check_xptr_tag<tiledb::Context>(ctx);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    tiledb::Group::create(*ctx.get(), uri);
#endif
    return uri;
}

// [[Rcpp::export]]
bool libtiledb_group_is_open(XPtr<tiledb::Group> grp) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    return grp->is_open();
#else
    return FALSE;
#endif
}

// [[Rcpp::export]]
std::string libtiledb_group_uri(XPtr<tiledb::Group> grp) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    return grp->uri();
#else
    return std::string("");
#endif
}

// [[Rcpp::export]]
std::string libtiledb_group_query_type(XPtr<tiledb::Group> grp) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    return _tiledb_query_type_to_string(grp->query_type());
#else
    return std::string("");
#endif
}

// [[Rcpp::export]]
bool libtiledb_group_put_metadata(XPtr<tiledb::Group> grp, std::string key, SEXP obj) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    // we implement a simpler interface here as the 'type' is given from
    // the supplied SEXP, as is the extent
    switch(TYPEOF(obj)) {
    case VECSXP: {
        Rcpp::stop("List objects are not supported.");
        break;// not reached
        }
    case REALSXP: {
        Rcpp::NumericVector v(obj);
        grp->put_metadata(key, TILEDB_FLOAT64, v.size(), v.begin());
        break;
    }
    case INTSXP: {
        Rcpp::IntegerVector v(obj);
        grp->put_metadata(key, TILEDB_INT32, v.size(), v.begin());
        break;
    }
    case STRSXP: {
        Rcpp::CharacterVector v(obj);
        std::string s(v[0]);
        // We use TILEDB_CHAR interchangeably with TILEDB_STRING_ASCII is this best string type?
        grp->put_metadata(key, TILEDB_STRING_ASCII, s.length(), s.c_str());
        break;
    }
    case LGLSXP: {              // experimental: map R logical (ie TRUE, FALSE, NA) to int8
        Rcpp::stop("Logical vector objects are not supported.");
        break;// not reached
    }
    default: {
        Rcpp::stop("No support (yet) for type '%d'.", TYPEOF(obj));
        break; // not reached
    }
    }
#endif
    return true;
}

// [[Rcpp::export]]
XPtr<tiledb::Group> libtiledb_group_delete_metadata(XPtr<tiledb::Group> grp, std::string key) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    grp->delete_metadata(key);
#endif
    return grp;
}

// [[Rcpp::export]]
SEXP libtiledb_group_get_metadata(XPtr<tiledb::Group> grp, std::string key) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    tiledb_datatype_t v_type;
    uint32_t v_num;
    const void* v;
    grp->get_metadata(key, &v_type, &v_num, &v);
    if (v == NULL) {
        return R_NilValue;
    }
    RObject vec = _metadata_to_sexp(v_type, v_num, v);
    vec.attr("key") = Rcpp::CharacterVector::create(key);
    return vec;
#else
    return R_NilValue;
#endif
}

// [[Rcpp::export]]
bool libtiledb_group_has_metadata(XPtr<tiledb::Group> grp, std::string key) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    tiledb_datatype_t value_type; // set by C++ API on return, not returned to R
    return grp->has_metadata(key, &value_type);
#else
    return false;
#endif
}

// [[Rcpp::export]]
double libtiledb_group_metadata_num(XPtr<tiledb::Group> grp) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    return grp->metadata_num();
#else
    return 0;
#endif
}

// [[Rcpp::export]]
SEXP libtiledb_group_get_metadata_from_index(XPtr<tiledb::Group> grp, int idx) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    std::string key;
    tiledb_datatype_t v_type;
    uint32_t v_num;
    const void* v;
    grp->get_metadata_from_index(static_cast<uint64_t>(idx), &key, &v_type, &v_num, &v);
    if (v == NULL) {
        return R_NilValue;
    }
    RObject vec = _metadata_to_sexp(v_type, v_num, v);
    vec.attr("key") = Rcpp::CharacterVector::create(key);
    return vec;
#else
    return R_NilValue;
#endif
}

// [[Rcpp::export]]
XPtr<tiledb::Group> libtiledb_group_add_member(XPtr<tiledb::Group> grp,
                                               std::string uri, bool relative,
                                               Nullable<Rcpp::String> optional_name = R_NilValue) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    if (optional_name.isNotNull()) {
        Rcpp::String string_name(optional_name);
        std::string name(string_name);
        grp->add_member(uri, relative, name);
    } else {
        grp->add_member(uri, relative);
    }
#endif
    return grp;
}

// [[Rcpp::export]]
XPtr<tiledb::Group> libtiledb_group_remove_member(XPtr<tiledb::Group> grp, std::string uri) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    grp->remove_member(uri);
#endif
    return grp;
}

// [[Rcpp::export]]
double libtiledb_group_member_count(XPtr<tiledb::Group> grp) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    return grp->member_count();
#else
    return 0;
#endif
}

// [[Rcpp::export]]
CharacterVector libtiledb_group_member(XPtr<tiledb::Group> grp, int idx) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    tiledb::Object obj = grp->member(idx);
    CharacterVector v = CharacterVector::create(_object_type_to_string(obj.type()), obj.uri(), obj.name().value_or(""));
#else
    CharacterVector v = CharacterVector::create("", "", "");
#endif
    return v;
}

// [[Rcpp::export]]
std::string libtiledb_group_dump(XPtr<tiledb::Group> grp, bool recursive) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,8,0)
    return grp->dump(recursive);
#else
    return std::string("");
#endif
}

// [[Rcpp::export]]
bool libtiledb_group_is_relative(XPtr<tiledb::Group> grp, const std::string &name) {
    check_xptr_tag<tiledb::Group>(grp);
#if TILEDB_VERSION >= TileDB_Version(2,12,0)
    return grp->is_relative(name);
#else
    return false;
#endif
}



/**
 * Filestore (via tiledb_experimental.h)
 */

// Creates array schema based on URL, or default schema if no URI provided
// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_filestore_schema_create(XPtr<tiledb::Context> ctx,
                                                            std::string uri) {
#if TILEDB_VERSION >= TileDB_Version(2,9,0)
    tiledb_ctx_t* ctx_ptr = ctx->ptr().get();
    tiledb_array_schema_t* schema_type_ptr;
    if (tiledb_filestore_schema_create(ctx_ptr,
                                       (uri == "" ? nullptr : uri.c_str()),
                                       &schema_type_ptr) == TILEDB_ERR) {
        Rcpp::stop("Error creating array schema from defaults");
    }
    auto schptr = new tiledb::ArraySchema(*ctx.get(), schema_type_ptr);
    auto schema = make_xptr<tiledb::ArraySchema>(schptr);
    return schema;
#else
    auto schptr = new tiledb::ArraySchema(*ctx.get(), TILEDB_SPARSE);
    auto schema = make_xptr<tiledb::ArraySchema>(schptr);
    return schema;
#endif
}

// Imports a file into a TileDB filestore array
// [[Rcpp::export]]
bool libtiledb_filestore_uri_import(XPtr<tiledb::Context> ctx,
                                    std::string filestore_uri,
                                    std::string file_uri) {
#if TILEDB_VERSION >= TileDB_Version(2,9,0)
    tiledb_ctx_t* ctx_ptr = ctx->ptr().get();
    if (tiledb_filestore_uri_import(ctx_ptr, filestore_uri.c_str(),
                                    file_uri.c_str(), TILEDB_MIME_AUTODETECT) == TILEDB_ERR) {
        Rcpp::stop("Error importing file into filestore");
        return false;           // not reached
    }
    return true;
#else
    return false;
#endif

}

// Export from a TileDB filestore array into a file uri
// [[Rcpp::export]]
bool libtiledb_filestore_uri_export(XPtr<tiledb::Context> ctx,
                                    std::string file_uri,
                                    std::string filestore_uri) {
#if TILEDB_VERSION >= TileDB_Version(2,9,0)
    tiledb_ctx_t* ctx_ptr = ctx->ptr().get();
    if (tiledb_filestore_uri_export(ctx_ptr, file_uri.c_str(), filestore_uri.c_str())  == TILEDB_ERR) {
        Rcpp::stop("Error exporting file from filestore");
        return false;           // not reached
    }
    return true;
#else
    return false;
#endif
}

// Write size bytes from buf into TileDB filestore
// [[Rcpp::export]]
bool libtiledb_filestore_buffer_import(XPtr<tiledb::Context> ctx,
                                       std::string filestore_uri,
                                       std::string buf, size_t size) {
#if TILEDB_VERSION >= TileDB_Version(2,9,0)
    tiledb_ctx_t* ctx_ptr = ctx->ptr().get();
    if (tiledb_filestore_buffer_import(ctx_ptr, filestore_uri.c_str(),
                                       static_cast<void*>(buf.data()), size,
                                       TILEDB_MIME_AUTODETECT) == TILEDB_ERR) {
        Rcpp::stop("Error importing file into filestore");
        return false;           // not reached
    }
    return true;
#else
    return false;
#endif
}

// Retrieve TileDB filestore content into buffer
// [[Rcpp::export]]
std::string libtiledb_filestore_buffer_export(XPtr<tiledb::Context> ctx,
                                              std::string filestore_uri,
                                              size_t offset, size_t size) {
    std::string buf("");
#if TILEDB_VERSION >= TileDB_Version(2,9,0)
    tiledb_ctx_t* ctx_ptr = ctx->ptr().get();
    buf.resize(size);
    if (tiledb_filestore_buffer_export(ctx_ptr, filestore_uri.c_str(), offset,
                                       static_cast<void*>(buf.data()), size) == TILEDB_ERR) {
        Rcpp::stop("Error exporting file from filestore");
    }
#endif
    return buf;

}

// Get size of uncompressed TileDB filestore array
// [[Rcpp::export]]
size_t libtiledb_filestore_size(XPtr<tiledb::Context> ctx, std::string filestore_uri) {
    size_t sz = 0;
#if TILEDB_VERSION >= TileDB_Version(2,9,0)
    tiledb_ctx_t* ctx_ptr = ctx->ptr().get();
    if (tiledb_filestore_size(ctx_ptr, filestore_uri.c_str(), &sz) == TILEDB_ERR) {
        Rcpp::stop("Error accessize filestore uri size");
    }
#endif
    return sz;
}

// Get MIME type of TileDB filestore as string
// [[Rcpp::export]]
std::string libtiledb_mime_type_to_str(int32_t mime_type) {
#if TILEDB_VERSION >= TileDB_Version(2,9,0)
    const char* ptr;
    if (tiledb_mime_type_to_str(static_cast<tiledb_mime_type_t>(mime_type),
                                &ptr) == TILEDB_ERR) {
        Rcpp::stop("Error converting mime type to string");
    }
    return std::string(ptr);
#else
    return std::string("");
#endif
}

// Create MIME type of TileDB filestore from string
// [[Rcpp::export]]
int32_t libtiledb_mime_type_from_str(std::string mime_type) {
#if TILEDB_VERSION >= TileDB_Version(2,9,0)
    tiledb_mime_type_t mt;
    if (tiledb_mime_type_from_str(mime_type.c_str(), &mt) == TILEDB_ERR) {
        Rcpp::stop("Error converting mime type from string");
    }
    return static_cast<int32_t>(mt);
#else
    return -1;
#endif
}
