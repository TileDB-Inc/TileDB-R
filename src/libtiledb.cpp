#include "libtiledb.h"

#include <fstream>
#include <unistd.h>
#include <Rcpp.h>
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
      return "FLOAT32";
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
    default:
      throw Rcpp::exception("unknown tiledb_datatype_t");
  }
}

tiledb_datatype_t _string_to_tiledb_datatype(std::string typestr) {
  if (typestr == "FLOAT32")  {
    return TILEDB_FLOAT32;
  } else if (typestr == "FLOAT64") {
    return TILEDB_FLOAT64;
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
  } else {
    std::stringstream errmsg;
    errmsg << "Unknown TileDB type \"" << typestr << "\"";
    throw Rcpp::exception(errmsg.str().c_str());
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
    default: {
      throw Rcpp::exception("unknown tiledb_datatype_t");
    }
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
    default:
        throw Rcpp::exception("unknown tiledb_datatype_t");
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
  } else {
    std::stringstream errmsg;
    errmsg << "Unknown TileDB layout \"" << lstr << "\"";
    throw Rcpp::exception(errmsg.str().c_str());
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
  } else {
    std::stringstream errmsg;
    errmsg << "Unknown TileDB filter \"" << filter << "\"";
    throw Rcpp::exception(errmsg.str().c_str());
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
    default: {
      throw Rcpp::exception("unknown tiledb_filter_t");
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
    std::stringstream errmsg;
    errmsg << "Unknown TileDB filter option \"" << filter_option << "\"";
    throw Rcpp::exception(errmsg.str().c_str());
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
  default: {
    throw Rcpp::exception("unknown tiledb_filter_option_t");
  }
  }
}

tiledb_query_type_t _string_to_tiledb_query_type(std::string qtstr) {
  if (qtstr == "READ") {
    return TILEDB_READ;
  } else if (qtstr == "WRITE") {
    return TILEDB_WRITE;
  } else {
    std::stringstream errmsg;
    errmsg << "Unknown TileDB query type \"" << qtstr << "\"";
    throw Rcpp::exception(errmsg.str().c_str());
  }
}

std::string _tiledb_query_type_to_string(tiledb_query_type_t qtype) {
  switch (qtype) {
    case TILEDB_READ:
      return "READ";
    case TILEDB_WRITE:
      return "WRITE";
    default:
      throw Rcpp::exception("unknown tiledb_query_type_t");
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
  return NumericVector::create(_["major"]=std::get<0>(ver),
                               _["minor"]=std::get<1>(ver),
                               _["patch"]=std::get<2>(ver));
}


/**
 * TileDB Context
 */

// [[Rcpp::export]]
XPtr<tiledb::Context> libtiledb_ctx(Nullable<XPtr<tiledb::Config>> config=R_NilValue) {
  if (config.isNull()) {
    return XPtr<tiledb::Context>(new tiledb::Context(), true);
  } else {
    XPtr<tiledb::Config> config_xptr(config);
    return XPtr<tiledb::Context>(new tiledb::Context(*config_xptr.get()), true);
  }
}


// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_ctx_config(XPtr<tiledb::Context> ctx) {
  return XPtr<tiledb::Config>(new tiledb::Config(ctx.get()->config()));
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
    std::stringstream errmsg;
    errmsg << "Unknown TileDB fs scheme: \"" << scheme << "://\"";
    throw Rcpp::exception(errmsg.str().c_str());
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
XPtr<tiledb::Config> libtiledb_config(Nullable<CharacterVector> config=R_NilValue) {
  XPtr<tiledb::Config> _config(new tiledb::Config(), true);
  if (config.isNotNull()) {
    auto config_vec = config.as();
    auto config_names = as<CharacterVector>(config_vec.names());
    for (auto &name : config_names) {
      auto param = as<std::string>(name);
      auto value = as<std::string>(config_vec[param]);
      _config->set(param, value);
    }
  }
  return _config;
}

// [[Rcpp::export]]
std::string libtiledb_config_save_to_file(XPtr<tiledb::Config> config, std::string filename) {
  config->save_to_file(filename);
  return filename;
}

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_config_load_from_file(std::string filename) {
  tiledb::Config* config = new tiledb::Config(filename);
  return XPtr<tiledb::Config>(config);
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
  const tiledb_datatype_t _type = _string_to_tiledb_datatype(type);
  if (_type != TILEDB_INT32 && _type != TILEDB_FLOAT64) {
    throw Rcpp::exception("only integer (INT32), and real (FLOAT64) domains are supported");
  }
  // check that the dimension type aligns with the domain and tiledb_extent type
  if (_type == TILEDB_INT32 && (TYPEOF(domain) != INTSXP || TYPEOF(tile_extent) != INTSXP)) {
    throw Rcpp::exception("domain or tile_extent does not match dimension type");
  } else if (_type == TILEDB_FLOAT64 && (TYPEOF(domain) != REALSXP || TYPEOF(tile_extent) != REALSXP)) {
    throw Rcpp::exception("domain or tile_extent does not match dimenson type");
  }
  try {
    if (_type == TILEDB_INT32) {
      using Dtype = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
      auto domain_vec = as<IntegerVector>(domain);
      if (domain_vec.length() != 2) {
        throw Rcpp::exception("dimension domain must be a c(lower bound, upper bound) pair");
      }
      auto tile_extent_vec = as<IntegerVector>(tile_extent);
      if (tile_extent_vec.length() != 1) {
        throw Rcpp::exception("tile_extent must be a scalar");
      }
      std::array<Dtype, 2> _domain = {domain_vec[0], domain_vec[1]};
      std::array<Dtype, 1> _tile_extent = {tile_extent_vec[0]};
      return XPtr<tiledb::Dimension>(
        new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0])));
    } else if (_type == TILEDB_FLOAT64) {
      using Dtype = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
      auto domain_vec = as<NumericVector>(domain);
      if (domain_vec.length() != 2) {
        throw Rcpp::exception("dimension domain must be a c(lower bound, upper bound) pair");
      }
      auto tile_extent_vec = as<NumericVector>(tile_extent);
      if (tile_extent_vec.length() != 1) {
        throw Rcpp::exception("tile_extent must be a scalar");
      }
      std::array<Dtype, 2> _domain = {domain_vec[0], domain_vec[1]};
      std::array<Dtype, 1> _tile_extent = {tile_extent_vec[0]};
      return XPtr<tiledb::Dimension>(
        new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0])));
    } else {
      std::stringstream errmsg;
      errmsg << "Unsupported tiledb type (id): " << _type << " this should not happen!";
      throw Rcpp::exception(errmsg.str().c_str());
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
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
        throw Rcpp::exception("tiledb_dim domain FLOAT64 value not representable as an R double");
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
        throw Rcpp::exception("tiledb_dim domain INT32 value not representable as an R integer");
      }
      return IntegerVector({d1, d2});
    }
    case TILEDB_UINT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT32>::type;
      auto d1 = dim->domain<DataType>().first;
      auto d2 = dim->domain<DataType>().second;
      if (d1 > std::numeric_limits<int32_t>::max() ||
          d2 > std::numeric_limits<int32_t>::max()) {
        throw Rcpp::exception("tiledb_dim domain UINT32 value not representable as an R integer");
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
        throw Rcpp::exception("tiledb_dim domain INT64 value not representable as an R integer");
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
        throw Rcpp::exception("tiledb_dim domain UINT64 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(d1),
                            static_cast<int32_t>(d2)});
    }
    default:
      throw Rcpp::exception("invalid tiledb_dim domain type");
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
        throw Rcpp::exception("tiledb_dim tile FLOAT64 value not representable as an R double");
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
        throw Rcpp::exception("tiledb_dim tile INT32 value not representable as an R integer");
      }
      return IntegerVector({t,});
    }
    case TILEDB_UINT32: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT32>::type;
      auto t = dim->tile_extent<DataType>();
      if (t > std::numeric_limits<int32_t>::max()) {
        throw Rcpp::exception("tiledb_dim tile UINT32 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(t),});
    }
    case TILEDB_INT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_INT64>::type;
      auto t = dim->tile_extent<DataType>();
      if (t <= R_NaInt || t > std::numeric_limits<int32_t>::max()) {
        throw Rcpp::exception("tiledb_dim tile INT64 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(t),});
    }
    case TILEDB_UINT64: {
      using DataType = tiledb::impl::tiledb_to_type<TILEDB_UINT64>::type;
      auto t = dim->tile_extent<DataType>();
      if (t > std::numeric_limits<int32_t>::max()) {
        throw Rcpp::exception("tiledb_dim tile UINT64 value not representable as an R integer");
      }
      return IntegerVector({static_cast<int32_t>(t),});
    }
    default:
      throw Rcpp::exception("invalid tiledb_dim domain type");
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
    throw Rcpp::exception("invalid tiledb_dim domain");
  }
  double domain_lb = domain[0];
  double domain_ub = domain[1];
  auto sub0 = subscript[0];
  if (sub0 == R_NaReal) {
    throw Rcpp::exception("NA subscript not supported");
  }
  if (sub0 < domain_lb || sub0 > domain_ub) {
    throw Rcpp::exception("subscript out of domain bounds");
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
      throw Rcpp::exception("NA subscripting not supported");
    }
    if (high < domain_lb || high > domain_ub) {
      std::stringstream errmsg;
      errmsg << "subscript out of domain bounds: (at index: [" << i << "] "
             << high << " < " << domain_lb;
      throw Rcpp::exception(errmsg.str().c_str());
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


/**
 * TileDB Domain
 */
// [[Rcpp::export]]
XPtr<tiledb::Domain> libtiledb_domain(XPtr<tiledb::Context> ctx, List dims) {
  R_xlen_t ndims = dims.length();
  if (ndims == 0) {
    throw Rcpp::exception("domain must have one or more dimensions");
  }
  for (R_xlen_t i=0; i < ndims; i++) {
    SEXP d = dims[i];
    if (TYPEOF(d) != EXTPTRSXP) {
      std::stringstream errmsg;
      errmsg << "Invalid tiledb_dim object at index " <<  i << " (type " << Rcpp::type2name(d) << ")";
      throw Rcpp::exception(errmsg.str().c_str());
    }
  }
  XPtr<tiledb::Domain> domain(new tiledb::Domain(*ctx.get()));
  try {
    for (auto& val : dims) {
      // TODO: we can't do much type checking for the cast here until we wrap EXTPTRSXP in S4 classes
      auto dim = as<XPtr<tiledb::Dimension>>(val);
      domain->add_dimension(*dim.get());
    }
    return domain;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
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
    throw Rcpp::exception("tiledb::Domain rank is not representable by an R integer");
  }
  return static_cast<int32_t>(rank);
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
void libtiledb_domain_dump(XPtr<tiledb::Domain> domain) {
  domain->dump();
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
        std::stringstream errmsg;
        errmsg << "Invalid filter object at index " <<  i << " (type " << Rcpp::type2name(filter) << ")";
        throw Rcpp::exception(errmsg.str().c_str());
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
                                            int ncells) {
  tiledb_datatype_t attr_dtype = _string_to_tiledb_datatype(type);
  if (ncells < 1) {
    throw Rcpp::exception("ncells must be >= 1");
  }
  if (attr_dtype == TILEDB_INT32) {
    using DType = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
    auto attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)));
    attr->set_filter_list(*filter_list);
    return attr;
  } else if (attr_dtype == TILEDB_FLOAT64) {
    using DType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
    auto attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)));
    attr->set_filter_list(*filter_list);
    return attr;
  } else if (attr_dtype == TILEDB_CHAR) {
    using DType = tiledb::impl::tiledb_to_type<TILEDB_CHAR>::type;
    auto attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)));
    attr->set_filter_list(*filter_list);
    return attr;
  } else {
    throw Rcpp::exception("only integer (INT32), logical (INT32), real (FLOAT64) and character (CHAR) attributes are supported");
  }
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

//[[Rcpp::export]]
bool libtiledb_attribute_is_variable_sized(XPtr<tiledb::Attribute> attr) {
  return attr->variable_sized();
}

//[[Rcpp::export]]
void libtiledb_attribute_dump(XPtr<tiledb::Attribute> attr) {
  attr->dump();
}


/**
 * TileDB Array Schema
 */
//[[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema(
    XPtr<tiledb::Context> ctx,
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
    throw Rcpp::exception("libtiledb_array_schema requires one or more attributes");
  }
  for (R_xlen_t i=0; i < nattr; i++)  {
    SEXP attr = attributes[i];
    if (TYPEOF(attr) != EXTPTRSXP) {
      std::stringstream errmsg;
      errmsg << "Invalid attribute object at index " <<  i << " (type " << Rcpp::type2name(attr) << ")";
      throw Rcpp::exception(errmsg.str().c_str());
    }
  }
  auto _cell_order = _string_to_tiledb_layout(cell_order);
  auto _tile_order = _string_to_tiledb_layout(tile_order);
  auto schema = XPtr<tiledb::ArraySchema>(
    new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(), sparse ? TILEDB_SPARSE : TILEDB_DENSE)));
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
  return XPtr<tiledb::ArraySchema>(new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(), at)));
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_load(XPtr<tiledb::Context> ctx, std::string uri) {
  return XPtr<tiledb::ArraySchema>(new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(), uri)));
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_load_with_key(XPtr<tiledb::Context> ctx,
                                                               std::string uri,
                                                               std::string key) {
  return XPtr<tiledb::ArraySchema>(
      new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(), uri,
                              TILEDB_AES_256_GCM, key.data(), (uint32_t) key.size())));
}

// [[Rcpp::export]]
void libtiledb_array_schema_set_domain(XPtr<tiledb::ArraySchema> schema,
                                       XPtr<tiledb::Domain> dom) {
  schema->set_domain(*dom);
}

// [[Rcpp::export]]
XPtr<tiledb::Domain> libtiledb_array_schema_get_domain(XPtr<tiledb::ArraySchema> schema) {
  return XPtr<tiledb::Domain>(new tiledb::Domain(schema->domain()));
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
XPtr<tiledb::FilterList> libtiledb_array_schema_get_coords_filter_list(XPtr<tiledb::ArraySchema> schema) {
  return XPtr<tiledb::FilterList>(new tiledb::FilterList(schema->coords_filter_list()));
}

// [[Rcpp::export]]
XPtr<tiledb::FilterList> libtiledb_array_schema_offsets_filter_list(XPtr<tiledb::ArraySchema> schema) {
  return XPtr<tiledb::FilterList>(new tiledb::FilterList(schema->offsets_filter_list()));
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
void libtiledb_array_schema_check(XPtr<tiledb::ArraySchema> schema) {
  schema->check();   // throws, rather than returning bool
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
XPtr<tiledb::Array> libtiledb_array_open(XPtr<tiledb::Context> ctx, std::string uri, std::string type) {
  auto query_type = _string_to_tiledb_query_type(type);
  auto array = XPtr<tiledb::Array>(new tiledb::Array(tiledb::Array(*ctx.get(), uri, query_type)));
  return array;
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open_with_key(XPtr<tiledb::Context> ctx, std::string uri, std::string type,
                                                  std::string enc_key) {
  auto query_type = _string_to_tiledb_query_type(type);
  auto array = XPtr<tiledb::Array>(new tiledb::Array(tiledb::Array(*ctx.get(), uri, query_type,
                                                                   TILEDB_AES_256_GCM,
                                                                   enc_key.data(),
                                                                   (uint32_t)enc_key.size())));
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
  return array->is_open() && array->query_type() != TILEDB_READ;
}

// [[Rcpp::export]]
bool libtiledb_array_is_open_for_writing(XPtr<tiledb::Array> array) {
  return array->is_open() && array->query_type() != TILEDB_WRITE;
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
List libtiledb_array_nonempty_domain(XPtr<tiledb::Array> array) {
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
    throw Rcpp::exception("Invalid tiledb_schema domain type");
  }
  return nonempty_domain;
}

// [[Rcpp::export]]
std::string libtiledb_array_consolidate(XPtr<tiledb::Context> ctx,
                                     std::string uri) {
  tiledb::Array::consolidate(*ctx.get(), uri);
  return uri;
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
    throw Rcpp::exception("invalid subarray datatype");
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_coordinates(XPtr<tiledb::Query> query,
                                                    SEXP coords) {
  if (TYPEOF(coords) == INTSXP) {
    IntegerVector sub(coords);
    query->set_coordinates(sub.begin(), sub.length());
    return query;
  } else if (TYPEOF(coords) == REALSXP) {
    NumericVector sub(coords);
    query->set_coordinates(sub.begin(), sub.length());
    return query;
  } else {
    throw Rcpp::exception("invalid subarray datatype");
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
    std::stringstream errmsg;
    errmsg << "Invalid attribute buffer type for attribute "
           << "\""<< attr << "\": " << Rcpp::type2name(buffer);
    throw Rcpp::exception(errmsg.str().c_str());
  }
}

// -- vlc_buf_t functions below

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
    buf->cols = sub[3] - sub[2] + 1;
  } else if (TYPEOF(subarray) == REALSXP) {
    auto sub = as<std::vector<double>>(subarray);
    auto max_elements = array->max_buffer_elements(sub);
    buf->offsets.resize(szoffsets <= 0 ? max_elements[attribute].first : szoffsets);
    buf->str.resize(szdata <= 0 ? max_elements[attribute].second : szdata);
    buf->rows = sub[1] - sub[0] + 1;
    buf->cols = sub[3] - sub[2] + 1;
  } else {
    Rcpp::stop("Invalid subarray buffer type for domain: '%s'", Rcpp::type2name(subarray));
  }
  return buf;
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
  return(bufptr);
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_buffer_var_char(XPtr<tiledb::Query> query,
                                                        std::string attr,
                                                        XPtr<vlc_buf_t> bufptr) {
  query->set_buffer(attr, bufptr->offsets, bufptr->str);
  return query;
}

// [[Rcpp::export]]
CharacterMatrix libtiledb_query_get_buffer_var_char(XPtr<vlc_buf_t> bufptr) {
  size_t n = bufptr->offsets.size();
  std::vector<uint64_t> str_sizes(n);
  for (size_t i = 0; i < n - 1; i++) {                          // all but last
    str_sizes[i] = bufptr->offsets[i + 1] - bufptr->offsets[i];
  }                                                             // last is total size minus last start
  str_sizes[n-1] = bufptr->str.size() * sizeof(char) - bufptr->offsets[n-1];

  // Get the strings
  CharacterMatrix mat(bufptr->rows, bufptr->cols);
  for (size_t i = 0; i < n; i++) {
    mat[i] = std::string(&bufptr->str[bufptr->offsets[i]], str_sizes[i]);
  }
  return(mat);
}

// -- vlv_buf_t functions below

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



// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_submit(XPtr<tiledb::Query> query) {
  query->submit();
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
XPtr<tiledb::Query> libtiledb_query_add_range(XPtr<tiledb::Query> query, int iidx,
                                              SEXP starts, SEXP ends,
                                              SEXP strides=R_NilValue) {
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
  } else {
    Rcpp::stop("Invalid data type for query range: '%s'", Rcpp::type2name(starts));
  }
  return query;
}

// [[Rcpp::export]]
R_xlen_t libtiledb_query_get_est_result_size(XPtr<tiledb::Query> query, std::string attr) {
  uint64_t est = query->est_result_size(attr);
  return static_cast<R_xlen_t>(est);
}

// [[Rcpp::export]]
NumericVector libtiledb_query_get_est_result_size_var(XPtr<tiledb::Query> query, std::string attr) {
  std::pair<uint64_t, uint64_t> est = query->est_result_size_var(attr);
  return NumericVector::create(static_cast<R_xlen_t>(est.first), static_cast<R_xlen_t>(est.second));
}

/**
 * Array helper functions
 */
// [[Rcpp::export]]
NumericVector libtiledb_zip_coords_numeric( List coords, R_xlen_t coord_length) {
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
IntegerVector libtiledb_zip_coords_integer( List coords, R_xlen_t coord_length) {
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

// [[Rcpp::export]]
std::string libtiledb_coords() {
  return tiledb_coords();
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
    std::stringstream errmsg;
    errmsg << "Invalid subarray buffer type for domain :"
           << Rcpp::type2name(subarray);
    throw Rcpp::exception(errmsg.str().c_str());
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
    std::stringstream errmsg;
    errmsg << "Invalid subarray buffer type for domain :"
           << Rcpp::type2name(subarray);
    throw Rcpp::exception(errmsg.str().c_str());
  }
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
    throw Rcpp::exception("invalid object type string");
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
      throw Rcpp::exception("invalid recursive walk order, must be \"PREORDER\" or \"POSTORDER\"");
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
XPtr<tiledb::VFS> tiledb_vfs(XPtr<tiledb::Context> ctx,
                             Nullable<XPtr<tiledb::Config>> config=R_NilValue) {
  if (config.isNull()) {
    return XPtr<tiledb::VFS>(new tiledb::VFS(*ctx.get()));
  } else {
    XPtr<tiledb::Config> config_xptr(config);
    return XPtr<tiledb::VFS>(new tiledb::VFS(*ctx.get(), *config_xptr.get()));
  }
}

// [[Rcpp::export]]
std::string tiledb_vfs_create_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->create_bucket(uri);
  return uri;
}

// [[Rcpp::export]]
std::string tiledb_vfs_remove_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->remove_bucket(uri);
  return uri;
}

// [[Rcpp::export]]
bool tiledb_vfs_is_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  return vfs->is_bucket(uri);
}

// [[Rcpp::export]]
bool tiledb_vfs_is_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  return vfs->is_empty_bucket(uri);
}

// [[Rcpp::export]]
std::string tiledb_vfs_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->empty_bucket(uri);
  return uri;
}

// [[Rcpp::export]]
std::string tiledb_vfs_create_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->create_dir(uri);
  return uri;
}

// [[Rcpp::export]]
bool tiledb_vfs_is_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  return vfs->is_dir(uri);
}

// [[Rcpp::export]]
std::string tiledb_vfs_remove_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->remove_dir(uri);
  return uri;
}

// [[Rcpp::export]]
bool tiledb_vfs_is_file(XPtr<tiledb::VFS> vfs, std::string uri) {
  return vfs->is_file(uri);
}

// [[Rcpp::export]]
std::string tiledb_vfs_remove_file(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->remove_file(uri);
  return uri;
}

// [[Rcpp::export]]
R_xlen_t tiledb_vfs_file_size(XPtr<tiledb::VFS> vfs, std::string uri) {
  uint64_t size = vfs->file_size(uri);
  if (size > std::numeric_limits<R_xlen_t>::max()) {
    throw Rcpp::exception("file size is greater than maximum R integer");
  }
  return static_cast<R_xlen_t>(size);
}

// [[Rcpp::export]]
std::string tiledb_vfs_move_file(XPtr<tiledb::VFS> vfs,
                                 std::string old_uri,
                                 std::string new_uri) {
  vfs->move_file(old_uri, new_uri);
  return new_uri;
}

// [[Rcpp::export]]
std::string tiledb_vfs_move_dir(XPtr<tiledb::VFS> vfs,
                                std::string old_uri,
                                std::string new_uri) {
  vfs->move_dir(old_uri, new_uri);
  return new_uri;
}

// [[Rcpp::export]]
std::string tiledb_vfs_touch(XPtr<tiledb::VFS> vfs, std::string uri) {
  vfs->touch(uri);
  return uri;
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
void libtiledb_stats_dump(std::string path) {
  FILE* fptr = nullptr;
  fptr = fopen(path.c_str(), "w");
  if (fptr == nullptr) {
    throw Rcpp::exception("error opening stats dump file for writing");
  }
  tiledb::Stats::dump(fptr);
  fclose(fptr);
}

// [[Rcpp::export]]
void libtiledb_stats_print() {
  // TODO: look up the proper way to do this in R
  // Done -- at least in a first pass
  // get a temporary filename from the per-session directory R uses
  Rcpp::Function rfunc("tempfile");
  std::string filename = Rcpp::as<std::string>(rfunc());
  // dump to the file
  libtiledb_stats_dump(filename);

  // and read and print from the file
  std::ifstream f(filename);
  std::string line;
  if (f.is_open()) {
    while (getline(f, line)) {
      Rprintf("%s\n", line.c_str());
    }
    f.close();
  }
  // remove tempfile (though R would too at end of session)
  if (unlink(filename.c_str()) == -1) {
    Rcpp::stop("Error removing temporary file ", filename);
  }
}
