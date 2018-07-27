#include "libtiledb.h"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(unwindProtect)]]
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

tiledb_compressor_t _string_to_tiledb_compressor(std::string compr) {
  if (compr == "NO_COMPRESSION") {
    return TILEDB_NO_COMPRESSION;
  } else if (compr == "GZIP") {
    return TILEDB_GZIP;
  } else if (compr == "ZSTD") {
    return TILEDB_ZSTD;
  } else if (compr == "LZ4") {
    return TILEDB_LZ4;
  } else if (compr == "BLOSC_LZ") {
    return TILEDB_BLOSC_LZ;
  } else if (compr == "BLOSC_LZ4") {
    return TILEDB_BLOSC_LZ4;
  } else if (compr == "BLOSC_LZ4HC") {
    return TILEDB_BLOSC_LZ4HC;
  } else if (compr == "BLOSC_SNAPPY") {
    return TILEDB_BLOSC_SNAPPY;
  } else if (compr == "BLOSC_ZLIB") {
    return TILEDB_BLOSC_ZLIB;
  } else if (compr == "BLOSC_ZSTD") {
    return TILEDB_BLOSC_ZSTD;
  } else if (compr == "RLE") {
    return TILEDB_RLE;
  } else if (compr == "BZIP2") {
    return TILEDB_BZIP2;
  } else if (compr == "DOUBLE_DELTA") {
    return TILEDB_DOUBLE_DELTA;
  } else {
    std::stringstream errmsg;
    errmsg << "Unknown TileDB compressor \"" << compr << "\"";
    throw Rcpp::exception(errmsg.str().c_str());
  }
}

const char* _tiledb_compresssor_to_string(tiledb_compressor_t compr) {
  switch(compr) {
    case TILEDB_NO_COMPRESSION:
     return "NO_COMPRESSION";
    case TILEDB_GZIP:
      return "GZIP";
    case TILEDB_ZSTD:
      return "ZSTD";
    case TILEDB_LZ4:
      return "LZ4";
    case TILEDB_BLOSC_LZ:
      return "BLOSC_LZ";
    case TILEDB_BLOSC_LZ4:
      return "BLOSC_LZ4";
    case TILEDB_BLOSC_LZ4HC:
      return "BLOSC_LZ4HC";
    case TILEDB_BLOSC_SNAPPY:
      return "BLOSC_SNAPPY";
    case TILEDB_BLOSC_ZLIB:
      return "BLOSC_ZLIB";
    case TILEDB_BLOSC_ZSTD:
      return "BLOSC_ZSTD";
    case TILEDB_RLE:
      return "RLE";
    case TILEDB_BZIP2:
      return "BZIP2";
    case TILEDB_DOUBLE_DELTA:
      return "DOUBLE_DELTA";
    default: {
      throw Rcpp::exception("unknown tiledb_compressor_t");
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

// [[Rcpp::export]]
NumericVector libtiledb_version() {
  try {
    auto ver = tiledb::version();
    return NumericVector::create(_["major"]=std::get<0>(ver),
                                 _["minor"]=std::get<1>(ver),
                                 _["patch"]=std::get<2>(ver));
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Context> libtiledb_ctx(Nullable<XPtr<tiledb::Config>> config=R_NilValue) {
  try {
    if (config.isNull()) {
      return XPtr<tiledb::Context>(new tiledb::Context(), true);
    } else {
      XPtr<tiledb::Config> config_xptr(config);
      return XPtr<tiledb::Context>(new tiledb::Context(*config_xptr.get()), true);
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_config_save_to_file(XPtr<tiledb::Config> config, std::string filename) {
  try {
    config->save_to_file(filename);
    return filename;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Config> libtiledb_config_load_from_file(std::string filename) {
  try {
    tiledb::Config* config = new tiledb::Config(filename);
    return XPtr<tiledb::Config>(config);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
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
XPtr<tiledb::Config> libtiledb_config(Nullable<CharacterVector> config=R_NilValue) {
  try {
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
  } catch (tiledb::TileDBError& err) {
    throw  Rcpp::exception(err.what());
  }
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
  try {
    (*config)[param] = value;
    return config;
  } catch (tiledb::TileDBError& err)  {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
CharacterVector libtiledb_config_get(XPtr<tiledb::Config> config,
                                     CharacterVector params) {
  try {
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
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
void libtiledb_config_dump(XPtr<tiledb::Config> config) {
  try {
    Rcout << "Config settings:\n";
    for (auto& p : *config) {
      Rcout << "\"" << p.first << "\" : \"" << p.second << "\"\n";
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
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
std::string libtiledb_dim_name(XPtr<tiledb::Dimension> dim) {
  try {
    return dim->name();
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
SEXP libtiledb_dim_domain(XPtr<tiledb::Dimension> dim) {
  try {
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
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
SEXP libtiledb_dim_tile_extent(XPtr<tiledb::Dimension> dim) {
  try {
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
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_dim_datatype(XPtr<tiledb::Dimension> dim) {
  try {
    return _tiledb_datatype_to_string(dim->type());
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
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
IntegerVector libtiledb_domain_ndim(XPtr<tiledb::Domain> domain) {
  try {
    unsigned int rank = domain->ndim();
    if (rank > std::numeric_limits<int32_t>::max()) {
      throw Rcpp::exception("tiledb::Domain rank is not representable by an R integer");
    }
    return IntegerVector({static_cast<int32_t>(rank),});
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
List libtiledb_domain_dimensions(XPtr<tiledb::Domain> domain) {
  try {
    List dimensions;
    for (auto& dim : domain->dimensions()) {
      dimensions.push_back(XPtr<tiledb::Dimension>(new tiledb::Dimension(dim)));
    }
    return dimensions;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_domain_datatype(XPtr<tiledb::Domain> domain) {
  try {
    auto dtype = domain->type();
    return _tiledb_datatype_to_string(dtype);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
void libtiledb_domain_dump(XPtr<tiledb::Domain> domain) {
  try {
    domain->dump();
    return;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}


/**
 * TileDB Compressor
 */
//[[Rcpp::export]]
XPtr<tiledb::Compressor> libtiledb_compressor(std::string compressor, int level) {
  try {
    tiledb_compressor_t compr = _string_to_tiledb_compressor(compressor);
    return XPtr<tiledb::Compressor>(new tiledb::Compressor({compr, level}));
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

//[[Rcpp::export]]
std::string libtiledb_compressor_type(XPtr<tiledb::Compressor> compressor) {
  try {
    return _tiledb_compresssor_to_string(compressor->compressor());
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

//[[Rcpp::export]]
int libtiledb_compressor_level(XPtr<tiledb::Compressor> compressor) {
  try {
    return compressor->level();
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

/**
 * TileDB Attribute
 */
//[[Rcpp::export]]
XPtr<tiledb::Attribute> libtiledb_attr(XPtr<tiledb::Context> ctx,
                                    std::string name,
                                    std::string type,
                                    XPtr<tiledb::Compressor> compressor,
                                    int ncells) {
 try {
   tiledb_datatype_t attr_dtype = _string_to_tiledb_datatype(type);
   if (ncells < 1) {
     throw Rcpp::exception("ncells must be >= 1");
   }
   if (attr_dtype == TILEDB_INT32) {
    using DType = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
    auto attr = XPtr<tiledb::Attribute>(
      new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)));
    attr->set_compressor(*compressor);
    return attr;
   } else if (attr_dtype == TILEDB_FLOAT64) {
    using DType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
    auto attr = XPtr<tiledb::Attribute>(
      new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)));
    attr->set_compressor(*compressor);
    return attr;
   } else {
    throw Rcpp::exception("only integer (INT32), and real (FLOAT64) attributes are supported");
   }
 } catch (tiledb::TileDBError& err) {
   throw Rcpp::exception(err.what());
 }
}

// [[Rcpp::export]]
std::string libtiledb_attr_name(XPtr<tiledb::Attribute> attr) {
  try {
    return attr->name();
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_attr_datatype(XPtr<tiledb::Attribute> attr) {
  try {
    return _tiledb_datatype_to_string(attr->type());
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Compressor> libtiledb_attr_compressor(XPtr<tiledb::Attribute> attr) {
  try {
    return XPtr<tiledb::Compressor>(new tiledb::Compressor(attr->compressor()));
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}
// [[Rcpp::export]]
int libtiledb_attr_ncells(XPtr<tiledb::Attribute> attr) {
  try {
    unsigned int ncells = attr->cell_val_num();
    if (ncells > std::numeric_limits<int32_t>::max()) {
      throw Rcpp::exception("tiledb_attr ncells value not representable as an R integer");
    }
    return static_cast<int32_t>(ncells);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

//[[Rcpp::export]]
void libtiledb_attr_dump(XPtr<tiledb::Attribute> attr) {
  try {
    attr->dump();
    return;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
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
    Nullable<XPtr<tiledb::Compressor>> coords_compressor = R_NilValue,
    Nullable<XPtr<tiledb::Compressor>> offsets_compressor = R_NilValue,
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
  try {
    auto schema = XPtr<tiledb::ArraySchema>(
      new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(), sparse ? TILEDB_SPARSE : TILEDB_DENSE)));
    schema->set_domain(*domain.get());
    for (SEXP a : attributes) {
      auto attr = as<XPtr<tiledb::Attribute>>(a);
      schema->add_attribute(*attr.get());
    }
    schema->set_cell_order(_cell_order);
    schema->set_tile_order(_tile_order);
    if (coords_compressor.isNotNull()) {
      XPtr<tiledb::Compressor> xptr_coords(coords_compressor);
      schema->set_coords_compressor(*xptr_coords);
    }
    if (offsets_compressor.isNotNull()) {
      XPtr<tiledb::Compressor> xptr_offsets(offsets_compressor);
      schema->set_offsets_compressor(*xptr_offsets);
    }
    schema->check();
    return schema;
 } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
 }
}

// [[Rcpp::export]]
XPtr<tiledb::Domain> libtiledb_array_schema_domain(XPtr<tiledb::ArraySchema> schema) {
  try {
    return XPtr<tiledb::Domain>(new tiledb::Domain(schema->domain()));
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
List libtiledb_array_schema_attributes(XPtr<tiledb::ArraySchema> schema) {
  try {
    List result;
    int nattr = schema->attribute_num();
    for (unsigned int i=0; i < nattr; i++) {
      auto attr = XPtr<tiledb::Attribute>(new tiledb::Attribute(schema->attribute(i)));
      result[attr->name()] = attr;
    }
    return result;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_array_schema_cell_order(XPtr<tiledb::ArraySchema> schema) {
  try {
    auto order = schema->cell_order();
    return _tiledb_layout_to_string(order);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_array_schema_tile_order(XPtr<tiledb::ArraySchema> schema) {
  try {
    auto order = schema->tile_order();
    return _tiledb_layout_to_string(order);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Compressor> libtiledb_array_schema_coords_compressor(XPtr<tiledb::ArraySchema> schema) {
  try {
    return XPtr<tiledb::Compressor>(new tiledb::Compressor(schema->coords_compressor()));
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Compressor> libtiledb_array_schema_offsets_compressor(XPtr<tiledb::ArraySchema> schema) {
  try {
    return XPtr<tiledb::Compressor>(new tiledb::Compressor(schema->offsets_compressor()));
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
bool libtiledb_array_schema_sparse(XPtr<tiledb::ArraySchema> schema) {
  try {
    return (schema->array_type() == TILEDB_SPARSE);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_schema_load(
    XPtr<tiledb::Context> ctx,std::string uri) {
  try {
    return XPtr<tiledb::ArraySchema>(
      new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(), uri)));
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
void libtiledb_array_schema_dump(XPtr<tiledb::ArraySchema> schema) {
  try {
    schema->dump();
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_array_create(std::string uri, XPtr<tiledb::ArraySchema> schema) {
  try {
    tiledb::Array::create(uri, *schema.get());
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array(XPtr<tiledb::Context> ctx,
                                    std::string uri,
                                    std::string type) {
  auto query_type = _string_to_tiledb_query_type(type);
  try {
     auto array = XPtr<tiledb::Array>(
      new tiledb::Array(tiledb::Array(*ctx.get(), uri, query_type)));
    return array;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
bool libtiledb_array_is_open(XPtr<tiledb::Array> array) {
  try {
    return array->is_open();
  } catch(tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_array_get_uri(XPtr<tiledb::Array> array) {
  try {
      return array->uri();
   } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> libtiledb_array_get_schema(XPtr<tiledb::Array> array) {
  try {
    return XPtr<tiledb::ArraySchema>(new tiledb::ArraySchema(array->schema()));
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_open(XPtr<tiledb::Array> array, std::string query_type) {
  tiledb_query_type_t qtype = _string_to_tiledb_query_type(query_type);
  try {
    array->open(qtype);
    return array;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_reopen(XPtr<tiledb::Array> array) {
  try {
    array->reopen();
    return array;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Array> libtiledb_array_close(XPtr<tiledb::Array> array) {
  try {
    array->close();
    return array;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_array_query_type(XPtr<tiledb::Array> array) {
  try {
    tiledb_query_type_t qtype = array->query_type();
    return _tiledb_query_type_to_string(qtype);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
List libtiledb_array_nonempty_domain(XPtr<tiledb::Array> array) {
  List nonempty_domain;
  try {
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
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_array_consolidate(XPtr<tiledb::Context> ctx,
                                     std::string uri) {
  try {
    tiledb::Array::consolidate(*ctx.get(), uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

/**
 * Query
 */
// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query(XPtr<tiledb::Context> ctx,
                                 XPtr<tiledb::Array> array,
                                 std::string type) {
  auto query_type = _string_to_tiledb_query_type(type);
  try {
    auto query = XPtr<tiledb::Query>(
      new tiledb::Query(tiledb::Query(*ctx.get(), *array.get(), query_type)));
    return query;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_layout(XPtr<tiledb::Query> query,
                                            std::string layout) {
  auto _layout = _string_to_tiledb_layout(layout);
  try {
    query->set_layout(_layout);
    return query;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_subarray(XPtr<tiledb::Query> query,
                                                 SEXP subarray) {
  try {
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
  } catch (tiledb::TileDBError& err){
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_coordinates(XPtr<tiledb::Query> query,
                                                    SEXP coords) {
  try {
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
  } catch (tiledb::TileDBError& err){
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_set_buffer(XPtr<tiledb::Query> query,
                                            std::string attr,
                                            SEXP buffer) {
  try {
    if (TYPEOF(buffer) == INTSXP) {
      IntegerVector vec(buffer);
      query->set_buffer(attr, vec.begin(), vec.length());
      return query;
    } else if (TYPEOF(buffer) == REALSXP) {
      NumericVector vec(buffer);
      query->set_buffer(attr, vec.begin(), vec.length());
      return query;
    } else {
      std::stringstream errmsg;
      errmsg << "Invalid attribute buffer type for attribute "
             << "\""<< attr << "\": " << Rcpp::type2name(buffer);
      throw Rcpp::exception(errmsg.str().c_str());
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_submit(XPtr<tiledb::Query> query) {
  try {
    query->submit();
    return query;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_finalize(XPtr<tiledb::Query> query) {
  try {
    query->finalize();
    return query;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
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
      return "UNINITIALIZED";
  }
}

// [[Rcpp::export]]
std::string libtiledb_query_status(XPtr<tiledb::Query> query) {
  try {
    tiledb::Query::Status status = query->query_status();
    return _query_status_to_string(status);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
R_xlen_t libtiledb_query_result_buffer_elements(XPtr<tiledb::Query> query,
                                                std::string attribute) {
  try {
    R_xlen_t nelem = query->result_buffer_elements()[attribute].second;
    return nelem;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
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
  try {
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
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

/**
 * Object functionality
 */
// [[Rcpp::export]]
std::string libtiledb_group_create(XPtr<tiledb::Context> ctx, std::string uri) {
  try {
    tiledb::create_group(*ctx.get(), uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

std::string _object_type_to_string(tiledb::Object::Type otype) {
  switch (otype) {
    case tiledb::Object::Type::Array:
      return "ARRAY";
    case tiledb::Object::Type::KeyValue:
      return "KEY_VALUE";
    case tiledb::Object::Type::Group:
      return "GROUP";
    case tiledb::Object::Type::Invalid:
      return "INVALID";
  }
}

// [[Rcpp::export]]
std::string libtiledb_object_type(XPtr<tiledb::Context> ctx, std::string uri) {
  try {
    auto obj = tiledb::Object::object(*ctx.get(), uri);
    return _object_type_to_string(obj.type());
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_object_remove(XPtr<tiledb::Context> ctx, std::string uri) {
  try {
    tiledb::Object::remove(*ctx.get(), uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string libtiledb_object_move(XPtr<tiledb::Context> ctx, std::string old_uri, std::string new_uri) {
  try {
    tiledb::Object::move(*ctx.get(), old_uri, new_uri);
    return new_uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

tiledb::Object::Type _string_to_object_type(std::string otype) {
  if (otype == "ARRAY") {
    return tiledb::Object::Type::Array;
  } else if (otype == "KEY_VALUE") {
    return tiledb::Object::Type::KeyValue;
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
  try {
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
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

/**
 * VFS functionality
 */
// [[Rcpp::export]]
XPtr<tiledb::VFS> tiledb_vfs(XPtr<tiledb::Context> ctx,
                             Nullable<XPtr<tiledb::Config>> config=R_NilValue) {
  try {
    if (config.isNull()) {
      return XPtr<tiledb::VFS>(new tiledb::VFS(*ctx.get()));
    } else {
      XPtr<tiledb::Config> config_xptr(config);
      return XPtr<tiledb::VFS>(new tiledb::VFS(*ctx.get(), *config_xptr.get()));
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string tiledb_vfs_create_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    vfs->create_bucket(uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string tiledb_vfs_remove_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    vfs->remove_bucket(uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
bool tiledb_vfs_is_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    return vfs->is_bucket(uri);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
bool tiledb_vfs_is_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    return vfs->is_empty_bucket(uri);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string tiledb_vfs_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    vfs->empty_bucket(uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string tiledb_vfs_create_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    vfs->create_dir(uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
bool tiledb_vfs_is_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    return vfs->is_dir(uri);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string tiledb_vfs_remove_dir(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    vfs->remove_dir(uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
bool tiledb_vfs_is_file(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    return vfs->is_file(uri);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string tiledb_vfs_remove_file(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    vfs->remove_file(uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
R_xlen_t tiledb_vfs_file_size(XPtr<tiledb::VFS> vfs, std::string uri) {
 try {
    uint64_t size = vfs->file_size(uri);
    if (size > std::numeric_limits<R_xlen_t>::max()) {
      throw Rcpp::exception("file size is greater than maximum R integer");
    }
    return static_cast<R_xlen_t>(size);
 } catch (tiledb::TileDBError& err) {
   throw Rcpp::exception(err.what());
 }
}

// [[Rcpp::export]]
std::string tiledb_vfs_move_file(XPtr<tiledb::VFS> vfs,
                                 std::string old_uri,
                                 std::string new_uri) {
  try {
    vfs->move_file(old_uri, new_uri);
    return new_uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string tiledb_vfs_move_dir(XPtr<tiledb::VFS> vfs,
                                std::string old_uri,
                                std::string new_uri) {
  try {
    vfs->move_dir(old_uri, new_uri);
    return new_uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
std::string tiledb_vfs_touch(XPtr<tiledb::VFS> vfs, std::string uri) {
  try {
    vfs->touch(uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

/**
 * Stats
 */

// [[Rcpp::export]]
void libtiledb_stats_enable() {
  try {
    tiledb::Stats::enable();
    return;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
void libtiledb_stats_disable() {
  try {
    tiledb::Stats::disable();
    return;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
void libtiledb_stats_print() {
  // TODO: look up the proper way to do this in R
  try {
    tiledb::Stats::dump(stdout);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
void libtiledb_stats_dump(std::string path) {
  FILE* fptr = nullptr;
  try {
    fptr = fopen(path.c_str(), "w");
    if (fptr == nullptr) {
      throw Rcpp::exception("error opening stats dump file for writing");
    }
    tiledb::Stats::dump(fptr);
  } catch (std::exception& err) {
    if (fptr != nullptr) {
      fclose(fptr);
    }
    throw Rcpp::exception(err.what());
  }
  fclose(fptr);
  return;
}
