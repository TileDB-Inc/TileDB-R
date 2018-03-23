#include "libtiledb.h"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
NumericVector tiledb_version() {
    auto ver = tiledb::Version::version();
    NumericVector Rver = NumericVector::create(ver.major(), ver.minor(), ver.patch()) ;
    return Rver;
}

// [[Rcpp::export]]
XPtr<tiledb::Context> tiledb_ctx(Nullable<XPtr<tiledb::Config>> config=R_NilValue) {
  try {
    if (config.isNull()) {
      return XPtr<tiledb::Context>(new tiledb::Context(), true);  
    } else {
      XPtr<tiledb::Config> config_xptr(config);
      return XPtr<tiledb::Context>(new tiledb::Context(*config_xptr), true);
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Config> tiledb_ctx_config(XPtr<tiledb::Context> ctx) {
  return XPtr<tiledb::Config>(new tiledb::Config(ctx.get()->config()));
}

// [[Rcpp::export]]
bool tiledb_ctx_is_supported_fs(XPtr<tiledb::Context> ctx, std::string scheme) {
  if (scheme == "file") {
    return true;
  } else if  (scheme == "s3") {
    return ctx->is_supported_fs(TILEDB_S3);
  } else if (scheme == "hdfs") {
    return ctx->is_supported_fs(TILEDB_HDFS);
  }
  return false;
}
  
// [[Rcpp::export]]
XPtr<tiledb::Config> tiledb_config(Nullable<CharacterVector> config=R_NilValue) {
  try {
    XPtr<tiledb::Config> tiledb_config(new tiledb::Config(), true);
    if (config.isNotNull()) {
      auto config_vec = config.as();
      auto config_names = as<CharacterVector>(config_vec.names());
      for (auto &name : config_names) {
        std::string param = as<std::string>(name);
        std::string value = as<std::string>(config_vec[param]);
        tiledb_config->set(param, value);
      }
    }
    return tiledb_config;
  } catch (tiledb::TileDBError& err) {
    throw  Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
CharacterVector tiledb_config_vector(XPtr<tiledb::Config> config) {
  CharacterVector config_vec;
  for (auto& p : *config) {
    config_vec[p.first]  = p.second;
  }
  return config_vec;
}
                                    
// [[Rcpp::export]]
XPtr<tiledb::Config> tiledb_config_set(XPtr<tiledb::Config> config,
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
CharacterVector tiledb_config_get(XPtr<tiledb::Config> config,
                                  std::string param) {
  try {
    return config->get(param);
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
void tiledb_config_dump(XPtr<tiledb::Config> config) {
  try {
    Rcout << "Config settings:\n";
    for (auto& p : *config) {
      Rcout << "\"" << p.first << "\" : \"" << p.second << "\"\n";
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
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
    std::string errmsg = "Unknown TileDB type \"" + typestr  + "\"";
    throw Rcpp::exception(errmsg.c_str());
  }
}

/**
 * TileDB Dimension
 */
// [[Rcpp::export]]
XPtr<tiledb::Dimension> tiledb_dim(XPtr<tiledb::Context> ctx, 
                                   std::string name,
                                   std::string type,
                                   NumericVector domain,
                                   NumericVector tile_extent) {
  const tiledb_datatype_t _type = _string_to_tiledb_datatype(type);
  if (_type != TILEDB_INT32 && _type != TILEDB_FLOAT64) {
    throw Rcpp::exception("only integer (INT32), and flaot (FLOAT64) domains are supported");
  }
  if (domain.length() != 2) {
    throw Rcpp::exception("dimension domain must be a c(lower bound, upper bound) pair");
  } 
  if (tile_extent.length() != 1) {
    throw Rcpp::exception("tile_extent must be a scalar");
  }
  try {
    if (_type == TILEDB_INT32) {
      using Dtype = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
      std::array<Dtype, 2> _domain = {domain[0], domain[1]};
      std::array<Dtype, 1> _tile_extent = {tile_extent[0]};
      return XPtr<tiledb::Dimension>(
        new tiledb::Dimension(tiledb::Dimension::create<Dtype>(*ctx.get(), name, _domain, _tile_extent[0])));
    } else if (_type == TILEDB_FLOAT64) {
      using Dtype = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
      std::array<Dtype, 2> _domain = {domain[0], domain[1]}; 
      std::array<Dtype, 1> _tile_extent = {tile_extent[0]};
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
XPtr<tiledb::Domain> tiledb_domain(XPtr<tiledb::Context> ctx, List dims) {
  size_t ndims = dims.length();
  if (ndims == 0) {
    throw Rcpp::exception("domain must have one or more dimensions");
  }
  for (size_t i=0; i < ndims; i++) {
    SEXP d = dims.at(i);
    int R_type_id = TYPEOF(d);
    if (R_type_id != EXTPTRSXP) {
      std::stringstream errmsg;
      errmsg << "Invalid tiledb_dim object at index " <<  i << " (typeid " << R_type_id << ")";
      throw Rcpp::exception(errmsg.str().c_str());
    }
  }
  XPtr<tiledb::Domain> domain(new tiledb::Domain(*ctx.get()));
  try {
    for (SEXP val : dims) {
      // TODO: we can't do much type checking for the cast here until we wrap EXTPTRSXP in S4 classes
      XPtr<tiledb::Dimension> dim = as<XPtr<tiledb::Dimension>>(val);
      domain->add_dimension(*dim.get());
    }
    return domain;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}