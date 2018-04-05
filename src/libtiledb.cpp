#include "libtiledb.h"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

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

//' @export  
// [[Rcpp::export]]
NumericVector libtiledb_version() {
  try {
    auto ver = tiledb::Version::version();
    return NumericVector::create(_["major"]=ver.major(),
                                 _["minor"]=ver.minor(),
                                 _["patch"]=ver.patch());
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Context> tiledb_ctx(Nullable<XPtr<tiledb::Config>> config=R_NilValue) {
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
XPtr<tiledb::Config> tiledb_config_load_from_file(std::string filename) {
  try {
    tiledb::Config* config = new tiledb::Config(filename); 
    return XPtr<tiledb::Config>(config);
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
  } else {
    std::stringstream errmsg;
    errmsg << "Unknown TileDB fs scheme: \"" << scheme << "://\"";
    throw Rcpp::exception(errmsg.str().c_str());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Config> tiledb_config(Nullable<CharacterVector> config=R_NilValue) {
  try {
    XPtr<tiledb::Config> tiledb_config(new tiledb::Config(), true);
    if (config.isNotNull()) {
      auto config_vec = config.as();
      auto config_names = as<CharacterVector>(config_vec.names());
      for (auto &name : config_names) {
        auto param = as<std::string>(name);
        auto value = as<std::string>(config_vec[param]);
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
    CharacterVector res;
    res[param] = config->get(param);
    return res; 
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

/**
 * TileDB Dimension
 */
// [[Rcpp::export]]
XPtr<tiledb::Dimension> tiledb_dim(XPtr<tiledb::Context> ctx, 
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
std::string tiledb_dim_name(XPtr<tiledb::Dimension> dim) {
  try {
    return dim->name();
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
SEXP tiledb_dim_domain(XPtr<tiledb::Dimension> dim) {
  try {
    auto dim_type = dim->type();
    switch (dim_type) {
      case TILEDB_FLOAT32:
        return NumericVector({dim->domain<float>().first, 
                              dim->domain<float>().second});
      case TILEDB_FLOAT64:
        return NumericVector({dim->domain<double>().first, 
                              dim->domain<double>().second});
      case TILEDB_CHAR:
        return IntegerVector({dim->domain<char>().first, 
                              dim->domain<char>().second});
      case TILEDB_INT8:
        return IntegerVector({dim->domain<int8_t>().first, 
                              dim->domain<int8_t>().second});
      case TILEDB_UINT8:
        return IntegerVector({dim->domain<uint8_t>().first, 
                              dim->domain<uint8_t>().second});
      case TILEDB_INT16:
        return IntegerVector({dim->domain<int16_t>().first, 
                              dim->domain<int16_t>().second});
      case TILEDB_UINT16:
        return IntegerVector({dim->domain<uint16_t>().first, 
                              dim->domain<uint16_t>().second});       
      case TILEDB_INT32:
        return IntegerVector({dim->domain<int32_t>().first, 
                              dim->domain<int32_t>().second});       
      case TILEDB_UINT32: {
        auto d1_u32 = dim->domain<uint32_t>().first;
        auto d2_u32 = dim->domain<uint32_t>().second;
        if (d1_u32 > std::numeric_limits<int32_t>::max() || 
            d2_u32 > std::numeric_limits<int32_t>::max()) {
          throw Rcpp::exception("invalid domain integer value");
        }
        return IntegerVector(static_cast<int32_t>(d1_u32), 
                             static_cast<int32_t>(d2_u32));
      }
      case TILEDB_INT64: {
        auto d1_i64 = dim->domain<int64_t>().first;
        auto d2_i64 = dim->domain<int64_t>().second;
        if (d1_i64 < std::numeric_limits<int32_t>::min() ||
            d1_i64 > std::numeric_limits<int32_t>::max() ||
            d2_i64 < std::numeric_limits<int32_t>::min() ||
            d2_i64 > std::numeric_limits<int32_t>::max()) {
          throw Rcpp::exception("invalid domain integer value"); 
        }
        return IntegerVector(static_cast<int32_t>(d1_i64), 
                             static_cast<int32_t>(d2_i64));
      }
      case TILEDB_UINT64: {
        auto d1_u64 = dim->domain<uint64_t>().first;
        auto d2_u64 = dim->domain<uint64_t>().second;
        if (d1_u64 > std::numeric_limits<int32_t>::max() ||
            d2_u64 > std::numeric_limits<int32_t>::max()) {
          throw Rcpp::exception("invalid domain integer value"); 
        }
        return IntegerVector(static_cast<int32_t>(d1_u64), 
                             static_cast<int32_t>(d2_u64));
      }
      default:
        throw Rcpp::exception("invalid tiledb::Dim domain type");
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}


/**
 * TileDB Domain
 */
// [[Rcpp::export]]
XPtr<tiledb::Domain> tiledb_domain(XPtr<tiledb::Context> ctx, List dims) {
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
void tiledb_domain_dump(XPtr<tiledb::Domain> domain) {
  try {
    domain->dump();
    Rcout << domain << std::endl;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

/**
 * TileDB Attribute 
 */
//[[Rcpp::export]]
XPtr<tiledb::Attribute> tiledb_attr(XPtr<tiledb::Context> ctx, std::string name, std::string type) {
 try {
   tiledb_datatype_t attr_dtype = _string_to_tiledb_datatype(type);
   if (attr_dtype == TILEDB_INT32) {
    using DType = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
    return XPtr<tiledb::Attribute>(
      new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)));
   } else if (attr_dtype == TILEDB_FLOAT64) {
    using DType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
    return XPtr<tiledb::Attribute>(
      new tiledb::Attribute(tiledb::Attribute::create<DType>(*ctx.get(), name)));
   } else {
    throw Rcpp::exception("only integer (INT32), and real (FLOAT64) attributes are supported");
   }
 } catch (tiledb::TileDBError& err) {
   throw Rcpp::exception(err.what());
 }
}

//[[Rcpp::export]]
void tiledb_attr_dump(XPtr<tiledb::Attribute> attr) {
  try {
    attr->dump();
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

/**
 * TileDB Array Schema
 */
//[[Rcpp::export]]
XPtr<tiledb::ArraySchema> tiledb_array_schema(
    XPtr<tiledb::Context> ctx, 
    XPtr<tiledb::Domain> domain,
    List attributes,
    std::string cell_order="COL_MAJOR",
    std::string tile_order="COL_MAJOR",
    bool sparse=false) {
  // check that external pointers are supported
  R_xlen_t nattr = attributes.length();
  if (nattr == 0) {
    throw Rcpp::exception("tiledb_array_schema requires one or more attributes");
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
    schema->check(); 
    return schema;
 } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
 } 
}

// [[Rcpp::export]]
void tiledb_array_schema_dump(XPtr<tiledb::ArraySchema> schema) {
  try {
    schema->dump();
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what()); 
  }
}

// [[Rcpp::export]]
std::string tiledb_array_create(XPtr<tiledb::ArraySchema> schema,
                                std::string uri) {
  try {
    // TODO: check that the ctx / schema ctx are the same
    // TODO: return the full expanded array path?
    tiledb::Array::create(uri, *schema.get()); 
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what()); 
  }
}

// [[Rcpp::export]]
List tiledb_array_nonempty_domain(XPtr<tiledb::ArraySchema> schema,
                                  std::string uri) {
  List nonempty_domain;
  try {
    auto domain = schema->domain();
    if (domain.type() == TILEDB_INT32) {
      using DType = tiledb::impl::tiledb_to_type<TILEDB_INT32>::type;
      auto res = tiledb::Array::non_empty_domain<DType>(uri, *schema.get());
      for (auto& d: res) {
        auto dim_name = d.first;
        auto dim_domain = d.second;
        nonempty_domain[dim_name] = IntegerVector::create(dim_domain.first,
                                                          dim_domain.second);
      }
    } else if (domain.type() == TILEDB_FLOAT64) {
      using DType = tiledb::impl::tiledb_to_type<TILEDB_FLOAT64>::type;
      auto res = tiledb::Array::non_empty_domain<DType>(uri, *schema.get());
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
std::string tiledb_array_consolidate(XPtr<tiledb::Context> ctx,
                                     std::string uri) {
  try {
    // TODO: return the full expanded array path?
    tiledb::Array::consolidate(*ctx.get(), uri);
    return uri;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what()); 
  }
}

// [[Rcpp::export]]
XPtr<tiledb::ArraySchema> tiledb_array_load(XPtr<tiledb::Context> ctx, 
                                            std::string uri) {
  try {
    return XPtr<tiledb::ArraySchema>(
      new tiledb::ArraySchema(tiledb::ArraySchema(*ctx.get(), uri)));
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

/**
 * Query 
 */
// [[Rcpp::export]]
XPtr<tiledb::Query> tiledb_query(XPtr<tiledb::Context> ctx,
                                 std::string uri,
                                 std::string type) {
  auto query_type = _string_to_tiledb_query_type(type);
  try {
    auto query = XPtr<tiledb::Query>(
      new tiledb::Query(tiledb::Query(*ctx.get(), uri, query_type)));
    query->set_layout(TILEDB_COL_MAJOR);
    return query;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what());
  }
}

// [[Rcpp::export]]
XPtr<tiledb::Query> tiledb_query_set_layout(XPtr<tiledb::Query> query,
                                            std::string layout) {
  auto _layout = _string_to_tiledb_layout(layout);
  try {
    query->set_layout(_layout);
    return query;
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what()); 
  }
}

XPtr<tiledb::Query> tiledb_query_set_subarray(XPtr<tiledb::Query> query,
                                              SEXP subarray) {
  try {
    if (TYPEOF(subarray) == INTSXP) {
      return query; 
    } else if (TYPEOF(subarray) == REALSXP) {
      return query; 
    } else {
      // TODO: better error
      throw Rcpp::exception("invalid subarray type");
    }
  } catch (tiledb::TileDBError& err){
    throw Rcpp::exception(err.what());
  } 
}

// [[Rcpp::export]]
XPtr<tiledb::Query> tiledb_query_set_buffer(XPtr<tiledb::Query> query,
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
      throw Rcpp::exception("invalid attribute buffer type");
    }
  } catch (tiledb::TileDBError& err) {
    throw Rcpp::exception(err.what()); 
  }  
}

// [[Rcpp::export]]
XPtr<tiledb::Query> tiledb_query_submit(XPtr<tiledb::Query> query) {
  try {
    query->submit();
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
    case tiledb::Query::Status::UNDEF:
      return "UNDEF";
  }
}

// [[Rcpp::export]]
std::string tiledb_query_status(XPtr<tiledb::Query> query) {
  try {
    tiledb::Query::Status status = query->query_status();
    return _query_status_to_string(status);
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
    // TODO: hack, make this a correct cast
    uint64_t size = vfs->file_size(uri);
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