// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/tiledb.h"
#include <Rcpp.h>

using namespace Rcpp;

// libtiledb_version
NumericVector libtiledb_version();
RcppExport SEXP _tiledb_libtiledb_version() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(libtiledb_version());
    return rcpp_result_gen;
END_RCPP
}
// tiledb_ctx
XPtr<tiledb::Context> tiledb_ctx(Nullable<XPtr<tiledb::Config>> config);
RcppExport SEXP _tiledb_tiledb_ctx(SEXP configSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Nullable<XPtr<tiledb::Config>> >::type config(configSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_ctx(config));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_config_load_from_file
XPtr<tiledb::Config> tiledb_config_load_from_file(std::string filename);
RcppExport SEXP _tiledb_tiledb_config_load_from_file(SEXP filenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_config_load_from_file(filename));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_ctx_config
XPtr<tiledb::Config> tiledb_ctx_config(XPtr<tiledb::Context> ctx);
RcppExport SEXP _tiledb_tiledb_ctx_config(SEXP ctxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_ctx_config(ctx));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_ctx_is_supported_fs
bool tiledb_ctx_is_supported_fs(XPtr<tiledb::Context> ctx, std::string scheme);
RcppExport SEXP _tiledb_tiledb_ctx_is_supported_fs(SEXP ctxSEXP, SEXP schemeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    Rcpp::traits::input_parameter< std::string >::type scheme(schemeSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_ctx_is_supported_fs(ctx, scheme));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_config
XPtr<tiledb::Config> tiledb_config(Nullable<CharacterVector> config);
RcppExport SEXP _tiledb_tiledb_config(SEXP configSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Nullable<CharacterVector> >::type config(configSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_config(config));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_config_vector
CharacterVector tiledb_config_vector(XPtr<tiledb::Config> config);
RcppExport SEXP _tiledb_tiledb_config_vector(SEXP configSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Config> >::type config(configSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_config_vector(config));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_config_set
XPtr<tiledb::Config> tiledb_config_set(XPtr<tiledb::Config> config, std::string param, std::string value);
RcppExport SEXP _tiledb_tiledb_config_set(SEXP configSEXP, SEXP paramSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Config> >::type config(configSEXP);
    Rcpp::traits::input_parameter< std::string >::type param(paramSEXP);
    Rcpp::traits::input_parameter< std::string >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_config_set(config, param, value));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_config_get
CharacterVector tiledb_config_get(XPtr<tiledb::Config> config, std::string param);
RcppExport SEXP _tiledb_tiledb_config_get(SEXP configSEXP, SEXP paramSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Config> >::type config(configSEXP);
    Rcpp::traits::input_parameter< std::string >::type param(paramSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_config_get(config, param));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_config_dump
void tiledb_config_dump(XPtr<tiledb::Config> config);
RcppExport SEXP _tiledb_tiledb_config_dump(SEXP configSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Config> >::type config(configSEXP);
    tiledb_config_dump(config);
    return R_NilValue;
END_RCPP
}
// tiledb_dim
XPtr<tiledb::Dimension> tiledb_dim(XPtr<tiledb::Context> ctx, std::string name, std::string type, SEXP domain, SEXP tile_extent);
RcppExport SEXP _tiledb_tiledb_dim(SEXP ctxSEXP, SEXP nameSEXP, SEXP typeSEXP, SEXP domainSEXP, SEXP tile_extentSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    Rcpp::traits::input_parameter< std::string >::type type(typeSEXP);
    Rcpp::traits::input_parameter< SEXP >::type domain(domainSEXP);
    Rcpp::traits::input_parameter< SEXP >::type tile_extent(tile_extentSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_dim(ctx, name, type, domain, tile_extent));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_dim_name
std::string tiledb_dim_name(XPtr<tiledb::Dimension> dim);
RcppExport SEXP _tiledb_tiledb_dim_name(SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Dimension> >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_dim_name(dim));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_dim_domain
SEXP tiledb_dim_domain(XPtr<tiledb::Dimension> dim);
RcppExport SEXP _tiledb_tiledb_dim_domain(SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Dimension> >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_dim_domain(dim));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_domain
XPtr<tiledb::Domain> tiledb_domain(XPtr<tiledb::Context> ctx, List dims);
RcppExport SEXP _tiledb_tiledb_domain(SEXP ctxSEXP, SEXP dimsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    Rcpp::traits::input_parameter< List >::type dims(dimsSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_domain(ctx, dims));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_domain_dump
void tiledb_domain_dump(XPtr<tiledb::Domain> domain);
RcppExport SEXP _tiledb_tiledb_domain_dump(SEXP domainSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Domain> >::type domain(domainSEXP);
    tiledb_domain_dump(domain);
    return R_NilValue;
END_RCPP
}
// tiledb_attr
XPtr<tiledb::Attribute> tiledb_attr(XPtr<tiledb::Context> ctx, std::string name, std::string type);
RcppExport SEXP _tiledb_tiledb_attr(SEXP ctxSEXP, SEXP nameSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    Rcpp::traits::input_parameter< std::string >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_attr(ctx, name, type));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_attr_dump
void tiledb_attr_dump(XPtr<tiledb::Attribute> attr);
RcppExport SEXP _tiledb_tiledb_attr_dump(SEXP attrSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Attribute> >::type attr(attrSEXP);
    tiledb_attr_dump(attr);
    return R_NilValue;
END_RCPP
}
// tiledb_array_schema
XPtr<tiledb::ArraySchema> tiledb_array_schema(XPtr<tiledb::Context> ctx, XPtr<tiledb::Domain> domain, List attributes, std::string cell_order, std::string tile_order, bool sparse);
RcppExport SEXP _tiledb_tiledb_array_schema(SEXP ctxSEXP, SEXP domainSEXP, SEXP attributesSEXP, SEXP cell_orderSEXP, SEXP tile_orderSEXP, SEXP sparseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    Rcpp::traits::input_parameter< XPtr<tiledb::Domain> >::type domain(domainSEXP);
    Rcpp::traits::input_parameter< List >::type attributes(attributesSEXP);
    Rcpp::traits::input_parameter< std::string >::type cell_order(cell_orderSEXP);
    Rcpp::traits::input_parameter< std::string >::type tile_order(tile_orderSEXP);
    Rcpp::traits::input_parameter< bool >::type sparse(sparseSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_array_schema(ctx, domain, attributes, cell_order, tile_order, sparse));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_array_schema_dump
void tiledb_array_schema_dump(XPtr<tiledb::ArraySchema> schema);
RcppExport SEXP _tiledb_tiledb_array_schema_dump(SEXP schemaSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::ArraySchema> >::type schema(schemaSEXP);
    tiledb_array_schema_dump(schema);
    return R_NilValue;
END_RCPP
}
// tiledb_array_create
std::string tiledb_array_create(XPtr<tiledb::ArraySchema> schema, std::string uri);
RcppExport SEXP _tiledb_tiledb_array_create(SEXP schemaSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::ArraySchema> >::type schema(schemaSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_array_create(schema, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_array_nonempty_domain
List tiledb_array_nonempty_domain(XPtr<tiledb::ArraySchema> schema, std::string uri);
RcppExport SEXP _tiledb_tiledb_array_nonempty_domain(SEXP schemaSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::ArraySchema> >::type schema(schemaSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_array_nonempty_domain(schema, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_array_consolidate
std::string tiledb_array_consolidate(XPtr<tiledb::Context> ctx, std::string uri);
RcppExport SEXP _tiledb_tiledb_array_consolidate(SEXP ctxSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_array_consolidate(ctx, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_array_load
XPtr<tiledb::ArraySchema> tiledb_array_load(XPtr<tiledb::Context> ctx, std::string uri);
RcppExport SEXP _tiledb_tiledb_array_load(SEXP ctxSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_array_load(ctx, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_query
XPtr<tiledb::Query> tiledb_query(XPtr<tiledb::Context> ctx, std::string uri, std::string type);
RcppExport SEXP _tiledb_tiledb_query(SEXP ctxSEXP, SEXP uriSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    Rcpp::traits::input_parameter< std::string >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_query(ctx, uri, type));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_query_set_layout
XPtr<tiledb::Query> tiledb_query_set_layout(XPtr<tiledb::Query> query, std::string layout);
RcppExport SEXP _tiledb_tiledb_query_set_layout(SEXP querySEXP, SEXP layoutSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Query> >::type query(querySEXP);
    Rcpp::traits::input_parameter< std::string >::type layout(layoutSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_query_set_layout(query, layout));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_query_set_buffer
XPtr<tiledb::Query> tiledb_query_set_buffer(XPtr<tiledb::Query> query, std::string attr, SEXP buffer);
RcppExport SEXP _tiledb_tiledb_query_set_buffer(SEXP querySEXP, SEXP attrSEXP, SEXP bufferSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Query> >::type query(querySEXP);
    Rcpp::traits::input_parameter< std::string >::type attr(attrSEXP);
    Rcpp::traits::input_parameter< SEXP >::type buffer(bufferSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_query_set_buffer(query, attr, buffer));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_query_submit
XPtr<tiledb::Query> tiledb_query_submit(XPtr<tiledb::Query> query);
RcppExport SEXP _tiledb_tiledb_query_submit(SEXP querySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Query> >::type query(querySEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_query_submit(query));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_query_status
std::string tiledb_query_status(XPtr<tiledb::Query> query);
RcppExport SEXP _tiledb_tiledb_query_status(SEXP querySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Query> >::type query(querySEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_query_status(query));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs
XPtr<tiledb::VFS> tiledb_vfs(XPtr<tiledb::Context> ctx, Nullable<XPtr<tiledb::Config>> config);
RcppExport SEXP _tiledb_tiledb_vfs(SEXP ctxSEXP, SEXP configSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::Context> >::type ctx(ctxSEXP);
    Rcpp::traits::input_parameter< Nullable<XPtr<tiledb::Config>> >::type config(configSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs(ctx, config));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_create_bucket
std::string tiledb_vfs_create_bucket(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_create_bucket(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_create_bucket(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_remove_bucket
std::string tiledb_vfs_remove_bucket(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_remove_bucket(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_remove_bucket(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_is_bucket
bool tiledb_vfs_is_bucket(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_is_bucket(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_is_bucket(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_is_empty_bucket
bool tiledb_vfs_is_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_is_empty_bucket(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_is_empty_bucket(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_empty_bucket
std::string tiledb_vfs_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_empty_bucket(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_empty_bucket(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_create_dir
std::string tiledb_vfs_create_dir(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_create_dir(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_create_dir(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_is_dir
bool tiledb_vfs_is_dir(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_is_dir(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_is_dir(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_remove_dir
std::string tiledb_vfs_remove_dir(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_remove_dir(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_remove_dir(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_is_file
bool tiledb_vfs_is_file(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_is_file(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_is_file(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_remove_file
std::string tiledb_vfs_remove_file(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_remove_file(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_remove_file(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_file_size
R_xlen_t tiledb_vfs_file_size(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_file_size(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_file_size(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_move_file
std::string tiledb_vfs_move_file(XPtr<tiledb::VFS> vfs, std::string old_uri, std::string new_uri);
RcppExport SEXP _tiledb_tiledb_vfs_move_file(SEXP vfsSEXP, SEXP old_uriSEXP, SEXP new_uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type old_uri(old_uriSEXP);
    Rcpp::traits::input_parameter< std::string >::type new_uri(new_uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_move_file(vfs, old_uri, new_uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_move_dir
std::string tiledb_vfs_move_dir(XPtr<tiledb::VFS> vfs, std::string old_uri, std::string new_uri);
RcppExport SEXP _tiledb_tiledb_vfs_move_dir(SEXP vfsSEXP, SEXP old_uriSEXP, SEXP new_uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type old_uri(old_uriSEXP);
    Rcpp::traits::input_parameter< std::string >::type new_uri(new_uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_move_dir(vfs, old_uri, new_uri));
    return rcpp_result_gen;
END_RCPP
}
// tiledb_vfs_touch
std::string tiledb_vfs_touch(XPtr<tiledb::VFS> vfs, std::string uri);
RcppExport SEXP _tiledb_tiledb_vfs_touch(SEXP vfsSEXP, SEXP uriSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<tiledb::VFS> >::type vfs(vfsSEXP);
    Rcpp::traits::input_parameter< std::string >::type uri(uriSEXP);
    rcpp_result_gen = Rcpp::wrap(tiledb_vfs_touch(vfs, uri));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tiledb_libtiledb_version", (DL_FUNC) &_tiledb_libtiledb_version, 0},
    {"_tiledb_tiledb_ctx", (DL_FUNC) &_tiledb_tiledb_ctx, 1},
    {"_tiledb_tiledb_config_load_from_file", (DL_FUNC) &_tiledb_tiledb_config_load_from_file, 1},
    {"_tiledb_tiledb_ctx_config", (DL_FUNC) &_tiledb_tiledb_ctx_config, 1},
    {"_tiledb_tiledb_ctx_is_supported_fs", (DL_FUNC) &_tiledb_tiledb_ctx_is_supported_fs, 2},
    {"_tiledb_tiledb_config", (DL_FUNC) &_tiledb_tiledb_config, 1},
    {"_tiledb_tiledb_config_vector", (DL_FUNC) &_tiledb_tiledb_config_vector, 1},
    {"_tiledb_tiledb_config_set", (DL_FUNC) &_tiledb_tiledb_config_set, 3},
    {"_tiledb_tiledb_config_get", (DL_FUNC) &_tiledb_tiledb_config_get, 2},
    {"_tiledb_tiledb_config_dump", (DL_FUNC) &_tiledb_tiledb_config_dump, 1},
    {"_tiledb_tiledb_dim", (DL_FUNC) &_tiledb_tiledb_dim, 5},
    {"_tiledb_tiledb_dim_name", (DL_FUNC) &_tiledb_tiledb_dim_name, 1},
    {"_tiledb_tiledb_dim_domain", (DL_FUNC) &_tiledb_tiledb_dim_domain, 1},
    {"_tiledb_tiledb_domain", (DL_FUNC) &_tiledb_tiledb_domain, 2},
    {"_tiledb_tiledb_domain_dump", (DL_FUNC) &_tiledb_tiledb_domain_dump, 1},
    {"_tiledb_tiledb_attr", (DL_FUNC) &_tiledb_tiledb_attr, 3},
    {"_tiledb_tiledb_attr_dump", (DL_FUNC) &_tiledb_tiledb_attr_dump, 1},
    {"_tiledb_tiledb_array_schema", (DL_FUNC) &_tiledb_tiledb_array_schema, 6},
    {"_tiledb_tiledb_array_schema_dump", (DL_FUNC) &_tiledb_tiledb_array_schema_dump, 1},
    {"_tiledb_tiledb_array_create", (DL_FUNC) &_tiledb_tiledb_array_create, 2},
    {"_tiledb_tiledb_array_nonempty_domain", (DL_FUNC) &_tiledb_tiledb_array_nonempty_domain, 2},
    {"_tiledb_tiledb_array_consolidate", (DL_FUNC) &_tiledb_tiledb_array_consolidate, 2},
    {"_tiledb_tiledb_array_load", (DL_FUNC) &_tiledb_tiledb_array_load, 2},
    {"_tiledb_tiledb_query", (DL_FUNC) &_tiledb_tiledb_query, 3},
    {"_tiledb_tiledb_query_set_layout", (DL_FUNC) &_tiledb_tiledb_query_set_layout, 2},
    {"_tiledb_tiledb_query_set_buffer", (DL_FUNC) &_tiledb_tiledb_query_set_buffer, 3},
    {"_tiledb_tiledb_query_submit", (DL_FUNC) &_tiledb_tiledb_query_submit, 1},
    {"_tiledb_tiledb_query_status", (DL_FUNC) &_tiledb_tiledb_query_status, 1},
    {"_tiledb_tiledb_vfs", (DL_FUNC) &_tiledb_tiledb_vfs, 2},
    {"_tiledb_tiledb_vfs_create_bucket", (DL_FUNC) &_tiledb_tiledb_vfs_create_bucket, 2},
    {"_tiledb_tiledb_vfs_remove_bucket", (DL_FUNC) &_tiledb_tiledb_vfs_remove_bucket, 2},
    {"_tiledb_tiledb_vfs_is_bucket", (DL_FUNC) &_tiledb_tiledb_vfs_is_bucket, 2},
    {"_tiledb_tiledb_vfs_is_empty_bucket", (DL_FUNC) &_tiledb_tiledb_vfs_is_empty_bucket, 2},
    {"_tiledb_tiledb_vfs_empty_bucket", (DL_FUNC) &_tiledb_tiledb_vfs_empty_bucket, 2},
    {"_tiledb_tiledb_vfs_create_dir", (DL_FUNC) &_tiledb_tiledb_vfs_create_dir, 2},
    {"_tiledb_tiledb_vfs_is_dir", (DL_FUNC) &_tiledb_tiledb_vfs_is_dir, 2},
    {"_tiledb_tiledb_vfs_remove_dir", (DL_FUNC) &_tiledb_tiledb_vfs_remove_dir, 2},
    {"_tiledb_tiledb_vfs_is_file", (DL_FUNC) &_tiledb_tiledb_vfs_is_file, 2},
    {"_tiledb_tiledb_vfs_remove_file", (DL_FUNC) &_tiledb_tiledb_vfs_remove_file, 2},
    {"_tiledb_tiledb_vfs_file_size", (DL_FUNC) &_tiledb_tiledb_vfs_file_size, 2},
    {"_tiledb_tiledb_vfs_move_file", (DL_FUNC) &_tiledb_tiledb_vfs_move_file, 3},
    {"_tiledb_tiledb_vfs_move_dir", (DL_FUNC) &_tiledb_tiledb_vfs_move_dir, 3},
    {"_tiledb_tiledb_vfs_touch", (DL_FUNC) &_tiledb_tiledb_vfs_touch, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_tiledb(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
