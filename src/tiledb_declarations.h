
#include <Rcpp.h>

using namespace Rcpp;

## type helpers from/to string
const char*               _tiledb_datatype_to_string(tiledb_datatype_t dtype);
tiledb_datatype_t         _string_to_tiledb_datatype(std::string typestr);
std::string               tiledb_datatype_R_type(std::string datatype);
const char*               _tiledb_layout_to_string(tiledb_layout_t layout);
tiledb_layout_t           _string_to_tiledb_layout(std::string lstr);
tiledb_filter_type_t      _string_to_tiledb_filter(std::string filter);
const char*               _tiledb_filter_to_string(tiledb_filter_type_t filter);
tiledb_filter_option_t    _string_to_tiledb_filter_option(std::string filter_option);
const char*               _tiledb_filter_option_to_string(tiledb_filter_option_t filter_option);
tiledb_query_type_t       _string_to_tiledb_query_type(std::string qtstr);
std::string               _tiledb_query_type_to_string(tiledb_query_type_t qtype);

## version
NumericVector             libtiledb_version();


## Context and Config
##
## C++ API for Context
## y Context()
## y Context(const Config& config)
##   void handle_error(int rc)
##   std::shared_ptr<tiledb_ctx_t> ptr()
##   Context& set_error_handler(const std::function<void(const std::string&)>& fn)
##   Config config()
## y bool is_supported_fs(tiledb_filesystem_t fs)
##   void cancel_tasks()
## y void set_tag(const std::string& key, const std::string& value)
##   static void default_error_handler(const std::string& msg)
##
## C++ API for Config
##   (ConfigIter and ConfigProxy classes)
## y Config()
## y Config(const std::string filename)
##   Config(tiledb_config_t** config)
## y void save_to_file(const std::string filename)
##   std::shared_ptr<tiledb_config_t> ptr()
## y Config& set(const std::string& param, const std::string& value)
## y std::string get(const std::string& param) const
##   impl::ConfigProxy operator[](const std::string& param)
## y Config& unset(const std::string& param)
##   iterator begin(const std::string& prefix)
##   iterator begin()
##   iterator end()
##   static void free(tiledb_config_t* config)
XPtr<tiledb::Context>     libtiledb_ctx(Nullable<XPtr<tiledb::Config>> config=R_NilValue);
void                      libtiledb_ctx_set_tag(XPtr<tiledb::Context> ctx, std::string key, std::string value);
std::string               libtiledb_config_save_to_file(XPtr<tiledb::Config> config, std::string filename);
XPtr<tiledb::Config>      libtiledb_config_load_from_file(std::string filename);
XPtr<tiledb::Config>      libtiledb_ctx_config(XPtr<tiledb::Context> ctx);
bool                      libtiledb_ctx_is_supported_fs(XPtr<tiledb::Context> ctx, std::string scheme);
XPtr<tiledb::Config>      libtiledb_config(Nullable<CharacterVector> config=R_NilValue);
CharacterVector           libtiledb_config_vector(XPtr<tiledb::Config> config);
XPtr<tiledb::Config>      libtiledb_config_set(XPtr<tiledb::Config> config, std::string param, std::string value);
CharacterVector           libtiledb_config_get(XPtr<tiledb::Config> config, CharacterVector params);
void                      libtiledb_config_dump(XPtr<tiledb::Config> config);


## Dimension
##
## C++ API
## y Dimension(const Context& ctx, tiledb_dimension_t* dim)
## y const std::string name()
## y tiledb_datatype_t type()
## y template <typename T> std::pair<T, T> domain()
##   std::string domain_to_str()
## y template <typename T> T tile_extent()
##   std::shared_ptr<tiledb_dimension_t> ptr()
##   template <typename T> static Dimension create(const Context& ctx, const std::string& name,
##                                                 const std::array<T, 2>& domain, T extent)
##   static Dimension create(const Context& ctx, const std::string& name, tiledb_datatype_t datatype,
##                           const void* domain, const void* extent)
XPtr<tiledb::Dimension>   libtiledb_dim(XPtr<tiledb::Context> ctx, std::string name, std::string type, SEXP domain, SEXP tile_extent);
std::string               libtiledb_dim_name(XPtr<tiledb::Dimension> dim);
SEXP                      libtiledb_dim_domain(XPtr<tiledb::Dimension> dim);
SEXP                      libtiledb_dim_tile_extent(XPtr<tiledb::Dimension> dim);
std::string               libtiledb_dim_datatype(XPtr<tiledb::Dimension> dim);
NumericVector             dim_domain_subarray(NumericVector domain, NumericVector subscript);


## Domain
##
## C++ API
##
## y Domain(const Context& ctx)
##   Domain(const Context& ctx, tiledb_domain_t* domain)
##   uint64_t cell_num()
## y void dump(FILE* out = nullptr)
## y tiledb_datatype_t type()
## y unsigned ndim()
## y std::vector<Dimension> dimensions()
##   Domain& add_dimension(const Dimension& d)
##   template <typename... Args> Domain& add_dimensions(Args... dims)
##   bool has_dimension(const std::string& name)
##   std::shared_ptr<tiledb_domain_t> ptr()
XPtr<tiledb::Domain>      libtiledb_domain(XPtr<tiledb::Context> ctx, List dims);
std::string               libtiledb_domain_get_type(XPtr<tiledb::Domain> domain);
int                       libtiledb_domain_get_ndim(XPtr<tiledb::Domain> domain);
List                      libtiledb_domain_get_dimensions(XPtr<tiledb::Domain> domain);
void                      libtiledb_domain_dump(XPtr<tiledb::Domain> domain);


## Filter
XPtr<tiledb::Filter>      libtiledb_filter(XPtr<tiledb::Context> ctx, std::string filter);
std::string               libtiledb_filter_type(XPtr<tiledb::Filter> filter);
R_xlen_t                  libtiledb_filter_get_option(XPtr<tiledb::Filter> filter, std::string filter_option_str);
void                      libtiledb_filter_set_option(XPtr<tiledb::Filter> filter, std::string filter_option_str, int value);


## Filter List
XPtr<tiledb::FilterList>  libtiledb_filter_list(XPtr<tiledb::Context> ctx, List filters);
void                      libtiledb_filter_list_set_max_chunk_size(XPtr<tiledb::FilterList> filterList, uint32_t max_chunk_sie);
int                       libtiledb_filter_list_max_chunk_size(XPtr<tiledb::FilterList> filterList);
int                       libtiledb_filter_list_nfilters(XPtr<tiledb::FilterList> filterList);
XPtr<tiledb::Filter>      libtiledb_filter_list_filter(XPtr<tiledb::FilterList> filterList, uint32_t filter_index);


## Attribute
XPtr<tiledb::Attribute>   libtiledb_attribute(XPtr<tiledb::Context> ctx, std::string name, std::string type, XPtr<tiledb::FilterList> filter_list, int ncells);
std::string               libtiledb_attribute_name(XPtr<tiledb::Attribute> attr);
std::string               libtiledb_attribute_datatype(XPtr<tiledb::Attribute> attr);
XPtr<tiledb::FilterList>  libtiledb_attribute_filter_list(XPtr<tiledb::Attribute> attr);
int                       libtiledb_attribute_get_cell_val_num(XPtr<tiledb::Attribute> attr);
void                      libtiledb_attribute_set_cell_val_num(XPtr<tiledb::Attribute> attr, int num);
void                      libtiledb_attribute_dump(XPtr<tiledb::Attribute> attr);


## Array Schema
##
## C++ API
##
## y ArraySchema(const Context& ctx, tiledb_array_type_t type)
## y ArraySchema(const Context& ctx, const std::string& uri)
##   ArraySchema(const Context& ctx, const std::string& uri, tiledb_encryption_type_t encryption_type,
##               const void* encryption_key, uint32_t key_length)
## y ArraySchema(const Context& ctx, const std::string& uri, tiledb_encryption_type_t encryption_type,
##               const std::string& encryption_key)
## y ArraySchema(const Context& ctx, tiledb_array_schema_t* schema)
## y void dump(FILE* out = nullptr)
## y tiledb_array_type_t array_type()
## y uint64_t capacity()
## y ArraySchema& set_capacity(uint64_t capacity)
## y tiledb_layout_t tile_order()
##   ArraySchema& set_tile_order(tiledb_layout_t layout)
##   ArraySchema& set_order(const std::array<tiledb_layout_t, 2>& p)
## y tiledb_layout_t cell_order()
##   ArraySchema& set_cell_order(tiledb_layout_t layout)
##   FilterList coords_filter_list()
## y ArraySchema& set_coords_filter_list(const FilterList& filter_list)
## y FilterList offsets_filter_list()
##   ArraySchema& set_offsets_filter_list(const FilterList& filter_list)
## y Domain domain()
##   ArraySchema& set_domain(const Domain& domain)
##   ArraySchema& add_attribute(const Attribute& attr)
##   std::shared_ptr<tiledb_array_schema_t> ptr()
## y void check()
## y std::unordered_map<std::string, Attribute> attributes()
##   Attribute attribute(const std::string& name)
##   unsigned attribute_num()
##   Attribute attribute(unsigned int i)
##   bool has_attribute(const std::string& name)
##   static std::string to_str(tiledb_array_type_t type)
##   static std::string to_str(tiledb_layout_t layout)
XPtr<tiledb::ArraySchema> libtiledb_array_schema(XPtr<tiledb::Context> ctx, XPtr<tiledb::Domain> domain, List attributes, std::string cell_order, std::string tile_order,
                                                 Nullable<XPtr<tiledb::FilterList>> coords_filter_list = R_NilValue, Nullable<XPtr<tiledb::FilterList>> offsets_filter_list = R_NilValue, bool sparse = false);
XPtr<tiledb::ArraySchema> libtiledb_array_schema_load(XPtr<tiledb::Context> ctx,std::string uri);
XPtr<tiledb::ArraySchema> libtiledb_array_schema_load_with_key(XPtr<tiledb::Context> ctx, std::string uri, std::string key);
XPtr<tiledb::ArraySchema> libtiledb_array_get_schema(XPtr<tiledb::Array> array);
XPtr<tiledb::Domain>      libtiledb_array_schema_get_domain(XPtr<tiledb::ArraySchema> schema);
List                      libtiledb_array_schema_attributes(XPtr<tiledb::ArraySchema> schema);
std::string               libtiledb_array_schema_get_array_type(XPtr<tiledb::ArraySchema> schema);
std::string               libtiledb_array_schema_get_cell_order(XPtr<tiledb::ArraySchema> schema);
std::string               libtiledb_array_schema_get_tile_order(XPtr<tiledb::ArraySchema> schema);
void                      libtiledb_array_schema_set_capacity(XPtr<tiledb::ArraySchema> schema, int cap);
int                       libtiledb_array_schema_get_capacity(XPtr<tiledb::ArraySchema> schema);
XPtr<tiledb::FilterList>  libtiledb_array_schema_get_coords_filter_list(XPtr<tiledb::ArraySchema> schema);
XPtr<tiledb::FilterList>  libtiledb_array_schema_get_offsets_filter_list(XPtr<tiledb::ArraySchema> schema);
int                       libtiledb_array_schema_get_attribute_num(XPtr<tiledb::ArraySchema> schema);
XPtr<tiledb::Attribute>   libtiledb_array_schema_get_attribute_from_index(XPtr<tiledb::ArraySchema> schema, int idx);
XPtr<tiledb::Attribute>   libtiledb_array_schema_get_attribute_from_name(XPtr<tiledb::ArraySchema> schema, std::string name);
bool                      libtiledb_array_schema_has_attribute(XPtr<tiledb::ArraySchema> schema, std::string name);
bool                      libtiledb_array_schema_sparse(XPtr<tiledb::ArraySchema> schema);
void                      libtiledb_array_schema_dump(XPtr<tiledb::ArraySchema> schema);
void                      libtiledb_array_schema_check(XPtr<tiledb::ArraySchema> schema);
std::string               libtiledb_array_create(std::string uri, XPtr<tiledb::ArraySchema> schema);
std::string               libtiledb_array_create_with_key(std::string uri, XPtr<tiledb::ArraySchema> schema, std::string encryption_key);


## Array
XPtr<tiledb::Array>       libtiledb_array_open(XPtr<tiledb::Context> ctx, std::string uri, std::string type);
XPtr<tiledb::Array>       libtiledb_array_open_with_key(XPtr<tiledb::Context> ctx, std::string uri, std::string type, std::string enc_key);
bool                      libtiledb_array_is_open(XPtr<tiledb::Array> array);
bool                      libtiledb_array_is_open_for_reading(XPtr<tiledb::Array> array);
bool                      libtiledb_array_is_open_for_writing(XPtr<tiledb::Array> array);
std::string               libtiledb_array_get_uri(XPtr<tiledb::Array> array);
XPtr<tiledb::Array>       libtiledb_array_open_with_ptr(XPtr<tiledb::Array> array, std::string query_type);
XPtr<tiledb::Array>       libtiledb_array_reopen(XPtr<tiledb::Array> array);
XPtr<tiledb::Array>       libtiledb_array_close(XPtr<tiledb::Array> array);
std::string               libtiledb_array_query_type(XPtr<tiledb::Array> array);
List                      libtiledb_array_nonempty_domain(XPtr<tiledb::Array> array);
std::string               libtiledb_array_consolidate(XPtr<tiledb::Context> ctx, std::string uri);


## Query
XPtr<tiledb::Query>       libtiledb_query(XPtr<tiledb::Context> ctx, XPtr<tiledb::Array> array, std::string type);
XPtr<tiledb::Query>       libtiledb_query_set_layout(XPtr<tiledb::Query> query, std::string layout);
XPtr<tiledb::Query>       libtiledb_query_set_subarray(XPtr<tiledb::Query> query, SEXP subarray);
XPtr<tiledb::Query>       libtiledb_query_set_coordinates(XPtr<tiledb::Query> query, SEXP coords);
XPtr<tiledb::Query>       libtiledb_query_set_buffer(XPtr<tiledb::Query> query, std::string attr, SEXP buffer);
XPtr<tiledb::Query>       libtiledb_query_submit(XPtr<tiledb::Query> query);
XPtr<tiledb::Query>       libtiledb_query_finalize(XPtr<tiledb::Query> query);
std::string               _query_status_to_string(tiledb::Query::Status status);
std::string               libtiledb_query_status(XPtr<tiledb::Query> query);
R_xlen_t                  libtiledb_query_result_buffer_elements(XPtr<tiledb::Query> query, std::string attribute);


## Array Helpers
NumericVector             libtiledb_zip_coords_numeric( List coords, R_xlen_t coord_length);
IntegerVector             libtiledb_zip_coords_integer( List coords, R_xlen_t coord_length);
std::string               libtiledb_coords();
R_xlen_t                  libtiledb_array_max_buffer_elements(XPtr<tiledb::Array> array, SEXP subarray, std::string attribute);


## Object functionality
std::string               libtiledb_group_create(XPtr<tiledb::Context> ctx, std::string uri);
std::string               _object_type_to_string(tiledb::Object::Type otype);
std::string               libtiledb_object_type(XPtr<tiledb::Context> ctx, std::string uri);
std::string               libtiledb_object_remove(XPtr<tiledb::Context> ctx, std::string uri);
std::string               libtiledb_object_move(XPtr<tiledb::Context> ctx, std::string old_uri, std::string new_uri);
tiledb::Object::Type      _string_to_object_type(std::string otype);
DataFrame                 libtiledb_object_walk(XPtr<tiledb::Context> ctx, std::string uri, std::string order, bool recursive = false);


## VFS
XPtr<tiledb::VFS>         tiledb_vfs(XPtr<tiledb::Context> ctx, Nullable<XPtr<tiledb::Config>> config=R_NilValue);
std::string               tiledb_vfs_create_bucket(XPtr<tiledb::VFS> vfs, std::string uri);
std::string               tiledb_vfs_remove_bucket(XPtr<tiledb::VFS> vfs, std::string uri);
bool                      tiledb_vfs_is_bucket(XPtr<tiledb::VFS> vfs, std::string uri);
bool                      tiledb_vfs_is_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri);
std::string               tiledb_vfs_empty_bucket(XPtr<tiledb::VFS> vfs, std::string uri);
std::string               tiledb_vfs_create_dir(XPtr<tiledb::VFS> vfs, std::string uri);
bool                      tiledb_vfs_is_dir(XPtr<tiledb::VFS> vfs, std::string uri);
std::string               tiledb_vfs_remove_dir(XPtr<tiledb::VFS> vfs, std::string uri);
bool                      tiledb_vfs_is_file(XPtr<tiledb::VFS> vfs, std::string uri);
std::string               tiledb_vfs_remove_file(XPtr<tiledb::VFS> vfs, std::string uri);
R_xlen_t                  tiledb_vfs_file_size(XPtr<tiledb::VFS> vfs, std::string uri);
std::string               tiledb_vfs_move_file(XPtr<tiledb::VFS> vfs, std::string old_uri, std::string new_uri);
std::string               tiledb_vfs_move_dir(XPtr<tiledb::VFS> vfs, std::string old_uri, std::string new_uri);
std::string               tiledb_vfs_touch(XPtr<tiledb::VFS> vfs, std::string uri);


## Stats
void                      libtiledb_stats_enable();
void                      libtiledb_stats_disable();
void                      libtiledb_stats_dump(std::string path);
void                      libtiledb_stats_print();
