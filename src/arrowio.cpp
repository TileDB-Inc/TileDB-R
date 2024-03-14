//  MIT License
//
//  Copyright (c) 2020-2023 TileDB Inc.
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
#include "nanoarrow/r.h"
//#include <nanoarrow.h>          // for C interface to Arrow

//#include <tiledb/arrowio>
#include "tiledb_arrowio.h"

#include "column_buffer.h"
#include "arrow_adapter.h"


ArrowSchema* schema_owning_ptr(void) {
  struct ArrowSchema* schema = new struct ArrowSchema;
  if (schema == nullptr) Rcpp::stop("Failed to allocate ArrowSchema");
  schema->release = NULL;
  spdl::debug("[schema_owning_ptr] created");
  return schema;
}

ArrowArray* array_owning_ptr(void) {
  struct ArrowArray* array = new struct ArrowArray;
  if (array == nullptr) Rcpp::stop("Failed to allocate ArrowArray");
  array->release = NULL;
  spdl::debug("[array_owning_ptr] created");
  return array;
}


// borrowed from arrow R package (version 7.0.0)  (licensed under Apache-2.0) and slightly extended/adapted
template <typename T>
struct Pointer {
    Pointer() : ptr_(new T()) {}
    explicit Pointer(SEXP x) {
        if (TYPEOF(x) == EXTPTRSXP) {
            ptr_ = (T*)R_ExternalPtrAddr(x);

        } else if (TYPEOF(x) == STRSXP && Rf_length(x) == 1) {
            // User passed a character representation of the pointer address
            SEXP char0 = STRING_ELT(x, 0);
            if (char0 == NA_STRING) {
                Rcpp::stop("Can't convert NA_character_ to pointer");
            }

            const char* input_chars = CHAR(char0);
            char* endptr;
            uint64_t ptr_value = strtoull(input_chars, &endptr, 0);
            if (endptr != (input_chars + strlen(input_chars))) {
                Rcpp::stop("Can't parse '%s' as a 64-bit integer address", input_chars);
            }
            ptr_ = reinterpret_cast<T*>(static_cast<uintptr_t>(ptr_value));

        } else if (Rf_inherits(x, "integer64") && Rf_length(x) == 1) {
            // User passed an integer64(1) of the pointer address an integer64 is a REALSXP
            // under the hood, with the bytes of each double reinterpreted as an int64.
            uint64_t ptr_value;
            memcpy(&ptr_value, REAL(x), sizeof(uint64_t));
            ptr_ = reinterpret_cast<T*>(static_cast<uintptr_t>(ptr_value));

        } else if (TYPEOF(x) == RAWSXP && Rf_length(x) == sizeof(T*)) {
            // User passed a raw(<pointer size>) with the literal bytes of the pointer.
            memcpy(&ptr_, RAW(x), sizeof(T*));

        } else if (TYPEOF(x) == REALSXP && Rf_length(x) == 1) {
            // User passed a double(1) of the static-casted pointer address.
            ptr_ = reinterpret_cast<T*>(static_cast<uintptr_t>(REAL(x)[0]));

        } else {
            Rcpp::stop("Can't convert input object to pointer: %d", TYPEOF(x));
        }
    }

    inline operator SEXP() const { return R_MakeExternalPtr(ptr_, R_NilValue, R_NilValue); }

    inline operator T*() const { return ptr_; }

    inline void finalize() { delete ptr_; }

    inline T* get() const { return ptr_; }

    T* ptr_;
};


// these functions are local to this compilation unit as is the defintion of Pointer
Pointer<ArrowArray> allocate_arrow_array()   { return {}; }
Pointer<ArrowSchema> allocate_arrow_schema() { return {}; }
void delete_arrow_array(Pointer<ArrowArray> ptr)   { ptr.finalize(); }
void delete_arrow_schema(Pointer<ArrowSchema> ptr) { ptr.finalize(); }

// [[Rcpp::export(.allocate_arrow_array_as_xptr)]]
SEXP allocate_arrow_array_as_xptr() {
    return allocate_arrow_array();
}

// [[Rcpp::export(.allocate_arrow_schema_as_xptr)]]
SEXP allocate_arrow_schema_as_xptr() {
    return allocate_arrow_schema();
}

// [[Rcpp::export(.delete_arrow_array_from_xptr)]]
void delete_arrow_array_from_xptr(SEXP sxp) {
    Pointer<ArrowArray> ptr(sxp);
    delete_arrow_array(ptr);
}

// [[Rcpp::export(.delete_arrow_schema_from_xptr)]]
void delete_arrow_schema_from_xptr(SEXP sxp) {
    Pointer<ArrowSchema> ptr(sxp);
    delete_arrow_schema(ptr);
}

// [[Rcpp::export]]
Rcpp::List libtiledb_query_export_buffer(XPtr<tiledb::Context> ctx,
                                         XPtr<tiledb::Query> query,
                                         std::string name) {
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    //auto arrptr = allocate_arrow_array(); 	// external pointer object
    //auto schptr = allocate_arrow_schema();
    auto schptr = schema_owning_ptr();
    auto arrptr = array_owning_ptr();
    adapter.export_buffer(name.c_str(),
                          static_cast<void*>(arrptr),
                          static_cast<void*>(schptr));
    spdl::debug(tfm::format("[libtiledb_query_export_buffer] name '%s'", name.c_str()));
    SEXP xpschema = R_MakeExternalPtr((void*) schptr, R_NilValue, R_NilValue);
    SEXP xparray = R_MakeExternalPtr((void*) arrptr, R_NilValue, R_NilValue);
    return Rcpp::List::create(xparray, xpschema);
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_import_buffer(XPtr<tiledb::Context> ctx,
                                                  XPtr<tiledb::Query> query,
                                                  std::string name,
                                                  Rcpp::List arrowpointers) {
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    adapter.import_buffer(name.c_str(),
                          R_ExternalPtrAddr(arrowpointers[0]),
                          R_ExternalPtrAddr(arrowpointers[1]));
    return(query);
}

Rcpp::XPtr<ArrowSchema> schema_owning_xptr(void);
Rcpp::XPtr<ArrowArray> array_owning_xptr(void);
Rcpp::XPtr<ArrowSchema> schema_setup_struct(Rcpp::XPtr<ArrowSchema> schxp, int64_t n_children);
Rcpp::XPtr<ArrowArray> array_setup_struct(Rcpp::XPtr<ArrowArray> arrxp, int64_t n_children);

// [[Rcpp::export]]
Rcpp::List libtiledb_query_export_arrow_table(XPtr<tiledb::Context> ctx,
                                              XPtr<tiledb::Query> query,
                                              std::vector<std::string> names) {
    size_t ncol = names.size();
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    Rcpp::XPtr<ArrowSchema> schemap = schema_owning_xptr();
    Rcpp::XPtr<ArrowArray> arrayp = array_owning_xptr();
    schemap = schema_setup_struct(schemap, ncol);
    arrayp = array_setup_struct(arrayp, ncol);

    arrayp->length = 0;

    for (size_t i=0; i<ncol; i++) {
        // this allocates, and properly wraps as external pointers controlling lifetime
        ArrowSchema* chldschemap = schema_owning_ptr();
        ArrowArray* chldarrayp = array_owning_ptr();

        spdl::debug(tfm::format("[libtiledb_query_export_arrow_table] Accessing %s at %d", names[i], i));
        adapter.export_buffer(names[i].c_str(), (void*) chldarrayp, (void*) chldschemap);

        spdl::debug(tfm::format("[libtiledb_query_export_arrow_table] Setting children %s at %d", names[i], i));
        schemap->children[i] = chldschemap;
        arrayp->children[i] = chldarrayp;

        if (chldarrayp->length > arrayp->length) {
            spdl::info(tfm::format("[libtiledb_query_export_arrow_table] Setting array length to %d", chldarrayp->length));
            arrayp->length = chldarrayp->length;
        }
        spdl::info(tfm::format("[libtiledb_query_export_arrow_table] Seeing %s (%s) at length %d null_count %d buffers %d",
                               names[i], chldschemap->format, chldarrayp->length, chldarrayp->null_count, chldarrayp->n_buffers));

    }

    Rcpp::List as = Rcpp::List::create(Rcpp::Named("array_data") = arrayp,
                                       Rcpp::Named("schema") = schemap);
    return as;
}

//' @noRd
// [[Rcpp::export]]
bool check_arrow_schema_tag(Rcpp::XPtr<ArrowSchema> xp) {
  check_xptr_tag<ArrowSchema>(xp);  // throws if mismatched
  return true;
}

//' @noRd
// [[Rcpp::export]]
bool check_arrow_array_tag(Rcpp::XPtr<ArrowArray> xp) {
  check_xptr_tag<ArrowArray>(xp);  // throws if mismatched
  return true;
}


// (Adapted) helper functions from nanoarrow
//
// Create an external pointer with the proper class and that will release any
// non-null, non-released pointer when garbage collected. We use a tagged XPtr,
// but do not set an XPtr finalizer
Rcpp::XPtr<ArrowSchema> schema_owning_xptr(void) {
    struct ArrowSchema* schema = (struct ArrowSchema*)ArrowMalloc(sizeof(struct ArrowSchema));
    spdl::trace(tfm::format("[schema_owning_xptr] Allocating %d bytes", sizeof(struct ArrowSchema)));
    if (schema == NULL) Rcpp::stop("Failed to allocate ArrowSchema");
    schema->release = NULL;
    Rcpp::XPtr<ArrowSchema> schema_xptr = make_xptr(schema, false);
    return schema_xptr;
}
// Create an external pointer with the proper class and that will release any
// non-null, non-released pointer when garbage collected. We use a tagged XPtr,
// but do not set an XPtr finalizer
Rcpp::XPtr<ArrowArray> array_owning_xptr(void) {
    struct ArrowArray* array = (struct ArrowArray*)ArrowMalloc(sizeof(struct ArrowArray));
    spdl::trace(tfm::format("[array_owning_xptr] Allocating %d bytes", sizeof(struct ArrowArray)));
    if (array == NULL) Rcpp::stop("Failed to allocate ArrowArray");
    array->release = NULL;
    Rcpp::XPtr<ArrowArray> array_xptr = make_xptr(array, false);
    return array_xptr;
}

// Helper function to register a finalizer -- eg for debugging purposes
inline void registerXptrFinalizer(SEXP s, R_CFinalizer_t f, bool onexit = true) {
    R_RegisterCFinalizerEx(s, f, onexit ? TRUE : FALSE);
}
extern "C" {
    void ArrowArrayReleaseInternal(struct ArrowArray *array); 		// made non-static in nanoarrow.c
    ArrowErrorCode ArrowArraySetStorageType(struct ArrowArray* array,	// ditto
                                            enum ArrowType storage_type);
    ArrowErrorCode localArrowSchemaSetType(struct ArrowSchema* schema, enum ArrowType type);
}

Rcpp::XPtr<ArrowSchema> schema_setup_struct(Rcpp::XPtr<ArrowSchema> schxp, int64_t n_children) {
    ArrowSchema* schema = schxp.get();
    auto type = NANOARROW_TYPE_STRUCT;

    ArrowSchemaInit(schema);    					// modified from ArrowSchemaInitFromType()
    int result = localArrowSchemaSetType(schema, type); // modified to call func with XPtr
    if (result != NANOARROW_OK) {
        schema->release(schema);
        Rcpp::stop("Error setting struct schema");
    }

    // now adapted from ArrowSchemaAllocateChildren
    if (schema->children != NULL) Rcpp::stop("Error allocation as children not null");

    if (n_children > 0) {
        auto ptr = (struct ArrowSchema**) ArrowMalloc(n_children * sizeof(struct ArrowSchema*));
        Rcpp::XPtr<ArrowSchema*> schema_ptrxp = make_xptr(ptr, false);
        schema->children = schema_ptrxp.get();
        if (schema->children == NULL) Rcpp::stop("Failed to allocate ArrowSchema*");

        schema->n_children = n_children;
        memset(schema->children, 0, n_children * sizeof(struct ArrowSchema*));

        for (int64_t i = 0; i < n_children; i++) {
            schema->children[i] = schema_owning_xptr();
            if (schema->children[i] == NULL) Rcpp::stop("Error allocation schema child %ld", i);
            schema->children[i]->release = NULL;
        }
    }
    return schxp;
}

Rcpp::XPtr<ArrowArray> array_setup_struct(Rcpp::XPtr<ArrowArray> arrxp, int64_t n_children) {
    ArrowArray* array = arrxp.get();
    auto storage_type = NANOARROW_TYPE_STRUCT;

    array->length = 0;
    array->null_count = 0;
    array->offset = 0;
    array->n_buffers = 0;
    array->n_children = 0;
    array->buffers = NULL;
    array->children = NULL;
    array->dictionary = NULL;
    array->release = &ArrowArrayReleaseInternal;
    array->private_data = NULL;

    auto private_data = (struct ArrowArrayPrivateData*) ArrowMalloc(sizeof(struct ArrowArrayPrivateData));
    if (private_data == NULL) {
        array->release = NULL;
        Rcpp::stop("Error allocating array private data");
    }
    ArrowBitmapInit(&private_data->bitmap);
    ArrowBufferInit(&private_data->buffers[0]);
    ArrowBufferInit(&private_data->buffers[1]);
    private_data->buffer_data[0] = NULL;
    private_data->buffer_data[1] = NULL;
    private_data->buffer_data[2] = NULL;
    array->private_data = private_data;
    array->buffers = (const void**)(&private_data->buffer_data);
    int result = ArrowArraySetStorageType(array, storage_type);
    if (result != NANOARROW_OK) {
        array->release(array);
        Rcpp::stop("Error setting array storage type");
    }

    ArrowLayoutInit(&private_data->layout, storage_type);
    // We can only know this not to be true when initializing based on a schema so assume this to be true.
    private_data->union_type_id_is_child_index = 1;


    // remainder from ArrowArrayAllocateChildren()
    if (array->children != NULL) Rcpp::stop("Error allocating array children as pointer not null");

    if (n_children == 0) {
        return arrxp;
    }

    auto ptr = (struct ArrowArray**) ArrowMalloc(n_children * sizeof(struct ArrowArray*));
    Rcpp::XPtr<ArrowArray*> array_ptrxp = make_xptr(ptr, false);
    array->children = array_ptrxp.get();
    if (array->children == NULL) Rcpp::stop("Failed to allocated ArrayArray*");

    memset(array->children, 0, n_children * sizeof(struct ArrowArray*));

    for (int64_t i = 0; i < n_children; i++) {
        array->children[i] = array_owning_xptr();
        if (array->children[i] == NULL) Rcpp::stop("Error allocation array child %ld", i);
        array->children[i]->release = NULL;
    }
    array->n_children = n_children;
    return arrxp;
}

inline void exitIfError(const ArrowErrorCode ec, const std::string& msg) {
    if (ec != NANOARROW_OK) Rcpp::stop(msg);
}

// Attaches a schema to an array external pointer. The nanoarrow R package
// attempts to do this whenever possible to avoid misinterpreting arrays.
void array_xptr_set_schema(SEXP array_xptr, SEXP schema_xptr) {
    R_SetExternalPtrTag(array_xptr, schema_xptr);
}

//  was: Rcpp::List
// [[Rcpp::export]]
nanoarrowXPtr libtiledb_to_arrow(Rcpp::XPtr<tiledb::ArrayBuffers> ab,
                                 Rcpp::XPtr<tiledb::Query> qry,
                                 Rcpp::List dicts) {
    check_xptr_tag<tiledb::ArrayBuffers>(ab);
    check_xptr_tag<tiledb::Query>(qry);
    std::vector<std::string> names = ab->names();
    auto ncol = names.size();
    std::vector<std::string> dictnames = dicts.names();

    // Schema first
    auto schemaxp = nanoarrow_schema_owning_xptr();
    auto sch = nanoarrow_output_schema_from_xptr(schemaxp);
    exitIfError(ArrowSchemaInitFromType(sch, NANOARROW_TYPE_STRUCT), "Bad schema init");
    exitIfError(ArrowSchemaSetName(sch, ""), "Bad schema name");
    exitIfError(ArrowSchemaAllocateChildren(sch, ncol), "Bad schema children alloc");

    // Array second
    auto arrayxp = nanoarrow_array_owning_xptr();
    auto arr = nanoarrow_output_array_from_xptr(arrayxp);
    exitIfError(ArrowArrayInitFromType(arr, NANOARROW_TYPE_STRUCT), "Bad array init");
    exitIfError(ArrowArrayAllocateChildren(arr, ncol), "Bad array children alloc");

    struct ArrowError ec;

    arr->length = 0;
    for (size_t i=0; i<ncol; i++) {
        bool is_factor = dicts[i] != R_NilValue;
        bool is_ordered = false;
        if (is_factor) {
            Rcpp::CharacterVector cvec = dicts[i];
            is_ordered = cvec.attr("ordered");
        }
        auto buf = ab->at(names[i]);        // buf is a shared_ptr to ColumnBuffer
        buf->update_size(*qry);
        spdl::info(tfm::format("[libtiledb_to_arrow] Accessing %s (%s:%s) at %d use_count=%d sz %d nm %s tp %d",
                               names[i], dictnames[i], (is_factor ? (is_ordered ? "<ordered>" : "<factor>") : ""), i,
                               buf.use_count(), buf->size(), buf->name(), buf->type()));

        // this is pair of array and schema pointer
        auto pp = tiledb::ArrowAdapter::to_arrow(buf);

        spdl::info(tfm::format("[libtiledb_to_arrow] Incoming name %s length %d",
                               std::string(pp.second->name), pp.first->length));
        ArrowArrayMove(pp.first.get(),   arr->children[i]);
        ArrowSchemaMove(pp.second.get(), sch->children[i]);

        if (is_factor) {        // create an arrow array of type string with the labels
            // this could be rewritten if we generalized ColumnBuffer to allow passing of strings
            std::vector<std::string> svec = Rcpp::as<std::vector<std::string>>(dicts[i]);
            auto darrxp = nanoarrow_array_owning_xptr();
            auto darr = nanoarrow_output_array_from_xptr(darrxp);
            exitIfError(ArrowArrayInitFromType(darr, NANOARROW_TYPE_STRING), "Bad string array init");
            exitIfError(ArrowArrayStartAppending(darr), "Bad string array append init");
            auto dschxp = nanoarrow_schema_owning_xptr();
            auto dsch = nanoarrow_output_schema_from_xptr(dschxp);
            exitIfError(ArrowSchemaInitFromType(dsch, NANOARROW_TYPE_STRING), "Bad string schema init");
            exitIfError(ArrowSchemaSetName(dsch, ""), "Bad string schema name");
            if (is_ordered) {
                dsch->flags |= ARROW_FLAG_DICTIONARY_ORDERED; // this line appears ignore
                sch->children[i]->flags |= ARROW_FLAG_DICTIONARY_ORDERED; // this one matters more
            }
            for (auto str: svec) {
                ArrowStringView asv = {str.data(), static_cast<int64_t>(str.size())};
                exitIfError(ArrowArrayAppendString(darr, asv), "Bad string append");
            }
            if (NANOARROW_OK != ArrowArrayFinishBuildingDefault(darr, &ec))
                Rcpp::stop(ec.message);

            spdl::debug(tfm::format("[libtiledb_to_arrow] dict %s fmt %s -- len %d nbuf %d",
                                    names[i], dsch->format, darr->length, darr->n_buffers));
            sch->children[i]->dictionary = dsch;
            arr->children[i]->dictionary = darr;
        }

        if (pp.first->length > arr->length) {
           spdl::debug(tfm::format("[libtiledb_to_arrow] Setting array length to %d", pp.first->length));
           arr->length = pp.first->length;
        }
    }
    spdl::info("[libtiledb_to_arrow] After children loop");
    //if (NANOARROW_OK != ArrowArrayFinishBuildingDefault(arr, &ec))
    //    Rcpp::stop(ec.message);
    spdl::info("[libtiledb_to_arrow] ArrowArrayFinishBuildingDefault");

    // Nanoarrow special: stick schema into xptr tag to return single SEXP
    array_xptr_set_schema(arrayxp, schemaxp); 			// embed schema in array

    spdl::trace("[libtiledb_to_arrow] returning from libtiledb_to_arrow");
    return arrayxp;
}


// [[Rcpp::export]]
Rcpp::XPtr<tiledb::ArrayBuffers> libtiledb_allocate_column_buffers(Rcpp::XPtr<tiledb::Context> ctx,
                                                                   Rcpp::XPtr<tiledb::Query> qry,
                                                                   std::string uri,
                                                                   std::vector<std::string> names,
                                                                   const size_t memory_budget) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Query>(qry);
    // allocate the ArrayBuffers object we will fill with ColumnBuffer objects and return
    auto abp = new tiledb::ArrayBuffers;
    // get the array and its pointer
    auto arrsp = std::make_shared<tiledb::Array>(*ctx.get(), uri, TILEDB_READ);
    for (auto name: names) {
        // returns a shared pointer to new column buffer for 'name'
        spdl::debug(tfm::format("[libtiledb_alloocate_column_buffers] creating %s", name));
        auto cbsp = tiledb::ColumnBuffer::create(arrsp, name, memory_budget, *ctx.get());
        abp->emplace(name, cbsp);
        cbsp->attach(*qry.get());
        spdl::debug(tfm::format("[libtiledb_alloocate_column_buffers] emplaced %s cnt %d membudget %d",
                                name, cbsp.use_count(), memory_budget));
    }
    return make_xptr<tiledb::ArrayBuffers>(abp);
}

// added two local copies to inject xptr for format
extern "C" {
    const char* ArrowSchemaFormatTemplate(enum ArrowType type);  // remove static in nanoarrow
    int ArrowSchemaInitChildrenIfNeeded(struct ArrowSchema* schema, enum ArrowType type); // ditto
}
ArrowErrorCode localArrowSchemaSetFormat(struct ArrowSchema* schema, const char* format) {
    if (schema->format != NULL) {
        ArrowFree((void*)schema->format);
    }

    if (format != NULL) {
        size_t format_size = strlen(format) + 1;
        schema->format = (const char*)ArrowMalloc(format_size);
        if (schema->format == NULL) {
            return ENOMEM;
        }
        Rcpp::XPtr<const char> schema_fmt_xp = make_xptr(schema->format, false);

        memcpy((void*)schema->format, format, format_size);
    } else {
        schema->format = NULL;
    }

    return NANOARROW_OK;
}
ArrowErrorCode localArrowSchemaSetType(struct ArrowSchema* schema, enum ArrowType type) {
    // We don't allocate the dictionary because it has to be nullptr
    // for non-dictionary-encoded arrays.

    // Set the format to a valid format string for type
    const char* template_format = ArrowSchemaFormatTemplate(type);

    // If type isn't recognized and not explicitly unset
    if (template_format == NULL && type != NANOARROW_TYPE_UNINITIALIZED) {
        return EINVAL;
    }

    NANOARROW_RETURN_NOT_OK(localArrowSchemaSetFormat(schema, template_format));

    // For types with an umabiguous child structure, allocate children
    return ArrowSchemaInitChildrenIfNeeded(schema, type);
}
