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
#include <nanoarrow.h>          // for C interface to Arrow
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
//#include <tiledb/arrowio>
#include "tiledb_arrowio.h"
#endif

#include "column_buffer.h"
#include "arrow_adapter.h"


ArrowSchema* schema_owning_ptr(void) {
  struct ArrowSchema* schema = (struct ArrowSchema*)ArrowMalloc(sizeof(struct ArrowSchema));
  if (schema == nullptr) Rcpp::stop("Failed to allocate ArrowSchema");
  schema->release = NULL;
  spdl::debug("[schema_owning_ptr] created");
  return schema;
}

ArrowArray* array_owning_ptr(void) {
  struct ArrowArray* array = (struct ArrowArray*)ArrowMalloc(sizeof(struct ArrowArray));
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


#if TILEDB_VERSION >= TileDB_Version(2,2,0)
// these functions are local to this compilation unit as is the defintion of Pointer
Pointer<ArrowArray> allocate_arrow_array()   { return {}; }
Pointer<ArrowSchema> allocate_arrow_schema() { return {}; }
void delete_arrow_array(Pointer<ArrowArray> ptr)   { ptr.finalize(); }
void delete_arrow_schema(Pointer<ArrowSchema> ptr) { ptr.finalize(); }
#endif

// [[Rcpp::export(.allocate_arrow_array_as_xptr)]]
SEXP allocate_arrow_array_as_xptr() {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    return allocate_arrow_array();
#else
    return R_NilValue;
#endif
}

// [[Rcpp::export(.allocate_arrow_schema_as_xptr)]]
SEXP allocate_arrow_schema_as_xptr() {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    return allocate_arrow_schema();
#else
    return R_NilValue;
#endif
}

// [[Rcpp::export(.delete_arrow_array_from_xptr)]]
void delete_arrow_array_from_xptr(SEXP sxp) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    Pointer<ArrowArray> ptr(sxp);
    delete_arrow_array(ptr);
#endif
}

// [[Rcpp::export(.delete_arrow_schema_from_xptr)]]
void delete_arrow_schema_from_xptr(SEXP sxp) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    Pointer<ArrowSchema> ptr(sxp);
    delete_arrow_schema(ptr);
#endif
}

// [[Rcpp::export]]
Rcpp::List libtiledb_query_export_buffer(XPtr<tiledb::Context> ctx,
                                         XPtr<tiledb::Query> query,
                                         std::string name) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
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
#else
    Rcpp::stop("This function requires TileDB 2.2.0 or greater.");
    return Rcpp::List::create(R_NilValue, R_NilValue); // not reached
#endif
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_import_buffer(XPtr<tiledb::Context> ctx,
                                                  XPtr<tiledb::Query> query,
                                                  std::string name,
                                                  Rcpp::List arrowpointers) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    adapter.import_buffer(name.c_str(),
                          R_ExternalPtrAddr(arrowpointers[0]),
                          R_ExternalPtrAddr(arrowpointers[1]));
#else
    Rcpp::stop("This function requires TileDB 2.2.0 or greater.");
#endif
    return(query);
}


// [[Rcpp::export]]
Rcpp::List libtiledb_query_export_arrow_table(XPtr<tiledb::Context> ctx,
                                              XPtr<tiledb::Query> query,
                                              std::vector<std::string> names) {
#if TILEDB_VERSION >= TileDB_Version(2,2,0)
    size_t ncol = names.size();
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    ArrowSchema* schemap = schema_owning_ptr();
    ArrowArray* arrayp = array_owning_ptr();
    ArrowSchemaInitFromType(schemap, NANOARROW_TYPE_STRUCT);
    ArrowSchemaAllocateChildren(schemap, ncol);
    ArrowArrayInitFromType(arrayp, NANOARROW_TYPE_STRUCT);
    ArrowArrayAllocateChildren(arrayp, ncol);
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
    SEXP xparray = R_MakeExternalPtr((void*) arrayp, R_NilValue, R_NilValue);
    SEXP xpschema = R_MakeExternalPtr((void*) schemap, R_NilValue, R_NilValue);

    Rcpp::List as = Rcpp::List::create(Rcpp::Named("array_data") = xparray,
                                       Rcpp::Named("schema") = xpschema);
    return as;
#else
    Rcpp::stop("This function requires TileDB (2.2.0 or greater).");
    return R_NilValue; // not reached
#endif
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
    if (array == NULL) Rcpp::stop("Failed to allocate ArrowArray");
    array->release = NULL;
    Rcpp::XPtr<ArrowArray> array_xptr = make_xptr(array, false);
    return array_xptr;
}

// [[Rcpp::export]]
Rcpp::List libtiledb_to_arrow(Rcpp::XPtr<tiledb::ArrayBuffers> ab,
                              Rcpp::XPtr<tiledb::Query> qry) {
    check_xptr_tag<tiledb::ArrayBuffers>(ab);
    check_xptr_tag<tiledb::Query>(qry);
    std::vector<std::string> names = ab->names();
    auto ncol = names.size();
    Rcpp::XPtr<ArrowSchema> schemaxp = schema_owning_xptr();
    Rcpp::XPtr<ArrowArray> arrayxp = array_owning_xptr();
    ArrowSchemaInitFromType((ArrowSchema*)R_ExternalPtrAddr(schemaxp), NANOARROW_TYPE_STRUCT);
    ArrowSchemaAllocateChildren((ArrowSchema*)R_ExternalPtrAddr(schemaxp), ncol);
    ArrowArrayInitFromType((ArrowArray*)R_ExternalPtrAddr(arrayxp), NANOARROW_TYPE_STRUCT);
    ArrowArrayAllocateChildren((ArrowArray*)R_ExternalPtrAddr(arrayxp), ncol);

    arrayxp->length = 0;

    for (size_t i=0; i<ncol; i++) {
        // this allocates, and properly wraps as external pointers controlling lifetime
        Rcpp::XPtr<ArrowSchema> chldschemaxp = schema_owning_xptr();
        Rcpp::XPtr<ArrowArray> chldarrayxp = array_owning_xptr();

        auto buf = ab->at(names[i]);        // buf is a shared_ptr to ColumnBuffer
        buf->update_size(*qry);
        spdl::info(tfm::format("[libtiledb_to_arrow] Accessing %s at %d use_count=%d sz %d nm %s tp %d",
                               names[i], i, buf.use_count(), buf->size(), buf->name(), buf->type()));

        // this is pair of array and schema pointer
        auto pp = tiledb::ArrowAdapter::to_arrow(buf);
        spdl::info(tfm::format("[libtiledb_to_arrow] Incoming name %s length %d",
                               std::string(pp.second->name), pp.first->length));
        memcpy((void*) chldschemaxp, pp.second.get(), sizeof(ArrowSchema));
        memcpy((void*) chldarrayxp, pp.first.get(), sizeof(ArrowArray));
        schemaxp->children[i] = chldschemaxp;
        arrayxp->children[i] = chldarrayxp;

        if (pp.first->length > arrayxp->length) {
            spdl::debug(tfm::format("[libtiledb_to_arrow] Setting array length to %d", pp.first->length));
            arrayxp->length = pp.first->length;
        }
    }
    Rcpp::List as = Rcpp::List::create(Rcpp::Named("array_data") = arrayxp,
                                       Rcpp::Named("schema") = schemaxp);
    return as;
}


// [[Rcpp::export]]
Rcpp::XPtr<tiledb::ArrayBuffers> libtiledb_allocate_column_buffers(Rcpp::XPtr<tiledb::Context> ctx,
                                                                   Rcpp::XPtr<tiledb::Query> qry,
                                                                   std::string uri,
                                                                   std::vector<std::string> names) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Query>(qry);
    // allocate the ArrayBuffers object we will with ColumnBuffer objects and return
    auto abp = new tiledb::ArrayBuffers;
    // get the array and its pointer
    auto arrsp = std::make_shared<tiledb::Array>(*ctx.get(), uri, TILEDB_READ);
    for (auto name: names) {
        // returns a shared pointer to new column buffer for 'name'
        auto cbsp = tiledb::ColumnBuffer::create(arrsp, name);
        abp->emplace(name, cbsp);
        cbsp->attach(*qry.get());
        spdl::debug(tfm::format("[libtiledb_alloocate_column_buffers] emplaced %s cnt %d", name, cbsp.use_count()));
    }
    return make_xptr<tiledb::ArrayBuffers>(abp);
}
