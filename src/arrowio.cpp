//  MIT License
//
//  Copyright (c) 2020-2024 TileDB Inc.
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


void array_xptr_set_schema(SEXP array_xptr, SEXP schema_xptr); // forward declaration, see below
SEXP array_xptr_get_schema(SEXP array_xptr);
inline void exitIfError(const ArrowErrorCode ec, const std::string& msg);

// [[Rcpp::export]]
nanoarrowXPtr libtiledb_query_export_buffer(XPtr<tiledb::Context> ctx,
                                            XPtr<tiledb::Query> query,
                                            std::string& name) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Query>(query);

    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    auto schemaxp = nanoarrow_schema_owning_xptr();
    auto sch = nanoarrow_output_schema_from_xptr(schemaxp);
    auto arrayxp = nanoarrow_array_owning_xptr();
    auto arr = nanoarrow_output_array_from_xptr(arrayxp);

    adapter.export_buffer(name.c_str(), arr, sch);
    spdl::debug(tfm::format("[libtiledb_query_export_buffer] name '%s'", name.c_str()));

    // Nanoarrow special: stick schema into xptr tag to return single SEXP
    array_xptr_set_schema(arrayxp, schemaxp); 			// embed schema in array
    return arrayxp;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_import_buffer(XPtr<tiledb::Context> ctx,
                                                  XPtr<tiledb::Query> query,
                                                  std::string& name,
                                                  nanoarrowXPtr naptr) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Query>(query);
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    // get schema xptr out of array xptr tag
    auto schptr = array_xptr_get_schema(naptr);

    adapter.import_buffer(name.c_str(),
                          (struct ArrowArray*) R_ExternalPtrAddr(naptr),
                          (struct ArrowSchema*) R_ExternalPtrAddr(schptr));

    return(query);
}

// [[Rcpp::export]]
Rcpp::List libtiledb_query_export_arrow_table(XPtr<tiledb::Context> ctx,
                                              XPtr<tiledb::Query> query,
                                              std::vector<std::string> names) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Query>(query);
    size_t ncol = names.size();
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    auto schemaxp = nanoarrow_schema_owning_xptr();
    auto sch = nanoarrow_output_schema_from_xptr(schemaxp);
    exitIfError(ArrowSchemaInitFromType(sch, NANOARROW_TYPE_STRUCT), "Bad schema init");
    exitIfError(ArrowSchemaSetName(sch, ""), "Bad schema name");
    exitIfError(ArrowSchemaAllocateChildren(sch, ncol), "Bad schema children alloc");

    auto arrayxp = nanoarrow_array_owning_xptr();
    auto arr = nanoarrow_output_array_from_xptr(arrayxp);
    exitIfError(ArrowArrayInitFromType(arr, NANOARROW_TYPE_STRUCT), "Bad array init");
    exitIfError(ArrowArrayAllocateChildren(arr, ncol), "Bad array children alloc");

    arr->length = 0;
    for (size_t i=0; i<ncol; i++) {
        spdl::debug(tfm::format("[libtiledb_query_export_arrow_table] Accessing %s at %d", names[i], i));

        adapter.export_buffer(names[i].c_str(), arr->children[i], sch->children[i]);

        if (arr->children[i]->length > arr->length) {
            spdl::info(tfm::format("[libtiledb_query_export_arrow_table] Setting array length to %d", arr->children[i]->length));
            arr->length = arr->children[i]->length;
        }
        spdl::info(tfm::format("[libtiledb_query_export_arrow_table] Seeing %s (%s) at length %d null_count %d buffers %d",
                               names[i], sch->children[i]->format, arr->children[i]->length, arr->children[i]->null_count, arr->children[i]->n_buffers));
    }
    Rcpp::List as = Rcpp::List::create(Rcpp::Named("array_data") = arrayxp,
                                       Rcpp::Named("schema") = schemaxp);
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

// Rcpp::XPtr<ArrowSchema> schema_setup_struct(Rcpp::XPtr<ArrowSchema> schxp, int64_t n_children) {
//     ArrowSchema* schema = schxp.get();
//     auto type = NANOARROW_TYPE_STRUCT;

//     ArrowSchemaInit(schema);    					// modified from ArrowSchemaInitFromType()
//     int result = localArrowSchemaSetType(schema, type); // modified to call func with XPtr
//     if (result != NANOARROW_OK) {
//         schema->release(schema);
//         Rcpp::stop("Error setting struct schema");
//     }

//     // now adapted from ArrowSchemaAllocateChildren
//     if (schema->children != NULL) Rcpp::stop("Error allocation as children not null");

//     if (n_children > 0) {
//         auto ptr = (struct ArrowSchema**) ArrowMalloc(n_children * sizeof(struct ArrowSchema*));
//         Rcpp::XPtr<ArrowSchema*> schema_ptrxp = make_xptr(ptr, false);
//         schema->children = schema_ptrxp.get();
//         if (schema->children == NULL) Rcpp::stop("Failed to allocate ArrowSchema*");

//         schema->n_children = n_children;
//         memset(schema->children, 0, n_children * sizeof(struct ArrowSchema*));

//         for (int64_t i = 0; i < n_children; i++) {
//             schema->children[i] = schema_owning_xptr();
//             if (schema->children[i] == NULL) Rcpp::stop("Error allocation schema child %ld", i);
//             schema->children[i]->release = NULL;
//         }
//     }
//     return schxp;
// }

// Rcpp::XPtr<ArrowArray> array_setup_struct(Rcpp::XPtr<ArrowArray> arrxp, int64_t n_children) {
//     ArrowArray* array = arrxp.get();
//     auto storage_type = NANOARROW_TYPE_STRUCT;

//     array->length = 0;
//     array->null_count = 0;
//     array->offset = 0;
//     array->n_buffers = 0;
//     array->n_children = 0;
//     array->buffers = NULL;
//     array->children = NULL;
//     array->dictionary = NULL;
//     array->release = &ArrowArrayReleaseInternal;
//     array->private_data = NULL;

//     auto private_data = (struct ArrowArrayPrivateData*) ArrowMalloc(sizeof(struct ArrowArrayPrivateData));
//     if (private_data == NULL) {
//         array->release = NULL;
//         Rcpp::stop("Error allocating array private data");
//     }
//     ArrowBitmapInit(&private_data->bitmap);
//     ArrowBufferInit(&private_data->buffers[0]);
//     ArrowBufferInit(&private_data->buffers[1]);
//     private_data->buffer_data[0] = NULL;
//     private_data->buffer_data[1] = NULL;
//     private_data->buffer_data[2] = NULL;
//     array->private_data = private_data;
//     array->buffers = (const void**)(&private_data->buffer_data);
//     int result = ArrowArraySetStorageType(array, storage_type);
//     if (result != NANOARROW_OK) {
//         array->release(array);
//         Rcpp::stop("Error setting array storage type");
//     }

//     ArrowLayoutInit(&private_data->layout, storage_type);
//     // We can only know this not to be true when initializing based on a schema so assume this to be true.
//     private_data->union_type_id_is_child_index = 1;


//     // remainder from ArrowArrayAllocateChildren()
//     if (array->children != NULL) Rcpp::stop("Error allocating array children as pointer not null");

//     if (n_children == 0) {
//         return arrxp;
//     }

//     auto ptr = (struct ArrowArray**) ArrowMalloc(n_children * sizeof(struct ArrowArray*));
//     Rcpp::XPtr<ArrowArray*> array_ptrxp = make_xptr(ptr, false);
//     array->children = array_ptrxp.get();
//     if (array->children == NULL) Rcpp::stop("Failed to allocated ArrayArray*");

//     memset(array->children, 0, n_children * sizeof(struct ArrowArray*));

//     for (int64_t i = 0; i < n_children; i++) {
//         array->children[i] = array_owning_xptr();
//         if (array->children[i] == NULL) Rcpp::stop("Error allocation array child %ld", i);
//         array->children[i]->release = NULL;
//     }
//     array->n_children = n_children;
//     return arrxp;
// }

inline void exitIfError(const ArrowErrorCode ec, const std::string& msg) {
    if (ec != NANOARROW_OK) Rcpp::stop(msg);
}

// Attaches a schema to an array external pointer. The nanoarrow R package
// attempts to do this whenever possible to avoid misinterpreting arrays.
void array_xptr_set_schema(SEXP array_xptr, SEXP schema_xptr) {
    R_SetExternalPtrTag(array_xptr, schema_xptr);
}
// Reverse: peel the schema out of the array via the XPtr tag
SEXP array_xptr_get_schema(SEXP array_xptr) {
    return R_ExternalPtrTag(array_xptr);
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
