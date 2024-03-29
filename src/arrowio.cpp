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

#include "tiledb_arrowio.h"

#include "column_buffer.h"
#include "arrow_adapter.h"


void _array_xptr_set_schema(SEXP array_xptr, SEXP schema_xptr); // forward declaration, see below
SEXP _array_xptr_get_schema(SEXP array_xptr);
inline void exitIfError(const ArrowErrorCode ec, const std::string& msg);
inline void* _getPtr(SEXP p) { return R_ExternalPtrAddr(p); }

// [[Rcpp::export]]
nanoarrowS3 libtiledb_query_export_buffer(XPtr<tiledb::Context> ctx,
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
    _array_xptr_set_schema(arrayxp, schemaxp); 			// embed schema in array
    return arrayxp;
}

// [[Rcpp::export]]
XPtr<tiledb::Query> libtiledb_query_import_buffer(XPtr<tiledb::Context> ctx,
                                                  XPtr<tiledb::Query> query,
                                                  std::string& name,
                                                  nanoarrowS3 naptr) {
    check_xptr_tag<tiledb::Context>(ctx);
    check_xptr_tag<tiledb::Query>(query);
    tiledb::arrow::ArrowAdapter adapter(ctx, query);

    // get schema xptr out of array xptr tag
    auto schptr = _array_xptr_get_schema(naptr);

    adapter.import_buffer(name.c_str(),
                          (struct ArrowArray*) _getPtr(naptr),
                          (struct ArrowSchema*) _getPtr(schptr));

    return(query);
}

// [[Rcpp::export]]
nanoarrowS3 libtiledb_query_export_arrow_table(XPtr<tiledb::Context> ctx,
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

    // Nanoarrow special: stick schema into xptr tag to return single SEXP
    _array_xptr_set_schema(arrayxp, schemaxp); 			// embed schema in array
    return arrayxp;
}


inline void exitIfError(const ArrowErrorCode ec, const std::string& msg) {
    if (ec != NANOARROW_OK) Rcpp::stop(msg);
}

// Attaches a schema to an array external pointer. The nanoarrow R package
// attempts to do this whenever possible to avoid misinterpreting arrays.
void _array_xptr_set_schema(SEXP array_xptr, SEXP schema_xptr) {
    R_SetExternalPtrTag(array_xptr, schema_xptr);
}
// Reverse: peel the schema out of the array via the XPtr tag
SEXP _array_xptr_get_schema(SEXP array_xptr) {
    return R_ExternalPtrTag(array_xptr);
}

//  was: Rcpp::List
// [[Rcpp::export]]
nanoarrowS3 libtiledb_to_arrow(Rcpp::XPtr<tiledb::ArrayBuffers> ab,
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
    _array_xptr_set_schema(arrayxp, schemaxp); 			// embed schema in array

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

// [[Rcpp::export]]
Rcpp::List nanoarrow2list(nanoarrowS3 naarrptr) {
    auto schptr = _array_xptr_get_schema(naarrptr);
    return Rcpp::List::create(naarrptr, schptr);
}
