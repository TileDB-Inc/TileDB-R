#ifndef ARROW_ADAPTER_H
#define ARROW_ADAPTER_H

#include "libtiledb.h"
#include <column_buffer.h>
#include <nanoarrow.h>

// https://arrow.apache.org/docs/format/CDataInterface.html
// https://arrow.apache.org/docs/format/Columnar.html#buffer-listing-for-each-layout
// https://arrow.apache.org/docs/format/CDataInterface.html#exporting-a-simple-int32-array

namespace tiledb {

/**
 * @brief The ArrowBuffer holds a shared pointer to a ColumnBuffer, which
 * manages the lifetime of a ColumnBuffer used to back an Arrow array.
 *
 * The ArrowArray.release callback will delete the ArrowBuffer, and
 * automatically decrement the use count of the ColumnBuffer's shared pointer.
 *
 */
struct ArrowBuffer {
    ArrowBuffer(std::shared_ptr<ColumnBuffer> buffer)
        : buffer_(buffer){};

    std::shared_ptr<ColumnBuffer> buffer_;
};

class ArrowAdapter {
   public:
    static void release_schema(struct ArrowSchema* schema) {
        spdl::debug(tfm::format("[ArrowAdapter] release_schema %s", schema->name));
        schema->release = nullptr;
    }

    static void release_array(struct ArrowArray* array) {
        auto arrow_buffer = static_cast<ArrowBuffer*>(array->private_data);

        spdl::debug(tfm::format(
            "[ArrowAdapter] release_array %s use_count=%d",
            arrow_buffer->buffer_->name(),
            arrow_buffer->buffer_.use_count()));

        // Delete the ArrowBuffer, which was allocated with new.
        // If the ArrowBuffer.buffer_ shared_ptr is the last reference to the
        // underlying ColumnBuffer, the ColumnBuffer will be deleted.
        delete arrow_buffer;

        if (array->buffers != nullptr) {
            free(array->buffers);
        }
        array->release = nullptr;
    }

    // /**
    //  * @brief Convert ColumnBuffer Enumeration to an Arrow array.
    //  *
    //  * @return auto [Arrow array, Arrow schema]
    //  */
    // static auto enumeration_to_arrow(std::shared_ptr<std::vector<std::string>> enmr) {
    //     spdl::warn("[enumeration_to_arrow] entered");
    //     std::unique_ptr<ArrowSchema> schema = std::make_unique<ArrowSchema>();
    //     std::unique_ptr<ArrowArray> array = std::make_unique<ArrowArray>();

    //     schema->format = to_arrow_format(TILEDB_STRING_UTF8).data();  // mandatory
    //     schema->name = nullptr;					                      // optional
    //     schema->metadata = nullptr;                               // optional
    //     schema->flags = 0;                                        // optional
    //     schema->n_children = 0;                                   // mandatory
    //     schema->children = nullptr;                               // optional
    //     schema->dictionary = nullptr;                             // optional
    //     schema->release = &release_schema;                        // mandatory
    //     schema->private_data = nullptr;                           // optional

    //     int n_buffers = 3;      // known constant

    //     auto column = std::make_shared<ColumnBuffer>("", TILEDB_STRING_UTF8,
    //                                                  enmr->size(), enmr->size(), // two placeholders
    //                                                  true, false);
    //     column->from_enumeration(enmr);

    //     // Create an ArrowBuffer to manage the lifetime of `column`.
    //     // - `arrow_buffer` holds a shared_ptr to `column`, which increments
    //     //   the use count and keeps the ColumnBuffer data alive.
    //     // - When the arrow array is released, `array->release()` is called with
    //     //   `arrow_buffer` in `private_data`. `arrow_buffer` is deleted, which
    //     //   decrements the the `column` use count. When the `column` use count
    //     //   reaches 0, the ColumnBuffer data will be deleted.
    //     auto arrow_buffer = new ArrowBuffer(column);

    //     array->length = enmr->size();               // mandatory: length is number of 'levels'
    //     array->null_count = 0;                      // mandatory
    //     array->offset = 0;                          // mandatory
    //     array->n_buffers = n_buffers;               // mandatory
    //     array->n_children = 0;                      // mandatory
    //     array->buffers = nullptr;                   // mandatory
    //     array->children = nullptr;                  // optional
    //     array->dictionary = nullptr;                // optional
    //     array->release = &release_array;            // mandatory
    //     array->private_data = (void*)arrow_buffer;  // mandatory

    //     spdl::debug(tfm::format("[ArrowAdapter::enumeration_array] create dictionary length '%d'",
    //                             array->length));

    //     array->buffers = (const void**)malloc(sizeof(void*) * n_buffers);
    //     assert(array->buffers != nullptr);
    //     array->buffers[0] = nullptr;  // validity
    //     array->buffers[2] = column->data<void*>().data();  // data
    //     array->buffers[1] = column->offsets().data();  // offsets

    //     return std::pair(std::move(array), std::move(schema));
    // }

    /**
     * @brief Convert ColumnBuffer to an Arrow array.
     *
     * @return auto [Arrow array, Arrow schema]
     */
    static auto to_arrow(std::shared_ptr<ColumnBuffer> column) {
        std::unique_ptr<ArrowSchema> schema = std::make_unique<ArrowSchema>();
        std::unique_ptr<ArrowArray> array = std::make_unique<ArrowArray>();

        schema->format = to_arrow_format(column->type()).data();  // mandatory
        schema->name = column->name().data();                     // optional
        schema->metadata = nullptr;                               // optional
        schema->flags = 0;                                        // optional
        schema->n_children = 0;                                   // mandatory
        schema->children = nullptr;                               // optional
        schema->dictionary = nullptr;                             // optional
        schema->release = &release_schema;                        // mandatory
        schema->private_data = nullptr;                           // optional

        int n_buffers = column->is_var() ? 3 : 2;

        // Create an ArrowBuffer to manage the lifetime of `column`.
        // - `arrow_buffer` holds a shared_ptr to `column`, which increments
        //   the use count and keeps the ColumnBuffer data alive.
        // - When the arrow array is released, `array->release()` is called with
        //   `arrow_buffer` in `private_data`. `arrow_buffer` is deleted, which
        //   decrements the the `column` use count. When the `column` use count
        //   reaches 0, the ColumnBuffer data will be deleted.
        auto arrow_buffer = new ArrowBuffer(column);

        array->length = column->size();             // mandatory
        array->null_count = 0;                      // mandatory
        array->offset = 0;                          // mandatory
        array->n_buffers = n_buffers;               // mandatory
        array->n_children = 0;                      // mandatory
        array->buffers = nullptr;                   // mandatory
        array->children = nullptr;                  // optional
        array->dictionary = nullptr;                // optional
        array->release = &release_array;            // mandatory
        array->private_data = (void*)arrow_buffer;  // mandatory

        spdl::debug(tfm::format(
            "[ArrowAdapter] create array name='%s' format='%s' use_count=%d addr=%p",
            column->name(), schema->format, column.use_count(), column->data<void*>().data()));

        array->buffers = (const void**)malloc(sizeof(void*) * n_buffers);
        assert(array->buffers != nullptr);
        array->buffers[0] = nullptr;  // validity
        array->buffers[n_buffers - 1] = column->data<void*>().data();  // data

        if (n_buffers == 3) {
            array->buffers[1] = column->offsets().data();  // offsets
        }

        if (column->is_nullable()) {
            schema->flags |= ARROW_FLAG_NULLABLE;

            // Count nulls
            for (auto v : column->validity()) {
                array->null_count += v == 0;
            }

            // Convert validity bytemap to a bitmap in place
            column->validity_to_bitmap();
            array->buffers[0] = column->validity().data();
        }

        // if (column->has_enumeration()) {
        //     std::shared_ptr<std::vector<std::string>> enmr = column->get_enumeration();
        //     auto pp = enumeration_to_arrow(enmr);
        //     array->dictionary = pp.first.get();
        //     schema->dictionary = pp.second.get();
        //     spdl::warn(tfm::format("[ArrowAdapter::to_arrow] name='%s' enum=%d string=%s",
        //                            column->name(), column->has_enumeration(),
        //                            (char*)array->dictionary->buffers[2]));
        // }


#if TILEDB_VERSION >= TileDB_Version(2,10,0)
        /* Workaround to cast TILEDB_BOOL from uint8 to 1-bit Arrow boolean. */
        if (column->type() == TILEDB_BOOL) {
            column->data_to_bitmap();
        }
#endif

        /* Workaround for Date column with is 32-bit but treated as 64-bit */
        if (column->type() == TILEDB_DATETIME_DAY) {
            column->date_cast();
        }

        return std::pair(std::move(array), std::move(schema));
    }


    /**
     * @brief Get Arrow format string from TileDB datatype.
     *
     * @param datatype TileDB datatype.
     * @return std::string_view Arrow format string.
     */
    static std::string_view to_arrow_format(tiledb_datatype_t datatype) {
        switch (datatype) {
            case TILEDB_STRING_ASCII:
            case TILEDB_STRING_UTF8:
                return "U";  // large because TileDB uses 64bit offsets
            case TILEDB_CHAR:
            case TILEDB_BLOB:
                return "Z";  // large because TileDB uses 64bit offsets
#if TILEDB_VERSION >= TileDB_Version(2,10,0)
            case TILEDB_BOOL:
                return "b";
#endif
            case TILEDB_INT32:
                return "i";
            case TILEDB_INT64:
                return "l";
            case TILEDB_FLOAT32:
                return "f";
            case TILEDB_FLOAT64:
                return "g";
            case TILEDB_INT8:
                return "c";
            case TILEDB_UINT8:
                return "C";
            case TILEDB_INT16:
                return "s";
            case TILEDB_UINT16:
                return "S";
            case TILEDB_UINT32:
                return "I";
            case TILEDB_UINT64:
                return "L";
            case TILEDB_TIME_SEC:
                return "tts";
            case TILEDB_TIME_MS:
                return "ttm";
            case TILEDB_TIME_US:
                return "ttu";
            case TILEDB_TIME_NS:
                return "ttn";
            case TILEDB_DATETIME_SEC:
                return "tss:";
            case TILEDB_DATETIME_MS:
                return "tsm:";
            case TILEDB_DATETIME_US:
                return "tsu:";
            case TILEDB_DATETIME_NS:
                return "tsn:";
            case TILEDB_DATETIME_DAY:
                return "tdD";
            default:
                break;
        }
        Rcpp::stop("ArrowAdapter: Unsupported TileDB datatype: %s ",
            tiledb::impl::type_to_str(datatype));
    }
};

};  // namespace tiledb

#endif
