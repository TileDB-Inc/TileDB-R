// borrowed from https://github.com/r-lib/archive/blob/aaf6341b674b974382574c16723b0a46f7ccd50c/src/connection/connection.h

#pragma once

#include "Rinternals.h"

// clang-format off
#ifdef __clang__
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wkeyword-macro"
#endif
#define class class_name
#define private private_ptr
#include <R_ext/Connections.h>
#undef class
#undef private
#ifdef __clang__
# pragma clang diagnostic pop
#endif
// clang-format on

#ifdef __cplusplus
extern "C" {
#endif

SEXP new_connection(const char* description,
                    const char* mode,
                    const char* class_name,
                    Rconnection* ptr);

size_t read_connection(SEXP connection, void* buf, size_t n);

#ifdef __cplusplus
}
#endif
