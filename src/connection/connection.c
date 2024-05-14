
// borrowed from https://github.com/r-lib/archive/blob/aaf6341b674b974382574c16723b0a46f7ccd50c/src/connection/connection.c

#include "connection.h"

SEXP new_connection(const char* description,
                    const char* mode,
                    const char* class_name,
                    Rconnection* ptr) {
    return R_new_custom_connection(description, mode, class_name, ptr);
}

size_t read_connection(SEXP connection, void* buf, size_t n) {
    return R_ReadConnection(R_GetConnection(connection), buf, n);
}
