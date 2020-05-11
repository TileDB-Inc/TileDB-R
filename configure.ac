AC_PREREQ([2.69])
AC_INIT([TileDB-R], [0.6.0])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    echo "could not determine R_HOME"
    exit 1
fi

CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXX11FLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_LANG(C++)
AC_REQUIRE_CPP
AC_PROG_CXX

## Default values
AS_IF([test $(uname) == "Darwin"], [TILEDB_RPATH="-Wl,-rpath,/usr/local/lib"], [TILEDB_RPATH=""])
AC_SUBST([TILEDB_INCLUDE], "")
AC_SUBST([TILEDB_LIBS], "-ltiledb")

## Reflect --with-tiledb if give
AC_ARG_WITH([tiledb],
            AC_HELP_STRING([--with-tiledb=PREFIX], [path to where TileDB is installed]),
            [TILEDB_INCLUDE="-I${with_tiledb}/include ${TILEDB_INCLUDE}"
             TILEDB_LIBS="-L${with_tiledb}/lib ${TILEDB_LIBS}"
             TILEDB_RPATH="-Wl,-rpath,${with_tiledb}/lib"],
            [])

## Setup
origCPPFLAGS="${CPPFLAGS}"
origLDFLAGS="${LDFLAGS}"
CPPFLAGS="${origCPPFLAGS} ${TILEDB_INCLUDE}"
LDFLAGS="${origLDFLAGS} ${TILEDB_LIBS} ${TILEDB_RPATH}"

## check for tiledb C-api header
#AC_CHECK_HEADER([tiledb/tiledb.h], [], [AC_MSG_ERROR([unable to find tiledb/tiledb.h])])

## check we can build
AC_MSG_CHECKING([compilation with TileDB])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <tiledb/tiledb>
                                    tiledb::Context ctx;])],
                  [have_tiledb=1; AC_MSG_RESULT(yes); ],
                  [have_tiledb=0; AC_MSG_RESULT(no, need to install TileDB)])

if test x"${have_tiledb}" = x"0"; then
    AC_DEFUN([AC_PROG_CMAKE], [AC_CHECK_PROG(have_cmake,cmake,yes)])
    AC_PROG_CMAKE
    if test x"${have_cmake}" != x"yes"; then
       AC_MSG_ERROR([please install 'cmake'])
    fi
    AC_MSG_RESULT([** Installing TileDB])
    cd src && tools/installTileDB.sh && cd -
    AC_SUBST([TILEDB_INCLUDE], "-Itiledb-inst/include")
    ## omitting libbz2.a and libz.a we R provides them
    if test -d src/tiledb-inst/lib64; then
        ## checking for lib64 used e.g. on Fedora
        AC_MSG_RESULT([** Now using src/tiledb-inst/lib64/])
        AC_SUBST([TILEDB_LIBS], "tiledb-inst/lib64/libtiledb.a tiledb-inst/lib64/libtbb.a tiledb-inst/lib64/libssl.a tiledb-inst/lib64/libcrypto.a tiledb-inst/lib64/libzstd.a tiledb-inst/lib64/liblz4.a")
    else
        ## default lib used e.g. on Debian and Ubuntu
        AC_MSG_RESULT([** Now using src/tiledb-inst/lib/])
        AC_SUBST([TILEDB_LIBS], "tiledb-inst/lib/libtiledb.a tiledb-inst/lib/libtbb.a tiledb-inst/lib/libssl.a tiledb-inst/lib/libcrypto.a tiledb-inst/lib/libzstd.a tiledb-inst/lib/liblz4.a")
    fi

    CPPFLAGS="${origCPPFLAGS} ${TILEDB_INCLUDE}"
    LDFLAGS="${origLDFLAGS} ${TILEDB_LIBS} ${TILEDB_RPATH}"

    #AC_MSG_CHECKING([compilation with TileDB])
    #AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <tiledb/tiledb>
    #                                tiledb::Context ctx;])],
    #              [AC_MSG_RESULT(yes); ],
    #              [AC_MSG_ERROR(no)])
fi

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
