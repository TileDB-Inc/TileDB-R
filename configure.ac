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
    AC_MSG_RESULT([installing TileDB...])
    #cd src && tools/installTileDB.sh && cd -
    cd inst && ../src/tools/fetchTileDB.sh && cd ..

    AC_SUBST([TILEDB_INCLUDE], "-Itiledb-inst/include")
    AC_MSG_RESULT([using inst/tiledb/{lib,include}])
    AC_SUBST([TILEDB_INCLUDE], "-I../inst/tiledb/include")
    AC_SUBST([TILEDB_LIBS], "-ltiledb -L../inst/tiledb/lib")
    RPATH=`"${R_HOME}/bin/R" -s -e 'cat(file.path(.libPaths()[[1]], "tiledb"))'`
    #echo "RPATH is ${RPATH}"
    AC_SUBST([TILEDB_RPATH], "${RPATH}")

    CPPFLAGS="${origCPPFLAGS} ${TILEDB_INCLUDE}"
    LDFLAGS="${origLDFLAGS} ${TILEDB_LIBS} ${TILEDB_RPATH}"

fi

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
