AC_PREREQ([2.69])
AC_INIT([TileDB-R], [0.1.0])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}" ; then
    echo "could not determine R_HOME"
    exit 1
fi

CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXX1XFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_LANG(C++)
AC_REQUIRE_CPP
AC_PROG_CXX

AS_IF([test $(uname) == "Darwin"],
            [
             TILEDB_RPATH="-Wl,-rpath,/usr/local/lib"
            ],
            [
             TILEDB_RPATH=""
            ])

AC_SUBST([TILEDB_INCLUDE], "")
AC_SUBST([TILEDB_LIBS], "-ltiledb")

AC_ARG_WITH([tiledb],
            AC_HELP_STRING([--with-tiledb=PREFIX],
                           [path to where TileDB is installed]),
            [
             TILEDB_INCLUDE="-I${with_tiledb}/include ${TILEDB_INCLUDE}"
             TILEDB_LIBS="-L${with_tiledb}/lib ${TILEDB_LIBS}"
             TILEDB_RPATH="-Wl,-rpath,${with_tiledb}/lib"
            ],
            [])

CPPFLAGS="${CPPFLAGS} ${TILEDB_INCLUDE}"
TILEDB_LIBS="${TILEDB_LIBS} ${TILEDB_RPATH}"
LDFLAGS="${LDFLAGS} ${TILEDB_LIBS}"

# check for tiledb C-api header
AC_CHECK_HEADER([tiledb/tiledb.h],
    [],
    [AC_MSG_ERROR([unable to find tiledb/tiledb.h])])

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT