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
AS_IF([test $(uname) == "Darwin"],
            [on_macos="yes"
             TILEDB_RPATH="-Wl,-rpath,/usr/local/lib"],
            [on_macos="no"
             TILEDB_RPATH=""])
AC_SUBST([TILEDB_INCLUDE], "")
AC_SUBST([TILEDB_LIBS], "-ltiledb")

## Allow forced build override, value of 'yes' or 'no' set in enable_building variable
AC_ARG_ENABLE([building],
	      AC_HELP_STRING([--enable-building],
	                     [build TileDB library locally instead of downloading prebuilt]))
	      #[building="${enable_building}"
	      # AC_MSG_RESULT([Enabled chosen])],
	      #[building="${enable_building}"],
	      # AC_MSG_RESULT([Enabled NOT chosen])],
	      #)

## Allow --with-tiledb if given (though system location is found too)
AC_ARG_WITH([tiledb],
            AC_HELP_STRING([--with-tiledb=PREFIX],
	                   [path to installed built of TileDB]),
            [TILEDB_INCLUDE="-I${with_tiledb}/include ${TILEDB_INCLUDE}"
             TILEDB_LIBS="-L${with_tiledb}/lib ${TILEDB_LIBS}"
             TILEDB_RPATH="-Wl,-rpath,${with_tiledb}/lib"],
            [])
#echo "enable_building: ${enable_building}"
#echo "with_tiledb: ${with_tiledb}"
## Sanity check
if test ! -z "${with_tiledb}" -a x"${enable_building}" = x"yes"; then
    AC_MSG_ERROR([Conflicting choice of preinstalled TileDB and enabling building TileDB.])
fi    

## Setup
origCPPFLAGS="${CPPFLAGS}"
origLDFLAGS="${LDFLAGS}"
CPPFLAGS="${origCPPFLAGS} ${TILEDB_INCLUDE}"
LDFLAGS="${origLDFLAGS} ${TILEDB_LIBS} ${TILEDB_RPATH}"

## Check for TileDB -- either just probing the header, or by compilations (~ 1 second)
checkTileDBViaHeader="yes"

## Use either one of the tests
if test x"${checkTileDBViaHeader}" = x"yes"; then
    ## simply check for tiledb C-api header
    AC_CHECK_HEADER([tiledb/tiledb.h], [have_tiledb="yes"], [have_tiledb="no"])
else
    ## formally check we can build
    AC_MSG_CHECKING([compilation with TileDB])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <tiledb/tiledb>
                                        tiledb::Context ctx;])],
                                       [have_tiledb="yes"; AC_MSG_RESULT(yes); ],
                                       [have_tiledb="no"; AC_MSG_RESULT(no, need to install TileDB)])
fi

## to test one can force some selection here
#have_tiledb="no"
#enable_building="yes"
if test x"${have_tiledb}" = x"no"; then
    if test x"${on_macos}" = x"yes"; then
        AC_MSG_RESULT([installing TileDB... -- TBD for macOS])
    else
        osrel=`src/tools/getOsRelease.sh`
	AC_MSG_RESULT([running on ${osrel}])
	if test x"${enable_building}" = x"yes"; then
	    AC_DEFUN([AC_PROG_CMAKE], [AC_CHECK_PROG(have_cmake,cmake,yes)])
            AC_PROG_CMAKE
            if test x"${have_cmake}" != x"yes"; then
                AC_MSG_ERROR([please install 'cmake'])
            fi
	    AC_DEFUN([AC_PROG_GIT], [AC_CHECK_PROG(have_git,git,yes)])
            AC_PROG_GIT
            if test x"${have_git}" != x"yes"; then
                AC_MSG_ERROR([please install 'git'])
            fi
            AC_MSG_RESULT([installing TileDB via building locally...])
	    src/tools/buildTileDB.sh 
	else
	    AC_DEFUN([AC_PROG_CURL], [AC_CHECK_PROG(have_curl,curl,yes)])
	    AC_PROG_CURL
	    if test x"${have_curl}" != x"yes"; then
	           AC_MSG_ERROR([please install 'curl'])
	    fi
            AC_MSG_RESULT([downloading TileDB library...])
            cd inst && ../src/tools/fetchTileDB.sh && cd ..
        fi
	
        AC_SUBST([TILEDB_INCLUDE], "-Itiledb-inst/include")
        AC_MSG_RESULT([using inst/tiledb/{lib,include}])
        RPATH=" -Wl,-rpath,'\$\$ORIGIN/../tiledb/lib'"
        AC_SUBST([TILEDB_INCLUDE], "-I../inst/tiledb/include")
        AC_SUBST([TILEDB_LIBS], "-ltiledb -L../inst/tiledb/lib")
        AC_SUBST([TILEDB_RPATH], "${RPATH}")

        CPPFLAGS="${origCPPFLAGS} ${TILEDB_INCLUDE}"
        LDFLAGS="${origLDFLAGS} ${TILEDB_LIBS} ${TILEDB_RPATH}"
    fi
fi

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
