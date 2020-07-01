## -*- mode: autoconf; autoconf-indentation: 4; -*-

##  MIT License
##
##  Copyright (c) 2017-2020 TileDB Inc.
##
##  Permission is hereby granted, free of charge, to any person obtaining a copy
##  of this software and associated documentation files (the "Software"), to deal
##  in the Software without restriction, including without limitation the rights
##  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
##  copies of the Software, and to permit persons to whom the Software is
##  furnished to do so, subject to the following conditions:
##
##  The above copyright notice and this permission notice shall be included in all
##  copies or substantial portions of the Software.
##
##  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
##  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
##  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
##  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
##  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
##  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
##  SOFTWARE.

## require at least autoconf 2.69
AC_PREREQ([2.69])
AC_INIT([TileDB-R], [0.7.1])

## this has to come early to affect other queries involving R
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

## Can we use pkg-config?
AC_PATH_PROG(have_pkg_config, pkg-config, no)
## If yes, also check for whether pkg-config knows tiledb
if test x"${have_pkg_config}" != x"no"; then
    AC_MSG_CHECKING([pkg-config knows TileDB])
    if pkg-config --exists tiledb; then
        AC_MSG_RESULT([yes])
        tiledb_include=$(pkg-config --cflags tiledb)
        tiledb_libs=$(pkg-config --libs tiledb)
        AC_SUBST([TILEDB_INCLUDE], "${tiledb_include}")
        AC_SUBST([TILEDB_LIBS],    "${tiledb_libs}")
        AC_SUBST([TILEDB_RPATH],   "")
    else
        AC_MSG_RESULT([no])
        have_pkg_config="no"
    fi
fi
## Otherwise use fallback values
if test x"${have_pkg_config}" = x"no"; then
    ## Default values suitable for system install, overridde as needed below
    AC_SUBST([TILEDB_INCLUDE], "")
    AC_SUBST([TILEDB_LIBS], "-ltiledb")
    AC_SUBST([TILEDB_RPATH], "")
fi

## Top-level system, possible values SunOS (Solaris), Darwin (macOS), Linux, ...
## Note that SunOS is not currently building; we would welcome help from anyone
## with access to such a system.
## Various Linux distribution have been tested and work, as do various macOS releases
uname=`uname`
#AC_MSG_RESULT([running on ${uname} (${on_macos})...])

## Allow forced build override, value of 'yes' or 'no' set in enable_building variable
## (and sets by autoconf standards the 'enable_building' variable to 'yes' or 'no')
AC_ARG_ENABLE([building],
          AC_HELP_STRING([--enable-building],
                         [build TileDB library locally instead of downloading prebuilt]))
          #[building="${enable_building}"
          # AC_MSG_RESULT([Enabled chosen])],
          #[building="${enable_building}"
          # AC_MSG_RESULT([Enabled NOT chosen])]
          #)

## Allow --with-tiledb if given (though system location is found too)
## (and sets by autconf standards the 'with_tiledb' variable
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

## On Solaris can only build from source
## But this does not currently work so we will bail out with error instead
## This can be overridden by selecting --enable-building but is not default
#if test x"${uname}" = x"SunOS"; then
#     enable_building="yes"
#fi

## to test one can force some selections here
#have_tiledb="no"
#enable_building="yes"
if test x"${have_tiledb}" = x"no"; then

    ## case one: build is selected (or forced) so try building TileDB
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
        tools/buildTileDB.sh

    ## case two: download considered
    ##   - on macOS or Linux a 'fast and slim' build with external library
    ##   - otherwise error
    else
        if test x"${uname}" = x"Linux" -o x"${uname}" = x"Darwin"; then
            #AC_DEFUN([AC_PROG_CURL], [AC_CHECK_PROG(have_curl,curl,yes)])
            #AC_PROG_CURL
            #if test x"${have_curl}" != x"yes"; then
            #    AC_MSG_ERROR([please install 'curl'])
            #fi
            AC_MSG_RESULT([downloading TileDB library...])

            osrel=`tools/getOsRelease.sh`
            AC_MSG_RESULT([installing TileDB for ${osrel}...])
            if test x"${uname}" = x"Darwin"; then
                os="macos"
            else
                os="linux"
            fi
            cd inst && ../tools/fetchTileDB.sh "${os}" && cd ..

        ## default case: be unhappy
        else
            AC_MSG_ERROR([currently unsupported system ${uname}])
        fi

    fi

    #AC_SUBST([TILEDB_INCLUDE], "-Itiledb-inst/include")
    AC_MSG_RESULT([using inst/tiledb/{lib,include}])
    RPATH=" -Wl,-rpath,'\$\$ORIGIN/../tiledb/lib'"
    AC_SUBST([TILEDB_INCLUDE], "-I../inst/tiledb/include")
    AC_SUBST([TILEDB_LIBS], "-ltiledb -L../inst/tiledb/lib")
    AC_SUBST([TILEDB_RPATH], "${RPATH}")

    CPPFLAGS="${origCPPFLAGS} ${TILEDB_INCLUDE}"
    LDFLAGS="${origLDFLAGS} ${TILEDB_LIBS} ${TILEDB_RPATH}"

fi

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
