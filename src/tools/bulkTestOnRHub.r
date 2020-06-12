#!/usr/bin/env Rscript

library(rhub)

selected <- c("debian-clang-devel",
              "debian-gcc-devel",
              "debian-gcc-release",
              "fedora-clang-devel",
              "fedora-gcc-devel",
              "ubuntu-gcc-devel",
              "ubuntu-gcc-release",
              "macos-highsierra-release",
              "macos-highsierra-release-cran")

check(".", platform = selected, email = getOption("email", "edd@debian.org"))


## just FYI on 2020-05-20 platform() returns
##q
## R> rhub::platforms()
## debian-clang-devel:
##   Debian Linux, R-devel, clang, ISO-8859-15 locale
## debian-gcc-devel:
##   Debian Linux, R-devel, GCC
## debian-gcc-devel-nold:
##   Debian Linux, R-devel, GCC, no long double
## debian-gcc-patched:
##   Debian Linux, R-patched, GCC
## debian-gcc-release:
##   Debian Linux, R-release, GCC
## fedora-clang-devel:
##   Fedora Linux, R-devel, clang, gfortran
## fedora-gcc-devel:
##   Fedora Linux, R-devel, GCC
## linux-x86_64-centos6-epel:
##   CentOS 6, stock R from EPEL
## linux-x86_64-centos6-epel-rdt:
##   CentOS 6 with Redhat Developer Toolset, R from EPEL
## linux-x86_64-rocker-gcc-san:
##   Debian Linux, R-devel, GCC ASAN/UBSAN
## macos-highsierra-release:
##   macOS 10.13.6 High Sierra, R-release, brew
## macos-highsierra-release-cran:
##   macOS 10.13.6 High Sierra, R-release, CRAN's setup
## solaris-x86-patched:
##   Oracle Solaris 10, x86, 32 bit, R-release
## solaris-x86-patched-ods:
##   Oracle Solaris 10, x86, 32 bit, R-release, Oracle Developer Studio 12.6
## ubuntu-gcc-devel:
##   Ubuntu Linux 16.04 LTS, R-devel, GCC
## ubuntu-gcc-release:
##   Ubuntu Linux 16.04 LTS, R-release, GCC
## ubuntu-rchk:
##   Ubuntu Linux 16.04 LTS, R-devel with rchk
## windows-x86_64-devel:
##   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
## windows-x86_64-oldrel:
##   Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
## windows-x86_64-patched:
##   Windows Server 2008 R2 SP1, R-patched, 32/64 bit
## windows-x86_64-release:
##   Windows Server 2008 R2 SP1, R-release, 32/64 bit
## R>
