#!/bin/bash

set -u

echo "::group::Set up apt"
# Refresh repo content 
apt update -qq

# Install packages for add-apt-repository
export DEBIAN_FRONTEND=noninteractive
apt install --yes --no-install-recommends software-properties-common dirmngr wget

# Get repo signing keys for R at CRAN and cran2deb4ubuntu, and for edd/misc
for key in C9A7585B49D51698710F3A115E25F516B04C661B E298A3A825C0D65DFD57CBB651716619E084DAB9 39A90F2E6D5DCC5DFB39462B67C2D66C4B1D4339; do
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys ${key}
    gpg -a --export ${key} | apt-key add -
done
# Add R builds mirrored at CRAN 
#add-apt-repository --yes "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
add-apt-repository --yes ppa:marutter/rrutter4.0
# Add 'cran2deb4ubuntu' repo with 5k CRAN binaries
add-apt-repository --yes ppa:c2d4u.team/c2d4u4.0+
# Add PPA with some AWS SDK packaging to skip AWS SDK build
add-apt-repository --yes ppa:edd/misc

# Refresh repo content metadata again
apt update -qq
echo "::endgroup::"



echo "::group::Install Binary Packages"
# Install and skip recommended packages
apt install --yes --no-install-recommends \
    git \
    cmake \
    libaws-c-common-dev \
    libaws-c-event-stream-dev \
    libaws-checksums-dev \
    libaws-sdk-cpp-only-s3-dev \
    libcapnp-dev \
    libcurl4-openssl-dev \
    liblz4-dev \
    libspdlog-dev \
    libssl-dev \
    libzstd-dev \
    r-base-dev \
    r-cran-bit64 \
    r-cran-curl \
    r-cran-data.table \
    r-cran-littler \
    r-cran-matrix \
    r-cran-palmerpenguins \
    r-cran-rcpp \
    r-cran-tibble \
    r-cran-tinytest \
    r-cran-zoo \
    valgrind 

apt clean

# littler helpers (currently only use install.r)
for script in install.r installGithub.r build.r rcc.r; do
    ln -s /usr/lib/R/site-library/littler/examples/${script} /usr/local/bin/
done
echo "::endgroup::"



echo "::group::Install R Packages"
# Some R packages not (yet) available as binaries
install.r nanotime nycflights13 simplermarkdown

## Not:
#    r-cran-bookdown \
#    r-cran-knitr \
#    r-cran-rmarkdown \
#    r-cran-testthat \
#    libv8-dev \
echo "::endgroup::"


echo "::group::Install cmake"
# deal with cmake
wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor - | tee /usr/share/keyrings/kitware-archive-keyring.gpg >/dev/null
echo 'deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ focal main' | tee /etc/apt/sources.list.d/kitware.list >/dev/null
apt-get update -qq
apt install --yes --no-install-recommends cmake
echo "::endgroup::"
