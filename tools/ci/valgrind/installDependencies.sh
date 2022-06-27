#!/bin/bash

set -u

echo "::group::Set up apt"
# Refresh repo content
apt update -qq

# Install packages for add-apt-repository
export DEBIAN_FRONTEND=noninteractive
# Install with recommends to get ca-certificates as well
apt install --yes wget
# marutter key
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran.asc
echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/" > /etc/apt/sources.list.d/cranubuntu.list
# edd key and cranapt / r2u for CRAN binaries
wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc | tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc
echo "deb [arch=amd64] https://dirk.eddelbuettel.com/cranapt jammy main" > /etc/apt/sources.list.d/cranapt.list
# edd launchpad key and launchpad PPA for AWS packages
wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_launchpad_ppa_key.asc | tee -a /etc/apt/trusted.gpg.d/eddppa_key.asc
echo "deb [arch=amd64] http://ppa.launchpad.net/edd/misc/ubuntu jammy main" > /etc/apt/sources.list.d/eddppa.list

# Refresh repo content metadata again
apt update -qq
echo "::endgroup::"


echo "::group::Install Binary Packages"
# Install and skip recommended packages
apt install --yes --no-install-recommends \
    cmake \
    git \
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
    python3-dbus \
    python3-apt \
    r-base-dev \
    r-cran-littler \
    valgrind

apt clean

# littler helpers (currently only use install.r)
for script in install.r installGithub.r build.r rcc.r; do
    ln -s /usr/lib/R/site-library/littler/examples/${script} /usr/local/bin/
done
echo "::endgroup::"


echo "::group::Install BSPM"
Rscript -e 'install.packages("bspm")'
export RHOME=$(R RHOME)
echo "suppressMessages(bspm::enable())" >> ${RHOME}/etc/Rprofile.site
echo "options(bspm.sudo=TRUE)" >> ${RHOME}/etc/Rprofile.site

## Pinning for cran2apt to resolve some package r-cran-* package versions issues in the distro
echo "Package: *" > /etc/apt/preferences.d/99cranapt
echo "Pin: release o=CRAN-Apt Project" >> /etc/apt/preferences.d/99cranapt
echo "Pin: release l=CRAN-Apt Packages" >> /etc/apt/preferences.d/99cranapt
echo "Pin-Priority: 700"  >> /etc/apt/preferences.d/99cranapt
echo "::endgroup::"


echo "::group::Install R Packages"
# This relies on bspm and installs binaries (i.e. r-cran-* packages)
install.r \
    bit64 \
    curl \
    data.table \
    Matrix \
    nanotime \
    nycflights13 \
    palmerpenguins \
    Rcpp \
    simplermarkdown \
    tibble \
    tinytest \
    zoo
echo "::endgroup::"
