#!/usr/bin/env Rscript
##
## also see bulkTestOnRHub.r

library(rhub)

argv <- commandArgs(trailingOnly=TRUE)
tgt <- if (length(argv) > 0) argv[1] else "."

selected <- c("debian-gcc-release",
              "macos-highsierra-release",
              "windows-x86_64-release")

check(tgt, platform = selected, email = getOption("email", "edd@debian.org"))
