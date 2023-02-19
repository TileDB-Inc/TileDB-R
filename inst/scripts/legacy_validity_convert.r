#!/usr/bin/env Rscript

## load docopt and tiledb
suppressMessages({
    library(docopt) 	# for command-line argument parsing and help generation
    library(tiledb)
})

## configuration for docopt
doc <- "Usage: convert.t [-h] [-v] [-d] [-t] [-o OUTDIR | -u] INDIR

-o --out OUTDIR     write converted array into OUTDIR, if not given conversion in place [default: ]
-u --usetmp         do not require output directory and use a temporary directory [default: FALSE]
-t --tolegacy       convert to (instead of from) legacy validity mode
-v --verbose        show extra output while processing
-d --debug          show extra debug information
-h --help           show this help tex
"

opt <- docopt(doc)			# docopt parsing
#if (opt$debug) print(opt)

tiledb:::.legacy_validity(inuri=opt$INDIR,
                          outdir=opt$out,
                          fromlegacy=!opt$tolegacy,
                          tolegacy=opt$tolegacy,
                          usetmp=opt$usetmp,
                          verbose=opt$verbose,
                          debug=opt$debug)
