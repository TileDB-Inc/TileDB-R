
## This Makefile is not included in the package sources so we can use GNUmake idioms. Yay.
## Rbuildignore excludes this Makefile, and the generated Rmd files

mdsources := 		$(wildcard *.md)
rmdfile := 		$(mdsources:.md=.Rmd)

%.Rmd:			%.md
			Rscript -e 'simplermarkdown::mdweave("$<", "$@")'

all:			${rmdfile}
