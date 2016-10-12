all: move rmd2md

vignettes:
	cd inst/vign;\
	Rscript -e "library(knitr);knit('raptr.Rmd')"

move:
	cp inst/vign/raptr.md vignettes;\
	cp -r inst/vign/figure vignettes/figure/

rmd2md:
	cd vignettes;\
	mv raptr.md raptr.Rmd

