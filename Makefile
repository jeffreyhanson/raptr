all: move rmd2md

move:
	cp inst/vign/raptr.md vignettes;\
	cp -r inst/vign/figure vignettes/figure/

rmd2md:
	cd vignettes;\
	mv raptr.md raptr.Rmd

