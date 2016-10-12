all: compile move rmd2md

clean:
	rm -rf vignettes/raptr.Rmd
	rm -rf vignettes/figures

compile:
	cd inst/vign;\
	Rscript -e "knitr::knit('raptr.Rmd')"

move:
	mv inst/vign/raptr.md vignettes;\
	mv -r inst/vign/figure vignettes/figure/

rmd2md:
	cd vignettes;\
	mv raptr.md raptr.Rmd

