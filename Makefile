all: rmd html

clean:
	rm -rf vignettes/raptr.Rmd
	rm -rf vignettes/figures

compile:
	cd inst/vign;\
	Rscript -e "knitr::knit('raptr.Rmd')"

move: compile
	mv inst/vign/raptr.md vignettes;\
	mv inst/vign/figure vignettes/figure/

rmd: move
	cd vignettes;\
	mv raptr.md raptr.Rmd

html: rmd
	cd vignettes;\
	Rscript -e "render('raptr.Rmd')"
	mv vignettes/raptr.html
	mv inst/doc/raptr.html
