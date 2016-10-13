all: rmd

clean:
	rm -rf vignettes/raptr.Rmd
	rm -rf vignettes/figures

compile:
	cd inst/vign;\
	Rscript -e "knitr::knit('raptr.Rmd')"

move: compile
	mv inst/vign/raptr.md vignettes;\
	mv inst/vign/figure vignettes/figure/

md2rmd: move
	cd vignettes;\
	mv raptr.md raptr.Rmd
