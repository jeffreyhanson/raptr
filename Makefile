all: build

clean:
	rm -rf vignettes/raptr.Rmd
	rm -rf vignettes/figures

compile:
	R -e "devtools::install_local('../raptr')"
	R -e "devtools::load_all()"
	R -e "devtools::document()"
	cd inst/vign;\
	Rscript -e "knitr::knit('raptr.Rmd')" \
	rm -rf cache

move: compile
	mv inst/vign/raptr.md vignettes;\
	rm -rf vignettes/figure
	mv inst/vign/figure vignettes

rmd: move
	cd vignettes;\
	mv raptr.md raptr.Rmd

build: rmd
	R -e "devtools::build_vignettes()"
	R -e "pkgdown::build_site()"

