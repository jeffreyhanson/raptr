all: build

clean:
	rm -rf inst/doc/raptr.Rmd
	rm -rf inst/doc/raptr.R
	rm -rf inst/doc/raptr.html

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
	R -e "staticdocs::build_site()"

post: build
	rm -rf vignettes/figure
	rm -f vignettes/raptr.Rmd
	cp inst/vign/placeholder.Rmd vignettes/raptr.Rmd
