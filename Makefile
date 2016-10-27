all: move

clean:
	rm -rf inst/doc/*
	rm -rf vignettes/*

compile:
	R -e "devtools::install_local('../raptr')"
	R -e "devtools::load_all()"
	R -e "devtools::document()"
	cd inst/vign;\
	R -e "knitr::knit('raptr.Rmd')";\
	rm -rf cache

move:
	mv inst/vign/raptr.md vignettes/raptr.Rmd
	mv inst/vign/figure vignettes

build: compile
	R -e "devtools::build_vignettes()"
	R -e "staticdocs::build_site()"

post: compile
	rm -rf vignettes/figure
	cp -f inst/vign/placeholder.Rmd vignettes/raptr.Rmd
	touch inst/doc/raptr.R
	touch inst/doc/raptr.Rmd
	touch inst/doc/raptr.html
