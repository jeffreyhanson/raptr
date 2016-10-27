all: site vignette check build

clean:
	rm -rf docs/*
	rm -rf inst/doc/*
	rm -rf vignettes/*

site:
	R -e "devtools::install_local('../raptr')"
	R -e "devtools::load_all()"
	R -e "devtools::document()"
	cp -f inst/vign/raptr.Rmd vignettes/raptr.Rmd
	R -e "staticdocs::build_site()"
	rm -rf vignettes/*
	rm -rf inst/doc/*

vignette:
	cd inst/vign;\
	R -e "knitr::knit('raptr.Rmd')";\
	rm -rf cache
	mv inst/vign/raptr.md vignettes/raptr.Rmd
	mv inst/vign/figure vignettes
	R -e "devtools::build_vignettes()"
	rm -rf vignettes/figure
	cp -f inst/vign/placeholder.Rmd vignettes/raptr.Rmd
	touch inst/doc/raptr.R
	touch inst/doc/raptr.Rmd
	touch inst/doc/raptr.html

check:
	R -e "devtools::check()"
	R -e "devtools::build_win()"

build:
	R -e "devtools::build()"
