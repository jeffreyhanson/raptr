all: clean man site vignette check

clean:
	rm -rf docs/*
	rm -rf inst/doc/*
	rm -rf vignettes/*

man:
	R --slave -e "devtools::document()"

site:
	R -e "devtools::load_all()"
	R -e "devtools::document()"
	cp -f inst/vign/raptr.Rmd vignettes/raptr.Rmd
	R -e "devtools::build_vignettes()"
	R -e "pkgdown::build_home()"
	R -e "pkgdown::build_reference()"
	R -e "pkgdown::build_news()"
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

test:
	echo "\n===== UNIT TESTS =====\n" > test.log 2>&1
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf
	rm -f gurobi.log

check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -d "valgrind --tool=memcheck" -e "devtools::check()" >> check.log 2>&1

checkwb:
	R --slave -e "devtools::build_win()"

build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "devtools::install_local('../prioritizr')"

.PHONY: clean mean site man test check checkwb build install
