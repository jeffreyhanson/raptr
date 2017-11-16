all: clean initc man site vignette test check

clean:
	rm -rf docs/*
	rm -rf inst/doc/*


initc:
	R --slave -e "Rcpp::compileAttributes()"
	R --slave -e "tools::package_native_routine_registration_skeleton('.', 'src/init.c', character_only = FALSE)"

man:
	R --slave -e "devtools::document()"

readme:
	R --slave -e "devtools::load_all();rmarkdown::render('README.Rmd')"

site:
	R -e "devtools::load_all();pkgdown::build_site()"

vignette:
	R -e "devtools::load_all();devtools::build_vignettes()"


codoc:
	echo "\n===== CODOC =====\n" > check.log 2>&1
	R --slave -e "devtools::load_all();tools::codoc(dir = ".")" >> check.log 2>&1

test:
	echo "\n===== UNIT TESTS =====\n" > test.log 2>&1
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf
	rm -f gurobi.log

check:
	echo "\n===== R CMD CHECK (valgrind) =====\n" > check.log 2>&1
	R --slave -d "valgrind --tool=memcheck" -e "devtools::check()" >> check.log 2>&1

qcheck:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check()" >> check.log 2>&1

wbcheck:
	R --slave -e "devtools::build_win()"

build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "devtools::install_local('../raptr')"

.PHONY: clean initc readme man site man test check codoc wbcheck qcheck build install
