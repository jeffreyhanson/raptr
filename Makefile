all: clean contrib initc data docs test check

clean:
	rm -rf man/*
	rm -rf data/*
	rm -rf docs/*
	rm -rf inst/doc/*

initc:
	R --slave -e "Rcpp::compileAttributes()"
	R --slave -e "tools::package_native_routine_registration_skeleton('.', 'src/init.c', character_only = FALSE)"

docs: man readme vigns site

man:
	R --slave -e "devtools::document()"

readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

contrib:
	R --slave -e "rmarkdown::render('CONTRIBUTING.Rmd')"

vigns:
	R --slave -e "devtools::build_vignettes()"
	cp -R doc inst/
	touch inst/doc/.gitkeep

spellcheck:
	R --slave -e "devtools::document();devtools::spell_check()"

data:
	Rscript --slave inst/extdata/data.R

site:
	R --slave -e "pkgdown::clean_site()"
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = FALSE)"
	cp -R doc inst/
	touch inst/doc/.gitkeep

quicksite:
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"
	cp -R doc inst/
	touch inst/doc/.gitkeep

test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

quickcheck:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1
	cp -R doc inst/
	touch inst/doc/.gitkeep

check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1
	cp -R doc inst/
	touch inst/doc/.gitkeep

wbcheck:
	R --slave -e "devtools::check_win_devel()"
	cp -R doc inst/

solarischeck:
	R --slave -e "rhub::check(platform = 'solaris-x86-patched', email = 'jeffrey.hanson@uqconnect.edu.au', show_status = FALSE)"

fedoracheck:
	R --slave -e "rhub::check(platform = 'fedora-gcc-devel', email = 'jeffrey.hanson@uqconnect.edu.au', show_status = FALSE)"

build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "devtools::install_local('../raptr')"

examples:
	R --slave -e "devtools::run_examples(test = TRUE);warnings()"  >> examples.log
	rm -f Rplots.pdf

.PHONY: initc clean docs readme contrib site test check checkwb solarischeck fedoracheck build install man data examples
