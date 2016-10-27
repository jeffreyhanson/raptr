all: move

clean:
	rm -rf inst/doc/raptr.Rmd
	rm -rf inst/doc/raptr.R
	rm -rf inst/doc/raptr.html

compile:
	R -e "devtools::install_local('../raptr')"
	R -e "devtools::load_all()"
	R -e "devtools::document()"
	cp inst/vign/raptr.Rmd vignette/raptr.Rmd
	R -e "devtools::build_vignettes()"
	R -e "staticdocs::build_site()"
	
move: compile
	cp -f inst/vign/placeholder.Rmd vignettes/raptr.Rmd
	cp -f vignettes/raptr.md inst/doc/raptr.Rmd
	rm inst/doc/raptr.R
	touch inst/doc/raptr.R
	touch inst/doc/raptr.Rmd
	touch inst/doc/raptr.html
