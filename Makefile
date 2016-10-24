all: build

clean:
	rm -rf vignettes/raptr.Rmd
	rm -rf vignettes/figures

compile:
	cd inst/vign;\
	Rscript -e "knitr::knit('raptr.Rmd')"

move: compile
	mv inst/vign/raptr.md vignettes;\
	rm -rf vignettes/figure
	mv inst/vign/figure vignettes

rmd: move
	cd vignettes;\
	mv raptr.md raptr.Rmd

build: rmd
	R -e "devtools::load_all()"
	R -e "devtools::document()"
	R -e "devtools::build_vignettes()"
	R -e "staticdocs::build_site()"

