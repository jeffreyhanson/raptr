.onLoad <- function(libname, pkgname) {
	# check gurobi package installed
	is.GurobiInstalled(verbose=TRUE)
	if (!options()$GurobiInstalled) {
		cat('\n\nType the code \'is.GurobiInstalled()\' to check if Gurobi is succesfully installed.')
	}
}
